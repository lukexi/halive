{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module Halive where

import DynFlags
import GHC
import Outputable
import Linker
import Packages
import GHC.Paths
import Control.Monad.IO.Class
import Control.Concurrent
import Control.Monad
import Data.IORef
import SandboxPath
import System.FSNotify
import System.FilePath
import qualified Filesystem.Path as FSP

directoryWatcher :: IO (Chan Event)
directoryWatcher = do
    let predicate event = case event of
            Modified path _ -> FSP.extension path `elem` map Just ["hs", "vert", "frag"]
            _               -> False
    eventChan <- newChan
    _ <- forkIO $ withManager $ \manager -> do
        -- start a watching job (in the background)
        let watchDirectory = "."
        _stopListening <- watchTreeChan
            manager
            watchDirectory
            predicate
            eventChan
        -- Keep the watcher alive forever
        forever $ threadDelay 10000000

    return eventChan



recompiler :: FilePath -> [FilePath] -> IO ()
recompiler mainFileName importPaths' = withGHCSession mainFileName importPaths' $ do
    mainThreadId <- liftIO myThreadId

    {-
    Watcher:
        Tell the main thread to recompile.
        If the main thread isn't done yet, kill it.
    Compiler:
        Wait for the signal to recompile.
        Before recompiling & running, mark that we've started,
        and after we're done running, mark that we're done.
    -}

    mainDone  <- liftIO $ newIORef False
    -- Start with a full MVar so we recompile right away.
    recompile <- liftIO $ newMVar ()

    -- Watch for changes and recompile whenever they occur
    watcher <- liftIO directoryWatcher
    _ <- liftIO . forkIO . forever $ do
        _ <- readChan watcher
        putMVar recompile ()
        mainIsDone <- readIORef mainDone
        unless mainIsDone $ killThread mainThreadId
    
    -- Start up the app
    forever $ do
        _ <- liftIO $ takeMVar recompile
        liftIO $ writeIORef mainDone False
        recompileTargets
        liftIO $ writeIORef mainDone True
        


withGHCSession :: FilePath -> [FilePath] -> Ghc () -> IO ()
withGHCSession mainFileName importPaths' action = do
    defaultErrorHandler defaultFatalMessager defaultFlushOut $ runGhc (Just libdir) $ do
        -- Add the main file's path to the import path list
        let mainFilePath = dropFileName mainFileName
            importPaths'' = mainFilePath:importPaths'

        -- Get the default dynFlags
        dflags0 <- getSessionDynFlags
        
        -- If there's a sandbox, add its package DB
        dflags1 <- liftIO getSandboxDb >>= \case
            Nothing -> return dflags0
            Just sandboxDB -> do
                let pkgs = map PkgConfFile [sandboxDB]
                return dflags0 { extraPkgConfs = (pkgs ++) . extraPkgConfs dflags0 }

        -- Make sure we're configured for live-reload, and turn off the GHCi sandbox
        -- since it breaks OpenGL/GUI usage
        let dflags2 = dflags1 { hscTarget = HscInterpreted
                              , ghcLink   = LinkInMemory
                              , ghcMode   = CompManager
                              , importPaths = importPaths''
                              } `gopt_unset` Opt_GhciSandbox
        
        -- We must set dynflags before calling initPackages or any other GHC API
        _ <- setSessionDynFlags dflags2

        -- Initialize the package database
        (dflags3, _) <- liftIO $ initPackages dflags2

        -- Initialize the dynamic linker
        liftIO $ initDynLinker dflags3 

        -- Set the given filename as a compilation target
        setTargets =<< sequence [guessTarget mainFileName Nothing]

        action

-- Recompiles the current targets
recompileTargets :: Ghc ()
recompileTargets = handleSourceError printException $ do
    graph <- depanal [] False

    _success <- load LoadAllTargets

    -- We must parse and typecheck modules before they'll be available for usage
    forM_ graph (typecheckModule <=< parseModule)
    
    setContext $ map (IIModule . ms_mod_name) graph

    rr <- runStmt "main" RunToCompletion
    case rr of
        RunOk _ -> liftIO $ putStrLn "OK"
        RunException exception -> liftIO $ print exception
        RunBreak _ _ _ -> liftIO $ putStrLn "Breakpoint"


-- A helper from interactive-diagrams to print out GHC API values, 
-- useful while debugging the API.
-- | Outputs any value that can be pretty-printed using the default style
output :: (GhcMonad m, MonadIO m) => Outputable a => a -> m ()
output a = do
    dfs <- getSessionDynFlags
    let style = defaultUserStyle
    let cntx  = initSDocContext dfs style
    liftIO $ print $ runSDoc (ppr a) cntx