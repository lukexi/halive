{-# LANGUAGE OverloadedStrings, LambdaCase, ScopedTypeVariables #-}
module Halive where

import GHC
import Linker
import Packages
import DynFlags
import Exception
import GHC.Paths
import Outputable

import Data.IORef
import Control.Monad
import Control.Concurrent
import Control.Monad.IO.Class

import System.FSNotify
import System.FilePath

import FindPackageDBs

directoryWatcher :: IO (Chan Event)
directoryWatcher = do
    let predicate event = case event of
            -- Modified path _ -> takeExtension path `elem` [".hs", ".vert", ".frag", ".pd"]
            Modified path _ -> takeExtension path `elem` [".hs"]
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
recompiler mainFileName importPaths' = do
    mainThreadId <- myThreadId

    {-
    Watcher:
        Tell the main thread to recompile.
        If the main thread isn't done yet, kill it.
    Compiler:
        Wait for the signal to recompile.
        Before recompiling & running, mark that we've started,
        and after we're done running, mark that we're done.
    -}

    -- Checks if the main function has returned, in the case of a batch program
    mainDone      <- newIORef False
    -- Start with a full MVar so we recompile right away.
    recompileLock <- newMVar ()
    -- We don't need to typecheck the first time, since the recompile will do it for us.
    typecheckLock <- newEmptyMVar

    let waitForRecompileSignal = liftIO (takeMVar recompileLock)
        sendRecompileSignal    = liftIO (putMVar recompileLock ())
        writeMainDone          = liftIO . writeIORef mainDone
        checkIfMainIsDone      = liftIO (readIORef mainDone)
        waitForTypecheckSignal = liftIO (takeMVar typecheckLock)
        sendTypecheckSignal    = putMVar typecheckLock ()

    typecheckSuccessful <- newIORef True
    _ <- forkOS . withGHCSession mainFileName importPaths' . forever $ do
        _ <- waitForTypecheckSignal
        liftIO (writeIORef typecheckSuccessful True)
        let onFailure = liftIO (writeIORef typecheckSuccessful False)
        gcatch 
            (typecheckTargets onFailure) 
            (\(_x :: SomeException) -> onFailure)
        success <- liftIO (readIORef typecheckSuccessful)
        when success $ do
            -- If main hasn't already completed running, kill the main thread.
            -- Otherwise, we don't need to do anything
            checkIfMainIsDone >>= \case
                False -> liftIO (killThread mainThreadId)
                True  -> return ()
            
            sendRecompileSignal

    -- Watch for changes and recompile whenever they occur
    watcher <- liftIO directoryWatcher
    _ <- liftIO . forkIO . forever $ do
        -- Block looking for changes in the directory watcher
        _ <- readChan watcher

        -- The main thread waits for our signal to let us know
        -- that the file has chainged again, to keep from 
        -- endlessly trying to recompile a broken file,
        -- or endlessly re-running a working batch program.
        sendTypecheckSignal
       
    
    -- Start up the GHC session that will compile and run the app
    withGHCSession mainFileName importPaths' . forever $ do
        -- Blocks until the file watcher has told us there is something new to do
        _ <- waitForRecompileSignal
        writeMainDone False
        -- We only need this gcatch on Windows, but I don't think
        -- it will hurt on Mac/Linux.
        recompileTargets
        writeMainDone True
        

-- Starts up a GHC session and then runs the given action within it
withGHCSession :: FilePath -> [FilePath] -> Ghc () -> IO ()
withGHCSession mainFileName extraImportPaths action = do
    defaultErrorHandler defaultFatalMessager defaultFlushOut $ runGhc (Just libdir) $ do
        -- Add the main file's path to the import path list
        let mainFilePath   = dropFileName mainFileName
            allImportPaths = mainFilePath:extraImportPaths

        -- Get the default dynFlags
        dflags0 <- getSessionDynFlags
        
        -- If there's a sandbox, add its package DB
        dflags1 <- updateDynFlagsWithCabalSandbox dflags0

        -- If this is a stack project, add its package DBs
        dflags2 <- updateDynFlagsWithStackDB dflags1

        -- Make sure we're configured for live-reload, and turn off the GHCi sandbox
        -- since it breaks OpenGL/GUI usage
        let dflags3 = dflags2 { hscTarget   = HscInterpreted
                              , ghcLink     = LinkInMemory
                              , ghcMode     = CompManager
                              , importPaths = allImportPaths
                              } `gopt_unset` Opt_GhciSandbox
        
        -- We must call setSessionDynFlags before calling initPackages or any other GHC API
        _ <- setSessionDynFlags dflags3

        -- Initialize the package database
        (dflags4, _) <- liftIO (initPackages dflags3)

        -- Initialize the dynamic linker
        liftIO (initDynLinker dflags4)

        -- Set the given filename as a compilation target
        setTargets =<< sequence [guessTarget mainFileName Nothing]

        action

typecheckTargets :: GhcMonad m => m () -> m ()
typecheckTargets onFailure = handleSourceError (\e -> onFailure >> printException e) $ do
    liftIO . putStrLn $ replicate 25 '#' ++ " Typechecking... " ++ replicate 25 '#'
    -- Get the dependencies of the main target
    graph <- depanal [] False

    -- Reload the main target
    loadSuccess <- load LoadAllTargets
    if failed loadSuccess 
        then onFailure
        else 
            -- Parse and typecheck modules to trigger any SourceErrors therein
            forM_ graph (typecheckModule <=< parseModule)

catchExceptions :: ExceptionMonad m => m () -> m ()
catchExceptions a = gcatch a 
    (\(_x :: SomeException) -> return ())

-- Recompiles the current targets
recompileTargets :: Ghc ()
recompileTargets =  catchExceptions $ handleSourceError printException $ do
    liftIO . putStrLn $ replicate 25 '*' ++ " Recompiling... " ++ replicate 25 '*'
    -- Get the dependencies of the main target
    graph <- depanal [] False

    -- Reload the main target
    loadSuccess <- load LoadAllTargets
    unless (failed loadSuccess) $ do
        -- We must parse and typecheck modules before they'll be available for usage
        forM_ graph (typecheckModule <=< parseModule)
        
        -- Load the dependencies of the main target
        setContext $ map (IIModule . ms_mod_name) graph

        -- Run the target file's "main" function
        rr <- runStmt "main" RunToCompletion
        liftIO $ case rr of
            RunOk _ -> 
                putStrLn "OK"
            RunException exception -> 
                print exception
            RunBreak _ _ _ -> 
                putStrLn "Breakpoint"
        


-- A helper from interactive-diagrams to print out GHC API values, 
-- useful while debugging the API.
-- | Outputs any value that can be pretty-printed using the default style
output :: (GhcMonad m, MonadIO m) => Outputable a => a -> m ()
output a = do
    dfs <- getSessionDynFlags
    let style = defaultUserStyle
    let cntx  = initSDocContext dfs style
    liftIO $ print $ runSDoc (ppr a) cntx
