{-# LANGUAGE OverloadedStrings, LambdaCase #-}
import DynFlags
import GHC
-- import Outputable
import Linker
import Packages
import GHC.Paths
import Control.Monad.IO.Class
import Control.Concurrent
import Control.Monad
import SandboxPath
import System.FSNotify
import qualified Filesystem.Path as FSP

main :: IO ()
-- main = recompiler "test.hs" "Test" "doodle"
main = recompiler "glfw.hs" "HotGLFW" "go"

directoryWatcher :: IO (Chan Event)
directoryWatcher = do
    let predicate event = case event of
            Modified path _ -> FSP.extension path == Just "hs"
            _               -> False
    eventChan <- newChan
    forkIO $ withManager $ \manager -> do
        -- start a watching job (in the background)
        let watchDirectory = "."
        watchTreeChan
            manager
            watchDirectory
            predicate
            eventChan
        -- Keep the watcher alive forever
        forever $ threadDelay 10000000

    return eventChan

recompiler :: String -> String -> String -> IO ()
recompiler mainFileName mainModuleName expression = do
    defaultErrorHandler defaultFatalMessager defaultFlushOut $ runGhc (Just libdir) $ do
        
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
                              } `gopt_unset` Opt_GhciSandbox
        
        packagesToLink <- setSessionDynFlags dflags2

        -- Initialize the package database
        (dflags3, _) <- liftIO $ initPackages dflags2

        -- Initialize the dynamic linker
        liftIO $ initDynLinker dflags3 

        -- Set the given filename as a compilation target
        setTargets =<< sequence [guessTarget mainFileName Nothing]

        -- Create a recompile function to call when the file changes
        let recompile = handleSourceError printException $ do
                graph <- depanal [] False

                load LoadAllTargets

                liftIO $ linkPackages dflags3 packagesToLink

                modSum <- getModSummary $ mkModuleName mainModuleName
                liftIO . putStrLn $ "Parsing..."
                p <- parseModule modSum
                liftIO . putStrLn $ "Typechecking..."
                _t <- typecheckModule p
                
                setContext $ map (IIModule . ms_mod_name) graph

                rr <- runStmt expression RunToCompletion
                case rr of
                    RunOk _ -> liftIO $ putStrLn "OK"
                    _ -> liftIO $ putStrLn "Error :*("
        -- Start up the app
        recompile
        -- Watch for changes and recompile whenever they occur
        watcher <- liftIO directoryWatcher
        forever $ do
            _ <- liftIO $ readChan watcher
            recompile
