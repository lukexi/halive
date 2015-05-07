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
import Data.String

main :: IO ()
-- main = recompiler "test.hs" "Test" "doodle"
main = recompiler "glfw.hs" "HotGLFW" "go"

watcherForFiles :: [FSP.FilePath] -> IO (Chan Event)
watcherForFiles watchedFiles = do
    let predicate event = case event of
            Modified path _ -> FSP.filename path `elem` watchedFiles
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

        let dflags2 = dflags1 { hscTarget = HscInterpreted
                              , ghcLink   = LinkInMemory
                              , ghcMode   = CompManager
                              } `gopt_unset` Opt_GhciSandbox
        
        packagesToLink <- setSessionDynFlags dflags2

        (dflags3, _) <- liftIO $ initPackages dflags2

        liftIO $ putStrLn "initDynLinker..."
        liftIO $ initDynLinker dflags3 

        liftIO $ putStrLn "Setting targets..."
        setTargets =<< sequence [guessTarget mainFileName Nothing]

        let recompile = do
                load LoadAllTargets

                liftIO $ linkPackages dflags3 packagesToLink

                modSum <- getModSummary $ mkModuleName mainModuleName
                liftIO . putStrLn $ "Parsing..."
                p <- parseModule modSum
                liftIO . putStrLn $ "Typechecking..."
                _t <- typecheckModule p
                
                setContext [IIModule $ moduleName $ ms_mod modSum]

                rr <- runStmt expression RunToCompletion
                case rr of
                    RunOk _ -> liftIO $ putStrLn "OK"
                    _ -> liftIO $ putStrLn "Error :*("
        -- Start up the app
        recompile
        -- Watch for changes and recompile whenever they occur
        watcher <- liftIO $ watcherForFiles [fromString mainFileName]
        forever . handleSourceError printException $ do
            _ <- liftIO $ readChan watcher
            recompile
