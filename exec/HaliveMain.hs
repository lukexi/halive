{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

import Banner
import System.Environment
import Control.Exception
import Control.Monad
import Control.Concurrent
import Data.IORef
import Control.Concurrent.STM
import Halive.SubHalive
import Halive.Recompiler
import Halive.Args
import System.FilePath

main :: IO ()
main = do
    args <- parseArgs <$> getArgs
    case args of
        Nothing -> putStrLn usage
        Just Args {..} -> do
            let mainFilePath = dropFileName mainFileName
            setEnv "Halive Active" "Yes"
            putStrLn banner
            withArgs targetArgs $
                startRecompiler (fileTypes ++ defaultFileTypes) mainFileName
                    (mainFilePath:includeDirs)
                    shouldCompile

defaultFileTypes :: [FileType]
defaultFileTypes = ["hs", "pd", "frag", "vert"]

printBanner :: String -> IO ()
printBanner title = putStrLn $ ribbon ++ " " ++ title ++ " " ++ ribbon
    where ribbon = replicate 25 '*'

startRecompiler :: [FileType] -> FilePath -> [FilePath] -> Bool -> IO b
startRecompiler fileTypes mainFileName includeDirs shouldCompile = do
    ghc <- startGHC
        (defaultGHCSessionConfig
            { gscImportPaths = includeDirs
            , gscCompilationMode = if shouldCompile then Compiled else Interpreted
            })

    recompiler <- recompilerWithConfig ghc RecompilerConfig
        { rccWatchAll = Just (".", fileTypes)
        , rccExpressions = ["main"]
        , rccFilePath = mainFileName
        }

    mainThreadId <- myThreadId

    newCodeTChan <- newTChanIO
    isMainRunning <- newIORef False
    _ <- forkIO $ forever $ do
        result <- atomically $ readTChan (recResultTChan recompiler)
        case result of
            Left errors -> do
                printBanner "Compilation Errors, Waiting...     "
                putStrLn errors
            Right values -> do
                printBanner "Compilation Success, Relaunching..."
                case values of
                    [newCode] -> do
                        atomically $ writeTChan newCodeTChan newCode
                        mainIsRunning <- readIORef isMainRunning
                        when mainIsRunning $ killThread mainThreadId
                    _ ->
                        error "Unexpected number of values received on recResultTChan"

    forever $ do
        newCode <- atomically $ readTChan newCodeTChan
        case getCompiledValue newCode of
            Just (mainFunc :: IO ()) -> do
                writeIORef isMainRunning True
                mainFunc `catch` (\x ->
                    putStrLn ("App killed: " ++ show (x :: SomeException)))
                writeIORef isMainRunning False
            Nothing -> do
                putStrLn "main was not of type IO ()"
