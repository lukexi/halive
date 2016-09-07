{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import System.FilePath


separateArgs :: [String] -> ([String], [String])
separateArgs args = (haliveArgs, drop 1 targetArgs)
  where (haliveArgs, targetArgs) = break (== "--") args

main :: IO ()
main = do
  (args, targetArgs) <- separateArgs <$> getArgs
  case args of
    [] -> putStrLn "Usage: halive <main.hs> <include dir> [-- <args to myapp>]"
    (mainFileName:includeDirs) -> do
      let mainFilePath = dropFileName mainFileName
      setEnv "Halive Active" "Yes"
      putStrLn banner
      withArgs targetArgs $ startRecompiler mainFileName (mainFilePath:includeDirs)

startRecompiler :: FilePath -> [FilePath] -> IO b
startRecompiler mainFileName includeDirs = do
    ghc <- startGHC
        (defaultGHCSessionConfig
            { gscImportPaths = includeDirs
            , gscCompilationMode = Compiled
            })
    recompiler <- recompilerForExpression ghc mainFileName "main" True

    mainThreadId <- myThreadId

    newCodeTChan <- newTChanIO
    isMainRunning <- newIORef False
    _ <- forkIO $ forever $ do
        result <- atomically $ readTChan (recResultTChan recompiler)
        case result of
            Left errors -> putStrLn (concat errors)
            Right newCode -> do
                atomically $ writeTChan newCodeTChan newCode
                mainIsRunning <- readIORef isMainRunning
                when mainIsRunning $ killThread mainThreadId

    forever $ do
        newCode <- atomically $ readTChan newCodeTChan
        case getCompiledValue newCode of
            Just mainFunc -> do
                writeIORef isMainRunning True
                (mainFunc :: IO ()) `catch` (\(x :: SomeException) ->
                    putStrLn ("App killed: " ++ show x))
                writeIORef isMainRunning False
            Nothing -> do
                putStrLn "main was not of type IO ()"

