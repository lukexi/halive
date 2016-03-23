{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
module Halive.Recompiler where
import Halive.SubHalive
import Halive.FileListener

import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad.Trans
import Control.Monad
import System.FSNotify
import System.Directory
import System.FilePath

data CompilationRequest = CompilationRequest
    { crFilePath         :: FilePath
    , crExpressionString :: String
    , crResultTChan      :: TChan CompilationResult
    }


type CompilationResult = Either [String] CompiledValue

startGHC :: MonadIO m => [FilePath] -> [FilePath] -> m (TChan CompilationRequest)
startGHC importPaths_ packageDBs = liftIO $ do
    ghcChan <- newTChanIO

    _ <- forkOS . void . withGHCSession importPaths_ packageDBs DebounceFix . forever $ do
        CompilationRequest{..} <- readTChanIO ghcChan
        liftIO . putStrLn $ "Compilation request!"
        
        result <- recompileExpressionInFile crFilePath crExpressionString
        writeTChanIO crResultTChan result
    return ghcChan




recompilerForExpression :: MonadIO m => (TChan CompilationRequest) -> FilePath -> String -> m (TChan CompilationResult)
recompilerForExpression ghcChan filePath expressionString = liftIO $ do

    resultTChan <- newTChanIO
    let compilationRequest = CompilationRequest 
            { crFilePath         = filePath
            , crExpressionString = expressionString 
            , crResultTChan      = resultTChan
            }

    fileEventListener <- eventListenerForFile filePath
    
    -- Compile immediately
    writeTChanIO ghcChan compilationRequest

    _ <- forkIO . forever $ do
        _ <- readTChanIO fileEventListener
        writeTChanIO ghcChan compilationRequest

    return resultTChan

