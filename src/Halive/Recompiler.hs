{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
module Halive.Recompiler where
import Halive.SubHalive
import Halive.FileListener

import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad.Trans
import Control.Monad

data CompilationRequest = CompilationRequest
    { crFilePath         :: FilePath
    , crExpressionString :: String
    , crResultTChan      :: TChan CompilationResult
    }


type CompilationResult = Either [String] CompiledValue

startGHC :: MonadIO m => GHCSessionConfig -> m (TChan CompilationRequest)
startGHC ghcSessionConfig = liftIO $ do
    ghcChan <- newTChanIO

    _ <- forkOS . void . withGHCSession ghcSessionConfig . forever $ do
        CompilationRequest{..} <- readTChanIO ghcChan
        --liftIO . putStrLn $ "SubHalive recompiling: " ++ show (crFilePath, crExpressionString)
        
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

