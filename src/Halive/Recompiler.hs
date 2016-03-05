{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
module Halive.Recompiler where
import Halive.SubHalive
import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad.Trans
import Control.Monad
import System.FSNotify
import System.Directory

atomicallyIO :: MonadIO m => STM a -> m a
atomicallyIO = liftIO . atomically

readTChanIO :: MonadIO m => TChan a -> m a
readTChanIO = atomicallyIO . readTChan

writeTChanIO :: MonadIO m => TChan a -> a -> m ()
writeTChanIO chan = atomicallyIO . writeTChan chan

tryReadTChanIO :: MonadIO m => TChan a -> m (Maybe a)
tryReadTChanIO = atomicallyIO . tryReadTChan

data CompilationRequest = CompilationRequest
    { crFilePath         :: FilePath
    , crExpressionString :: String
    , crResultTChan      :: TChan CompilationResult
    }


type CompilationResult = Either [String] CompiledValue

startGHC :: MonadIO m => [FilePath] -> m (TChan CompilationRequest)
startGHC importPaths_ = liftIO $ do
    ghcChan <- newTChanIO

    _ <- forkOS . void . withGHCSession importPaths_ DebounceFix . forever $ do
        CompilationRequest{..} <- readTChanIO ghcChan
        
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
        _ <- readChan fileEventListener
        writeTChanIO ghcChan compilationRequest

    return resultTChan


fileModifiedPredicate :: FilePath -> Event -> Bool
fileModifiedPredicate fileName event = case event of
    Modified path _ -> path == fileName
    _               -> False

eventListenerForFile :: FilePath -> IO (Chan Event)
eventListenerForFile fileName = do
    predicate <- fileModifiedPredicate <$> canonicalizePath fileName
    eventChan <- newChan
    _ <- forkIO . withManager $ \manager -> do
        let watchDirectory = "."
        _stop <- watchTreeChan manager watchDirectory predicate eventChan
        forever (threadDelay 10000000)
    return eventChan
