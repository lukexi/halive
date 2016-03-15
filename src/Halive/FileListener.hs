{-# LANGUAGE LambdaCase #-}
module Halive.FileListener where

import Control.Concurrent
import Control.Concurrent.STM
import qualified System.FSNotify as FSNotify
import System.FSNotify hiding (Event)
import System.Directory
import System.FilePath
import Control.Monad.Trans
import Control.Monad

type FileEventChan = TChan FSNotify.Event

atomicallyIO :: MonadIO m => STM a -> m a
atomicallyIO = liftIO . atomically

readTChanIO :: MonadIO m => TChan a -> m a
readTChanIO = atomicallyIO . readTChan

writeTChanIO :: MonadIO m => TChan a -> a -> m ()
writeTChanIO chan = atomicallyIO . writeTChan chan

tryReadTChanIO :: MonadIO m => TChan a -> m (Maybe a)
tryReadTChanIO = atomicallyIO . tryReadTChan

fileModifiedPredicate :: FilePath -> FSNotify.Event -> Bool
fileModifiedPredicate fileName event = case event of
    Modified path _ -> path == fileName
    _               -> False

eventListenerForFile :: FilePath -> IO FileEventChan
eventListenerForFile fileName = do
    predicate <- fileModifiedPredicate <$> canonicalizePath fileName
    eventChan <- newTChanIO
    _ <- forkIO . withManager $ \manager -> do
        let watchDirectory = takeDirectory fileName
        _stop <- watchTree manager watchDirectory predicate (writeTChanIO eventChan)
        forever (threadDelay 10000000)
    return eventChan

onFileEvent eventChan action = 
    tryReadTChanIO eventChan >>= \case
        Just _  -> action
        Nothing -> return ()