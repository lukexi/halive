{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}

module Halive.FileListener where

import Control.Concurrent
import Control.Concurrent.STM
import qualified System.FSNotify as FSNotify
import System.FSNotify hiding (Event)
import System.Directory
import System.FilePath
import Control.Monad.Trans
import Control.Monad
import Data.Time

type FileEventChan = TChan FSNotify.Event

data ShouldReadFile = ReadFileOnEvents | JustReportEvents deriving (Eq, Show)

data FileEventListener = FileEventListener
    { felEventTChan           :: TChan (Either FSNotify.Event String)
    , felIgnoreNextEventsNear :: TVar (Maybe UTCTime)
    , felStopLock             :: MVar ()
    }

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

eventListenerForFile :: (MonadIO m) => FilePath -> ShouldReadFile -> m FileEventListener
eventListenerForFile fileName shouldReadFile = liftIO $ do
    eventChan        <- newTChanIO
    ignoreEventsNear <- newTVarIO Nothing

    stopLock <- forkEventListenerThread fileName shouldReadFile eventChan ignoreEventsNear

    return FileEventListener
        { felEventTChan = eventChan
        , felIgnoreNextEventsNear = ignoreEventsNear
        , felStopLock = stopLock
        }

killFileEventListener :: (MonadIO m) => FileEventListener -> m ()
killFileEventListener eventListener = liftIO $ putMVar (felStopLock eventListener) ()


forkEventListenerThread fileName shouldReadFile eventChan ignoreEventsNear = do
    predicate        <- fileModifiedPredicate <$> canonicalizePath fileName
    -- If an ignore time is set, ignore file changes for the next 100 ms
    let ignoreTime = 0.1
    stopLock <- newEmptyMVar
    _ <- forkIO . withManager $ \manager -> do
        let watchDirectory = takeDirectory fileName

        stop <- watchTree manager watchDirectory predicate $ \e -> do
            mTimeToIgnore <- atomically $ readTVar ignoreEventsNear
            let timeOfEvent = eventTime e
                shouldIgnore = case mTimeToIgnore of
                    Nothing -> False
                    Just timeToIgnore -> abs (timeOfEvent `diffUTCTime` timeToIgnore) < ignoreTime
            unless shouldIgnore $ do
                if (shouldReadFile == ReadFileOnEvents)
                    then do
                        fileContents <- readFile fileName
                        let !len = length fileContents
                        writeTChanIO eventChan (Right fileContents)
                    else writeTChanIO eventChan (Left e)

        () <- takeMVar stopLock
        stop
    return stopLock

setIgnoreTimeNow :: MonadIO m => FileEventListener -> m ()
setIgnoreTimeNow fileEventListener = setIgnoreTime fileEventListener =<< liftIO getCurrentTime

setIgnoreTime :: MonadIO m => FileEventListener -> UTCTime -> m ()
setIgnoreTime FileEventListener{..} time = void . liftIO . atomically $ writeTVar felIgnoreNextEventsNear (Just time)

readFileEvent :: MonadIO m => FileEventListener -> m (Either FSNotify.Event String)
readFileEvent FileEventListener{..} = readTChanIO felEventTChan

onFileEvent :: MonadIO m => FileEventListener -> m () -> m ()
onFileEvent FileEventListener{..} = onTChanRead felEventTChan

onTChanRead :: MonadIO m => TChan a -> m () -> m ()
onTChanRead eventChan action =
    tryReadTChanIO eventChan >>= \case
        Just _  -> action
        Nothing -> return ()
