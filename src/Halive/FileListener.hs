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
    , felIgnoreNextEventsNear :: TMVar UTCTime 
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
    predicate        <- fileModifiedPredicate <$> canonicalizePath fileName
    eventChan        <- newTChanIO
    ignoreEventsNear <- newEmptyTMVarIO

    -- If an ignore time is set, ignore file changes for the next 100 ms
    let ignoreTime = 0.1
    _ <- forkIO . withManager $ \manager -> do
        let watchDirectory = takeDirectory fileName
        _stop <- watchTree manager watchDirectory predicate $ \e -> do
            mTimeToIgnore <- atomically $ tryTakeTMVar ignoreEventsNear
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

        forever (threadDelay 10000000)
    return FileEventListener { felEventTChan = eventChan, felIgnoreNextEventsNear = ignoreEventsNear }

setIgnoreTimeNow :: MonadIO m => FileEventListener -> m ()
setIgnoreTimeNow fileEventListener = setIgnoreTime fileEventListener =<< liftIO getCurrentTime

setIgnoreTime :: MonadIO m => FileEventListener -> UTCTime -> m ()
setIgnoreTime FileEventListener{..} time = void . liftIO . atomically $ swapTMVar felIgnoreNextEventsNear time

readFileEvent :: MonadIO m => FileEventListener -> m (Either FSNotify.Event String)
readFileEvent FileEventListener{..} = readTChanIO felEventTChan

onFileEvent :: MonadIO m => FileEventListener -> m () -> m ()
onFileEvent FileEventListener{..} = onTChanRead felEventTChan

onTChanRead :: MonadIO m => TChan a -> m () -> m ()
onTChanRead eventChan action = 
    tryReadTChanIO eventChan >>= \case
        Just _  -> action
        Nothing -> return ()