{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

module Halive.FileListener where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad
import           Control.Monad.Trans
import           Data.IORef
import           Data.List              (isInfixOf)
import           Data.Time
import           System.Directory
import           System.FilePath
import           System.FSNotify        hiding (Event)
import qualified System.FSNotify        as FSNotify

type FileEventChan = TChan FSNotify.Event

data ShouldReadFile = ReadFileOnEvents | JustReportEvents deriving (Eq, Show)

data FileEventListener = FileEventListener
    { felEventTChan           :: TChan (Either FSNotify.Event String)
    , felIgnoreNextEventsNear :: TVar (Maybe UTCTime)
    , felStopMVar             :: MVar ()
    }

atomicallyIO :: MonadIO m => STM a -> m a
atomicallyIO = liftIO . atomically

readTChanIO :: MonadIO m => TChan a -> m a
readTChanIO = atomicallyIO . readTChan

writeTChanIO :: MonadIO m => TChan a -> a -> m ()
writeTChanIO chan = atomicallyIO . writeTChan chan

tryReadTChanIO :: MonadIO m => TChan a -> m (Maybe a)
tryReadTChanIO = atomicallyIO . tryReadTChan

peekTChanIO :: MonadIO m => TChan a -> m a
peekTChanIO = atomicallyIO . peekTChan

exhaustTChan :: TChan a -> STM [a]
exhaustTChan chan = unfoldM (tryReadTChan chan)

exhaustTChanIO :: MonadIO m => TChan a -> m [a]
exhaustTChanIO = atomicallyIO . exhaustTChan

-- A version of exhaustTChan that blocks until there is something to read
waitExhaustTChan :: TChan a -> STM [a]
waitExhaustTChan chan = peekTChan chan >> exhaustTChan chan

waitExhaustTChanIO :: MonadIO m => TChan a -> m [a]
waitExhaustTChanIO = atomicallyIO . waitExhaustTChan

-- | Take a monadic stream returning Maybes and
-- pull a list from it until it returns Nothing
unfoldM :: Monad m => m (Maybe a) -> m [a]
unfoldM f = f >>= \case
    Just a  -> (a:) <$> unfoldM f
    Nothing -> return []

fileModifiedPredicate :: FilePath -> FSNotify.Event -> Bool
fileModifiedPredicate fileName event = case event of
    Modified path _ -> path == fileName
    Added    path _ -> path == fileName
    _               -> False

-- Returns True if the event filepath is a common editor file
isACommonEditorFile :: FSNotify.Event -> Bool
isACommonEditorFile event = case event of
    Modified path _ -> any (`isInfixOf` path) emacsFragments
    _               -> False
  where emacsFragments = ["#", "flymake", "flycheck"]

eventListenerForFile :: MonadIO m => FilePath -> ShouldReadFile -> m FileEventListener
eventListenerForFile fileName shouldReadFile = liftIO $ do
    eventChan        <- newTChanIO
    ignoreEventsNear <- newTVarIO Nothing

    stopMVar <- forkFileListenerThread fileName shouldReadFile eventChan ignoreEventsNear

    return FileEventListener
        { felEventTChan = eventChan
        , felIgnoreNextEventsNear = ignoreEventsNear
        , felStopMVar = stopMVar
        }

eventListenerForDirectory :: MonadIO m => FilePath -> [String] -> m FileEventListener
eventListenerForDirectory watchDirectory fileTypes = liftIO $ do
    eventChan        <- newTChanIO
    ignoreEventsNear <- newTVarIO Nothing

    stopMVar <- forkDirectoryListenerThread watchDirectory fileTypes eventChan

    return FileEventListener
        { felEventTChan = eventChan
        , felIgnoreNextEventsNear = ignoreEventsNear
        , felStopMVar = stopMVar
        }

killFileEventListener :: MonadIO m => FileEventListener -> m ()
killFileEventListener eventListener = liftIO $ putMVar (felStopMVar eventListener) ()

-- Pass a list like ["hs", "pd", "frag", "vert"] to match only those filetypes,
-- or an empty list to match all
modifiedWithExtensionPredicate :: [String] -> FSNotify.Event -> Bool
modifiedWithExtensionPredicate fileTypes event = case event of
    Modified path _ -> null fileTypes || drop 1 (takeExtension path) `elem` fileTypes
    _               -> False

forkDirectoryListenerThread :: FilePath
                            -> [String]
                            -> TChan (Either FSNotify.Event String)
                            -> IO (MVar ())
forkDirectoryListenerThread watchDirectory fileTypes eventChan = do
    let predicate e = modifiedWithExtensionPredicate fileTypes e
                      && not (isACommonEditorFile e)

    -- Configures debounce time for fsnotify
    let watchConfig = defaultConfig
            { confDebounce = Debounce 0.1 }
    stopMVar <- newEmptyMVar
    _ <- forkIO . withManagerConf watchConfig $ \manager -> do

        stop <- watchTree manager watchDirectory predicate $ \e ->
            writeTChanIO eventChan (Left e)
        () <- takeMVar stopMVar
        stop
    return stopMVar

forkFileListenerThread :: FilePath
                       -> ShouldReadFile
                       -> TChan (Either FSNotify.Event String)
                       -> TVar (Maybe UTCTime)
                       -> IO (MVar ())
forkFileListenerThread fileName shouldReadFile eventChan ignoreEventsNear = do
    leftPredicate <- fileModifiedPredicate <$> canonicalizePath fileName
    let predicate e = leftPredicate e && not (isACommonEditorFile e)
    -- If an ignore time is set, ignore file changes for the next 100 ms
        ignoreTime = 0.1
    -- Configures debounce time for fsnotify
        watchConfig = defaultConfig
            { confDebounce = Debounce 0.1 }

    stopMVar <- newEmptyMVar
    _ <- forkIO . withManagerConf watchConfig $ \manager -> do
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
                            `catch` (\err -> do
                                putStrLn $
                                    "Event listener failed to read " ++ fileName ++
                                    ": " ++ show (err::SomeException)
                                return "")
                        let !_len = length fileContents
                        writeTChanIO eventChan (Right fileContents)
                    else writeTChanIO eventChan (Left e)

        () <- takeMVar stopMVar
        stop
    return stopMVar

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

-- | Creates a getter for a set of resources that will be rebuilt whenever the file changes.
-- Takes a filename and an action to create a resource based on that file.
-- getWatchedResource <- makeWatchedResource "resources/shapes.frag" $ do
--        shader <- createShaderProgram "resources/shapes.vert" "resources/shapes.frag"
--        useProgram shader
--
--        uTime       <- getShaderUniform shader "uTime"
--
--        (quadVAO, quadVertCount) <- makeScreenSpaceQuad shader
--        return (quadVAO, quadVertCount, uTime)
-- Then use
-- (quadVAO, quadVertCount, uResolution, uMouse, uTime) <- getWatchedResource
-- in main loop
makeWatchedResource :: FilePath -> IO a -> IO (IO a)
makeWatchedResource fileName action = do
    absFileName <- makeAbsolute fileName
    listener <- eventListenerForFile absFileName JustReportEvents

    resourceRef <- newIORef =<< action

    -- Checks event listener, rebuilds resource if needed,
    -- then returns newest version of resource
    let getWatchedResource = do
            onFileEvent listener $ writeIORef resourceRef =<< action
            readIORef resourceRef
    return getWatchedResource
