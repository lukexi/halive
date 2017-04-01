{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
module Halive.Recompiler where
import Halive.SubHalive
import Halive.FileListener

import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad.Trans
import Control.Monad
import Data.Text (Text)
import qualified Data.Text as Text
import Data.IORef
import Data.Typeable

data CompilationRequest = CompilationRequest
    { crCodeSource       :: CodeSource
    , crExpressionString :: String
    , crResultTChan      :: TChan CompilationResult
    , crFileContents     :: Maybe Text
    -- ^ This is intentionally lazy, since we want to evaluate the string on
    -- the SubHalive thread (as it may be e.g. a TextSeq that needs conversion)
    -- In the future, we may want to pass GHC's StringBuffer type here instead,
    -- and construct those in a smarter way.
    }

type CompilationResult = Either String CompiledValue

-- This is used to implement a workaround for the GHC API crashing
-- when used after application startup, when it tries to load libraries
-- for the first time. By wrapping main in withGHC, startGHC will block until
-- the GHC API is initialized before allowing the application to start.
withGHC :: MonadIO m
        => GHCSessionConfig
        -> (TChan CompilationRequest -> m b)
        -> m b
withGHC ghcSessionConfig action = do
    ghcChan <- startGHC ghcSessionConfig
    action ghcChan

startGHCDefault :: MonadIO m => m (TChan CompilationRequest)
startGHCDefault = startGHC defaultGHCSessionConfig

startGHC :: MonadIO m => GHCSessionConfig -> m (TChan CompilationRequest)
startGHC ghcSessionConfig = liftIO $ do
    ghcChan <- newTChanIO

    -- Grab this thread's ID (need to run this on the main thread, of course)
    mainThreadID <- case gscMainThreadID ghcSessionConfig of
        Just threadID -> return threadID
        Nothing -> myThreadId

    initialFileLock <- liftIO newEmptyMVar

    _ <- forkIO . void . withGHCSession mainThreadID ghcSessionConfig $ do

        -- See SubHalive.hs:GHCSessionConfig
        forM_ (gscStartupFile ghcSessionConfig) $
            \(startupFile, startupExpr) ->
                recompileExpressionInSource (SourceFile startupFile) Nothing startupExpr

        liftIO $ putMVar initialFileLock ()
        forever $ do
            CompilationRequest{..} <- readTChanIO ghcChan

            result <- recompileExpressionInSource
                crCodeSource crFileContents crExpressionString
            writeTChanIO crResultTChan result

    () <- liftIO (takeMVar initialFileLock)

    return ghcChan


data Recompiler = Recompiler
    { recResultTChan       :: TChan CompilationResult
    , recFileEventListener :: FileEventListener
    , recListenerThread    :: ThreadId
    }

recompilerForFile :: MonadIO m
                  => TChan CompilationRequest
                  -> FilePath
                  -> String
                  -> m Recompiler
recompilerForFile ghcChan filePath expressionString =
    recompilerWithConfig ghcChan RecompilerConfig
        { rccWatchAll = Nothing
        , rccExpression = expressionString
        , rccFilePath = filePath
        }

data RecompilerConfig = RecompilerConfig
    { rccWatchAll   :: Maybe (FilePath, [String]) -- if Nothing, just watch given file
    , rccExpression :: String
    , rccFilePath   :: FilePath
    }

recompilerWithConfig :: MonadIO m
                     => TChan CompilationRequest
                     -> RecompilerConfig
                     -> m Recompiler
recompilerWithConfig ghcChan RecompilerConfig{..} = liftIO $ do
    resultTChan <- newTChanIO
    let compilationRequest = CompilationRequest
            { crCodeSource       = SourceFile rccFilePath
            , crExpressionString = rccExpression
            , crResultTChan      = resultTChan
            , crFileContents     = Nothing
            }

    -- Recompile on file event notifications
    fileEventListener <- case rccWatchAll of
        Nothing -> eventListenerForFile rccFilePath JustReportEvents
        Just (watchDir, fileTypes) -> eventListenerForDirectory watchDir fileTypes
    listenerThread <- forkIO . forever $ do
        _ <- readFileEvent fileEventListener
        writeTChanIO ghcChan compilationRequest

    -- Compile for the first time immediately
    writeTChanIO ghcChan compilationRequest

    return Recompiler
        { recResultTChan       = resultTChan
        , recFileEventListener = fileEventListener
        , recListenerThread    = listenerThread
        }

killRecompiler :: MonadIO m => Recompiler -> m ()
killRecompiler recompiler = do
    liftIO $ killThread (recListenerThread recompiler)

renameRecompilerForExpression :: MonadIO m => Recompiler
                                           -> TChan CompilationRequest
                                           -> FilePath
                                           -> String
                                           -> m Recompiler
renameRecompilerForExpression recompiler ghcChan filePath expressionString = do
    killRecompiler recompiler
    recompilerForFile ghcChan filePath expressionString

-- Compile an anonymous expression.
compileExpression :: MonadIO m
                  => TChan CompilationRequest
                  -> String -- ^ A name to assist with error reporting
                  -> Text   -- ^ The actual code source
                  -> String -- ^ An expression to pull from the code after compilation
                  -> m (TChan CompilationResult)
compileExpression ghcChan name code expressionString = do
    resultTChan <- liftIO newTChanIO
    liftIO $ atomically $ writeTChan ghcChan $ CompilationRequest
        { crCodeSource       = SourceCode name
        , crExpressionString = expressionString
        , crResultTChan      = resultTChan
        , crFileContents     = Just code
        }
    return resultTChan

-- | liveExpression returns an action to get to the latest version of the expression,
-- updating it whenever the code changes (unless there is an error).
-- It also takes a default argument to use until the first compilation completes.
-- The action is meant to be called before each use of the value.
liveExpression :: Typeable a
               => TChan CompilationRequest
               -> FilePath
               -> String
               -> a
               -> IO (IO a)
liveExpression ghcChan fileName expression defaultVal = do
    recompiler <- recompilerForFile ghcChan fileName expression
    valueRef <- newIORef defaultVal
    forkIO . forever $ do
        result <- atomically (readTChan (recResultTChan recompiler))
        case result of
            Left err -> putStrLn err
            Right maybeVal -> case getCompiledValue maybeVal of
                Just newVal -> writeIORef valueRef newVal
                Nothing -> putStrLn ("Got incorrect type for " ++ fileName ++ ":" ++ expression)

    return (readIORef valueRef)
