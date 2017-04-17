{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
module Halive.Recompiler where
import Halive.SubHalive
import Halive.FileListener
import System.Mem
import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad.Trans
import Control.Monad
import Data.Text (Text)
import qualified Data.Text as Text
import Data.IORef
import Data.Typeable
import GHC
data CompilationRequest = CompilationRequest
    { crFilePath          :: FilePath
    , crExpressionStrings :: [String]
    , crResultTChan       :: TChan CompilationResult
    , crFileContents      :: Maybe String
    -- ^ This is intentionally lazy, since we want to evaluate the string on
    -- the SubHalive thread (as it may be e.g. a TextSeq that needs conversion)
    -- In the future, we may want to pass GHC's StringBuffer type here instead,
    -- and construct those in a smarter way.
    }

type CompilationResult = Either String [CompiledValue]

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
    _ <- forkIO . void $ do

        case gscKeepLibsInMemory ghcSessionConfig of

            -- In this mode we keep the ghc session alive continuously,
            -- and process all compilation requests in it.
            -- This trades possibly high memory usage for very fast compilation,
            -- since libraries don't have to be loaded in repeatedly.
            Always -> do

                -- See SubHalive.hs:GHCSessionConfig
                withGHCSession mainThreadID ghcSessionConfig $ do
                    compileInitialFile ghcSessionConfig
                    liftIO $ putMVar initialFileLock ()

                    forever $ do
                        CompilationRequest{..} <- readTChanIO ghcChan

                        result <- recompileExpressionsInFile
                            crFilePath crFileContents crExpressionStrings
                        writeTChanIO crResultTChan result

            -- In this mode we create a fresh GHC session for each set of compilation
            -- requests. This trades slower compilations for lower memory usage
            -- when not compiling.
            Opportunistic -> do
                withGHCSession mainThreadID ghcSessionConfig $
                    compileInitialFile ghcSessionConfig
                liftIO $ putMVar initialFileLock ()

                forever $ do
                    requests <- waitExhaustTChanIO ghcChan

                    withGHCSession mainThreadID ghcSessionConfig $
                        forM_ requests $ \CompilationRequest{..} -> do
                            result <- recompileExpressionsInFile
                                crFilePath crFileContents crExpressionStrings
                            writeTChanIO crResultTChan result
                    liftIO performGC

    -- Wait for the initial file to complete
    () <- liftIO (takeMVar initialFileLock)

    return ghcChan

compileInitialFile :: GHCSessionConfig -> Ghc ()
compileInitialFile ghcSessionConfig =
    forM_ (gscStartupFile ghcSessionConfig) $
        \(startupFile, startupExpr) ->
            recompileExpressionsInFile startupFile Nothing [startupExpr]

data Recompiler = Recompiler
    { recResultTChan       :: TChan CompilationResult
    , recFileEventListener :: FileEventListener
    , recListenerThread    :: ThreadId
    }

recompilerForExpression :: MonadIO m
                        => TChan CompilationRequest
                        -> FilePath
                        -> String
                        -> m Recompiler
recompilerForExpression ghcChan filePath expressionString =
    recompilerWithConfig ghcChan RecompilerConfig
        { rccWatchAll = Nothing
        , rccExpressions = [expressionString]
        , rccFilePath = filePath
        }

data RecompilerConfig = RecompilerConfig
    { rccWatchAll :: Maybe (FilePath, [String]) -- if Nothing, just watch given file
    , rccExpressions :: [String]
    , rccFilePath :: FilePath
    }

recompilerWithConfig :: MonadIO m
                     => TChan CompilationRequest
                     -> RecompilerConfig
                     -> m Recompiler
recompilerWithConfig ghcChan RecompilerConfig{..} = liftIO $ do
    resultTChan <- newTChanIO
    let compilationRequest = CompilationRequest
            { crFilePath          = rccFilePath
            , crExpressionStrings = rccExpressions
            , crResultTChan       = resultTChan
            , crFileContents      = Nothing
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
    recompilerForExpression ghcChan filePath expressionString

compileExpressions :: MonadIO m
                   => TChan CompilationRequest
                   -> Text
                   -> [String]
                   -> m (TChan CompilationResult)
compileExpressions ghcChan code expressionStrings = do
    resultTChan <- liftIO newTChanIO
    liftIO $ atomically $ writeTChan ghcChan $ CompilationRequest
        { crFilePath          = ""
        , crExpressionStrings = expressionStrings
        , crResultTChan       = resultTChan
        , crFileContents      = Just $ Text.unpack code
        }
    return resultTChan

compileExpression :: MonadIO m
                  => TChan CompilationRequest
                  -> Text
                  -> String
                  -> m (TChan CompilationResult)
compileExpression ghcChan code expressionString =
    compileExpressions ghcChan code [expressionString]

compileExpressionInFile :: MonadIO m
                        => TChan CompilationRequest
                        -> FilePath
                        -> String
                        -> m (TChan CompilationResult)
compileExpressionInFile ghcChan fileName expressionString = do
    resultTChan <- liftIO newTChanIO
    liftIO $ atomically $ writeTChan ghcChan $ CompilationRequest
        { crFilePath          = fileName
        , crExpressionStrings = [expressionString]
        , crResultTChan       = resultTChan
        , crFileContents      = Nothing
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
    recompiler <- recompilerForExpression ghcChan fileName expression
    valueRef <- newIORef defaultVal
    _ <- forkIO . forever $ do
        result <- atomically (readTChan (recResultTChan recompiler))
        case result of
            Left errors -> putStrLn errors
            Right values ->
                case values of
                    [value] ->
                        case getCompiledValue value of
                            Just newVal -> writeIORef valueRef newVal
                            Nothing -> putStrLn ("Got incorrect type for " ++ fileName ++ ":" ++ expression)
                    _ ->
                        error "Unexpected number of values received on recResultTChan"

    return (readIORef valueRef)
