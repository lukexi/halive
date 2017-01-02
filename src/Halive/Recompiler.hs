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
    , crFileContents     :: Maybe String
    -- ^ This is intentionally lazy, since we want to evaluate the string on
    -- the SubHalive thread (as it may be e.g. a TextSeq that needs conversion)
    -- In the future, we may want to pass GHC's StringBuffer type here instead,
    -- and construct those in a smarter way.
    }

type CompilationResult = Either [String] CompiledValue

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


startGHC :: MonadIO m => GHCSessionConfig -> m (TChan CompilationRequest)
startGHC ghcSessionConfig = liftIO $ do
    ghcChan <- newTChanIO

    -- Grab this thread's ID (need to run this on the main thread, of course)
    mainThreadID <- myThreadId

    initialFileLock <- liftIO newEmptyMVar

    _ <- forkIO . void . withGHCSession mainThreadID ghcSessionConfig $ do

        -- See SubHalive.hs:GHCSessionConfig
        forM_ (gscStartupFile ghcSessionConfig) $
            \(startupFile, startupExpr) ->
                recompileExpressionInFile startupFile Nothing startupExpr

        liftIO $ putMVar initialFileLock ()
        forever $ do
            CompilationRequest{..} <- readTChanIO ghcChan
            liftIO . putStrLn $ "SubHalive recompiling: "
                ++ show (crFilePath, crExpressionString)

            result <- recompileExpressionInFile
                crFilePath crFileContents crExpressionString
            writeTChanIO crResultTChan result

    () <- liftIO $ takeMVar initialFileLock

    return ghcChan


data Recompiler = Recompiler
    { recResultTChan :: TChan CompilationResult
    , recFileEventListener :: FileEventListener
    , recListenerThread :: ThreadId
    }

recompilerForExpression :: MonadIO m
                        => TChan CompilationRequest
                        -> FilePath
                        -> String
                        -> Bool
                        -> m Recompiler
recompilerForExpression ghcChan filePath expressionString compileImmediately =
    recompilerWithConfig ghcChan RecompilerConfig
        { rccWatchAll = Nothing
        , rccExpression = expressionString
        , rccFilePath = filePath
        , rccCompileImmediately = compileImmediately
        }

data RecompilerConfig = RecompilerConfig
    { rccWatchAll :: Maybe (FilePath, [String]) -- if Nothing, just watch given file
    , rccExpression :: String
    , rccFilePath :: FilePath
    , rccCompileImmediately :: Bool
    }

recompilerWithConfig :: MonadIO m
                     => TChan CompilationRequest
                     -> RecompilerConfig
                     -> m Recompiler
recompilerWithConfig ghcChan RecompilerConfig{..} = liftIO $ do
    resultTChan <- newTChanIO
    let compilationRequest = CompilationRequest
            { crFilePath         = rccFilePath
            , crExpressionString = rccExpression
            , crResultTChan      = resultTChan
            , crFileContents     = Nothing
            }


    -- Compile for the first time immediately
    when rccCompileImmediately $
        writeTChanIO ghcChan compilationRequest

    -- Recompile on file event notifications
    fileEventListener <- case rccWatchAll of 
        Nothing -> eventListenerForFile rccFilePath JustReportEvents
        Just (watchDir, fileTypes) -> eventListenerForDirectory watchDir fileTypes
    listenerThread <- forkIO . forever $ do
        _ <- readFileEvent fileEventListener
        writeTChanIO ghcChan compilationRequest

    return Recompiler
        { recResultTChan = resultTChan
        , recFileEventListener = fileEventListener
        , recListenerThread = listenerThread
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
    recompilerForExpression ghcChan filePath expressionString False
