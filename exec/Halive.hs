{-# LANGUAGE OverloadedStrings, LambdaCase, ScopedTypeVariables #-}
module Halive where

import GHC
import DynFlags
import Exception

import Data.IORef
import Control.Monad
import Control.Concurrent
import Control.Monad.IO.Class

import System.FSNotify
import System.FilePath

import Halive.SubHalive

directoryWatcher :: IO (Chan Event)
directoryWatcher = do
    let predicate event = case event of
            Modified path _ -> takeExtension path `elem` [".hs", ".vert", ".frag", ".pd"]
            -- Modified path _ -> takeExtension path `elem` [".hs"]
            _               -> False
    eventChan <- newChan
    _ <- forkIO $ withManager $ \manager -> do
        -- start a watching job (in the background)
        let watchDirectory = "."
        _stopListening <- watchTreeChan manager watchDirectory predicate eventChan
        -- Keep the watcher alive forever
        forever $ threadDelay 10000000

    return eventChan



recompiler :: FilePath -> [FilePath] -> IO ()
recompiler mainFileName importPaths' = do
    mainThreadId <- myThreadId

    {-
    Watcher:
        Tell the main thread to recompile.
        If the main thread isn't done yet, kill it.
    Compiler:
        Wait for the signal to recompile.
        Before recompiling & running, mark that we've started,
        and after we're done running, mark that we're done.
    -}

    -- Checks if the main function has returned, in the case of a batch program
    mainDone      <- newIORef False
    -- Start with a full MVar so we recompile right away.
    recompileLock <- newMVar ()
    -- We don't need to typecheck the first time, since the recompile will do it for us.
    typecheckLock <- newEmptyMVar

    let waitForRecompileSignal = liftIO (takeMVar recompileLock)
        sendRecompileSignal    = liftIO (putMVar recompileLock ())
        writeMainDone          = liftIO . writeIORef mainDone
        checkIfMainIsDone      = liftIO (readIORef mainDone)
        waitForTypecheckSignal = liftIO (takeMVar typecheckLock)
        sendTypecheckSignal    = putMVar typecheckLock ()

    typecheckSuccessful <- newIORef True
    _ <- forkOS . withGHCSession' mainFileName importPaths' . forever $ do
        _ <- waitForTypecheckSignal
        liftIO (writeIORef typecheckSuccessful True)
        let onFailure = liftIO (writeIORef typecheckSuccessful False)
        gcatch 
            (typecheckTargets onFailure) 
            (\(_x :: SomeException) -> onFailure)
        success <- liftIO (readIORef typecheckSuccessful)
        when success $ do
            -- If main hasn't already completed running, kill the main thread.
            -- Otherwise, we don't need to do anything
            checkIfMainIsDone >>= \case
                False -> liftIO (killThread mainThreadId)
                True  -> return ()
            
            sendRecompileSignal

    -- Watch for changes and recompile whenever they occur
    watcher <- liftIO directoryWatcher
    _ <- liftIO . forkIO . forever $ do
        -- Block looking for changes in the directory watcher
        _ <- readChan watcher

        -- The main thread waits for our signal to let us know
        -- that the file has chainged again, to keep from 
        -- endlessly trying to recompile a broken file,
        -- or endlessly re-running a working batch program.
        sendTypecheckSignal
       
    
    -- Start up the GHC session that will compile and run the app
    withGHCSession' mainFileName importPaths' . forever $ do
        -- Blocks until the file watcher has told us there is something new to do
        _ <- waitForRecompileSignal
        writeMainDone False
        recompileTargetMain
        writeMainDone True

-- | Use the default exception handlers, add the main file's directory
-- as an import path, and always use the main file as a recompilation target
withGHCSession' :: String -> [FilePath] -> Ghc a -> IO a
withGHCSession' mainFileName extraImportPaths action = do
    let mainFilePath   = dropFileName mainFileName
        allImportPaths = mainFilePath:extraImportPaths
    defaultErrorHandler defaultFatalMessager defaultFlushOut $
        withGHCSession allImportPaths [] NoDebounceFix $ do
            -- Set the given filename as a compilation target
            setTargets =<< sequence [guessTarget mainFileName Nothing]
            action



typecheckTargets :: GhcMonad m => m () -> m ()
typecheckTargets onFailure = handleSourceError (\e -> onFailure >> printException e) $ do
    liftIO . putStrLn $ replicate 25 '#' ++ " Typechecking... " ++ replicate 25 '#'
    -- Get the dependencies of the main target
    graph <- depanal [] False

    -- Reload the main target
    loadSuccess <- load LoadAllTargets
    if failed loadSuccess 
        then onFailure
        else 
            -- Parse and typecheck modules to trigger any SourceErrors therein
            forM_ graph (typecheckModule <=< parseModule)

-- | Prints and masks all exceptions
logCaughtExceptions :: ExceptionMonad m => m () -> m ()
logCaughtExceptions a = gcatch a 
    (\(x :: SomeException) -> 
        liftIO $ putStrLn ("Caught exception during reocmpileTargetMain: " ++ show x))

-- Recompiles the current targets
recompileTargetMain :: Ghc ()
recompileTargetMain = logCaughtExceptions $ handleSourceError printException $ do
    liftIO . putStrLn $ replicate 25 '*' ++ " Recompiling... " ++ replicate 25 '*'
    -- Get the dependencies of the main target
    graph <- depanal [] False

    -- Reload the main target
    loadSuccess <- load LoadAllTargets
    unless (failed loadSuccess) $ do
        -- We must parse and typecheck modules before they'll be available for usage
        forM_ graph (typecheckModule <=< parseModule)
        
        -- Load the dependencies of the main target
        setContext $ map (IIModule . ms_mod_name) graph

        -- Run the target file's "main" function
        rr <- runStmt "main" RunToCompletion
        liftIO $ case rr of
            RunOk _ -> 
                putStrLn "OK"
            RunException exception -> 
                print exception
            RunBreak _ _ _ -> 
                putStrLn "Breakpoint"

