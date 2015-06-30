module Halive.Concurrent (
  killThreads,
  registerThread,
  forkIO',
  forkOS'
  ) where

import Control.Concurrent
import System.IO.Unsafe
import           Data.Set (Set)
import qualified Data.Set as Set
import Prelude hiding (mapM_)
import Data.Foldable

-- | A small collection of helper functions for creating threads that can be killed each time Halive re-runs your main function.
-- This is helpful for programs where you control the threads, but doesn't solve the problem of libraries that use threads 
-- (unless you unpack them and replace all forkIO/forkOS with forkIO'/forkOS')
-- It would be good to ask GHC devs about this; 
-- perhaps a GHC flag that registers threads similar to this module, for development use only?

-- An internal global variable to hold threads that should be killed
{-# NOINLINE registeredThreads #-}
registeredThreads :: MVar (Set ThreadId)
registeredThreads = unsafePerformIO (newMVar Set.empty)

-- | Kill all threads registered to be killed.
-- Meant to be called at the beginning of your program to clean up threads from the last execution before continuing
killThreads :: IO ()
killThreads = modifyMVar_ registeredThreads $ \threadIDs -> do
  mapM_ killThread threadIDs
  return Set.empty

-- | Register a thread to be killed when killThreads is called
registerThread :: ThreadId -> IO ()
registerThread threadID = modifyMVar_ registeredThreads (return . Set.insert threadID)

-- | Fork a thread and register it to be killed when killThreads is called
forkIO' :: IO () -> IO ThreadId
forkIO' action = do
  threadID <- forkIO action
  registerThread threadID
  return threadID

-- | Fork an OS thread and register it to be killed when killThreads is called
forkOS' :: IO () -> IO ThreadId
forkOS' action = do
  threadID <- forkOS action
  registerThread threadID
  return threadID