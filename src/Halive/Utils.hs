{-# LANGUAGE ScopedTypeVariables #-}
module Halive.Utils where
import Foreign.Store
import Data.Word

-- | Takes a unique integer representing your value,
-- along with an IO action to create the first instance
-- of your value to be used on subsequent recompilations.
reacquire :: forall a. Word32 -> IO a -> IO a
reacquire storeID create = do
    -- See if an existing store exists.
    maybeStore <- lookupStore storeID :: IO (Maybe (Store a))
    case maybeStore of
        -- If so, return the value inside
        Just store -> readStore store
        -- Otherwise, create the value, store it, and return it.
        Nothing -> do
            value <- create
            writeStore (Store storeID) value
            return value

-- TODO: a version of forkIO that records each threadID so they
-- can be killed when the program restarts, probably via
-- a 'killOldThreads' function the user calls at the 
-- start of their program.