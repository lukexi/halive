{-# LANGUAGE ScopedTypeVariables #-}
module Halive.Utils where
import Foreign.Store
import Data.Word

import Control.Monad.State
import System.Environment

isHaliveActive :: MonadIO m => m Bool
isHaliveActive = liftIO $ do
    r <- lookupEnv "Halive Active"
    case r of
        Just "Yes" -> return True
        _          -> return False

-- | Takes a unique integer representing your value,
-- along with an IO action to create the first instance
-- of your value to be used on subsequent recompilations.
reacquire :: forall a m. (MonadIO m) => Word32 -> m a -> m a
reacquire storeID create = do
    -- See if an existing store exists.
    maybeStore <- liftIO (lookupStore storeID) :: m (Maybe (Store a))
    case maybeStore of
        -- If so, return the value inside
        Just store -> liftIO (readStore store)
        -- Otherwise, create the value, store it, and return it.
        Nothing -> do
            value <- create
            persist storeID value
            return value

persist :: MonadIO m => Word32 -> a -> m ()
persist storeID value = liftIO (writeStore (Store storeID) value)

persistState :: (MonadState s m, MonadIO m) => Word32 -> m ()
persistState storeID = persist storeID =<< get
