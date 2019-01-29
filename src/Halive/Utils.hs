{-# LANGUAGE ScopedTypeVariables #-}
module Halive.Utils where
import Foreign.Store
import Data.Word

import Control.Monad.State
import System.Environment

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Typeable
import Data.Dynamic

type StoreMap = Map String Dynamic

isHaliveActive :: MonadIO m => m Bool
isHaliveActive = liftIO $ do
    r <- lookupEnv "Halive Active"
    case r of
        Just "Yes" -> return True
        _          -> return False

-- | Takes a unique name representing your value,
-- along with an IO action to create the first instance
-- of your value to be used on subsequent recompilations.
reacquire :: (Typeable a, MonadIO m) => String -> m a -> m a
reacquire name create = do
    -- See if the value exists already
    storeMap <- getStoreMap
    case fromDynamic =<< Map.lookup name storeMap of
        -- If so, return the value inside
        Just value -> return value
        -- Otherwise, create the value, store it, and return it.
        Nothing -> do
            value <- create
            persist name value
            return value

persistState :: (MonadState s m, MonadIO m, Typeable s) => String -> m ()
persistState name = persist name =<< get

storeMapID :: Word32
storeMapID = 0

getStoreMap :: MonadIO m => m StoreMap
getStoreMap = do
    -- See if we've created the storeMap already
    maybeStore <- liftIO (lookupStore storeMapID)
    case maybeStore of
        -- If so, return the existing storeMap inside
        Just store -> liftIO (readStore store)
        -- Otherwise, create the value, store it, and return it.
        Nothing -> do
            let storeMap = mempty
            writeStoreMap storeMap
            return storeMap

modifyStoreMap :: MonadIO m => (StoreMap -> StoreMap) -> m ()
modifyStoreMap f = do
    storeMap <- getStoreMap
    writeStoreMap (f storeMap)

writeStoreMap :: MonadIO m => StoreMap -> m ()
writeStoreMap = liftIO . writeStore (Store storeMapID)

persist :: (Typeable a, MonadIO m) => String -> a -> m ()
persist name value =
    modifyStoreMap (Map.insert name (toDyn value))
