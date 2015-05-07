module Test (test) where
import Foreign.Store
import Control.Concurrent
import Control.Monad
test :: Int
test = 13

doodle = do
    let storeID = 0
    putStrLn $ "Looking up store: " ++ show storeID
    maybeStore <- lookupStore storeID :: IO (Maybe (Store (Int -> Int)))
    case maybeStore of
        Just store -> do
            f <- readStore store
            print (f 8)
        Nothing -> do
            putStrLn "Creating a new store..."
            s <- writeStore (Store storeID) ((*2) :: Int -> Int)
            print s
            