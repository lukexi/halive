module Test (test) where
import Foreign.Store
test :: Int
test = 13

doodle = do
    let storeID = Store 0
    putStrLn $ "Looking up store: " ++ show storeID
    maybeStore <- lookupStore 0 :: IO (Maybe (Store (Int -> Int)))
    case maybeStore of
        Just store -> do
            f <- readStore store
            print (f 6)
        Nothing -> do
            putStrLn "Creating a new store..."
            s <- writeStore (Store 0) ((*2) :: Int -> Int)
            print s
            