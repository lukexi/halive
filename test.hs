module Test (test) where
import Foreign.Store
test :: Int
test = 13

doodle storeID = do
    putStrLn $ "Looking up store: " ++ show storeID
    maybeStore <- lookupStore storeID :: IO (Maybe (Store (Int -> Int)))
    case maybeStore of
        Just store -> do
            f <- readStore store
            print (f 6)
        Nothing -> putStrLn "Couldn't find anything :*("