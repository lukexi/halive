import Halive.Recompiler
import Halive.SubHalive
import Control.Concurrent.STM
import Control.Monad

main :: IO a
main = do
    ghc <- startGHC defaultGHCSessionConfig

    fooRecompiler <- recompilerForExpression ghc "test/TestFileFoo.hs" "foo" True
    barRecompiler <- recompilerForExpression ghc "test/TestFileBar.hs" "bar" True

    forever $ do
        result <- atomically
            (readTChan (recResultTChan fooRecompiler)
            `orElse`
             readTChan (recResultTChan barRecompiler))
        case result of
            Left errors -> putStrLn (concat errors)
            Right value -> case getCompiledValue value of
                Just value -> do
                    putStrLn value
                Nothing -> do
                    putStrLn "Error: foo or bar was not of type String"
