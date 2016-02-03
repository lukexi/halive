import Halive.Recompiler
import Halive.SubHalive
import Control.Concurrent.STM
import Control.Monad

main :: IO a
main = do
    ghc <- startGHC []

    fooRecompiler <- recompilerForExpression ghc "test/TestFileFoo.hs" "foo"
    barRecompiler <- recompilerForExpression ghc "test/TestFileBar.hs" "bar"

    forever $ do
        result <- atomically 
            (readTChan fooRecompiler
            `orElse`
             readTChan barRecompiler)
        case result of
            Left errors -> putStrLn (concat errors)
            Right value -> putStrLn (getCompiledValue value)
