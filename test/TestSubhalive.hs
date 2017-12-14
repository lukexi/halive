import Halive.Recompiler
import Halive.SubHalive
import Control.Concurrent.STM
import Control.Monad

main :: IO a
main = do
    ghc <- startGHC defaultGHCSessionConfig

    fooRecompiler <- recompilerForExpression ghc "test/TestFileFoo.hs" "foo"
    barRecompiler <- recompilerForExpression ghc "test/TestFileBar.hs" "bar"

    forever $ do
        result <- atomically
            (readTChan (recResultTChan fooRecompiler)
            `orElse`
             readTChan (recResultTChan barRecompiler))
        case result of
            Left errors -> putStrLn errors
            Right values -> forM_ values $ \value ->
                case getCompiledValue value of
                    Just v ->
                        putStrLn v
                    Nothing ->
                        putStrLn "Error: foo or bar was not of type String"
