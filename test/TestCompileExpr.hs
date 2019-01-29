{-# LANGUAGE OverloadedStrings #-}
import Control.Concurrent.STM
import Halive

main :: IO ()
main = do

    ghc <- startGHC defaultGHCSessionConfig
    resultChan <- compileExpression ghc
        "main = print 123456789"
        "main"
    result <-  atomically (readTChan resultChan)
    putStrLn "Got result:"
    print result
