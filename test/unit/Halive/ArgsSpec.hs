{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Halive.ArgsSpec where

import Test.Hspec
import Halive.Args
import Data.Maybe
import Data.Foldable

deriving instance Show Args

spec :: Spec
spec = 
    describe "Args.parseArgs" $ do

        describe "mainfile" $ do
            it "is required" $
                forM_
                    [ ""
                    , "-f filetype"
                    , "-f filetype -- targetArg"
                    , "-- targetArg"
                    ] $ \args -> parseArgs (words args) `shouldSatisfy` isNothing

            it "can be parsed" $
                case parseArgs (words "mainfile") of
                    Just Args {..} -> mainFileName `shouldBe` "mainfile"

        describe "include dirs" $ do
            it "are optional" $
                forM_
                    [ "mainfile"
                    , "mainfile -f filetype"
                    , "mainfile -- targetArg"
                    ] $ \args -> case parseArgs (words args) of
                                    Just Args {..} -> includeDirs `shouldBe` []
            
            it "can be parsed" $
                forM_ 
                    [ "mainfile include1 include2"
                    , "mainfile include1 -f filetype include2"
                    , "mainfile include1 include2 -- targerArg"
                    ] $ \args -> case parseArgs (words args) of
                        Just Args {..} -> includeDirs `shouldMatchList` ["include1", "include2"]

        describe "file types" $ do
            it "are optional" $
                case parseArgs (words "mainfile") of
                    Just Args {..} -> fileTypes `shouldBe` []
                    
            it "can be specified in any position" $
                case parseArgs (words "-f 1 mainfile -f 2 includedir -f 3") of
                    Just Args {..} -> fileTypes `shouldMatchList` ["1", "2", "3"]

            it "can be parsed using -f" $
                case parseArgs (words "mainfile -f filetype") of
                    Just Args {..} -> fileTypes `shouldBe` ["filetype"]

            it "can be parsed using --file-type" $
                case parseArgs (words "mainfile --file-type filetype") of
                    Just Args {..} -> fileTypes `shouldBe` ["filetype"]

            it "can't be parsed when flag is misused" $
                forM_
                    [ "mainfile -f"
                    , "mainfile -f --"
                    , "mainfile -f -- x"
                    ] $ \args -> parseArgs (words args) `shouldSatisfy` isNothing

        describe "target args" $ do
            it "are optional" $
                case parseArgs (words "mainfile") of
                    Just Args {..} -> targetArgs `shouldBe` []

            it "capture everything after `--`" $ 
                case parseArgs (words "mainfile -- -f a b c") of
                    Just Args {..} -> targetArgs `shouldBe` ["-f", "a", "b", "c"]