{-# LANGUAGE CPP #-}

import Halive
import Banner
import System.Environment
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

separateArgs :: [String] -> ([String], [String])
separateArgs args = (haliveArgs, drop 1 targetArgs)
  where (haliveArgs, targetArgs) = break (== "--") args

main :: IO ()
main = do
  (args, targetArgs) <- separateArgs <$> getArgs
  print (args, targetArgs)
  case args of
    [] -> putStrLn "Usage: halive <main.hs> <include dir> [-- <args to myapp>]"
    (mainName:includeDirs) -> do
      putStrLn banner
      withArgs targetArgs $ recompiler mainName includeDirs
