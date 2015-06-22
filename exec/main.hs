import Halive
import Banner
import System.Environment

separateArgs :: [String] -> ([String], [String])
separateArgs args = (haliveArgs, drop 1 targetArgs)
  where (haliveArgs, targetArgs) = break (== "--") args

main :: IO ()
main = do
  (args, targetArgs) <- separateArgs <$> getArgs
  case args of
    [] -> putStrLn "Usage: halive <main.hs> <include dir> [-- <args to myapp>]"
    (mainName:includeDirs) -> do
      putStrLn banner
      withArgs targetArgs $ recompiler mainName includeDirs
