import Halive
import Banner
import System.Environment

separateArgs args = do
  let (haliveArgs, targetArgs) = break (=="--") args
  in  (haliveArgs, drop 1 targetArgs)

main :: IO ()
main = do
    (args, targetArgs) <- separateArgs <$> getArgs
    print targetArgs
    case args of
        [] -> putStrLn "Usage: halive <main.hs> <include dir> [-- <args to myapp>]"
        (mainName:includeDirs) -> do
            putStrLn banner
            withArgs targetArgs $ recompiler mainName includeDirs
