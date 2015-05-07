import Halive
import Banner
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> putStrLn "Usage: halive <main.hs> <include dir>"
        (mainName:includeDirs) -> do
            putStrLn banner
            recompiler mainName includeDirs