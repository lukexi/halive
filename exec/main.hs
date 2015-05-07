import Halive
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> putStrLn "Usage: halive <main.hs> <include dir>"
        (mainName:includeDirs) -> recompiler mainName includeDirs