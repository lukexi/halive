module SandboxPath where
import Data.Maybe
import Data.Traversable (traverse)
import Control.Applicative ((<$>))
import System.Directory
import System.FilePath
import Data.List
import Data.Char

-- From ghc-mod
mightExist :: FilePath -> IO (Maybe FilePath)
mightExist f = do
    exists <- doesFileExist f
    return $ if exists then (Just f) else (Nothing)

-- | Get path to sandbox config file
getSandboxDb :: IO (Maybe FilePath)
getSandboxDb = do
    currentDir <- getCurrentDirectory
    config <- traverse readFile =<< mightExist (currentDir </> "cabal.sandbox.config")
    return $ (extractSandboxDbDir =<< config)

-- | Extract the sandbox package db directory from the cabal.sandbox.config file.
--   Exception is thrown if the sandbox config file is broken.
extractSandboxDbDir :: String -> Maybe FilePath
extractSandboxDbDir conf = extractValue <$> parse conf
  where
    key = "package-db:"
    keyLen = length key

    parse = listToMaybe . filter (key `isPrefixOf`) . lines
    extractValue = dropWhileEnd isSpace . dropWhile isSpace . drop keyLen

-- main = print =<< getSandboxDb