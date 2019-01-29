{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Halive.FindPackageDBs where
import Data.Maybe

import Control.Monad.IO.Class
import Data.Char
import Data.List
import System.Directory
import System.FilePath
import System.Process
import Control.Exception
import DynFlags

-- | Extract the sandbox package db directory from the cabal.sandbox.config file.
--   Exception is thrown if the sandbox config file is broken.
extractKey :: String -> String -> Maybe FilePath
extractKey key conf = extractValue <$> parse conf
  where
    keyLen = length key

    parse = listToMaybe . filter (key `isPrefixOf`) . lines
    extractValue = dropWhileEnd isSpace . dropWhile isSpace . drop keyLen
-- From ghc-mod
mightExist :: FilePath -> IO (Maybe FilePath)
mightExist f = do
    exists <- doesFileExist f
    return $ if exists then (Just f) else (Nothing)

addExtraPkgConfs :: [FilePath] -> DynFlags -> DynFlags
addExtraPkgConfs pkgConfs dflags = dflags
    { packageDBFlags =
        let newPkgConfs = map (PackageDB . PkgConfFile) pkgConfs
        in newPkgConfs ++ packageDBFlags dflags
    }


------------------------
---------- Cabal Sandbox
------------------------

-- | Get path to sandbox's package DB via the cabal.sandbox.config file
getSandboxDb :: IO (Maybe FilePath)
getSandboxDb = do
    currentDir <- getCurrentDirectory
    config <- traverse readFile =<< mightExist (currentDir </> "cabal.sandbox.config")
    return $ (extractKey "package-db:" =<< config)

updateDynFlagsWithCabalSandbox :: MonadIO m => DynFlags -> m DynFlags
updateDynFlagsWithCabalSandbox dflags =
    liftIO getSandboxDb >>= \case
        Nothing -> return dflags
        Just sandboxDB -> do
            let pkgs = map (PackageDB . PkgConfFile) [sandboxDB]
            return dflags { packageDBFlags = pkgs ++  packageDBFlags dflags }

------------------------
---------- Stack project
------------------------

-- | Get path to the project's snapshot and local package DBs via 'stack path'
getStackDb :: IO (Maybe [FilePath])
getStackDb = do
    pathInfo <- readProcess "stack" ["path"] "" `catch` (\(_e::IOException) -> return [])
    return . Just . catMaybes $ map (flip extractKey pathInfo)
        ["global-pkg-db:", "local-pkg-db:", "snapshot-pkg-db:"]

updateDynFlagsWithStackDB :: MonadIO m => DynFlags -> m DynFlags
updateDynFlagsWithStackDB dflags =
    liftIO getStackDb >>= \case
        Nothing -> return dflags
        Just stackDBs -> do
            let pkgs = map (PackageDB . PkgConfFile) stackDBs
            return dflags { packageDBFlags = pkgs ++ packageDBFlags dflags }

updateDynFlagsWithGlobalDB :: MonadIO m => DynFlags -> m DynFlags
updateDynFlagsWithGlobalDB dflags = do
    xs <- liftIO $ lines <$> readProcess "ghc" ["--print-global-package-db"] ""
        `catch` (\(_e :: SomeException) -> return [])
    case xs of
        [pkgconf] -> do
          let flgs = PackageDB (PkgConfFile pkgconf) : packageDBFlags dflags
          return dflags { packageDBFlags = flgs }
        _ -> return dflags
