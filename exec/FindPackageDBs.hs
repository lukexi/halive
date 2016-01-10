{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
module FindPackageDBs where
import Data.Maybe

import System.Directory
import System.FilePath
import System.Process
import System.Environment (lookupEnv)
import Data.List
import Data.Char
import Control.Monad.IO.Class

import GHC
import DynFlags

#if !MIN_VERSION_base(4,8,0)
import Data.Traversable (traverse)
import Control.Applicative ((<$>))
#endif

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
            let pkgs = map PkgConfFile [sandboxDB]
            return dflags { extraPkgConfs = (pkgs ++) . extraPkgConfs dflags }

------------------------
---------- Stack project
------------------------

-- | Get path to the project's snapshot and local package DBs via 'stack path'
getStackDb :: IO (Maybe [FilePath])
getStackDb = do
    exists <- doesFileExist "stack.yaml"
    if not exists
        then return Nothing
        else do
            pathInfo <- readProcess "stack" ["path"] ""
            return . Just . catMaybes $ map (flip extractKey pathInfo) ["local-pkg-db:", "snapshot-pkg-db:"]

updateDynFlagsWithStackDB :: MonadIO m => DynFlags -> m DynFlags
updateDynFlagsWithStackDB dflags =
    liftIO getStackDb >>= \case
        Nothing -> return dflags
        Just stackDBs -> do
            let pkgs = map PkgConfFile stackDBs
                dflags' = dflags { extraPkgConfs = (pkgs ++) . extraPkgConfs dflags }
            maybe (return dflags' ) (nastyHack dflags') =<< liftIO (lookupEnv "HALIVE_STACK_COMPONENT")

    where
      cmd c = "echo|stack -v ghci " ++ c ++ " 2>&1 >/dev/null |sed -ne 's/ @([^)]*)$//; s/.*Run process: ghc --interactive //p'"
      nastyHack :: MonadIO m => DynFlags -> String -> m DynFlags
      nastyHack dflags' component = do
            ghciArguments <- words <$> liftIO (readProcess "sh" ["-c", cmd component] "")
            let packageIdArguments = map noLoc $ filter ("-package-id=" `isPrefixOf`) ghciArguments
            fst' <$> parseDynamicFlagsCmdLine (gopt_set dflags' Opt_HideAllPackages) packageIdArguments
      fst' (x, _, _) = x
