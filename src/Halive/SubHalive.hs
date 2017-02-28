{-# LANGUAGE OverloadedStrings, LambdaCase, ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}
module Halive.SubHalive (
    module Halive.SubHalive
#if __GLASGOW_HASKELL__ >= 800
    , module GHC.LanguageExtensions
#else
    , ExtensionFlag(..)
#endif

    ) where

import GHC
#if __GLASGOW_HASKELL__ >= 800
import GHC.LanguageExtensions
#else
import Module
#endif
import DynFlags
import Exception
import ErrUtils
import HscTypes
import GHC.Paths
import Outputable
import StringBuffer

--import Packages
import Linker

import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Data.Time
import Halive.FindPackageDBs

import Control.Concurrent
import System.Signal
import Data.Dynamic

data FixDebounce = DebounceFix | NoDebounceFix deriving Eq

data CompliationMode = Interpreted | Compiled deriving Eq

data GHCSessionConfig = GHCSessionConfig
    { gscFixDebounce        :: FixDebounce
    , gscImportPaths        :: [FilePath]
    , gscPackageDBs         :: [FilePath]
    , gscLibDir             :: FilePath
#if __GLASGOW_HASKELL__ >= 800
    , gscLanguageExtensions :: [Extension]
    , gscNoLanguageExtensions :: [Extension]
#else
    , gscLanguageExtensions :: [ExtensionFlag]
    , gscNoLanguageExtensions :: [ExtensionFlag]
#endif
    , gscCompilationMode    :: CompliationMode
    , gscStartupFile        :: Maybe (FilePath, String)
        -- ^ Allow API users to block until a given file is compiled,
        -- to work around a bug where the GHC API crashes while loading libraries
        -- if the main thread is doing work (possibly due to accessing said libraries in some way)
    , gscVerbosity          :: Int
    , gscMainThreadID       :: Maybe ThreadId
    }



defaultGHCSessionConfig :: GHCSessionConfig
defaultGHCSessionConfig = GHCSessionConfig
    { gscFixDebounce = DebounceFix
    , gscImportPaths = []
    , gscPackageDBs  = []
    , gscLanguageExtensions = []
    , gscNoLanguageExtensions = []
    , gscLibDir = libdir
    , gscCompilationMode = Interpreted
    , gscStartupFile = Nothing
    , gscVerbosity = 0
    , gscMainThreadID = Nothing
    }

--pkgConfRefToString = \case
--    GlobalPkgConf -> "GlobalPkgConf"
--    UserPkgConf -> "UserPkgConf"
--    PkgConfFile file -> "PkgConfFile " ++ show file

--extraPkgConfsToString dflags = show $ map pkgConfRefToString $ extraPkgConfs dflags $ []

logIO :: MonadIO m => String -> m ()
logIO = liftIO . putStrLn

-- Starts up a GHC session and then runs the given action within it
withGHCSession :: ThreadId -> GHCSessionConfig -> Ghc a -> IO a
withGHCSession mainThreadID GHCSessionConfig{..} action = do
    -- Work around https://ghc.haskell.org/trac/ghc/ticket/4162
    let
        restoreControlC f = do
            liftIO $ installHandler sigINT (\_signal -> killThread mainThreadID)
            f

    -- defaultErrorHandler defaultFatalMessager defaultFlushOut $ runGhc (Just libdir) $ do
    runGhc (Just gscLibDir) . restoreControlC $ do
        -- Get the default dynFlags
        dflags0 <- getSessionDynFlags
        -- Add passed-in package DBs
        let dflags1 = addExtraPkgConfs dflags0 gscPackageDBs
        -- If there's a sandbox, add its package DB
        dflags2 <- updateDynFlagsWithCabalSandbox dflags1
        -- If this is a stack project, add its package DBs
        dflags3 <- updateDynFlagsWithStackDB dflags2
        dflags4 <- updateDynFlagsWithGlobalDB dflags3

        -- Make sure we're configured for live-reload
        let dflags5 = dflags4 { hscTarget   = if gscCompilationMode == Compiled then HscAsm else HscInterpreted
                              , optLevel    = if gscCompilationMode == Compiled then 2 else 0
                              , ghcLink     = LinkInMemory
                              , ghcMode     = CompManager
                              , importPaths = gscImportPaths
                              , objectDir = Just ".halive"
                              , hiDir     = Just ".halive"
                              , stubDir   = Just ".halive"
                              , dumpDir   = Just ".halive"
                              , verbosity = gscVerbosity
                              }
                              -- turn off the GHCi sandbox
                              -- since it breaks OpenGL/GUI usage
                              `gopt_unset` Opt_GhciSandbox
            -- GHC seems to try to "debounce" compilations within
            -- about a half second (i.e., it won't recompile)
            -- This fixes that, but probably isn't quite what we want
            -- since it will cause extra files to be recompiled...
            dflags6 = if gscFixDebounce == DebounceFix
                        then dflags5 `gopt_set` Opt_ForceRecomp
                        else dflags5
            dflags7 =
                flip (foldl xopt_unset) gscNoLanguageExtensions $
                    foldl xopt_set dflags6 gscLanguageExtensions

        -- We must call setSessionDynFlags before calling initPackages or any other GHC API
        packageIDs <- setSessionDynFlags dflags7


        -- Works around a yet-unidentified segfault when loading
        -- 5/1/2016: I've implemented this in a different way,
        -- (by just passing in a file to compile that will trigger
        -- loads of all its dependencies)
        -- but this is still a viable approach... not quite as convenient though!
        --let gscPreloadPackagesForModules = ["Sound.Pd"]
        --preloadPackageKeys <- forM gscPreloadPackagesForModules $ \modName ->
        --    modulePackageKey <$> findModule (mkModuleName modName) Nothing
        --let finalPackageIDs = preloadPackageKeys ++ packageIDs
        let finalPackageIDs = packageIDs
        --logIO $ "linkPackages: " ++ show (map packageKeyString finalPackageIDs)

         -- Initialize the package database and dynamic linker.
         -- Explicitly calling these avoids crashes on some of my machines.

#if __GLASGOW_HASKELL__ >= 800
        hscEnv1 <- getSession
        liftIO $ linkPackages hscEnv1 finalPackageIDs
        hscEnv2 <- getSession
        liftIO (initDynLinker hscEnv2)
#else
        dflags7 <- getSessionDynFlags
        liftIO $ linkPackages dflags7 finalPackageIDs
        dflags8 <- getSessionDynFlags
        liftIO (initDynLinker dflags8)
#endif

        action

-- See note below - this isn't actually called right now
gatherErrors :: GhcMonad m => SourceError -> m [String]
gatherErrors sourceError = do
    printException sourceError
    dflags <- getSessionDynFlags
    let errorSDocs = pprErrMsgBagWithLoc (srcErrorMessages sourceError)
        errorStrings = map (showSDoc dflags) errorSDocs
    return errorStrings

--newtype CompiledValue = CompiledValue HValue
newtype CompiledValue = CompiledValue Dynamic deriving Show

--getCompiledValue :: CompiledValue -> a
--getCompiledValue (CompiledValue r) = unsafeCoerce r
getCompiledValue :: Typeable a => CompiledValue -> Maybe a
getCompiledValue (CompiledValue r) = fromDynamic r

fileContentsStringToBuffer :: (MonadIO m) => Maybe String -> m (Maybe (StringBuffer, UTCTime))
fileContentsStringToBuffer mFileContents = forM mFileContents $ \fileContents -> do
    now <- liftIO getCurrentTime
    return (stringToStringBuffer fileContents, now)

-- | We return the uncoerced HValue, which lets us send polymorphic values back through channels
recompileExpressionInFile :: FilePath -> Maybe String -> String -> Ghc (Either [String] CompiledValue)
recompileExpressionInFile fileName mFileContents expression =
    -- NOTE: handleSourceError doesn't actually seem to do anything, and we use
    -- the IORef + log_action solution instead. The API docs claim 'load' should
    -- throw SourceErrors but it doesn't afaict.
    catchExceptions . handleSourceError (fmap Left . gatherErrors) $ do
        --logIO $ "Recompiling " ++ show (fileName, expression)
        -- Prepend a '*' to prevent GHC from trying to load from any previously compiled object files
        -- see http://stackoverflow.com/questions/12790341/haskell-ghc-dynamic-compliation-only-works-on-first-compile
        --logIO "guessTarget"
        target <- guessTarget ('*':fileName) Nothing
        mFileContentsBuffer <- fileContentsStringToBuffer mFileContents
        --logIO "setTargets"
        setTargets [target { targetContents = mFileContentsBuffer }]

        errorsRef <- liftIO (newIORef "")
        dflags <- getSessionDynFlags
        --logIO "setSessionDynFlags"
        _ <- setSessionDynFlags dflags { log_action = logHandler errorsRef }

        -- Get the dependencies of the main target
        --logIO "depanal"
        graph <- depanal [] False

        --logIO $ "Loading " ++ show (fileName, expression)
        -- Reload the main target
        --logIO "load LoadAllTargets"
        loadSuccess <- load LoadAllTargets
        --logIO $ "Done loading " ++ show (fileName, expression)

        if failed loadSuccess
            then do
                errors <- liftIO (readIORef errorsRef)
                return (Left [errors])
            else do
                --logIO "typecheckModule"
                -- We must parse and typecheck modules before they'll be available for usage
                forM_ graph (typecheckModule <=< parseModule)

                -- Load the dependencies of the main target

                -- This brings all top-level definitions into scope (whether exported or not),
                -- but only works on interpreted modules
                --setContext (IIModule . ms_mod_name <$> graph)

                setContext (IIDecl . simpleImportDecl . ms_mod_name <$> graph)

                --logIO $ "Compiling " ++ show (fileName, expression)
                --result <- compileExpr expression
                --logIO "dynCompileExpr"
                result <- dynCompileExpr expression
                --logIO $ "Done compiling " ++ show (fileName, expression)

                return (Right (CompiledValue result))

catchExceptions :: ExceptionMonad m => m (Either [String] a) -> m (Either [String] a)
catchExceptions a = gcatch a
    (\(_x :: SomeException) -> do
        liftIO (putStrLn ("Caught exception during recompileExpressionInFile: " ++ show _x))
        return (Left [show _x]))


-- A helper from interactive-diagrams to print out GHC API values,
-- useful while debugging the API.
-- | Outputs any value that can be pretty-printed using the default style
output :: (GhcMonad m, Outputable a) => a -> m ()
output a = do
    dfs <- getSessionDynFlags
    let style = defaultUserStyle
    let cntx  = initSDocContext dfs style
    liftIO $ print $ runSDoc (ppr a) cntx

logHandler :: IORef String -> LogAction
#if __GLASGOW_HASKELL__ >= 800
logHandler ref dflags _warnReason severity srcSpan style msg =
#else
logHandler ref dflags severity srcSpan style msg =
#endif
    case severity of
       SevError   -> modifyIORef' ref (++ ('\n':messageWithLocation))
       SevFatal   -> modifyIORef' ref (++ ('\n':messageWithLocation))
       SevWarning -> modifyIORef' ref (++ ('\n':messageWithLocation))
       _          -> do
            putStr messageOther
            return () -- ignore the rest
    where cntx = initSDocContext dflags style
          locMsg = mkLocMessage severity srcSpan msg
          messageWithLocation = show (runSDoc locMsg cntx)
          messageOther = show (runSDoc msg cntx)
