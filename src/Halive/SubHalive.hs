{-# LANGUAGE OverloadedStrings, LambdaCase, ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
module Halive.SubHalive (module Halive.SubHalive, ExtensionFlag(..)) where

import GHC
import DynFlags
import Exception
import ErrUtils
import HscTypes
import GHC.Paths
import Outputable
import StringBuffer

--import Packages
import Linker
import Module

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
    , gscLanguageExtensions :: [ExtensionFlag]
    , gscCompilationMode    :: CompliationMode
    , gscStartupFile        :: Maybe (FilePath, String)
        -- ^ Allow API users to block until a given file is compiled,
        -- to work around a bug where the GHC API crashes while loading libraries
        -- if the main thread is doing work (possibly due to accessing said libraries in some way)
    , gscVerbosity          :: Int
    }



defaultGHCSessionConfig :: GHCSessionConfig
defaultGHCSessionConfig = GHCSessionConfig
    { gscFixDebounce = DebounceFix
    , gscImportPaths = []
    , gscPackageDBs  = []
    , gscLanguageExtensions = []
    , gscLibDir = libdir
    , gscCompilationMode = Interpreted
    , gscStartupFile = Nothing
    , gscVerbosity = 0
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
    let restoreControlC f = do
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

        -- Make sure we're configured for live-reload
        let dflags4 = dflags3 { hscTarget   = if gscCompilationMode == Compiled then HscAsm else HscInterpreted
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
            dflags5 = if gscFixDebounce == DebounceFix
                        then dflags4 `gopt_set` Opt_ForceRecomp
                        else dflags4
            dflags6 = foldl xopt_set dflags5 gscLanguageExtensions

        -- We must call setSessionDynFlags before calling initPackages or any other GHC API
        packageIDs <- setSessionDynFlags dflags6
        dflags7 <- getSessionDynFlags


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
        liftIO $ linkPackages dflags7 finalPackageIDs

        -- Initialize the package database and dynamic linker.
        -- Explicitly calling these avoids crashes on some of my machines.

        --logIO $ "initDynLinker"
        dflags8 <- getSessionDynFlags
        liftIO (initDynLinker dflags8)



        --logIO $ "withGHCSession Done"

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
        target <- guessTarget ('*':fileName) Nothing
        mFileContentsBuffer <- fileContentsStringToBuffer mFileContents
        setTargets [target { targetContents = mFileContentsBuffer }]

        errorsRef <- liftIO (newIORef "")
        dflags <- getSessionDynFlags
        _ <- setSessionDynFlags dflags { log_action = logHandler errorsRef }

        -- Get the dependencies of the main target
        graph <- depanal [] False

        --logIO $ "Loading " ++ show (fileName, expression)
        -- Reload the main target
        loadSuccess <- load LoadAllTargets
        --logIO $ "Done loading " ++ show (fileName, expression)

        if failed loadSuccess
            then do
                errors <- liftIO (readIORef errorsRef)
                return (Left [errors])
            else do
                -- We must parse and typecheck modules before they'll be available for usage
                forM_ graph (typecheckModule <=< parseModule)

                -- Load the dependencies of the main target
                -- This brings all top-level definitions into scope (whether exported or not),
                -- but only works on interpreted modules
                --setContext (IIModule . ms_mod_name <$> graph)
                setContext (IIDecl . simpleImportDecl . ms_mod_name <$> graph)

                --logIO $ "Compiling " ++ show (fileName, expression)
                --result <- compileExpr expression
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
output :: (GhcMonad m, MonadIO m) => Outputable a => a -> m ()
output a = do
    dfs <- getSessionDynFlags
    let style = defaultUserStyle
    let cntx  = initSDocContext dfs style
    liftIO $ print $ runSDoc (ppr a) cntx

logHandler :: IORef String -> LogAction
logHandler ref dflags severity srcSpan style msg =
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
