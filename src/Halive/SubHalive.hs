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
import Paths_halive

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

-- Starts up a GHC session and then runs the given action within it
withGHCSession :: ThreadId -> GHCSessionConfig -> Ghc a -> IO a
withGHCSession mainThreadID GHCSessionConfig{..} action = do
    -- Work around https://ghc.haskell.org/trac/ghc/ticket/4162
    let restoreControlC f = do
            liftIO $ installHandler sigINT
                (\_signal -> killThread mainThreadID)
            f

    -- defaultErrorHandler defaultFatalMessager defaultFlushOut $ runGhc (Just libdir) $ do
    runGhc (Just gscLibDir) . restoreControlC $ do

        packageIDs <-
                getSessionDynFlags
            >>= updateDynFlagsWithGlobalDB
            -- If this is a stack project, add its package DBs
            >>= updateDynFlagsWithStackDB
            -- If there's a sandbox, add its package DB
            >>= updateDynFlagsWithCabalSandbox
            -- Add passed-in package DBs
            >>= (pure . addExtraPkgConfs gscPackageDBs)
            -- Make sure we're configured for live-reload
            >>= (\d -> pure d
                { hscTarget   = if gscCompilationMode == Compiled then HscAsm else HscInterpreted
                , optLevel    = if gscCompilationMode == Compiled then 2 else 0
                , ghcLink     = LinkInMemory
                , ghcMode     = CompManager
                , importPaths = gscImportPaths
                , objectDir   = Just ".halive"
                , hiDir       = Just ".halive"
                , stubDir     = Just ".halive"
                , dumpDir     = Just ".halive"
                , verbosity   = gscVerbosity
                })
            -- turn off the GHCi sandbox
            -- since it breaks OpenGL/GUI usage
            >>= (pure . (`gopt_unset` Opt_GhciSandbox))
            -- GHC seems to try to "debounce" compilations within
            -- about a half second (i.e., it won't recompile)
            -- This fixes that, but probably isn't quite what we want
            -- since it will cause extra files to be recompiled...
            >>= (pure . (if gscFixDebounce == DebounceFix
                            then (`gopt_set` Opt_ForceRecomp)
                            else id))
            >>= (pure . flip (foldl xopt_unset) gscNoLanguageExtensions
                      . flip (foldl xopt_set) gscLanguageExtensions)
            -- We must call setSessionDynFlags before calling initPackages or any other GHC API
            >>= setSessionDynFlags


        -- Initialize the package database and dynamic linker.
        -- Explicitly calling these avoids crashes on some of my machines.
#if __GLASGOW_HASKELL__ >= 800
        getSession >>= \hscEnv ->
            liftIO $ linkPackages hscEnv packageIDs
        liftIO . initDynLinker =<< getSession
#else
        getSessionDynFlags >>= \dflags ->
            liftIO $ linkPackages dflags packageIDs
        liftIO . initDynLinker =<< getSessionDynFlags
#endif

        action



newtype CompiledValue = CompiledValue Dynamic deriving Show

getCompiledValue :: Typeable a => CompiledValue -> Maybe a
getCompiledValue (CompiledValue r) = fromDynamic r

fileContentsStringToBuffer :: (MonadIO m) => String -> m (StringBuffer, UTCTime)
fileContentsStringToBuffer fileContents = do
    now <- liftIO getCurrentTime
    return (stringToStringBuffer fileContents, now)

-- | Takes a filename, optionally its contents, and an expression.
-- Returns a list of errors or a Dynamic compiled value
recompileExpressionInFile :: FilePath
                          -> Maybe String
                          -> String
                          -> Ghc (Either String CompiledValue)
recompileExpressionInFile fileName mFileContents expression =

    catchExceptions . handleSourceError (fmap Left . gatherErrors) $ do

        -- Set up an error accumulator
        errorsRef <- liftIO (newIORef "")
        _ <- getSessionDynFlags >>=
            \dflags -> setSessionDynFlags dflags
                { log_action = logHandler errorsRef }

        mFileContentsBuffer <- mapM fileContentsStringToBuffer mFileContents

        -- Set the target
        target <- case fileName of
            -- We'd like to just use a Module name for the target,
            -- but load/depanal fails with "Foo is a package module"
            -- As a workaround, we install a blank dummy X.hs file
            -- and use it as the target.
            "" -> guessTarget' =<< liftIO (getDataFileName "X.hs")
            other -> guessTarget' other

        logIO "Setting targets..."
        setTargets [target { targetContents = mFileContentsBuffer }]



        -- Reload the main target
        logIO "Loading..."
        loadSuccess <- load LoadAllTargets

        if succeeded loadSuccess
            then do

                logIO "Analyzing deps..."
                -- Get the dependencies of the main target (and update the session with them)
                graph <- depanal [] False
                -- -- We must parse and typecheck modules before they'll be available for usage
                -- forM_ graph (typecheckModule <=< parseModule)

                -- Load the dependencies of the main target
                setContext $
                    (IIDecl . simpleImportDecl . ms_mod_name <$> graph)

                -- Compile the expression and return the result
                result <- dynCompileExpr expression

                return (Right (CompiledValue result))
            else do
                -- Extract the errors from the accumulator
                errors <- liftIO (readIORef errorsRef)
                return (Left errors)

-- Prepend a '*' to prevent GHC from trying to load from any previously compiled object files
-- see http://stackoverflow.com/questions/12790341/haskell-ghc-dynamic-compliation-only-works-on-first-compile
guessTarget' :: GhcMonad m => String -> m Target
guessTarget' fileName = guessTarget ('*':fileName) Nothing

catchExceptions :: ExceptionMonad m => m (Either String a) -> m (Either String a)
catchExceptions a = gcatch a
    (\(_x :: SomeException) -> do
        liftIO (putStrLn ("Caught exception during recompileExpressionInFile: " ++ show _x))
        return (Left (show _x))
        )



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



-- A helper from interactive-diagrams to print out GHC API values,
-- useful while debugging the API.
-- | Outputs any value that can be pretty-printed using the default style
output :: (GhcMonad m, Outputable a) => a -> m ()
output a = do
    dfs <- getSessionDynFlags
    let style = defaultUserStyle
    let cntx  = initSDocContext dfs style
    liftIO $ print $ runSDoc (ppr a) cntx


-- NOTE: handleSourceError (which calls gatherErrors above)
-- doesn't actually seem to do anything, so we use
-- the IORef + log_action solution instead.
-- The API docs claim 'load' should
-- throw SourceErrors but it doesn't afaict.
gatherErrors :: GhcMonad m => SourceError -> m String
gatherErrors sourceError = do
    printException sourceError
    dflags <- getSessionDynFlags
    let errorSDocs = pprErrMsgBagWithLoc (srcErrorMessages sourceError)
        errorStrings = map (showSDoc dflags) errorSDocs
    return (concat errorStrings)


--pkgConfRefToString = \case
--    GlobalPkgConf -> "GlobalPkgConf"
--    UserPkgConf -> "UserPkgConf"
--    PkgConfFile file -> "PkgConfFile " ++ show file

--extraPkgConfsToString dflags = show $ map pkgConfRefToString $ extraPkgConfs dflags $ []

logIO :: MonadIO m => String -> m ()
logIO = liftIO . putStrLn
