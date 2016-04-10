{-# LANGUAGE OverloadedStrings, LambdaCase, ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
module Halive.SubHalive where

import GHC
import DynFlags
import Exception
import ErrUtils
import HscTypes
import GHC.Paths
import Outputable
import Unsafe.Coerce
import StringBuffer
--import Packages
--import Linker
import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Data.Time
import Halive.FindPackageDBs

import Control.Concurrent
import System.Signal

data FixDebounce = DebounceFix | NoDebounceFix deriving Eq

data GHCSessionConfig = GHCSessionConfig
    { gscFixDebounce        :: FixDebounce
    , gscImportPaths        :: [FilePath]
    , gscPackageDBs         :: [FilePath]
    , gscLibDir             :: FilePath
    , gscLanguageExtensions :: [ExtensionFlag]
    , gscCompilationMode    :: HscTarget
    }

-- Probably shouldn't be here, but needed for Rumpus
defaultLanguageExtensions :: [ExtensionFlag]
defaultLanguageExtensions = 
    [ Opt_FlexibleContexts
    , Opt_RecordWildCards
    , Opt_ViewPatterns
    , Opt_LambdaCase
    , Opt_MultiWayIf
    , Opt_BangPatterns
    ]

defaultGHCSessionConfig :: GHCSessionConfig
defaultGHCSessionConfig = GHCSessionConfig 
    { gscFixDebounce = DebounceFix
    , gscImportPaths = []
    , gscPackageDBs  = []
    , gscLanguageExtensions = defaultLanguageExtensions
    , gscLibDir = libdir
    , gscCompilationMode = HscInterpreted
    --, gscCompilationMode = HscAsm
    }


-- Starts up a GHC session and then runs the given action within it
withGHCSession :: ThreadId -> GHCSessionConfig -> Ghc a -> IO a
withGHCSession mainThreadID GHCSessionConfig{..} action = do
    -- Work around https://ghc.haskell.org/trac/ghc/ticket/4162
    let restoreControlC action = do
            liftIO $ installHandler sigINT (\_signal -> killThread mainThreadID)
            action
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
        let dflags4 = dflags3 { hscTarget   = gscCompilationMode
                              , optLevel    = 2
                              , ghcLink     = LinkInMemory
                              , ghcMode     = CompManager
                              , importPaths = gscImportPaths
                              --, verbosity = 5
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
        _ <- setSessionDynFlags dflags6

        -- NOTE: I've disabled these init calls as they seem to happen implicitly,
        -- but searching for initDynLinker online suggests it may be required
        -- for certain configurations, so if trouble arises try turning them back on.
        -- (they don't hurt anything but slow startup)

        -- Initialize the package database and dynamic linker
        --(dflags7, _) <- liftIO (initPackages dflags6)
        --liftIO (initDynLinker dflags7)

        action

-- See note below - this isn't actually called right now
gatherErrors :: GhcMonad m => SourceError -> m [String]
gatherErrors sourceError = do
    printException sourceError
    dflags <- getSessionDynFlags
    let errorSDocs = pprErrMsgBagWithLoc (srcErrorMessages sourceError)
        errorStrings = map (showSDoc dflags) errorSDocs
    return errorStrings

newtype CompiledValue = CompiledValue HValue

getCompiledValue :: CompiledValue -> a
getCompiledValue (CompiledValue r) = unsafeCoerce r

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

        -- Reload the main target
        loadSuccess <- load LoadAllTargets

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
                
                result <- compileExpr expression

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
       SevError   ->  modifyIORef' ref (++ ('\n':printDoc))
       SevFatal   ->  modifyIORef' ref (++ ('\n':printDoc))
       SevWarning ->  modifyIORef' ref (++ ('\n':printDoc))
       _          ->  return () -- ignore the rest
    where cntx = initSDocContext dflags style
          locMsg = mkLocMessage severity srcSpan msg
          printDoc = show (runSDoc locMsg cntx) 
