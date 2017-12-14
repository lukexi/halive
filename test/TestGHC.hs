import GHC.Paths
import GHC
import DynFlags
import Linker
import Control.Monad.IO.Class
import Data.Time.Clock.POSIX
import Data.Time
import StringBuffer
import Data.Dynamic
import System.Directory
import System.FilePath

logIO :: MonadIO m => String -> m ()
logIO = liftIO . putStrLn

withGHC :: Ghc a -> IO a
withGHC action = defaultErrorHandler defaultFatalMessager defaultFlushOut $ runGhc (Just libdir) $ do

    packageIDs <-
            getSessionDynFlags
        >>= (\d -> pure d
            { ghcLink     = LinkInMemory
            , ghcMode     = CompManager
            , hscTarget   = HscAsm
            , optLevel    = 2
            , verbosity   = 0
            })
        -- turn off the GHCi sandbox
        -- since it breaks OpenGL/GUI usage
        >>= (pure . (`gopt_unset` Opt_GhciSandbox))
        >>= (pure . (if dynamicGhc then addWay' WayDyn else id))
        -- We must call setSessionDynFlags before calling initPackages or any other GHC API
        >>= setSessionDynFlags

    getSession >>= \hscEnv ->
        liftIO $ linkPackages hscEnv packageIDs
    liftIO . initDynLinker =<< getSession

    action

fileContentsStringToBuffer :: (MonadIO m) => String -> m (StringBuffer, UTCTime)
fileContentsStringToBuffer fileContents = do
    now <- liftIO getCurrentTime
    return (stringToStringBuffer fileContents, now)

ourFile :: String
ourFile = unlines
    [ "main = print $ 123456789"
    ]

main :: IO ()
main = withGHC $ do
    logIO ""
    logIO "Starting..."

    let expression = "main"
    fileContents <- fileContentsStringToBuffer ourFile

    -- Set the target

    -- Create a dummy temporary file to sate GHC's desires for one,
    -- even though we're passing it the text as a buffer.
    tempDir <- liftIO $ getTemporaryDirectory
    now <- show . diffTimeToPicoseconds . realToFrac <$> liftIO getPOSIXTime
    let tempFile = tempDir </> "halive_" ++ now <.> "hs"
    liftIO $ writeFile tempFile ""

    target <- guessTarget tempFile Nothing

    logIO "Setting targets..."
    setTargets [target { targetContents = Just fileContents }]

    -- logIO "Dep analysis..."
    -- graph <- depanal [mkModuleName "Main"] False

    -- Reload the main target
    logIO "Loading..."
    -- setContext $ [ IIModule . mkModuleName $ "Main" ]
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
                -- [IIDecl
                --  . simpleImportDecl
                --  . mkModuleName $ "Main"]

            -- Compile the expression and return the result
            result <- dynCompileExpr expression

            case fromDynamic result of
                Just a -> liftIO (a :: IO ())
                Nothing -> return ()
            -- liftIO (print result)
        else do
            return ()

