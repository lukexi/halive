import qualified GHC
import GHC              
import qualified GHC.Paths
import DynFlags
import MonadUtils       ( liftIO )
import Linker
import Control.Monad

mainFileName, expression, mainModuleName :: String
(mainFileName, mainModuleName, expression) = ("glfw.hs", "go", "HotGLFW")

main :: IO ()
main = GHC.defaultErrorHandler defaultFatalMessager defaultFlushOut $ GHC.runGhc (Just GHC.Paths.libdir) $ do
    GHC.getSessionDynFlags >>= \dflags -> do
        void $ GHC.setSessionDynFlags dflags
            { ghcMode   = CompManager
            , hscTarget = HscInterpreted
            , ghcLink   = LinkInMemory
            }

    handleSourceError GHC.printException $ do
        liftIO . initDynLinker =<< GHC.getSessionDynFlags

        GHC.setTargets =<< sequence [guessTarget mainFileName Nothing]
        _ <- GHC.load LoadAllTargets

        modSum <- getModSummary (mkModuleName mainModuleName)
        setContext [IIModule (moduleName (ms_mod modSum))]

        rr <- runStmt expression RunToCompletion
        case rr of
            RunOk _ -> liftIO $ putStrLn "OK"
            _ -> liftIO $ putStrLn "erRrorror"
        return ()