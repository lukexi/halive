import DynFlags
import GHC
-- import Outputable
import GHC.Paths
import Control.Monad.IO.Class
import Unsafe.Coerce
import Control.Concurrent
import Control.Monad
import Foreign.Store
import SandboxPath

-- sandboxFlags :: String
-- sandboxFlags = "-package-db=/Users/lukexi/Projects/Thop/.cabal-sandbox/x86_64-osx-ghc-7.10.1-packages.conf.d"

main :: IO ()
main = do
    sandboxFlags <- getSandboxFlags
    Store storeID <- newStore ((*2) :: Int -> Int)
    (leftovers, _) <- liftIO $ parseStaticFlags [noLoc sandboxFlags]
    defaultErrorHandler defaultFatalMessager defaultFlushOut $ runGhc (Just libdir) $ do
        -- we have to call 'setSessionDynFlags' before doing
        -- everything else
        dflags <- getSessionDynFlags
        (dflags2, _, _) <- liftIO $ parseDynamicFlags dflags leftovers
        -- If we want to make GHC interpret our code on the fly, we
        -- ought to set those two flags, otherwise we
        -- wouldn't be able to use 'setContext' below
        setSessionDynFlags $ dflags2 { hscTarget = HscInterpreted
                                     , ghcLink   = LinkInMemory
                                     }
        setTargets =<< sequence [guessTarget "test.hs" Nothing]

        forever . handleSourceError printException $ do
            liftIO $ threadDelay 500000
            load LoadAllTargets
            liftIO . putStrLn $ "Getting summary..."
            modSum <- getModSummary $ mkModuleName "Test"
            liftIO . putStrLn $ "Parsing..."
            p <- parseModule modSum
            liftIO . putStrLn $ "Typechecking..."
            _t <- typecheckModule p
            -- liftIO . putStrLn $ "Desugaring..."
            -- d <- desugarModule t
            -- liftIO . putStrLn $ "Loading..."
            -- _l <- loadModule d

            setContext [IIModule $ moduleName $ ms_mod modSum]

            act <- unsafeCoerce <$> compileExpr ("doodle " ++ show storeID)
            liftIO act
    


            -- liftIO . putStrLn $ "Loaded"
            -- n <- getNamesInScope
            
            -- c <- return $ coreModule d
     
            -- g <- getModuleGraph
            -- mapM showModule g     
            -- liftIO . putStrLn $ showSDoc dflags $ ppr $ (parsedSource d,"/n-----/n",  typecheckedSource d)
        

-- loadLoop = forever $ do
--     load LoadAllTargets
--     -- Bringing the module into the context
--     setContext [IIModule $ mkModuleName "Test"]
--     -- evaluating and running an action
--     act <- unsafeCoerce <$> compileExpr "print test"           
--     liftIO act
--     liftIO $ threadDelay 500000