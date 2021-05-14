{-# LANGUAGE TypeApplications #-}

module App
  ( runGateway,
  )
where

import App.Server
import App.Types
import Beckn.Exit
import Beckn.Storage.Common (prepareDBConnections)
import Beckn.Storage.Redis.Config (prepareRedisConnections)
import qualified Beckn.Types.App as App
import Beckn.Utils.App
import Beckn.Utils.Dhall (readDhallConfigDefault)
import Beckn.Utils.Migration
import qualified Beckn.Utils.Monitoring.Prometheus.Metrics as Metrics
import Beckn.Utils.Servant.SignatureAuth
import qualified Data.Cache as C
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (exitSuccess)
import EulerHS.Runtime as E
import qualified EulerHS.Runtime as R
import Network.Wai.Handler.Warp
  ( defaultSettings,
    runSettings,
    setOnClose,
    setOnOpen,
    setPort,
  )
import System.Environment
import System.Exit (ExitCode)
import System.Posix.Signals (Handler (Catch), installHandler, sigINT, sigTERM)
import Utils.Common

runGateway :: (AppCfg -> AppCfg) -> IO ()
runGateway configModifier = do
  appCfg <- configModifier <$> readDhallConfigDefault "beckn-gateway"
  let port = appCfg ^. #port
  let metricsPort = appCfg ^. #metricsPort
  Metrics.serve metricsPort
  -- shutdown and activeConnections will be used to signal and detect our exit criteria
  shutdown <- newEmptyTMVarIO
  activeConnections <- newTVarIO (0 :: Int)
  void $ installHandler sigTERM (Catch $ handleShutdown shutdown exitSigTERMFailure) Nothing
  void $ installHandler sigINT (Catch $ handleShutdown shutdown exitSigINTFailure) Nothing
  hostname <- (T.pack <$>) <$> lookupEnv "POD_NAME"
  let loggerRt = getEulerLoggerRuntime hostname $ appCfg ^. #loggerConfig
      settings =
        setOnOpen (\_ -> atomically $ modifyTVar' activeConnections (+ 1) >> return True) $
          setOnClose (\_ -> atomically $ modifyTVar' activeConnections (subtract 1)) $
            setPort port defaultSettings
  let redisCfg = appCfg ^. #redisCfg
  let migrationPath = appCfg ^. #migrationPath
  let dbCfg = appCfg ^. #dbCfg
  let autoMigrate = appCfg ^. #autoMigrate
  cache <- C.newCache Nothing
  appEnv <- buildAppEnv appCfg cache
  E.withFlowRuntime (Just loggerRt) $ \flowRt -> do
    flowRt' <- runFlowR flowRt appEnv $ do
      withLogTag "Server startup" $ do
        let shortOrgId = appEnv ^. #gwId
        getManager <-
          handleLeft exitAuthManagerPrepFailure "Could not prepare authentication manager: " $
            prepareAuthManager flowRt appEnv "Proxy-Authorization" shortOrgId
        authManager <- L.runIO getManager
        logInfo "Initializing Redis Connections..."
        try (prepareRedisConnections redisCfg)
          >>= handleLeft @SomeException exitRedisConnPrepFailure "Exception thrown: "
        _ <-
          prepareDBConnections
            >>= handleLeft exitDBConnPrepFailure "Exception thrown: "
        migrateIfNeeded migrationPath dbCfg autoMigrate
          >>= handleLeft exitDBMigrationFailure "Couldn't migrate database: "
        logInfo ("Runtime created. Starting server at port " <> show port)
        return $ flowRt {R._httpClientManagers = Map.singleton signatureAuthManagerKey authManager}
    threadId <- forkIO $ runSettings settings $ run shutdown (App.EnvR flowRt' appEnv)

    -- Wait for shutdown
    exitCode <- atomically $ readTMVar shutdown
    -- Wait to drain all connections
    putStrLn @String "Draining connections"
    waitForDrain activeConnections 120000000
    -- Kill the thread
    killThread threadId
    exitWith exitCode
  where
    handleShutdown :: TMVar ExitCode -> ExitCode -> IO ()
    handleShutdown shutdown code = atomically $ putTMVar shutdown code

    waitForDrain :: TVar Int -> Int -> IO ()
    waitForDrain activeConnections ms = do
      conns <- readTVarIO activeConnections
      unless (ms == 0 || conns == 0) $ do
        -- Wait 100ms and recurse
        let sleep = 100000
        threadDelay sleep
        waitForDrain activeConnections $ ms - sleep
