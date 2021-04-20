{-# LANGUAGE QuantifiedConstraints #-}
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
import Control.Concurrent
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
import System.Posix.Signals (Handler (Catch), installHandler, sigINT, sigTERM)
import Utils.Common

runGateway :: (AppCfg -> AppCfg) -> IO ()
runGateway configModifier = do
  appCfg <- configModifier <$> readDhallConfigDefault "beckn-gateway"
  let port = appCfg ^. #port
  let metricsPort = appCfg ^. #metricsPort
  Metrics.serve metricsPort
  -- shutdown and activeConnections will be used to signal and detect our exit criteria
  appEnv <- buildAppEnv appCfg 
  let shutdown = appEnv ^. #isShutdown
  activeConnections <- newTVarIO (0 :: Int)
  threadId <- myThreadId
  void $ installHandler sigTERM (Catch $ handleShutdown activeConnections shutdown exitSigTERM threadId) Nothing
  void $ installHandler sigINT (Catch $ handleShutdown activeConnections shutdown exitSigINT threadId) Nothing
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
    runSettings settings $ run (App.EnvR flowRt' appEnv)
