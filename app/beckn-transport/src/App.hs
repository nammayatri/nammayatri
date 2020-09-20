{-# LANGUAGE TypeApplications #-}

module App where

import qualified App.Server as App
import App.Types
import Beckn.External.FCM.Utils
import Beckn.Storage.DB.Config (prepareDBConnections)
import Beckn.Storage.Redis.Config
import qualified Beckn.Types.App as App
import Beckn.Utils.Common
import Beckn.Utils.Dhall (readDhallConfigDefault)
import Beckn.Utils.Logging
import Beckn.Utils.Migration
import qualified Beckn.Utils.Monitoring.Prometheus.Metrics as Metrics
import Beckn.Utils.Servant.Server
import EulerHS.Prelude
import qualified EulerHS.Runtime as R
import Network.Wai
import Network.Wai.Handler.Warp
  ( Settings,
    defaultSettings,
    runSettings,
    setOnExceptionResponse,
    setPort,
  )

runTransporterBackendApp :: Bool -> IO ()
runTransporterBackendApp isTest = do
  appEnv <- readDhallConfigDefault "beckn-transport"
  Metrics.serve (metricsPort appEnv)
  runTransporterBackendApp' isTest appEnv $
    setOnExceptionResponse transporterExceptionResponse $
      setPort (port appEnv) defaultSettings

-- | Prepare common applications options
prepareAppOptions :: Flow ()
prepareAppOptions =
  -- FCM token ( options key = FCMTokenKey )
  createFCMTokenRefreshThread

runTransporterBackendApp' :: Bool -> AppEnv -> Settings -> IO ()
runTransporterBackendApp' isTest appEnv settings = do
  let loggerCfg =
        getEulerLoggerConfig
          isTest
          "/tmp/beckn-transport.log"
          $ loggerConfig appEnv
  R.withFlowRuntime (Just loggerCfg) $ \flowRt -> do
    putStrLn @String "Initializing DB Connections..."
    try (runFlowR flowRt appEnv prepareDBConnections) >>= \case
      Left (e :: SomeException) -> putStrLn @String ("Exception thrown: " <> show e)
      Right _ -> do
        putStrLn @String "Initializing Redis Connections..."
        try (runFlowR flowRt appEnv $ prepareRedisConnections $ redisCfg appEnv) >>= \case
          Left (e :: SomeException) -> putStrLn @String ("Exception thrown: " <> show e)
          Right _ -> do
            putStrLn @String "Initializing Options..."
            try (runFlowR flowRt appEnv prepareAppOptions) >>= \case
              Left (e :: SomeException) -> putStrLn @String ("Exception thrown: " <> show e)
              Right _ ->
                putStrLn @String ("Runtime created. Starting server at port " <> show (port appEnv))
            _ <- migrateIfNeeded (migrationPath appEnv) (dbCfg appEnv) (autoMigrate appEnv)
            runSettings settings $ App.run (App.EnvR flowRt appEnv)

transporterExceptionResponse :: SomeException -> Response
transporterExceptionResponse = exceptionResponse
