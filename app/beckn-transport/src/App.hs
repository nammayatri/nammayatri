{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module App where

import qualified App.Server as App
import App.Types
import Beckn.External.FCM.Utils
import Beckn.Storage.Redis.Config
import qualified Beckn.Types.App as App
import qualified Beckn.Types.Storage.Organization as Organization
import Beckn.Utils.Common
import Beckn.Utils.Dhall (readDhallConfigDefault)
import Beckn.Utils.Logging
import Beckn.Utils.Migration
import qualified Beckn.Utils.Monitoring.Prometheus.Metrics as Metrics
import Beckn.Utils.Servant.Server
import Beckn.Utils.Servant.SignatureAuth
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
import qualified Storage.Queries.Organization as Storage

runTransporterBackendApp :: (AppEnv -> AppEnv) -> IO ()
runTransporterBackendApp configModifier = do
  appEnv <- configModifier <$> readDhallConfigDefault "beckn-transport"
  Metrics.serve (metricsPort appEnv)
  runTransporterBackendApp' appEnv $
    setOnExceptionResponse transporterExceptionResponse $
      setPort (port appEnv) defaultSettings

-- | Prepare common applications options
prepareAppOptions :: Flow ()
prepareAppOptions =
  -- FCM token ( options key = FCMTokenKey )
  createFCMTokenRefreshThread

runTransporterBackendApp' :: AppEnv -> Settings -> IO ()
runTransporterBackendApp' appEnv settings = do
  let loggerCfg = getEulerLoggerConfig $ appEnv ^. #loggerConfig
  R.withFlowRuntime (Just loggerCfg) $ \flowRt -> do
    putStrLn @String "Setting up for signature auth..."
    try (runFlowR flowRt appEnv Storage.loadAllProviders) >>= \case
      Left (e :: SomeException) -> putStrLn @String ("Exception thrown: " <> show e)
      Right allProviders -> do
        let allShortIds = map (App._getShortOrganizationId . Organization._shortId) allProviders
        case prepareAuthManagers flowRt appEnv allShortIds of
          Left err -> putStrLn @String ("Could not prepare authentication managers: " <> show err)
          Right getManagers -> do
            managerMap <- getManagers
            putStrLn @Text $ "Loaded http managers - " <> show (keys managerMap)
            let flowRt' = flowRt {R._httpClientManagers = managerMap}
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
                runSettings settings $ App.run (App.EnvR flowRt' appEnv)

transporterExceptionResponse :: SomeException -> Response
transporterExceptionResponse = exceptionResponse
