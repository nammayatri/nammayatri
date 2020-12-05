{-# LANGUAGE OverloadedLabels #-}
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
import qualified Beckn.Utils.Registry as Registry
import Beckn.Utils.Servant.Server
import Beckn.Utils.Servant.SignatureAuth (signatureAuthManager, signatureAuthManagerKey)
import qualified Data.Map.Strict as Map
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
  let loggerCfg = getEulerLoggerConfig $ loggerConfig appEnv
  R.withFlowRuntime (Just loggerCfg) $ \flowRt -> do
    putStrLn @String "Initializing DB Connections..."
    authManager <- prepareAuthManager flowRt
    let flowRt' = flowRt {R._httpClientManagers = Map.singleton signatureAuthManagerKey authManager}
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
            runSettings settings $ App.run (App.EnvR flowRt' appEnv)
  where
    prepareAuthManager flowRt = do
      let selfId = appEnv ^. #selfId
      creds <-
        runFlowR flowRt appEnv $
          Registry.lookupOrg selfId
            >>= maybe (error $ "No creds found for id: " <> selfId) pure
      let uniqueKeyId = creds ^. #_uniqueKeyId

      privateKey <-
        maybe (error $ "No private key found for credential: " <> show uniqueKeyId) pure (Registry.decodeKey <$> creds ^. #_signPrivKey)
          >>= maybe (error $ "No private key to decode: " <> fromMaybe "No Key" (creds ^. #_signPrivKey)) pure
      signatureAuthManager flowRt appEnv "Authorization" privateKey selfId uniqueKeyId (appEnv ^. #signatureExpiry)

transporterExceptionResponse :: SomeException -> Response
transporterExceptionResponse = exceptionResponse
