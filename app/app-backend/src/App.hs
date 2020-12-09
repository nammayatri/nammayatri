{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module App where

import qualified App.Server as App
import App.Types
import Beckn.External.FCM.Utils
import Beckn.Storage.DB.Config (prepareDBConnections)
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

runAppBackend :: (AppEnv -> AppEnv) -> IO ()
runAppBackend configModifier = do
  appEnv <- configModifier <$> readDhallConfigDefault "app-backend"
  Metrics.serve (metricsPort appEnv)
  runAppBackend' appEnv $
    setOnExceptionResponse appExceptionResponse $
      setPort (port appEnv) defaultSettings

-- | Prepare common applications options
prepareAppOptions :: Flow ()
prepareAppOptions =
  -- FCM token ( options key = FCMTokenKey )
  createFCMTokenRefreshThread

runAppBackend' :: AppEnv -> Settings -> IO ()
runAppBackend' appEnv settings = do
  let loggerCfg = getEulerLoggerConfig $ loggerConfig appEnv
  R.withFlowRuntime (Just loggerCfg) $ \flowRt -> do
    putStrLn @String "Setting up for signature auth..."
    authManager <- case appEnv ^. #bapSelfId of
      Nothing -> error "Self id not configured"
      Just selfId -> do
        mbCreds <-
          runFlowR flowRt appEnv $
            Registry.lookupOrg selfId
        case mbCreds of
          Nothing -> error $ "No credentials for: " <> selfId
          Just creds -> do
            let privKey = fromJust $ Registry.decodeKey =<< creds ^. #_signPrivKey
            let keyId = creds ^. #_keyId
            -- 10 minutes should be long enough for messages to go through
            signatureAuthManager "Authorization" privKey selfId keyId (appEnv ^. #signatureExpiry)
    let flowRt' = flowRt {R._httpClientManagers = Map.singleton signatureAuthManagerKey authManager}
    putStrLn @String "Initializing DB Connections..."
    let prepare = prepareDBConnections
    try (runFlowR flowRt' appEnv prepare) >>= \case
      Left (e :: SomeException) -> putStrLn @String ("Exception thrown: " <> show e)
      Right _ -> do
        putStrLn @String "Initializing Options..."
        try (runFlowR flowRt' appEnv prepareAppOptions) >>= \case
          Left (e :: SomeException) -> putStrLn @String ("Exception thrown: " <> show e)
          Right _ ->
            putStrLn @String ("Runtime created. Starting server at port " <> show (port appEnv))
        _ <- migrateIfNeeded (migrationPath appEnv) (dbCfg appEnv) (autoMigrate appEnv)
        runSettings settings $ App.run (App.EnvR flowRt' appEnv)

appExceptionResponse :: SomeException -> Response
appExceptionResponse = exceptionResponse
