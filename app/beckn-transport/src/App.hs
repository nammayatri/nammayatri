{-# LANGUAGE TypeApplications #-}

module App where

import qualified App.Server as App
import App.Types
import Beckn.Constants.APIErrorCode
import Beckn.External.FCM.Utils
import Beckn.Storage.DB.Config (prepareDBConnections)
import Beckn.Storage.Redis.Config
import qualified Beckn.Types.App as App
import Beckn.Utils.Common
import Beckn.Utils.Dhall (readDhallConfigDefault)
import Beckn.Utils.Migration
import qualified Beckn.Utils.Monitoring.Prometheus.Metrics as Metrics
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BS
import EulerHS.Prelude
import qualified EulerHS.Runtime as R
import qualified EulerHS.Types as T
import qualified Network.HTTP.Types as H
import Network.Wai
import Network.Wai.Handler.Warp
  ( Settings,
    defaultSettings,
    runSettings,
    setOnExceptionResponse,
    setPort,
  )
import Servant

runTransporterBackendApp :: IO ()
runTransporterBackendApp = do
  appEnv <- readDhallConfigDefault tyEnv "beckn-transport"
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
  let loggerCfg =
        T.defaultLoggerConfig
          { T._logToFile = True,
            T._logFilePath = "/tmp/beckn-transport.log",
            T._isAsync = True,
            T._logRawSql = logRawSql appEnv
          }
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
transporterExceptionResponse exception = do
  let anyException = fromException exception
  case anyException of
    Just ex ->
      responseLBS
        (H.Status (errHTTPCode ex) (BS.pack $ errReasonPhrase ex))
        ((H.hContentType, "application/json") : errHeaders ex)
        (errBody ex)
    Nothing ->
      responseLBS
        H.internalServerError500
        [(H.hContentType, "application/json")]
        (Aeson.encode internalServerErr)
