{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module App where

import qualified App.Server as App
import App.Types
import Beckn.Constants.APIErrorCode (internalServerErr)
import Beckn.External.FCM.Utils
import Beckn.Storage.DB.Config
import Beckn.Types.App
import qualified Beckn.Types.App as App
import Beckn.Utils.Common
import qualified Beckn.Utils.Monitoring.Prometheus.Metrics as Metrics
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.Vault.Lazy as V
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
import qualified Storage.DB.Config as Config
import qualified System.Environment as SE

runAppBackend :: IO ()
runAppBackend = do
  port <- fromMaybe 8013 . (>>= readMaybe) <$> SE.lookupEnv "PORT"
  metricsPort <- fromMaybe 9999 . (>>= readMaybe) <$> SE.lookupEnv "METRICS_PORT"
  Metrics.serve metricsPort
  runAppBackend' port $
    setOnExceptionResponse appExceptionResponse $
      setPort port defaultSettings

-- | Prepare common applications options
prepareAppOptions :: Flow ()
prepareAppOptions =
  -- FCM token ( options key = FCMTokenKey )
  createFCMTokenRefreshThread

runAppBackend' :: Int -> Settings -> IO ()
runAppBackend' port settings = do
  let dbEnv = DbEnv Config.defaultDbConfig Config.connectionTag
  let appEnv = AppEnv dbEnv
  let loggerCfg =
        T.defaultLoggerConfig
          { T._logToFile = True,
            T._logFilePath = "/tmp/app-backend.log",
            T._isAsync = True
          }
  reqHeadersKey <- V.newKey
  R.withFlowRuntime (Just loggerCfg) $ \flowRt -> do
    putStrLn @String "Initializing DB Connections..."
    let prepare = prepareDBConnections
    try (runFlowR flowRt appEnv prepare) >>= \case
      Left (e :: SomeException) -> putStrLn @String ("Exception thrown: " <> show e)
      Right _ -> do
        putStrLn @String "Initializing Options..."
        try (runFlowR flowRt appEnv prepareAppOptions) >>= \case
          Left (e :: SomeException) -> putStrLn @String ("Exception thrown: " <> show e)
          Right _ ->
            putStrLn @String ("Runtime created. Starting server at port " <> show port)
        runSettings settings $ App.run reqHeadersKey (App.EnvR flowRt appEnv)

appExceptionResponse :: SomeException -> Response
appExceptionResponse exception = do
  let anyException = fromException exception
  case anyException of
    Just ex ->
      responseLBS
        (H.Status (errHTTPCode ex) $ BS.pack $ errReasonPhrase ex)
        ((H.hContentType, "application/json") : errHeaders ex)
        $ errBody ex
    Nothing ->
      responseLBS
        H.internalServerError500
        [(H.hContentType, "application/json")]
        (Aeson.encode internalServerErr)
