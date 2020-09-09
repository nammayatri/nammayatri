{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module App where

import qualified App.Server as App
import App.Types
import Beckn.Constants.APIErrorCode (internalServerErr)
import Beckn.External.FCM.Utils
import Beckn.Storage.DB.Config (prepareDBConnections)
import qualified Beckn.Types.App as App
import Beckn.Utils.Common
import Beckn.Utils.Dhall (readDhallConfigDefault)
import Beckn.Utils.Logging
import Beckn.Utils.Migration
import qualified Beckn.Utils.Monitoring.Prometheus.Metrics as Metrics
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BS
import EulerHS.Prelude
import qualified EulerHS.Runtime as R
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

runAppBackend :: IO ()
runAppBackend = do
  appEnv <- readDhallConfigDefault tyEnv "app-backend"
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
  let loggerCfg = getEulerLoggerConfig "/tmp/app-backend.log" $ loggerConfig appEnv
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
            putStrLn @String ("Runtime created. Starting server at port " <> show (port appEnv))
        _ <- migrateIfNeeded (migrationPath appEnv) (dbCfg appEnv) (autoMigrate appEnv)
        runSettings settings $ App.run (App.EnvR flowRt appEnv)

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
