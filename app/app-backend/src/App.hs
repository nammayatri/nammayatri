{-# LANGUAGE TypeApplications #-}

module App where

import qualified App.Server as App
import App.Types
import Beckn.Constants.APIErrorCode (internalServerErr)
import qualified Beckn.Types.App as App
import Beckn.Utils.Common (prepareAppOptions, runFlowR)
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
import Storage.DB.Config
import qualified System.Environment as SE

runAppBackend :: IO ()
runAppBackend = do
  port <- fromMaybe 8013 . (>>= readMaybe) <$> SE.lookupEnv "PORT"
  runAppBackend' port $
    setOnExceptionResponse appExceptionResponse $
      setPort port defaultSettings

runAppBackend' :: Int -> Settings -> IO ()
runAppBackend' port settings = do
  let appEnv = AppEnv App.CommonEnv
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
