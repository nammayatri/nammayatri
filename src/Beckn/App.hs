module Beckn.App where

import qualified Data.Aeson as Aeson
import qualified Data.Vault.Lazy as V
import EulerHS.Prelude
import qualified Network.HTTP.Client as Client
import Network.Wai
import Network.Wai.Handler.Warp
  ( Settings
  , defaultSettings
  , run
  , runSettings
  , setOnExceptionResponse
  , setPort
  )
import Servant.Server
import Servant
import qualified Data.ByteString.Char8 as BS
import qualified EulerHS.Interpreters as R
import qualified EulerHS.Runtime as R
import qualified EulerHS.Language as L
import qualified EulerHS.Types as T
import qualified Network.HTTP.Types as H
import qualified Beckn.App.Server as App
import qualified Beckn.Types.App as App
import qualified System.Environment as SE
import Beckn.Storage.DB.Config
import Beckn.Utils.Storage
import Beckn.Constants.APIErrorCode

runBecknBackendApp :: IO ()
runBecknBackendApp = do
  port <- fromMaybe 8012 . (>>= readMaybe) <$> SE.lookupEnv "PORT"
  runBecknBackendApp' port $
    setOnExceptionResponse becknExceptionResponse $
    setPort port defaultSettings

runBecknBackendApp' :: Int -> Settings -> IO ()
runBecknBackendApp' port settings = do
  reqHeadersKey <- V.newKey
  let loggerCfg =
        T.defaultLoggerConfig
          { T._logToFile = True
          , T._logFilePath = "/tmp/newton-backend.log" -- change this to Axis file Name
          , T._isAsync = False
          --, T._logToConsole = False -- enable this for perf in production
          }
  R.withFlowRuntime (Just loggerCfg) $ \flowRt -> do
    putStrLn @String "Initializing DB Connections..."
    let prepare = prepareDBConnections :: L.Flow ()
    try (R.runFlow flowRt prepare) >>= \case
      Left (e :: SomeException) -> putStrLn @String ("Exception thrown: " <> show e)
      Right _ -> do
        putStrLn @String
          ("Runtime created. Starting server at port " <> show port)
        runSettings settings $
          App.run reqHeadersKey $
          App.Env flowRt

becknExceptionResponse :: SomeException -> Response
becknExceptionResponse exception = do
  let anyException = fromException exception
  case anyException of
    Just ex ->
      responseLBS
        (H.Status (errHTTPCode ex) (BS.pack $ errReasonPhrase ex))
        ((H.hContentType, "application/json") : (errHeaders ex))
        (errBody ex)
    Nothing ->
      responseLBS
        H.internalServerError500
        [(H.hContentType, "application/json")]
        (Aeson.encode $ internalServerErr)

prepareDBConnections :: L.Flow ()
prepareDBConnections = do
  ePool <- L.initSqlDBConnection mysqlDBC
  throwOnFailedWithLog
    ePool
    SqlDBConnectionFailedException
    "Failed to initialize connection to Postgres."
  conn <- L.getSqlDBConnection mysqlDBC
  throwOnFailedWithLog
    conn
    SqlDBConnectionFailedException
    "Failed to get connection to Postgres."
  --res <- testDBConnection
  --case res of
    --Right _ -> pure ()
    --Left err ->
      --throwFailedWithLog
        --SqlDBConnectionFailedException
        --"Failed to test the connection to postgres"
