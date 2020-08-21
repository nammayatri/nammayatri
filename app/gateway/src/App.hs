module App
  ( runGateway,
  )
where

import App.Server
import App.Types
import Beckn.Constants.APIErrorCode (internalServerErr)
import qualified Beckn.Types.App as App
import Beckn.Utils.Dhall (ZL (Z), readDhallConfigDefault)
import Beckn.Utils.Migration
import qualified Beckn.Utils.Monitoring.Prometheus.Metrics as Metrics
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.Cache as C
import qualified Data.Vault.Lazy as V
import EulerHS.Prelude
import EulerHS.Runtime as E
import EulerHS.Types as E
import qualified Network.HTTP.Types as H
import Network.Wai (Response, responseLBS)
import Network.Wai.Handler.Warp
  ( defaultSettings,
    runSettings,
    setOnExceptionResponse,
    setPort,
  )
import Servant.Server

runGateway :: IO ()
runGateway = do
  appCfg <- readDhallConfigDefault Z "beckn-gateway"
  Metrics.serve (metricsPort appCfg)
  let loggerCfg =
        E.defaultLoggerConfig
          { E._logToFile = True,
            E._logFilePath = "/tmp/beckn-gateway.log",
            E._isAsync = True
          }
  let settings =
        setOnExceptionResponse gatewayExceptionResponse $
          setPort (port appCfg) defaultSettings
  cache <- C.newCache Nothing
  E.withFlowRuntime (Just loggerCfg) $ \flowRt -> do
    reqHeadersKey <- V.newKey
    _ <- runMigrations appCfg
    runSettings settings $ run reqHeadersKey (App.EnvR flowRt $ mkAppEnv appCfg cache)
  where
    runMigrations AppCfg {..} =
      migrateIfNeeded migrationPath dbCfg autoMigrate

gatewayExceptionResponse :: SomeException -> Response
gatewayExceptionResponse exception = do
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
