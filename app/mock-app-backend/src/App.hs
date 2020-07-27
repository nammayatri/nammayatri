module App
  ( runMockApp,
  )
where

import App.Server
import App.Types
import Beckn.Constants.APIErrorCode (internalServerErr)
import qualified Beckn.Types.App as App
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BS
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
import System.Environment (lookupEnv)

runMockApp :: IO ()
runMockApp = do
  port <- fromMaybe 8016 . (>>= readMaybe) <$> lookupEnv "MOCK_APP_PORT"
  let appEnv = AppEnv
  let loggerCfg =
        E.defaultLoggerConfig
          { E._logToFile = True,
            E._logFilePath = "/tmp/mock-app-backend.log",
            E._isAsync = True
          }
  let settings =
        setOnExceptionResponse mockAppExceptionResponse $
          setPort port defaultSettings
  E.withFlowRuntime (Just loggerCfg) $ \flowRt -> do
    reqHeadersKey <- V.newKey
    runSettings settings $ run reqHeadersKey (App.EnvR flowRt appEnv)

mockAppExceptionResponse :: SomeException -> Response
mockAppExceptionResponse exception = do
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
