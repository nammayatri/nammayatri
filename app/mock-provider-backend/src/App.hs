{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module App
  ( runMockProvider,
  )
where

import App.Server
import App.Types
import qualified Beckn.Types.App as App
import Beckn.Utils.Common
import Beckn.Utils.Dhall (readDhallConfigDefault)
import Beckn.Utils.Migration
import qualified Beckn.Utils.Monitoring.Prometheus.Metrics as Metrics
import Beckn.Utils.Servant.Server (exceptionResponse)
import Beckn.Utils.Servant.SignatureAuth
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import EulerHS.Prelude
import EulerHS.Runtime as E
import qualified EulerHS.Runtime as R
import Network.Wai (Response)
import Network.Wai.Handler.Warp
  ( defaultSettings,
    runSettings,
    setOnExceptionResponse,
    setPort,
  )
import System.Environment

runMockProvider :: (AppEnv -> AppEnv) -> IO ()
runMockProvider configModifier = do
  appEnv <- configModifier <$> readDhallConfigDefault "mock-provider-backend"
  Metrics.serve (metricsPort appEnv)
  hostname <- (T.pack <$>) <$> lookupEnv "POD_NAME"
  let loggerRt = getEulerLoggerRuntime hostname $ appEnv ^. #loggerConfig
  let settings =
        setOnExceptionResponse mockProviderExceptionResponse $
          setPort (port appEnv) defaultSettings
  E.withFlowRuntime (Just loggerRt) $ \flowRt -> do
    _ <- migrateIfNeeded (migrationPath appEnv) (dbCfg appEnv) (autoMigrate appEnv)
    let selfId = appEnv ^. #selfId
    case prepareAuthManager flowRt appEnv "Authorization" selfId of
      Left err -> putStrLn @String ("Could not prepare authentication manager: " <> show err)
      Right getManager -> do
        authManager <- getManager
        let flowRt' = flowRt {R._httpClientManagers = Map.singleton signatureAuthManagerKey authManager}
        runSettings settings $ run $ App.EnvR flowRt' appEnv

mockProviderExceptionResponse :: SomeException -> Response
mockProviderExceptionResponse = exceptionResponse
