{-# LANGUAGE OverloadedLabels #-}

module App
  ( runMockProvider,
  )
where

import App.Server
import App.Types
import Beckn.Exit
import qualified Beckn.Types.App as App
import Beckn.Utils.App
import Beckn.Utils.Common
import Beckn.Utils.Dhall (readDhallConfigDefault)
import Beckn.Utils.Migration
import qualified Beckn.Utils.Monitoring.Prometheus.Metrics as Metrics
import Beckn.Utils.Servant.Server (exceptionResponse)
import Beckn.Utils.Servant.SignatureAuth
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified EulerHS.Language as L
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

runMockProvider :: (AppCfg -> AppCfg) -> IO ()
runMockProvider configModifier = do
  appCfg <- configModifier <$> readDhallConfigDefault "mock-provider-backend"
  Metrics.serve (appCfg ^. #metricsPort)
  hostname <- (T.pack <$>) <$> lookupEnv "POD_NAME"
  let loggerRt = getEulerLoggerRuntime hostname $ appCfg ^. #loggerConfig
  let settings =
        setOnExceptionResponse mockProviderExceptionResponse $
          setPort (appCfg ^. #port) defaultSettings
      appEnv = mkAppEnv appCfg
  E.withFlowRuntime (Just loggerRt) $ \flowRt -> do
    flowRt' <- runFlowR flowRt appEnv $ do
      withLogContext "Server startup" $ do
        logInfo "Setting up for signature auth..."
        let selfId = appCfg ^. #selfId
        getManager <-
          prepareAuthManager flowRt appEnv "Authorization" selfId
            & handleLeft exitAuthManagerPrepFailure "Could not prepare authentication manager: "
        authManager <- L.runIO getManager
        logInfo ("Runtime created. Starting server at port " <> show (appCfg ^. #port))
        migrateIfNeeded (appCfg ^. #migrationPath) (appCfg ^. #dbCfg) (appCfg ^. #autoMigrate)
          >>= handleLeft exitDBMigrationFailure "Couldn't migrate database: "
        return $ flowRt {R._httpClientManagers = Map.singleton signatureAuthManagerKey authManager}
    runSettings settings $ run $ App.EnvR flowRt' appEnv

mockProviderExceptionResponse :: SomeException -> Response
mockProviderExceptionResponse = exceptionResponse
