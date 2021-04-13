{-# LANGUAGE OverloadedLabels #-}

module App
  ( runMockApp,
  )
where

import App.Server
import App.Types
import Beckn.Exit
import qualified Beckn.Types.App as App
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

runMockApp :: (AppEnv -> AppEnv) -> IO ()
runMockApp configModifier = do
  appEnv <- configModifier <$> readDhallConfigDefault "mock-app-backend"
  Metrics.serve (metricsPort appEnv)
  hostname <- (T.pack <$>) <$> lookupEnv "POD_NAME"
  let loggerRt = getEulerLoggerRuntime hostname $ appEnv ^. #loggerConfig
  let settings =
        setOnExceptionResponse mockAppExceptionResponse $
          setPort (port appEnv) defaultSettings
  E.withFlowRuntime (Just loggerRt) $ \flowRt -> do
    flowRt' <- runFlowR flowRt appEnv $ do
      withLogContext "Server startup" $ do
        logInfo "Setting up for signature auth..."
        let selfId = appEnv ^. #selfId
        case prepareAuthManager flowRt appEnv "Authorization" selfId of
          Left err -> do
            logError ("Could not prepare authentication manager: " <> show err)
            L.runIO $ exitWith exitAuthManagerPrepFailure
          Right getManager -> do
            authManager <- L.runIO getManager
            logInfo ("Runtime created. Starting server at port " <> show (port appEnv))
            migrateIfNeeded (migrationPath appEnv) (dbCfg appEnv) (autoMigrate appEnv) >>= \case
              Left e -> do
                logError ("Couldn't migrate database: " <> show e)
                L.runIO $ exitWith exitDBMigrationFailure
              Right _ -> do
                return $ flowRt {R._httpClientManagers = Map.singleton signatureAuthManagerKey authManager}
    runSettings settings $ run $ App.EnvR flowRt' appEnv

mockAppExceptionResponse :: SomeException -> Response
mockAppExceptionResponse = exceptionResponse
