{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module App where

import qualified App.Server as App
import App.Types
import Beckn.Exit
import Beckn.Storage.DB.Config (prepareDBConnections)
import qualified Beckn.Types.App as App
import Beckn.Utils.Common
import Beckn.Utils.Dhall (readDhallConfigDefault)
import Beckn.Utils.Migration
import qualified Beckn.Utils.Monitoring.Prometheus.Metrics as Metrics
import Beckn.Utils.Servant.Server
import Beckn.Utils.Servant.SignatureAuth
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified EulerHS.Runtime as R
import Network.Wai
import Network.Wai.Handler.Warp
  ( Settings,
    defaultSettings,
    runSettings,
    setOnExceptionResponse,
    setPort,
  )
import System.Environment

runAppBackend :: (AppEnv -> AppEnv) -> IO ()
runAppBackend configModifier = do
  appEnv <- configModifier <$> readDhallConfigDefault "app-backend"
  Metrics.serve (metricsPort appEnv)
  runAppBackend' appEnv $
    setOnExceptionResponse appExceptionResponse $
      setPort (port appEnv) defaultSettings

runAppBackend' :: AppEnv -> Settings -> IO ()
runAppBackend' appEnv settings = do
  hostname <- (T.pack <$>) <$> lookupEnv "POD_NAME"
  let loggerRt = getEulerLoggerRuntime hostname $ appEnv ^. #loggerConfig
  R.withFlowRuntime (Just loggerRt) $ \flowRt -> do
    flowRt' <- runFlowR flowRt appEnv $ do
      withLogContext "Server startup" $ do
        logInfo "Setting up for signature auth..."
        let shortOrgId = appEnv ^. #bapSelfId
        case prepareAuthManager flowRt appEnv "Authorization" shortOrgId of
          Left err -> do
            logError ("Could not prepare authentication manager: " <> show err)
            L.runIO $ exitWith exitAuthManagerPrepFailure
          Right getManager -> do
            authManager <- L.runIO getManager
            logInfo "Initializing DB Connections..."
            prepareDBConnections >>= \case
              Left e -> do
                logError ("Exception thrown: " <> show e)
                L.runIO $ exitWith exitDBConnPrepFailure
              Right _ -> do
                migrateIfNeeded (migrationPath appEnv) (dbCfg appEnv) (autoMigrate appEnv) >>= \case
                  Left e -> do
                    logError ("Couldn't migrate database: " <> show e)
                    L.runIO $ exitWith exitDBMigrationFailure
                  Right _ -> do
                    logInfo ("Runtime created. Starting server at port " <> show (port appEnv))
                    return $ flowRt {R._httpClientManagers = Map.singleton signatureAuthManagerKey authManager}
    runSettings settings $ App.run (App.EnvR flowRt' appEnv)

appExceptionResponse :: SomeException -> Response
appExceptionResponse = exceptionResponse
