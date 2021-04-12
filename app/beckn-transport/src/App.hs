{-# LANGUAGE OverloadedLabels #-}

module App where

import qualified App.Server as App
import App.Types
import Beckn.Storage.Redis.Config
import qualified Beckn.Types.App as App
import Beckn.Types.Id
import qualified Beckn.Types.Storage.Organization as Organization
import Beckn.Utils.Common
import Beckn.Utils.Dhall (readDhallConfigDefault)
import Beckn.Utils.Migration
import qualified Beckn.Utils.Monitoring.Prometheus.Metrics as Metrics
import Beckn.Utils.Servant.Server
import Beckn.Utils.Servant.SignatureAuth
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
import qualified Storage.Queries.Organization as Storage
import System.Environment
import System.Exit (ExitCode (..))

runTransporterBackendApp :: (AppEnv -> AppEnv) -> IO ()
runTransporterBackendApp configModifier = do
  appEnv <- configModifier <$> readDhallConfigDefault "beckn-transport"
  Metrics.serve (metricsPort appEnv)
  runTransporterBackendApp' appEnv $
    setOnExceptionResponse transporterExceptionResponse $
      setPort (port appEnv) defaultSettings

runTransporterBackendApp' :: AppEnv -> Settings -> IO ()
runTransporterBackendApp' appEnv settings = do
  hostname <- (T.pack <$>) <$> lookupEnv "POD_NAME"
  let loggerRt = getEulerLoggerRuntime hostname $ appEnv ^. #loggerConfig
  R.withFlowRuntime (Just loggerRt) $ \flowRt -> do
    flowRt' <- runFlowR flowRt appEnv $ do
      withLogContext "Server startup" $ do
        logInfo "Setting up for signature auth..."
        try Storage.loadAllProviders >>= \case
          Left (e :: SomeException) -> do
            logError ("Exception thrown: " <> show e)
            L.runIO . exitWith $ ExitFailure 1
          Right allProviders -> do
            let allShortIds = map (getShortId . Organization._shortId) allProviders
            case prepareAuthManagers flowRt appEnv allShortIds of
              Left err -> do
                logError ("Could not prepare authentication managers: " <> show err)
                L.runIO . exitWith $ ExitFailure 2
              Right getManagers -> do
                managerMap <- L.runIO getManagers
                logInfo $ "Loaded http managers - " <> show (keys managerMap)
                logInfo "Initializing Redis Connections..."
                try (prepareRedisConnections $ redisCfg appEnv) >>= \case
                  Left (e :: SomeException) -> do
                    logError ("Exception thrown: " <> show e)
                    L.runIO . exitWith $ ExitFailure 3
                  Right _ -> do
                    migrateIfNeeded (migrationPath appEnv) (dbCfg appEnv) (autoMigrate appEnv) >>= \case
                      Left e -> do
                        logError ("Couldn't migrate database: " <> show e)
                        L.runIO $ exitWith (ExitFailure 4)
                      Right _ -> do
                        logInfo ("Runtime created. Starting server at port " <> show (port appEnv))
                        return $ flowRt {R._httpClientManagers = managerMap}
    runSettings settings $ App.run (App.EnvR flowRt' appEnv)

transporterExceptionResponse :: SomeException -> Response
transporterExceptionResponse = exceptionResponse
