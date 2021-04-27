{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module App where

import qualified App.Server as App
import App.Types
import Beckn.Exit
import Beckn.Storage.Redis.Config
import qualified Beckn.Types.App as App
import Beckn.Types.Id
import qualified Beckn.Types.Storage.Organization as Organization
import Beckn.Utils.App
import Beckn.Utils.Common
import Beckn.Utils.Dhall (readDhallConfigDefault)
import Beckn.Utils.Migration
import qualified Beckn.Utils.Monitoring.Prometheus.Metrics as Metrics
import Beckn.Utils.Servant.SignatureAuth
import qualified Data.Text as T
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified EulerHS.Runtime as R
import Network.Wai.Handler.Warp
  ( Settings,
    defaultSettings,
    runSettings,
    setPort,
  )
import qualified Storage.Queries.Organization as Storage
import System.Environment

runTransporterBackendApp :: (AppCfg -> AppCfg) -> IO ()
runTransporterBackendApp configModifier = do
  appCfg <- configModifier <$> readDhallConfigDefault "beckn-transport"
  Metrics.serve (appCfg ^. #metricsPort)
  runTransporterBackendApp' appCfg $
    setPort (appCfg ^. #port) defaultSettings

runTransporterBackendApp' :: AppCfg -> Settings -> IO ()
runTransporterBackendApp' appCfg settings = do
  hostname <- (T.pack <$>) <$> lookupEnv "POD_NAME"
  let loggerRt = getEulerLoggerRuntime hostname $ appCfg ^. #loggerConfig
      appEnv = mkAppEnv appCfg
  R.withFlowRuntime (Just loggerRt) $ \flowRt -> do
    flowRt' <- runFlowR flowRt appEnv $ do
      withLogTag "Server startup" $ do
        logInfo "Setting up for signature auth..."
        allProviders <-
          try Storage.loadAllProviders
            >>= handleLeft exitLoadAllProvidersFailure "Exception thrown: " . first (id @SomeException)
        let allShortIds = map (getShortId . Organization._shortId) allProviders
        getManagers <-
          prepareAuthManagers flowRt appEnv allShortIds
            & handleLeft exitAuthManagerPrepFailure "Could not prepare authentication managers: "
        managerMap <- L.runIO getManagers
        logInfo $ "Loaded http managers - " <> show (keys managerMap)
        logInfo "Initializing Redis Connections..."
        try (prepareRedisConnections $ appCfg ^. #redisCfg)
          >>= handleLeft exitRedisConnPrepFailure "Exception thrown: " . first (id @SomeException)
        migrateIfNeeded (appCfg ^. #migrationPath) (appCfg ^. #dbCfg) (appCfg ^. #autoMigrate)
          >>= handleLeft exitDBMigrationFailure "Couldn't migrate database: "
        logInfo ("Runtime created. Starting server at port " <> show (appCfg ^. #port))
        return $ flowRt {R._httpClientManagers = managerMap}
    runSettings settings $ App.run (App.EnvR flowRt' appEnv)
