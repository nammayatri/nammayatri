module App where

import qualified App.Server as App
import App.Types
import Beckn.Exit
import Beckn.Storage.Redis.Config
import qualified Beckn.Types.App as App
import Beckn.Types.Flow
import Beckn.Types.Id
import Beckn.Utils.App
import Beckn.Utils.Dhall (readDhallConfigDefault)
import qualified Beckn.Utils.FlowLogging as L
import Beckn.Utils.Migration
import qualified Beckn.Utils.Monitoring.Prometheus.Metrics as Metrics
import Beckn.Utils.Servant.SignatureAuth
import qualified Data.Text as T
import EulerHS.Prelude
import qualified EulerHS.Runtime as R
import Network.Wai.Handler.Warp
  ( defaultSettings,
    runSettings,
    setGracefulShutdownTimeout,
    setInstallShutdownHandler,
    setPort,
  )
import qualified Storage.Queries.Organization as Storage
import System.Environment (lookupEnv)
import qualified Types.Storage.Organization as Organization
import Utils.Common

runTransporterBackendApp :: (AppCfg -> AppCfg) -> IO ()
runTransporterBackendApp configModifier = do
  appCfg <- configModifier <$> readDhallConfigDefault "beckn-transport"
  Metrics.serve (appCfg.metricsPort)
  runTransporterBackendApp' appCfg

runTransporterBackendApp' :: AppCfg -> IO ()
runTransporterBackendApp' appCfg = do
  hostname <- (T.pack <$>) <$> lookupEnv "POD_NAME"
  let loggerRt = L.getEulerLoggerRuntime hostname $ appCfg.loggerConfig
  appEnv <- buildAppEnv appCfg
  let settings =
        defaultSettings
          & setGracefulShutdownTimeout (Just $ getSeconds appCfg.graceTerminationPeriod)
          & setInstallShutdownHandler (handleShutdown appEnv.isShuttingDown . shutdownAction appEnv)
          & setPort (appCfg.port)
  R.withFlowRuntime (Just loggerRt) $ \flowRt -> do
    flowRt' <- runFlowR flowRt appEnv $ do
      withLogTag "Server startup" $ do
        logInfo "Setting up for signature auth..."
        allProviders <-
          try Storage.loadAllProviders
            >>= handleLeft @SomeException exitLoadAllProvidersFailure "Exception thrown: "
        let allShortIds = map (getShortId . Organization.shortId) allProviders
        managersSettings <-
          prepareAuthManagers flowRt appEnv allShortIds
            & handleLeft exitAuthManagerPrepFailure "Could not prepare authentication managers: "
        managers <- createManagers managersSettings
        logInfo $ "Loaded http managers - " <> show (keys managers)
        logInfo "Initializing Redis Connections..."
        try (prepareRedisConnections $ appCfg.redisCfg)
          >>= handleLeft @SomeException exitRedisConnPrepFailure "Exception thrown: "
        migrateIfNeeded (appCfg.migrationPath) (appCfg.dbCfg) (appCfg.autoMigrate)
          >>= handleLeft exitDBMigrationFailure "Couldn't migrate database: "
        logInfo ("Runtime created. Starting server at port " <> show (appCfg.port))
        return $ flowRt {R._httpClientManagers = managers}
    runSettings settings $ App.run (App.EnvR flowRt' appEnv)
  where
    shutdownAction appEnv closeSocket = do
      releaseAppEnv appEnv
      closeSocket
