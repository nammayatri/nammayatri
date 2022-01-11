module App where

import qualified App.Server as App
import App.Types
import Beckn.Exit
import Beckn.Storage.Common (prepareDBConnections)
import Beckn.Storage.Redis.Config (prepareRedisConnections)
import qualified Beckn.Types.App as App
import Beckn.Types.Flow
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
import System.Environment (lookupEnv)
import Utils.Common

runAppBackend :: (AppCfg -> AppCfg) -> IO ()
runAppBackend configModifier = do
  appCfg <- configModifier <$> readDhallConfigDefault "app-backend"
  Metrics.serve (appCfg.metricsPort)
  runAppBackend' appCfg

runAppBackend' :: AppCfg -> IO ()
runAppBackend' appCfg = do
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
        let shortOrgId = appCfg.bapSelfIds.cabs
        let shortOrgIdMetro = appCfg.bapSelfIds.metro
        managers <-
          prepareAuthManagers flowRt appEnv [shortOrgId, shortOrgIdMetro]
            & createManagers
        logInfo "Initializing DB Connections..."
        _ <- prepareDBConnections >>= handleLeft exitDBConnPrepFailure "Exception thrown: "
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
