module App where

import qualified App.Server as App
import qualified Data.Text as T
import Environment
import EulerHS.Prelude
import qualified EulerHS.Runtime as R
import Kernel.Exit
import Kernel.Storage.Esqueleto.Migration (migrateIfNeeded)
import qualified Kernel.Tools.Metrics.Init as Metrics
import qualified Kernel.Types.App as App
import Kernel.Types.Flow
import Kernel.Utils.App
import Kernel.Utils.Common
import Kernel.Utils.Dhall (readDhallConfigDefault)
import qualified Kernel.Utils.FlowLogging as L
import Kernel.Utils.Servant.SignatureAuth
import Network.Wai.Handler.Warp
  ( defaultSettings,
    runSettings,
    setGracefulShutdownTimeout,
    setInstallShutdownHandler,
    setPort,
  )
import System.Environment (lookupEnv)

runAppBackend :: (AppCfg -> AppCfg) -> IO ()
runAppBackend configModifier = do
  appCfg <- configModifier <$> readDhallConfigDefault "app-backend"
  Metrics.serve (appCfg.metricsPort)
  runAppBackend' appCfg

runAppBackend' :: AppCfg -> IO ()
runAppBackend' appCfg = do
  hostname <- (T.pack <$>) <$> lookupEnv "POD_NAME"
  let loggerRt = L.getEulerLoggerRuntime hostname $ appCfg.loggerConfig
  appEnv <-
    try (buildAppEnv appCfg)
      >>= handleLeftIO @SomeException exitBuildingAppEnvFailure "Couldn't build AppEnv: "
  let settings =
        defaultSettings
          & setGracefulShutdownTimeout (Just $ getSeconds appCfg.graceTerminationPeriod)
          & setInstallShutdownHandler (handleShutdown appEnv.isShuttingDown (releaseAppEnv appEnv))
          & setPort (appCfg.port)
  R.withFlowRuntime (Just loggerRt) $ \flowRt -> do
    flowRt' <- runFlowR flowRt appEnv $ do
      withLogTag "Server startup" $ do
        migrateIfNeeded appCfg.migrationPath appCfg.autoMigrate appCfg.esqDBCfg
          >>= handleLeft exitDBMigrationFailure "Couldn't migrate database: "
        logInfo "Setting up for signature auth..."
        flowRt' <-
          modFlowRtWithAuthManagers
            flowRt
            appEnv
            [ (appCfg.bapSelfIds.cabs, appCfg.bapSelfUniqueKeyIds.cabs),
              (appCfg.bapSelfIds.metro, appCfg.bapSelfUniqueKeyIds.metro)
            ]
        logInfo ("Runtime created. Starting server at port " <> show (appCfg.port))
        pure flowRt'
    runSettings settings $ App.run (App.EnvR flowRt' appEnv)
