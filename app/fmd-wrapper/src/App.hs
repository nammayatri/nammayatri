{-# LANGUAGE TypeApplications #-}

module App
  ( runFMDWrapper,
  )
where

import App.Server
import App.Types
import Beckn.Exit
import Beckn.Storage.Redis.Config (prepareRedisConnections)
import qualified Beckn.Types.App as App
import Beckn.Types.Flow
import Beckn.Utils.App
import Beckn.Utils.Dhall (readDhallConfigDefault)
import Beckn.Utils.FlowLogging
import Beckn.Utils.Migration
import qualified Beckn.Utils.Monitoring.Prometheus.Metrics as Metrics
import Beckn.Utils.Servant.SignatureAuth
import qualified Data.Map.Strict as Map
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
import System.Environment
import Utils.Common

runFMDWrapper :: (AppCfg -> AppCfg) -> IO ()
runFMDWrapper configModifier = do
  appCfg <- configModifier <$> readDhallConfigDefault "fmd-wrapper"
  Metrics.serve (appCfg.metricsPort)
  hostname <- (T.pack <$>) <$> lookupEnv "POD_NAME"
  let loggerRt = getEulerLoggerRuntime hostname $ appCfg.loggerConfig
  appEnv <- buildAppEnv appCfg
  let settings =
        defaultSettings
          & setGracefulShutdownTimeout (Just $ getSeconds appCfg.graceTerminationPeriod)
          & setInstallShutdownHandler (handleShutdown $ appEnv.isShuttingDown)
          & setPort (appCfg.port)
  R.withFlowRuntime (Just loggerRt) $ \flowRt -> do
    flowRt' <- runFlowR flowRt appEnv $ do
      withLogTag "Server startup" $ do
        let shortOrgId = appCfg.selfId
        authManager <-
          prepareAuthManager flowRt appEnv "Authorization" shortOrgId
            & handleLeft exitAuthManagerPrepFailure "Could not prepare authentication manager: "
        managers <- createManagers $ Map.singleton signatureAuthManagerKey authManager
        try (prepareRedisConnections $ appCfg.redisCfg)
          >>= handleLeft @SomeException exitRedisConnPrepFailure "Exception thrown: "
        migrateIfNeeded (appCfg.migrationPath) (appCfg.dbCfg) (appCfg.autoMigrate)
          >>= handleLeft exitDBMigrationFailure "Couldn't migrate database: "
        logInfo ("Runtime created. Starting server at port " <> show (appCfg.port))
        return $ flowRt {R._httpClientManagers = managers}
    runSettings settings $ run $ App.EnvR flowRt' appEnv
