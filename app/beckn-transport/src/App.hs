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
        try (prepareRedisConnections $ redisCfg appEnv)
          >>= handleLeft exitRedisConnPrepFailure "Exception thrown: " . first (id @SomeException)
        migrateIfNeeded (migrationPath appEnv) (dbCfg appEnv) (autoMigrate appEnv)
          >>= handleLeft exitDBMigrationFailure "Couldn't migrate database: "
        logInfo ("Runtime created. Starting server at port " <> show (port appEnv))
        return $ flowRt {R._httpClientManagers = managerMap}
    runSettings settings $ App.run (App.EnvR flowRt' appEnv)

transporterExceptionResponse :: SomeException -> Response
transporterExceptionResponse = exceptionResponse
