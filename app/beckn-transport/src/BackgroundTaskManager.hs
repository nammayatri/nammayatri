{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module BackgroundTaskManager where

import App.Routes
import App.Types
import Beckn.Exit
import Beckn.Storage.Common
import Beckn.Storage.Redis.Config
import qualified Beckn.Types.App as App
import Beckn.Types.Id
import qualified Beckn.Types.Storage.Organization as Organization
import Beckn.Utils.App
import Beckn.Utils.Dhall (readDhallConfigDefault)
import qualified Beckn.Utils.Servant.Server as Server
import Beckn.Utils.Servant.SignatureAuth
import Control.Concurrent
import qualified Data.Text as T
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (exitSuccess)
import qualified EulerHS.Runtime as R
import Network.Wai.Handler.Warp
import Servant
import qualified Services.Allocation.Runner as Runner
import qualified Storage.Queries.Organization as Storage
import System.Environment
import Utils.Common

runBackgroundTaskManager :: (AppCfg -> AppCfg) -> IO ()
runBackgroundTaskManager configModifier = do
  appCfg <- configModifier <$> readDhallConfigDefault "beckn-transport-btm"
  hostname <- (T.pack <$>) <$> lookupEnv "POD_NAME"
  let loggerRt = getEulerLoggerRuntime hostname $ appCfg ^. #loggerConfig
  let redisCfg = appCfg ^. #redisCfg
  let checkConnections = prepareRedisConnections redisCfg >> prepareDBConnections
  let port = appCfg ^. #bgtmPort
  appEnv <- buildAppEnv appCfg

  R.withFlowRuntime (Just loggerRt) $ \flowRt -> do
    flowRt' <- runFlowR flowRt appEnv $ do
      withLogTag "BTM startup" $ do
        _ <-
          try checkConnections
            >>= handleLeft @SomeException exitConnCheckFailure "Connections check failed. Exception thrown: "
        logInfo "Setting up for signature auth..."
        allProviders <-
          try Storage.loadAllProviders
            >>= handleLeft @SomeException exitLoadAllProvidersFailure "Exception thrown: "
        let allShortIds = map (getShortId . Organization._shortId) allProviders
        getManagers <-
          prepareAuthManagers flowRt appEnv allShortIds
            & handleLeft exitAuthManagerPrepFailure "Could not prepare authentication managers: "
        managerMap <- L.runIO getManagers
        logInfo ("Loaded http managers - " <> show (keys managerMap))
        logInfo $ "Starting Background Task Manager on port " <> show port
        return $ flowRt {R._httpClientManagers = managerMap}
    let settings =
          setGracefulShutdownTimeout (Just 120)
            . setInstallShutdownHandler (handleShutdown $ appEnv ^. #isShuttingDown)
            $ setPort port defaultSettings
    void . forkIO . runSettings settings $ Server.run healthCheckAPI healthCheckServer EmptyContext (App.EnvR flowRt' appEnv)
    runFlowR flowRt' appEnv Runner.run