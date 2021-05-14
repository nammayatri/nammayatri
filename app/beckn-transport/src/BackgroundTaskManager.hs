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
import qualified Data.Text as T
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (exitSuccess)
import qualified EulerHS.Runtime as R
import Network.Wai.Handler.Warp
import Servant
import qualified Services.Allocation.Runner as Runner
import qualified Storage.Queries.Organization as Storage
import System.Environment
import System.Posix.Signals
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
  putStrLn @Text $ "Starting Background Task Manager on port " <> show port

  shutdown <- newEmptyTMVarIO
  activeTask <- newEmptyTMVarIO

  void $ installHandler sigTERM (Catch $ handleShutdown shutdown exitSigTERMFailure) Nothing
  void $ installHandler sigINT (Catch $ handleShutdown shutdown exitSigINTFailure) Nothing

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
        return $ flowRt {R._httpClientManagers = managerMap}
    let settings = setPort port defaultSettings
    appThreadId <- forkIO $ runFlowR flowRt' appEnv $ Runner.run shutdown activeTask
    apiThreadId <- forkIO $ runSettings settings $ Server.run healthCheckAPI (healthCheckServer shutdown) EmptyContext (App.EnvR flowRt' appEnv)
    putStrLn @Text "Background Task Manager is ready."
    exitCode <- atomically $ readTMVar shutdown
    waitForTaskToComplete activeTask
    putStrLn @Text "Shutting down..."
    killThread appThreadId
    killThread apiThreadId
    exitWith exitCode
  where
    handleShutdown shutdown exitcode = do
      putStrLn @Text "Received shutdown signal. Handling..."
      liftIO . atomically $ putTMVar shutdown exitcode
    waitForTaskToComplete activeTask = do
      putStrLn @Text "Waiting for active task to complete."
      _ <- atomically $ putTMVar activeTask ()
      putStrLn @Text "Active task is complete. Shutting down..."
      pure ()
