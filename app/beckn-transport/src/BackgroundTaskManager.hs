{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module BackgroundTaskManager where

import App.Routes
import App.Types
import Beckn.Storage.DB.Config
import Beckn.Storage.Redis.Config
import qualified Beckn.Types.App as App
import Beckn.Types.Id
import qualified Beckn.Types.Storage.Organization as Organization
import Beckn.Utils.Common
import Beckn.Utils.Dhall (readDhallConfigDefault)
import Beckn.Utils.Logging
import qualified Beckn.Utils.Servant.Server as Server
import Beckn.Utils.Servant.SignatureAuth
import EulerHS.Prelude
import qualified EulerHS.Runtime as R
import Network.Wai.Handler.Warp
import Servant
import qualified Services.Allocation.Runner as Runner
import qualified Storage.Queries.Organization as Storage
import System.Posix.Signals

runBackgroundTaskManager :: (AppEnv -> AppEnv) -> IO ()
runBackgroundTaskManager configModifier = do
  appEnv <- configModifier <$> readDhallConfigDefault "beckn-transport-btm"
  let loggerRt = getEulerLoggerRuntime $ appEnv ^. #loggerConfig
  let redisCfg = appEnv ^. #redisCfg
  let checkConnections = prepareRedisConnections redisCfg >> prepareDBConnections
  let port = appEnv ^. #bgtmPort
  putStrLn @Text $ "Starting Background Task Manager on port " <> show port

  shutdown <- newEmptyTMVarIO
  activeTask <- newEmptyTMVarIO

  void $ installHandler sigTERM (Catch $ handleShutdown shutdown) Nothing
  void $ installHandler sigINT (Catch $ handleShutdown shutdown) Nothing

  R.withFlowRuntime (Just loggerRt) $ \flowRt -> do
    try (runFlowR flowRt appEnv checkConnections) >>= \case
      Left (e :: SomeException) -> putStrLn @Text ("Connections check failed. Exception thrown: " <> show e) >> handleShutdown shutdown
      Right _ -> do
        putStrLn @Text "Setting up for signature auth..."
        try (runFlowR flowRt appEnv Storage.loadAllProviders) >>= \case
          Left (e :: SomeException) -> putStrLn @Text ("Exception thrown: " <> show e) >> handleShutdown shutdown
          Right allProviders -> do
            let allShortIds = map (getShortId . Organization._shortId) allProviders
            prepareAuthManagers flowRt appEnv allShortIds & \case
              Left err -> putStrLn @Text ("Could not prepare authentication managers: " <> show err) >> handleShutdown shutdown
              Right getManagers -> do
                managerMap <- getManagers
                putStrLn @Text $ "Loaded http managers - " <> show (keys managerMap)
                let flowRt' = flowRt {R._httpClientManagers = managerMap}
                let settings = setPort port defaultSettings
                appThreadId <- forkIO $ runFlowR flowRt' appEnv $ Runner.run shutdown activeTask
                apiThreadId <- forkIO $ runSettings settings $ Server.run healthCheckAPI (healthCheckServer shutdown) EmptyContext (App.EnvR flowRt' appEnv)
                putStrLn @Text "Background Task Manager is ready."
                atomically $ readTMVar shutdown
                waitForTaskToComplete activeTask
                putStrLn @Text "Shutting down..."
                killThread appThreadId
                killThread apiThreadId
                exitSuccess
  where
    handleShutdown shutdown = do
      putStrLn @Text "Received shutdown signal. Handling..."
      liftIO . atomically $ putTMVar shutdown ()
    waitForTaskToComplete activeTask = do
      putStrLn @Text "Waiting for active task to complete."
      _ <- atomically $ putTMVar activeTask ()
      putStrLn @Text "Active task is complete. Shutting down..."
      pure ()
