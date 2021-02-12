{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module BackgroundTaskManager where

import App.Routes
import App.Types
import Beckn.Storage.Redis.Config
import qualified Beckn.Types.App as App
import qualified Beckn.Types.Storage.Organization as Organization
import Beckn.Utils.Common
import Beckn.Utils.Dhall (readDhallConfigDefault)
import Beckn.Utils.Logging
import qualified Beckn.Utils.Servant.Server as Server
import Beckn.Utils.Servant.SignatureAuth (prepareAuthManagers)
import EulerHS.Prelude
import qualified EulerHS.Runtime as R
import Network.Wai.Handler.Warp
import Servant
import qualified Services.Allocation.Runner as Runner
import qualified Storage.Queries.Organization as Storage
import System.Posix.Signals

runBackgroundTaskManager :: (AppEnv -> AppEnv) -> IO ()
runBackgroundTaskManager configModifier = do
  appEnv <- configModifier <$> readDhallConfigDefault "beckn-transport"
  let loggerCfg = getEulerLoggerConfig $ appEnv ^. #loggerConfig
  let redisCfg = appEnv ^. #redisCfg
  let checkConnections = prepareRedisConnections redisCfg

  shutdown <- newEmptyTMVarIO
  activeTask <- newEmptyTMVarIO

  void $ installHandler sigTERM (Catch $ handleShutdown shutdown) Nothing
  void $ installHandler sigINT (Catch $ handleShutdown shutdown) Nothing

  appThreadId <- forkIO $
    R.withFlowRuntime (Just loggerCfg) $ \flowRt -> do
      putStrLn @String "Setting up for signature auth..."
      try (runFlowR flowRt appEnv Storage.loadAllProviders) >>= \case
        Left (e :: SomeException) -> putStrLn @String ("Exception thrown: " <> show e)
        Right allProviders -> do
          let allShortIds = map (App._getShortOrganizationId . Organization._shortId) allProviders
          case prepareAuthManagers flowRt appEnv allShortIds of
            Left err -> putStrLn @String ("Could not prepare authentication managers: " <> show err)
            Right getManagers -> do
              managerMap <- getManagers
              putStrLn @Text $ "Loaded http managers - " <> show (keys managerMap)
              let flowRt' = flowRt {R._httpClientManagers = managerMap}
              putStrLn @String "Initializing Redis Connections..."
              try (runFlowR flowRt appEnv checkConnections) >>= \case
                Left (e :: SomeException) -> putStrLn @Text ("Connections check failed. Exception thrown: " <> show e)
                Right _ -> do
                  let settings = setPort (bgtmPort appEnv) defaultSettings
                  _bgtmThreadId <- forkIO $ runFlowR flowRt' appEnv $ Runner.run shutdown activeTask
                  runSettings settings $ Server.run healthCheckAPI (healthCheckServer shutdown) EmptyContext (App.EnvR flowRt' appEnv)

  atomically $ readTMVar shutdown
  putStrLn @Text "Shutting down..."
  waitForTaskToComplete activeTask
  killThread appThreadId
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
