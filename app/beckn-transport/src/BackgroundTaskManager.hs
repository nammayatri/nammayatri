{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module BackgroundTaskManager where

import App.Types
import Beckn.Storage.DB.Config
import Beckn.Storage.Redis.Config
-- import qualified Beckn.Types.App as App
import Beckn.Utils.Common
import Beckn.Utils.Dhall (readDhallConfigDefault)
import Beckn.Utils.Logging
-- import qualified Beckn.Utils.Servant.Server as Server
import EulerHS.Prelude
import qualified EulerHS.Runtime as R
-- import Network.Wai.Handler.Warp
-- import Servant
import qualified Services.Runner as Runner
import System.Posix.Signals

runBackgroundTaskManager :: (AppEnv -> AppEnv) -> IO ()
runBackgroundTaskManager configModifier = do
  appEnv <- configModifier <$> readDhallConfigDefault "beckn-transport"
  -- run server with health-check api
  -- use different port settings
  let loggerCfg = getEulerLoggerConfig $ appEnv ^. #loggerConfig
  let redisCfg = appEnv ^. #redisCfg
  let checkConnections = prepareRedisConnections redisCfg >> prepareDBConnections

  shutdown <- newEmptyTMVarIO
  activeTask <- newEmptyTMVarIO

  void $ installHandler sigTERM (Catch $ handleShutdown shutdown) Nothing
  void $ installHandler sigINT (Catch $ handleShutdown shutdown) Nothing

  appThreadId <- forkIO $
    R.withFlowRuntime (Just loggerCfg) $ \flowRt -> do
      try (runFlowR flowRt appEnv checkConnections) >>= \case
        Left (e :: SomeException) -> putStrLn @Text ("Connections check failed. Exception thrown: " <> show e)
        Right _ -> do
          -- setPort (port appEnv) defaultSettings
          -- runSettings defaultSettings $ Server.run undefined undefined EmptyContext (App.EnvR flowRt appEnv) -- Need ServiceHealthCheckAPI
          runFlowR flowRt appEnv $ Runner.run shutdown activeTask

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
