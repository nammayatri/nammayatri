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
import Beckn.Utils.App hiding (handleShutdown)
import Beckn.Utils.Dhall (readDhallConfigDefault)
import qualified Beckn.Utils.Servant.Server as Server
import Beckn.Utils.Servant.SignatureAuth
import Control.Concurrent
import Control.Concurrent.STM.TMVar
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
  activeTask <- newEmptyTMVarIO
  appEnv <- buildAppEnv appCfg
  let shutdown = appEnv ^. #isShutdown

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
    let settings = setPort port defaultSettings
    apiThreadId <- forkIO $ runSettings settings $ Server.run healthCheckAPI healthCheckServer EmptyContext (App.EnvR flowRt' appEnv)
    btmThreadId <- myThreadId
    void $ installHandler sigTERM (Catch $ handleShutdown shutdown activeTask exitSigTERM apiThreadId btmThreadId) Nothing
    void $ installHandler sigINT (Catch $ handleShutdown shutdown activeTask exitSigINT apiThreadId btmThreadId) Nothing
    runFlowR flowRt' appEnv $ Runner.run activeTask
  where
    handleShutdown shutdown activeTask exitcode apiThreadId btmThreadId = do
      isLocked <- atomically $ do
        isEmptyTMVar shutdown >>= \case
          True -> do
            putTMVar shutdown ()
            return True
          False -> return False
      when isLocked $ do
        putStrLn @Text "Received shutdown signal. Handling..."
        waitForTaskToComplete activeTask
        throwTo apiThreadId exitcode
        throwTo btmThreadId exitcode
    waitForTaskToComplete activeTask = do
      putStrLn @Text "Waiting for active task to complete."
      _ <- atomically $ putTMVar activeTask ()
      putStrLn @Text "Active task is complete. Shutting down..."
      pure ()
