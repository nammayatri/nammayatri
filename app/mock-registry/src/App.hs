{-# LANGUAGE TypeApplications #-}

module App where

import qualified App.Server as App
import App.Types
import qualified Beckn.Types.App as App
import Beckn.Utils.App
import Beckn.Utils.Dhall (readDhallConfigDefault)
import EulerHS.Prelude
import qualified EulerHS.Runtime as R
import Network.Wai.Handler.Warp
  ( defaultSettings,
    runSettings,
    setGracefulShutdownTimeout,
    setInstallShutdownHandler,
    setPort,
  )

runRegistryService :: IO ()
runRegistryService = do
  appCfg <- readDhallConfigDefault "registry"
  appEnv <- buildAppEnv appCfg
  let settings =
        defaultSettings
          & setGracefulShutdownTimeout (Just $ appCfg.graceTerminationPeriod)
          & setInstallShutdownHandler (handleShutdown $ appEnv.isShuttingDown)
          & setPort (appCfg.port)
  R.withFlowRuntime Nothing $ \flowRt -> do
    putStrLn @String $ "Registry service: Runtime created. Starting server at port " <> show (appCfg.port)
    runSettings settings $ App.runServer (App.EnvR flowRt appEnv)
