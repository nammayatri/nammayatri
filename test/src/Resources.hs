module Resources where

import qualified App.BackgroundTaskManager.Types as BecknTransport
import qualified "app-backend" App.Types as BecknApp
import qualified "beckn-transport" App.Types as BecknTransport
import Beckn.Utils.Dhall (readDhallConfig)
import EulerHS.Prelude
import GHC.IO (unsafePerformIO)

{-# NOINLINE transporterAppEnv #-}
transporterAppEnv :: BecknTransport.AppEnv
transporterAppEnv = unsafePerformIO $ do
  appCfg <- readDhallConfig "../dhall-configs/dev/beckn-transport.dhall"
  let updLogCfg =
        appCfg.loggerConfig{logToFile = False,
                            logToConsole = False
                           }
      updAppCfg = appCfg{loggerConfig = updLogCfg}
  BecknTransport.buildAppEnv updAppCfg

{-# NOINLINE appBackendEnv #-}
appBackendEnv :: BecknApp.AppEnv
appBackendEnv = unsafePerformIO $ do
  appCfg <- readDhallConfig "../dhall-configs/dev/app-backend.dhall"
  let updLogCfg =
        appCfg.loggerConfig{logToFile = False,
                            logToConsole = False
                           }
      updAppCfg = appCfg{loggerConfig = updLogCfg}
  BecknApp.buildAppEnv updAppCfg

prepareTestResources :: IO ()
prepareTestResources =
  return $
    transporterAppEnv
      `seq` appBackendEnv
      `seq` ()

releaseTestResources :: IO ()
releaseTestResources = do
  BecknApp.releaseAppEnv appBackendEnv
  BecknTransport.releaseAppEnv transporterAppEnv
