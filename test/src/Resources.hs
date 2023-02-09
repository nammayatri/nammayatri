module Resources where

import qualified "dynamic-offer-driver-app" Environment as ARDU
import qualified "rider-app" Environment as BecknApp
import qualified "static-offer-driver-app" Environment as BecknTransport
import EulerHS.Prelude
import GHC.IO (unsafePerformIO)
import Kernel.Utils.Dhall (readDhallConfig)

{-# NOINLINE transporterAppEnv #-}
transporterAppEnv :: BecknTransport.AppEnv
transporterAppEnv = unsafePerformIO $ do
  appCfg <- readDhallConfig "../dhall-configs/dev/static-offer-driver-app.dhall"
  let updLogCfg =
        appCfg.loggerConfig{logToFile = False,
                            logToConsole = False
                           }
      updAppCfg = appCfg{loggerConfig = updLogCfg}
  BecknTransport.buildAppEnv updAppCfg

{-# NOINLINE appBackendEnv #-}
appBackendEnv :: BecknApp.AppEnv
appBackendEnv = unsafePerformIO $ do
  appCfg <- readDhallConfig "../dhall-configs/dev/rider-app.dhall"
  let updLogCfg =
        appCfg.loggerConfig{logToFile = False,
                            logToConsole = False
                           }
      updAppCfg = appCfg{loggerConfig = updLogCfg}
  BecknApp.buildAppEnv updAppCfg

{-# NOINLINE driverOfferBppEnv #-}
driverOfferBppEnv :: ARDU.AppEnv
driverOfferBppEnv = unsafePerformIO $ do
  appCfg <- readDhallConfig "../dhall-configs/dev/dynamic-offer-driver-app.dhall"
  let updLogCfg =
        appCfg.loggerConfig{logToFile = False,
                            logToConsole = False
                           }
      updAppCfg = appCfg{loggerConfig = updLogCfg}
  ARDU.buildAppEnv updAppCfg

prepareTestResources :: IO ()
prepareTestResources =
  return $
    transporterAppEnv
      `seq` driverOfferBppEnv
      `seq` appBackendEnv
      `seq` ()

releaseTestResources :: IO ()
releaseTestResources = do
  BecknApp.releaseAppEnv appBackendEnv
  BecknTransport.releaseAppEnv transporterAppEnv
  ARDU.releaseAppEnv driverOfferBppEnv
