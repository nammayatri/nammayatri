module Resources where

import Beckn.Utils.Dhall (readDhallConfig)
import qualified "app-backend" Environment as BecknApp
import qualified "beckn-transport" Environment as BecknTransport
import qualified "driver-offer-bpp" Environment as ARDU
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

{-# NOINLINE driverOfferBppEnv #-}
driverOfferBppEnv :: ARDU.AppEnv
driverOfferBppEnv = unsafePerformIO $ do
  appCfg <- readDhallConfig "../dhall-configs/dev/driver-offer-bpp.dhall"
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
