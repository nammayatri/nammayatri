{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

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
