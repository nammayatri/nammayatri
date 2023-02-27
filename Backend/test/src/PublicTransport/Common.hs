{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module PublicTransport.Common where

import qualified "mock-public-transport-provider-platform" Environment as Bpp
import qualified "public-transport-rider-platform" Environment as Bap
import GHC.IO (unsafePerformIO)
import Kernel.Prelude
import Kernel.Types.Time
import Kernel.Utils.Dhall (readDhallConfig)
import Servant.Client
import Utils

kafkaConsumerTimeoutMilliseconds :: Int
kafkaConsumerTimeoutMilliseconds = 500

publicTransportBapUrl :: BaseUrl
publicTransportBapUrl =
  BaseUrl
    { baseUrlScheme = Http,
      baseUrlHost = "localhost",
      baseUrlPort = 8023,
      baseUrlPath = ""
    }

publicTransportBapClientEnv :: ClientEnv
publicTransportBapClientEnv = mkClientEnv defaultManager publicTransportBapUrl

callPublicTransportBap :: (Show a) => ClientM a -> IO a
callPublicTransportBap = runClient' publicTransportBapClientEnv

mockBppUrl :: BaseUrl
mockBppUrl =
  BaseUrl
    { baseUrlScheme = Http,
      baseUrlHost = "localhost",
      baseUrlPort = 8091,
      baseUrlPath = ""
    }

mockPublicTransportBppClientEnv :: ClientEnv
mockPublicTransportBppClientEnv = mkClientEnv defaultManager mockBppUrl

callMockPublicTransportBpp :: (Show a) => ClientM a -> IO a
callMockPublicTransportBpp = runClient' mockPublicTransportBppClientEnv

mockWaitTimeSeconds :: Seconds
mockWaitTimeSeconds = 1

{-# NOINLINE publicTransportBapEnv #-}
publicTransportBapEnv :: Bap.AppEnv
publicTransportBapEnv = unsafePerformIO $ do
  appCfg <- readDhallConfig "../dhall-configs/dev/public-transport-rider-platform.dhall"
  let updLogCfg =
        appCfg.loggerConfig{logToFile = False,
                            logToConsole = False
                           }
      updAppCfg = appCfg{loggerConfig = updLogCfg}
  Bap.buildAppEnv updAppCfg

{-# NOINLINE mockBppEnv #-}
mockBppEnv :: Bpp.AppEnv
mockBppEnv = unsafePerformIO $ do
  appCfg <- readDhallConfig "../dhall-configs/dev/mock-public-transport-provider-platform.dhall"
  let updLogCfg =
        appCfg.loggerConfig{logToFile = False,
                            logToConsole = False
                           }
      updAppCfg = appCfg{loggerConfig = updLogCfg}
  Bpp.buildAppEnv updAppCfg
