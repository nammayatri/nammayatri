module PublicTransport.Common where

import Beckn.Prelude
import Beckn.Types.Time
import Beckn.Utils.Dhall (readDhallConfig)
import qualified "mock-public-transport-bpp" Environment as Bpp
import qualified "public-transport-bap" Environment as Bap
import GHC.IO (unsafePerformIO)
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
mockWaitTimeSeconds = 2

{-# NOINLINE publicTransportBapEnv #-}
publicTransportBapEnv :: Bap.AppEnv
publicTransportBapEnv = unsafePerformIO $ do
  appCfg <- readDhallConfig "../dhall-configs/dev/public-transport-bap.dhall"
  let updLogCfg =
        appCfg.loggerConfig{logToFile = False,
                            logToConsole = False
                           }
      updAppCfg = appCfg{loggerConfig = updLogCfg}
  Bap.buildAppEnv updAppCfg

{-# NOINLINE mockBppEnv #-}
mockBppEnv :: Bpp.AppEnv
mockBppEnv = unsafePerformIO $ do
  appCfg <- readDhallConfig "../dhall-configs/dev/mock-public-transport-bpp.dhall"
  let updLogCfg =
        appCfg.loggerConfig{logToFile = False,
                            logToConsole = False
                           }
      updAppCfg = appCfg{loggerConfig = updLogCfg}
  Bpp.buildAppEnv updAppCfg
