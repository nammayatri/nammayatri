module App.BackgroundTaskManager.Types
  ( BTMCfg (),
    BTMEnv (..),
    DriverAllocationConfig (..),
    Env,
    FlowHandler,
    FlowServer,
    Log (..),
    buildBTMEnv,
    module App,
    releaseBTMEnv,
  )
where

import App.Types as App (AppCfg, AppEnv (..))
import qualified App.Types as App
import Beckn.External.Exotel.Types (ExotelCfg)
import Beckn.Storage.DB.Config (DBConfig)
import Beckn.Types.Common
import Beckn.Utils.Dhall (FromDhall)
import Beckn.Utils.IOLogging
import Beckn.Utils.Servant.Client (HttpClientOptions)
import Beckn.Utils.Servant.SignatureAuth
import EulerHS.Prelude
import Types.App (SortMode)
import Types.Metrics
import Types.Shard

data BTMCfg = BTMCfg
  { appCfg :: App.AppCfg,
    metricsPort :: Int,
    driverAllocationConfig :: DriverAllocationConfig,
    httpClientOptions :: HttpClientOptions
  }
  deriving (Generic, FromDhall)

data DriverAllocationConfig = DriverAllocationConfig
  { driverNotificationExpiry :: Seconds,
    rideAllocationExpiry :: Seconds,
    defaultSortMode :: SortMode,
    driverBatchSize :: Int,
    requestsNumPerIteration :: Integer,
    processDelay :: Milliseconds,
    shards :: Shards
  }
  deriving (Generic, FromDhall)

data BTMEnv = BTMEnv
  { config :: BTMCfg,
    dbCfg :: DBConfig,
    nwAddress :: BaseUrl,
    encService :: (String, Word16),
    fcmJsonPath :: Maybe Text,
    exotelCfg :: Maybe ExotelCfg,
    fcmUrl :: BaseUrl,
    isShuttingDown :: TMVar (),
    coreMetrics :: CoreMetricsContainer,
    defaultRadiusOfSearch :: Meters,
    driverPositionInfoExpiry :: Maybe Seconds,
    driverAllocationConfig :: DriverAllocationConfig,
    btmMetrics :: BTMMetricsContainer,
    loggerEnv :: LoggerEnv
  }
  deriving (Generic)

buildBTMEnv :: BTMCfg -> IO BTMEnv
buildBTMEnv btmConfig@BTMCfg {..} = do
  App.AppEnv {..} <- App.buildAppEnv appCfg
  btmMetrics <- registerBTMMetricsContainer
  return $
    BTMEnv
      { config = btmConfig,
        ..
      }

releaseBTMEnv :: BTMEnv -> IO ()
releaseBTMEnv BTMEnv {..} =
  releaseLoggerEnv loggerEnv

type Env = EnvR BTMEnv

type FlowHandler = FlowHandlerR BTMEnv

type FlowServer api = FlowServerR BTMEnv api

instance AuthenticatingEntity BTMEnv where
  getSigningKey = (.config.appCfg.signingKey)
  getSignatureExpiry = (.config.appCfg.signatureExpiry)
