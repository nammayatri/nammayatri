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
  )
where

import App.Types as App (AppCfg, AppEnv (..))
import qualified App.Types as App
import Beckn.External.Exotel.Types (ExotelCfg)
import Beckn.Storage.DB.Config (DBConfig)
import Beckn.Types.Common
import Beckn.Types.Credentials
import Beckn.Utils.Dhall (FromDhall)
import Beckn.Utils.Servant.Client (HttpClientOptions)
import Beckn.Utils.Servant.SignatureAuth
import EulerHS.Prelude
import Types.App (SortMode)
import Types.Metrics
import Types.Shard

data BTMCfg = BTMCfg
  { appCfg :: App.AppCfg,
    metricsPort :: Int,
    driverAllocationConfig :: DriverAllocationConfig
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
  { dbCfg :: DBConfig,
    selfId :: Text,
    nwAddress :: BaseUrl,
    credRegistry :: [Credential],
    signingKeys :: [SigningKey],
    encService :: (String, Word16),
    fcmJsonPath :: Maybe Text,
    exotelCfg :: Maybe ExotelCfg,
    signatureExpiry :: Seconds,
    fcmUrl :: BaseUrl,
    isShuttingDown :: TMVar (),
    coreMetrics :: CoreMetricsContainer,
    defaultRadiusOfSearch :: Meters,
    driverPositionInfoExpiry :: Maybe Seconds,
    driverAllocationConfig :: DriverAllocationConfig,
    btmMetrics :: BTMMetricsContainer,
    httpClientOptions :: HttpClientOptions
  }
  deriving (Generic)

buildBTMEnv :: BTMCfg -> IO BTMEnv
buildBTMEnv BTMCfg {..} = do
  App.AppEnv {..} <- App.buildAppEnv appCfg
  btmMetrics <- registerBTMMetricsContainer
  return $
    BTMEnv
      { ..
      }

type Env = EnvR BTMEnv

type FlowHandler = FlowHandlerR BTMEnv

type FlowServer api = FlowServerR BTMEnv api

instance AuthenticatingEntity BTMEnv where
  getSelfUrl = nwAddress
  getRegistry = credRegistry
  getSigningKeys = signingKeys
  getSignatureExpiry = signatureExpiry
