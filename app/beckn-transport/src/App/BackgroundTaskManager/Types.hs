module App.BackgroundTaskManager.Types
  ( BTMCfg (),
    BTMEnv (..),
    Env,
    Flow,
    FlowHandler,
    FlowServer,
    Log (..),
    buildBTMEnv,
    module App,
  )
where

import App.Types as App (AppCfg, AppEnv (..))
import qualified App.Types as App
import Beckn.Types.Common
import Beckn.Utils.Dhall (FromDhall)
import Beckn.Utils.Servant.SignatureAuth
import Data.Time (NominalDiffTime)
import EulerHS.Prelude
import Types.App (SortMode)
import Types.Metrics
import Types.Shard
import qualified Utils.Metrics as Metrics

data BTMCfg = BTMCfg
  { appCfg :: App.AppCfg,
    metricsPort :: Int,
    driverAllocationConfig :: DriverAllocationConfig
  }
  deriving (Generic, FromDhall)

data DriverAllocationConfig = DriverAllocationConfig
  { driverNotificationExpiry :: NominalDiffTime,
    rideAllocationExpiry :: NominalDiffTime,
    defaultSortMode :: SortMode,
    requestsNumPerIteration :: Integer,
    processDelay :: NominalDiffTime,
    shards :: Shards
  }
  deriving (Generic, FromDhall)

data BTMEnv = BTMEnv
  { appEnv :: App.AppEnv,
    driverAllocationConfig :: DriverAllocationConfig,
    btmMetrics :: BTMMetricsContainer
  }
  deriving (Generic)

buildBTMEnv :: BTMCfg -> IO BTMEnv
buildBTMEnv BTMCfg {..} = do
  appEnv <- App.buildAppEnv appCfg
  btmMetrics <- registerBTMMetricsContainer
  return $
    BTMEnv
      { ..
      }

type Env = EnvR BTMEnv

type Flow = FlowR BTMEnv

type FlowHandler = FlowHandlerR BTMEnv

type FlowServer api = FlowServerR BTMEnv api

instance AuthenticatingEntity BTMEnv where
  getSelfId btmEnv = getSelfId btmEnv.appEnv
  getSelfUrl btmEnv = getSelfUrl btmEnv.appEnv
  getRegistry btmEnv = getRegistry btmEnv.appEnv
  getSigningKeys btmEnv = getSigningKeys btmEnv.appEnv
  getSignatureExpiry btmEnv = getSignatureExpiry btmEnv.appEnv

instance BTMMetrics Flow where
  incrementTaskCounter = Metrics.incrementTaskCounterFlow
  incrementFailedTaskCounter = Metrics.incrementFailedTaskCounterFlow
  putTaskDuration = Metrics.putTaskDurationFlow
