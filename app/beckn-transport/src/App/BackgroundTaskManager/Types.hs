module App.BackgroundTaskManager.Types
  ( BTMCfg (),
    BTMEnv (..),
    DriverAllocationConfig (..),
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
import Beckn.Types.App (EnvR, FlowHandlerR, FlowServerR)
import Beckn.Types.Common
import Beckn.Utils.Dhall (FromDhall)
import Beckn.Utils.Servant.SignatureAuth
import Data.Time (NominalDiffTime)
import EulerHS.Prelude
import Types.App (SortMode)
import Types.Metrics

data BTMCfg = BTMCfg
  { appCfg :: App.AppCfg,
    driverAllocationConfig :: DriverAllocationConfig
  }
  deriving (Generic, FromDhall)

data BTMEnv = BTMEnv
  { appEnv :: App.AppEnv,
    driverAllocationConfig :: DriverAllocationConfig,
    metricsBTMTaskCounter :: TaskCounterMetric,
    metricsBTMTaskDuration :: TaskDurationMetric,
    metricsBTMFailedTaskCounter :: FailedTaskCounterMetric
  }
  deriving (Generic)

data DriverAllocationConfig = DriverAllocationConfig
  { driverNotificationExpiry :: NominalDiffTime,
    rideAllocationExpiry :: NominalDiffTime,
    defaultSortMode :: SortMode,
    requestsNumPerIteration :: Integer,
    processDelay :: NominalDiffTime
  }
  deriving (Generic, FromDhall)

buildBTMEnv :: BTMCfg -> IO BTMEnv
buildBTMEnv BTMCfg {..} = do
  appEnv <- App.buildAppEnv appCfg
  metricsBTMTaskCounter <- registerTaskCounter
  metricsBTMTaskDuration <- registerTaskDurationMetric
  metricsBTMFailedTaskCounter <- registerFailedTaskCounter
  return $
    BTMEnv
      { ..
      }

type Env = EnvR BTMEnv

type Flow = FlowR BTMEnv

type FlowHandler = FlowHandlerR BTMEnv

type FlowServer api = FlowServerR BTMEnv api

instance AuthenticatingEntity BTMEnv where
  getSelfId = getSelfId . appEnv
  getSelfUrl = getSelfUrl . appEnv
  getRegistry = getRegistry . appEnv
  getSigningKeys = getSigningKeys . appEnv
  getSignatureExpiry = getSignatureExpiry . appEnv
