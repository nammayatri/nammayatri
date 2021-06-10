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
import Beckn.Types.App (EnvR, FlowHandlerR, FlowServerR)
import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Types.Storage.Organization
import Beckn.Utils.Dhall (FromDhall)
import Beckn.Utils.Servant.SignatureAuth
import qualified Data.Map as Map
import Data.Time (NominalDiffTime)
import EulerHS.Prelude
import Types.App (SortMode)
import Types.Metrics
import Types.Shard
import qualified Utils.Metrics as Metrics

data BTMCfg = BTMCfg
  { appCfg :: App.AppCfg,
    metricsPort :: Int,
    driverNotificationExpiry :: NominalDiffTime,
    rideAllocationExpiry :: NominalDiffTime,
    defaultSortMode :: SortMode,
    requestsNumPerIteration :: Integer,
    processDelay :: NominalDiffTime,
    shards :: [Shard]
  }
  deriving (Generic, FromDhall)

data BTMEnv = BTMEnv
  { appEnv :: App.AppEnv,
    driverNotificationExpiry :: NominalDiffTime,
    rideAllocationExpiry :: NominalDiffTime,
    defaultSortMode :: SortMode,
    requestsNumPerIteration :: Integer,
    processDelay :: NominalDiffTime,
    shards :: Map Int (ShortId Organization),
    metricsBTMTaskCounter :: TaskCounterMetric,
    metricsBTMTaskDuration :: TaskDurationMetric,
    metricsBTMFailedTaskCounter :: FailedTaskCounterMetric
  }
  deriving (Generic)

shardToPair :: Shard -> (Int, ShortId Organization)
shardToPair shard = (shard.shardId, ShortId (shard.shortOrgId))

buildBTMEnv :: BTMCfg -> IO BTMEnv
buildBTMEnv BTMCfg {..} = do
  appEnv <- App.buildAppEnv appCfg
  metricsBTMTaskCounter <- registerTaskCounter
  metricsBTMTaskDuration <- registerTaskDurationMetric
  metricsBTMFailedTaskCounter <- registerFailedTaskCounter
  return $
    BTMEnv
      { shards = Map.fromList $ map shardToPair shards,
        ..
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

instance BTMMetrics Flow where
  incrementTaskCounter = do
    taskCounter <- asks metricsBTMTaskCounter
    Metrics.incrementTaskCounter taskCounter
  incrementFailedTaskCounter = do
    failedTaskCounter <- asks metricsBTMFailedTaskCounter
    Metrics.incrementFailedTaskCounter failedTaskCounter
  putTaskDuration duration = do
    taskDurationMetric <- asks metricsBTMTaskDuration
    Metrics.putTaskDuration taskDurationMetric duration
