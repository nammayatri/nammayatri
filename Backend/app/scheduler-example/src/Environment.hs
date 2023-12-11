{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Environment where

import App.Scheduler.Types (SchedulerJobType)
import qualified Data.Map as M
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config
import Kernel.Storage.Hedis (HedisCfg, HedisEnv, connectHedis, connectHedisCluster)
import Kernel.Types.Common hiding (id)
import Kernel.Types.Flow
import Kernel.Utils.App (getPodName, lookupDeploymentVersion)
import Kernel.Utils.Dhall (FromDhall)
import Kernel.Utils.IOLogging
import Kernel.Utils.Shutdown
import Lib.Scheduler.Types hiding (id)
import Tools.Metrics
import Prelude (id)

data AppCfg = AppCfg
  { esqDBCfg :: EsqDBConfig,
    port :: Int,
    loggerConfig :: LoggerConfig,
    hedisCfg :: HedisCfg,
    hedisClusterCfg :: HedisCfg,
    hedisNonCriticalCfg :: HedisCfg,
    hedisNonCriticalClusterCfg :: HedisCfg,
    graceTerminationPeriod :: Seconds,
    migrationPath :: Maybe FilePath,
    autoMigrate :: Bool,
    cutOffHedisCluster :: Bool,
    hedisMigrationStage :: Bool,
    enablePrometheusMetricLogging :: Bool,
    jobInfoMapx :: M.Map SchedulerJobType Bool,
    schedulerSetName :: Text,
    streamName :: Text,
    schedulerType :: SchedulerType,
    enableRedisLatencyLogging :: Bool,
    groupName :: Text,
    criticalAPIs :: ApiPriorityList
  }
  deriving (Generic, FromDhall)

data AppEnv = AppEnv
  { port :: Int,
    loggerConfig :: LoggerConfig,
    graceTerminationPeriod :: Seconds,
    esqDBEnv :: EsqDBEnv,
    isShuttingDown :: Shutdown,
    loggerEnv :: LoggerEnv,
    schedulerSetName :: Text,
    streamName :: Text,
    schedulerType :: SchedulerType,
    coreMetrics :: CoreMetricsContainer,
    version :: DeploymentVersion,
    criticalAPIs :: ApiPriorityList,
    hedisMigrationStage :: Bool,
    hedisEnv :: HedisEnv,
    hedisClusterEnv :: HedisEnv,
    hedisNonCriticalEnv :: HedisEnv,
    hedisNonCriticalClusterEnv :: HedisEnv,
    enablePrometheusMetricLogging :: Bool,
    enableRedisLatencyLogging :: Bool,
    groupName :: Text,
    jobInfoMap :: M.Map Text Bool
  }
  deriving (Generic)

buildAppEnv :: AppCfg -> IO AppEnv
buildAppEnv AppCfg {..} = do
  podName <- getPodName
  version <- lookupDeploymentVersion
  loggerEnv <- prepareLoggerEnv loggerConfig podName
  esqDBEnv <- prepareEsqDBEnv esqDBCfg loggerEnv
  hedisEnv <- connectHedis hedisCfg id
  hedisNonCriticalEnv <- connectHedis hedisNonCriticalCfg id
  hedisClusterEnv <-
    if cutOffHedisCluster
      then pure hedisEnv
      else connectHedisCluster hedisClusterCfg id
  hedisNonCriticalClusterEnv <-
    if cutOffHedisCluster
      then pure hedisNonCriticalEnv
      else connectHedisCluster hedisNonCriticalClusterCfg id
  let jobInfoMap :: (M.Map Text Bool) = M.mapKeys show jobInfoMapx
  coreMetrics <- registerCoreMetricsContainer
  isShuttingDown <- mkShutdown
  return $ AppEnv {..}

releaseAppEnv :: AppEnv -> IO ()
releaseAppEnv AppEnv {..} =
  releaseLoggerEnv loggerEnv

type FlowHandler = FlowHandlerR AppEnv

type FlowServer api = FlowServerR AppEnv api

type Flow = FlowR AppEnv
