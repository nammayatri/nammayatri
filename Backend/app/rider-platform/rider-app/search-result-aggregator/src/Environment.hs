{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Environment where

import Kernel.Prelude
import Kernel.Storage.Hedis (HedisCfg, HedisEnv, connectHedis, connectHedisCluster)
import Kernel.Storage.Hedis.AppPrefixes (riderAppPrefix)
import Kernel.Types.Common
import Kernel.Utils.App (getPodName, lookupDeploymentVersion)
import Kernel.Utils.Dhall (FromDhall)
import Kernel.Utils.IOLogging
import Kernel.Utils.Shutdown
import Tools.Metrics
import Tools.Streaming.Kafka

data AppCfg = AppCfg
  { port :: Int,
    loggerConfig :: LoggerConfig,
    graceTerminationPeriod :: Seconds,
    kafkaConsumerCfgs :: KafkaConsumerCfgs,
    hedisCfg :: HedisCfg,
    hedisNonCriticalCfg :: HedisCfg,
    hedisNonCriticalClusterCfg :: HedisCfg,
    hedisMigrationStage :: Bool,
    cutOffHedisCluster :: Bool,
    hedisClusterCfg :: HedisCfg,
    enableRedisLatencyLogging :: Bool,
    enablePrometheusMetricLogging :: Bool,
    criticalAPIs :: ApiPriorityList
  }
  deriving (Generic, FromDhall)

data AppEnv = AppEnv
  { port :: Int,
    loggerConfig :: LoggerConfig,
    graceTerminationPeriod :: Seconds,
    isShuttingDown :: Shutdown,
    loggerEnv :: LoggerEnv,
    coreMetrics :: CoreMetricsContainer,
    kafkaConsumerEnv :: KafkaConsumerEnv,
    hedisEnv :: HedisEnv,
    hedisNonCriticalEnv :: HedisEnv,
    hedisNonCriticalClusterEnv :: HedisEnv,
    hedisMigrationStage :: Bool,
    cutOffHedisCluster :: Bool,
    hedisClusterEnv :: HedisEnv,
    version :: DeploymentVersion,
    criticalAPIs :: ApiPriorityList,
    enableRedisLatencyLogging :: Bool,
    enablePrometheusMetricLogging :: Bool
  }
  deriving (Generic)

buildAppEnv :: AppCfg -> IO AppEnv
buildAppEnv AppCfg {..} = do
  podName <- getPodName
  version <- lookupDeploymentVersion
  loggerEnv <- prepareLoggerEnv loggerConfig podName
  coreMetrics <- registerCoreMetricsContainer
  isShuttingDown <- mkShutdown
  kafkaConsumerEnv <- buildKafkaConsumerEnv kafkaConsumerCfgs
  hedisEnv <- connectHedis hedisCfg riderAppPrefix
  -- let riderAppNonCriticalPrefix = riderAppPrefix
  hedisNonCriticalEnv <- connectHedis hedisNonCriticalCfg riderAppPrefix
  hedisNonCriticalClusterEnv <-
    if cutOffHedisCluster
      then pure hedisNonCriticalEnv
      else connectHedisCluster hedisNonCriticalClusterCfg riderAppPrefix
  hedisClusterEnv <-
    if cutOffHedisCluster
      then pure hedisEnv
      else connectHedisCluster hedisClusterCfg riderAppPrefix
  return $ AppEnv {..}

releaseAppEnv :: AppEnv -> IO ()
releaseAppEnv AppEnv {..} = do
  releaseLoggerEnv loggerEnv
  releaseKafkaConsumerEnv kafkaConsumerEnv

type FlowHandler = FlowHandlerR AppEnv

type FlowServer api = FlowServerR AppEnv api
