{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wwarn=ambiguous-fields #-}

module Environment where

import qualified Data.Text as T
import EulerHS.Prelude hiding (maybe)
import Kernel.Storage.Esqueleto.Config
import Kernel.Storage.Hedis
import Kernel.Streaming.Kafka.Producer.Types
import Kernel.Tools.Metrics.CoreMetrics
import Kernel.Types.CacheFlow as CF
import Kernel.Types.Common hiding (id)
import Kernel.Types.Flow (FlowR)
import Kernel.Utils.App (lookupDeploymentVersion)
import Kernel.Utils.Dhall (FromDhall)
import Kernel.Utils.IOLogging
import Lib.Scheduler (SchedulerType)
import System.Environment (lookupEnv)

data ProducerType = Rider | Driver
  deriving (Generic, Read, Show)

data AppCfg = AppCfg
  { esqDBCfg :: EsqDBConfig,
    esqDBReplicaCfg :: EsqDBConfig,
    hedisCfg :: HedisCfg,
    hedisClusterCfg :: HedisCfg,
    hedisNonCriticalCfg :: HedisCfg,
    hedisNonCriticalClusterCfg :: HedisCfg,
    hedisMigrationStage :: Bool,
    cutOffHedisCluster :: Bool,
    loggerConfig :: LoggerConfig,
    enableRedisLatencyLogging :: Bool,
    batchSize :: Int,
    waitTimeMilliSec :: Double,
    enablePrometheusMetricLogging :: Bool,
    streamName :: Text,
    producerTimestampKey :: Text,
    cacheConfig :: CF.CacheConfig,
    cacConfig :: CF.CacConfig,
    schedulerSetName :: Text,
    entryId :: Text,
    reviverInterval :: Minutes,
    reviveThreshold :: Seconds,
    maxShards :: Int,
    producersPerPod :: Int,
    metricsPort :: Int,
    schedulerType :: SchedulerType,
    kvConfigUpdateFrequency :: Int,
    runReviver :: Bool,
    kafkaProducerCfg :: KafkaProducerCfg
  }
  deriving (Generic, FromDhall)

data AppEnv = AppEnv
  { esqDBEnv :: EsqDBEnv,
    esqDBReplicaEnv :: EsqDBEnv,
    maxShards :: Int,
    producersPerPod :: Int,
    hedisEnv :: HedisEnv,
    hedisNonCriticalEnv :: HedisEnv,
    hedisNonCriticalClusterEnv :: HedisEnv,
    hedisClusterEnv :: HedisEnv,
    cutOffHedisCluster :: Bool,
    hedisMigrationStage :: Bool,
    hostname :: Maybe Text,
    coreMetrics :: CoreMetricsContainer,
    loggerEnv :: LoggerEnv,
    enableRedisLatencyLogging :: Bool,
    enablePrometheusMetricLogging :: Bool,
    waitTimeMilliSec :: Double,
    loggerConfig :: LoggerConfig,
    batchSize :: Int,
    version :: DeploymentVersion,
    streamName :: Text,
    producerTimestampKey :: Text,
    cacheConfig :: CF.CacheConfig,
    cacConfig :: CF.CacConfig,
    schedulerSetName :: Text,
    entryId :: Text,
    schedulerType :: SchedulerType,
    reviverInterval :: Minutes,
    reviveThreshold :: Seconds,
    runReviver :: Bool,
    kafkaProducerTools :: KafkaProducerTools
  }
  deriving (Generic)

buildAppEnv :: AppCfg -> ProducerType -> IO AppEnv
buildAppEnv AppCfg {..} producerType = do
  let modifierFunc = (show producerType <>)
  hedisEnv <- connectHedis hedisCfg modifierFunc
  version <- lookupDeploymentVersion
  hostname <- map T.pack <$> lookupEnv "POD_NAME"
  coreMetrics <- registerCoreMetricsContainer
  loggerEnv <- prepareLoggerEnv loggerConfig hostname
  kafkaProducerTools <- buildKafkaProducerTools kafkaProducerCfg
  hedisNonCriticalEnv <- connectHedis hedisNonCriticalCfg modifierFunc
  hedisClusterEnv <-
    if cutOffHedisCluster
      then pure hedisEnv
      else connectHedisCluster hedisClusterCfg id
  hedisNonCriticalClusterEnv <-
    if cutOffHedisCluster
      then pure hedisNonCriticalEnv
      else connectHedisCluster hedisNonCriticalClusterCfg id
  esqDBEnv <- prepareEsqDBEnv esqDBCfg loggerEnv
  esqDBReplicaEnv <- prepareEsqDBEnv esqDBReplicaCfg loggerEnv
  pure $ AppEnv {..}

type FlowHandler = FlowHandlerR AppEnv

type Flow = FlowR AppEnv
