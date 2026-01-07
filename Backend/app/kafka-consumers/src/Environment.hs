{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Environment where

import qualified Data.Text as T
import EulerHS.Prelude hiding (maybe, show)
import Kafka.Consumer
import Kernel.External.Encryption (EncTools)
import Kernel.Sms.Config (SmsConfig)
import Kernel.Storage.Clickhouse.Config
import Kernel.Storage.Esqueleto.Config (EsqDBConfig, EsqDBEnv, prepareEsqDBEnv)
import Kernel.Storage.Hedis.Config
import qualified Kernel.Storage.InMem as IM
import Kernel.Streaming.Kafka.Producer.Types (KafkaProducerCfg, KafkaProducerTools, buildKafkaProducerTools, castCompression)
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics
import qualified Kernel.Types.CacheFlow as CF
import Kernel.Types.Common (Microseconds, Seconds)
import Kernel.Types.Flow (FlowR)
import Kernel.Types.SlidingWindowCounters
import qualified Kernel.Types.SlidingWindowCounters as SWC
import Kernel.Utils.App (lookupDeploymentVersion)
import Kernel.Utils.Common (CacConfig, CacheConfig)
import Kernel.Utils.Dhall
import Kernel.Utils.IOLogging
import Kernel.Utils.Servant.Client
import Kernel.Utils.Shutdown
import System.Environment (lookupEnv)
import Prelude (show)

data ConsumerConfig = ConsumerConfig
  { topicNames :: [TopicName],
    consumerProperties :: !ConsumerProperties
  }

instance FromDhall ConsumerConfig where
  autoWith _ =
    record
      ( ConsumerConfig
          <$> field "topicNames" (map TopicName <$> list strictText)
          <*> field "consumerProperties" customeDecoder
      )
    where
      customeDecoder =
        record $
          let cgId = field "groupId" strictText
              bs = field "brockers" (map BrokerAddress <$> list strictText)
              kc = field "kafkaCompression" (castCompression <$> auto)
              isAutoCommitM = shouldAutoCommit <$> field "autoCommit" (maybe integer)
           in (\a b c d -> a <> logLevel KafkaLogInfo <> b <> c <> compression d)
                . (groupId . ConsumerGroupId)
                <$> cgId
                <*> isAutoCommitM
                <*> (brokersList <$> bs)
                <*> kc

      shouldAutoCommit = \case
        Nothing -> noAutoCommit
        Just v -> autoCommit (Millis $ fromIntegral v)

data ConsumerType = LOCATION_UPDATE | AVAILABILITY_TIME | BROADCAST_MESSAGE | PERSON_STATS deriving (Generic, FromDhall, Read, Eq)

type ConsumerRecordD = ConsumerRecord (Maybe ByteString) (Maybe ByteString)

instance Show ConsumerType where
  show AVAILABILITY_TIME = "availability-time"
  show BROADCAST_MESSAGE = "broadcast-message"
  show PERSON_STATS = "person-stats"
  show LOCATION_UPDATE = "location-update"

type Seconds' = Integer

type Flow = FlowR AppEnv

data AppCfg = AppCfg
  { esqDBCfg :: EsqDBConfig,
    esqDBReplicaCfg :: EsqDBConfig,
    hedisCfg :: HedisCfg,
    hedisClusterCfg :: HedisCfg,
    hedisSecondaryClusterCfg :: HedisCfg,
    hedisNonCriticalCfg :: HedisCfg,
    hedisNonCriticalClusterCfg :: HedisCfg,
    hedisMigrationStage :: Bool,
    cutOffHedisCluster :: Bool,
    dumpEvery :: Seconds,
    kafkaConsumerCfg :: ConsumerConfig,
    timeBetweenUpdates :: Seconds',
    availabilityTimeWindowOption :: SWC.SlidingWindowOptions,
    granualityPeriodType :: PeriodType,
    loggerConfig :: LoggerConfig,
    cacheConfig :: CacheConfig,
    cacConfig :: CacConfig,
    httpClientOptions :: HttpClientOptions,
    enableRedisLatencyLogging :: Bool,
    enablePrometheusMetricLogging :: Bool,
    healthCheckAppCfg :: Maybe HealthCheckAppCfg,
    kvConfigUpdateFrequency :: Int,
    metricsPort :: Int,
    encTools :: EncTools,
    kafkaProducerCfg :: KafkaProducerCfg,
    serviceClickhouseCfg :: ClickhouseCfg,
    kafkaClickhouseCfg :: ClickhouseCfg,
    dashboardClickhouseCfg :: ClickhouseCfg,
    kafkaReadBatchSize :: Int,
    kafkaReadBatchDelay :: Seconds,
    consumerStartTime :: Maybe Integer,
    consumerEndTime :: Maybe Integer,
    inMemConfig :: CF.InMemConfig
  }
  deriving (Generic, FromDhall)

data AppEnv = AppEnv
  { hedisCfg :: HedisCfg,
    consumerType :: ConsumerType,
    dumpEvery :: Seconds,
    hostname :: Maybe Text,
    hedisEnv :: HedisEnv,
    hedisNonCriticalEnv :: HedisEnv,
    hedisNonCriticalClusterEnv :: HedisEnv,
    hedisClusterEnv :: HedisEnv,
    cutOffHedisCluster :: Bool,
    hedisMigrationStage :: Bool,
    kafkaConsumerCfg :: ConsumerConfig,
    timeBetweenUpdates :: Seconds',
    availabilityTimeWindowOption :: SWC.SlidingWindowOptions,
    granualityPeriodType :: PeriodType,
    loggerConfig :: LoggerConfig,
    loggerEnv :: LoggerEnv,
    esqDBEnv :: EsqDBEnv,
    esqDBReplicaEnv :: EsqDBEnv,
    cacheConfig :: CacheConfig,
    coreMetrics :: Metrics.CoreMetricsContainer,
    version :: Metrics.DeploymentVersion,
    enableRedisLatencyLogging :: Bool,
    enablePrometheusMetricLogging :: Bool,
    requestId :: Maybe Text,
    shouldLogRequestId :: Bool,
    sessionId :: Maybe Text,
    kafkaProducerForART :: Maybe KafkaProducerTools,
    cacConfig :: CacConfig,
    healthCheckAppCfg :: Maybe HealthCheckAppCfg,
    isShuttingDown :: Shutdown,
    httpClientOptions :: HttpClientOptions,
    encTools :: EncTools,
    kafkaProducerTools :: KafkaProducerTools,
    serviceClickhouseEnv :: ClickhouseEnv,
    kafkaClickhouseEnv :: ClickhouseEnv,
    serviceClickhouseCfg :: ClickhouseCfg,
    dashboardClickhouseCfg :: ClickhouseCfg,
    dashboardClickhouseEnv :: ClickhouseEnv,
    kafkaClickhouseCfg :: ClickhouseCfg,
    kafkaReadBatchSize :: Int,
    kafkaReadBatchDelay :: Seconds,
    consumerStartTime :: Maybe Integer,
    consumerEndTime :: Maybe Integer,
    inMemEnv :: CF.InMemEnv,
    url :: Maybe Text
  }
  deriving (Generic)

data HealthCheckAppCfg = HealthCheckAppCfg
  { healthcheckPort :: Int,
    graceTerminationPeriod :: Seconds,
    notificationMinDelay :: Microseconds,
    driverInactiveDelay :: Seconds,
    driverAllowedDelayForLocationUpdateInSec :: Seconds,
    driverLocationHealthCheckIntervalInSec :: Seconds,
    fcmNofificationSendCount :: Int,
    smsCfg :: SmsConfig,
    driverInactiveSmsTemplate :: Text,
    loggerConfig :: LoggerConfig,
    batchSize :: Integer,
    numberOfShards :: Integer,
    enabledMerchantCityIds :: [Text]
  }
  deriving (Generic, FromDhall)

buildAppEnv :: AppCfg -> ConsumerType -> IO AppEnv
buildAppEnv AppCfg {..} consumerType = do
  hostname <- map T.pack <$> lookupEnv "POD_NAME"
  version <- lookupDeploymentVersion
  hedisEnv <- connectHedis hedisCfg id
  hedisNonCriticalEnv <- connectHedis hedisNonCriticalCfg id
  let requestId = Nothing
  shouldLogRequestId <- fromMaybe False . (>>= readMaybe) <$> lookupEnv "SHOULD_LOG_REQUEST_ID"
  let sessionId = Nothing
  let kafkaProducerForART = Nothing
  hedisClusterEnv <-
    if cutOffHedisCluster
      then pure hedisEnv
      else connectHedisCluster hedisClusterCfg id
  hedisNonCriticalClusterEnv <-
    if cutOffHedisCluster
      then pure hedisNonCriticalEnv
      else connectHedisCluster hedisNonCriticalClusterCfg id
  loggerEnv <- prepareLoggerEnv loggerConfig hostname
  coreMetrics <- Metrics.registerCoreMetricsContainer
  esqDBEnv <- prepareEsqDBEnv esqDBCfg loggerEnv
  esqDBReplicaEnv <- prepareEsqDBEnv esqDBReplicaCfg loggerEnv
  kafkaProducerTools <- buildKafkaProducerTools kafkaProducerCfg
  isShuttingDown <- mkShutdown
  serviceClickhouseEnv <- createConn serviceClickhouseCfg
  kafkaClickhouseEnv <- createConn kafkaClickhouseCfg
  dashboardClickhouseEnv <- createConn dashboardClickhouseCfg
  inMemEnv <- IM.setupInMemEnv inMemConfig (Just hedisClusterEnv)
  let url = Nothing
  pure $ AppEnv {..}

releaseAppEnv :: AppEnv -> IO ()
releaseAppEnv AppEnv {..} = do
  releaseLoggerEnv loggerEnv
  disconnectHedis hedisEnv
