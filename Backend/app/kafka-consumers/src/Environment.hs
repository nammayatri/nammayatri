{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Environment where

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import EulerHS.Prelude hiding (maybe, show)
import Kafka.Consumer
import Kernel.External.Encryption (EncTools)
import Kernel.External.Types (SchedulerType)
import qualified Kernel.Prelude
import qualified Kernel.Prelude as Kernel
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
import Kernel.Types.Version (CloudType)
import Kernel.Utils.App (lookupDeploymentVersion)
import Kernel.Utils.Common (CacConfig, CacheConfig)
import Kernel.Utils.Dhall
import Kernel.Utils.IOLogging
import Kernel.Utils.Servant.Client
import Kernel.Utils.Shutdown
import qualified Lib.Finance.Core.Types as Finance
import Lib.SessionizerMetrics.Prometheus.Internal (EventCounterMetric, registerEventRequestCounterMetric)
import Lib.SessionizerMetrics.Types.Event (EventStreamMap)
import Passetto.Client (PassettoContext)
import Passetto.Lib (mkPassettoContextAuto)
import qualified "dynamic-offer-driver-app" SharedLogic.External.LocationTrackingService.Types as LT
import System.Environment (lookupEnv)
import Transporter.RedisStream.Types (RedisStreamCfg)
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

data ConsumerType = LOCATION_UPDATE | BROADCAST_MESSAGE | FLEET_COMMUNICATION_DISPATCH | RIDE_EVENTS_CONSUMER | DOCUMENT_AUDIT_CONSUMER deriving (Generic, FromDhall, Read, Eq)

type ConsumerRecordD = ConsumerRecord (Maybe ByteString) (Maybe ByteString)

instance Show ConsumerType where
  show BROADCAST_MESSAGE = "broadcast-message"
  show LOCATION_UPDATE = "location-update"
  show FLEET_COMMUNICATION_DISPATCH = "fleet-communication-dispatch"
  show RIDE_EVENTS_CONSUMER = "ride-events-consumer"
  show DOCUMENT_AUDIT_CONSUMER = "document-audit-consumer"

-- | Which transport a given Dhall deployment uses. Each consumer-type Dhall
-- file picks one; switching is a config change, not a code change.
data TransportKind = Kafka | RedisStream deriving (Generic, FromDhall, Read, Eq, Show)

type Flow = FlowR AppEnv

data AppCfg = AppCfg
  { esqDBCfg :: EsqDBConfig,
    esqDBReplicaCfg :: EsqDBConfig,
    hedisCfg :: HedisCfg,
    ltsRedisCfg :: HedisCfg,
    secondaryLTSRedisCfg :: Maybe HedisCfg,
    hedisClusterCfg :: HedisCfg,
    hedisSecondaryClusterCfg :: HedisCfg,
    hedisNonCriticalCfg :: HedisCfg,
    hedisNonCriticalClusterCfg :: HedisCfg,
    hedisMigrationStage :: Bool,
    cutOffHedisCluster :: Bool,
    transport :: TransportKind,
    kafkaConsumerCfg :: ConsumerConfig,
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
    secondaryKafkaProducerCfg :: Maybe KafkaProducerCfg,
    serviceClickhouseCfg :: ClickhouseCfg,
    kafkaClickhouseCfg :: ClickhouseCfg,
    dashboardClickhouseCfg :: ClickhouseCfg,
    kafkaReadBatchSize :: Int,
    kafkaReadBatchDelay :: Seconds,
    consumerStartTime :: Maybe Integer,
    consumerEndTime :: Maybe Integer,
    inMemConfig :: CF.InMemConfig,
    smsCfg :: SmsConfig,
    redisStreamCfg :: Maybe RedisStreamCfg,
    ltsCfg :: LT.LocationTrackingeServiceConfig,
    eventStreamMap :: [EventStreamMap],
    schedulerSetName :: Text,
    schedulerType :: SchedulerType,
    maxShards :: Int,
    jobInfoMap :: M.Map Text Bool,
    blackListedJobs :: [Text],
    shortDurationRetryCfg :: RetryCfg
  }
  deriving (Generic, FromDhall)

data AppEnv = AppEnv
  { hedisCfg :: HedisCfg,
    consumerType :: ConsumerType,
    transport :: TransportKind,
    hostname :: Maybe Text,
    hedisEnv :: HedisEnv,
    ltsHedisEnv :: HedisEnv,
    secondaryLTSHedisEnv :: Maybe HedisEnv,
    hedisNonCriticalEnv :: HedisEnv,
    hedisNonCriticalClusterEnv :: HedisEnv,
    hedisClusterEnv :: HedisEnv,
    secondaryHedisClusterEnv :: Maybe HedisEnv,
    cutOffHedisCluster :: Bool,
    hedisMigrationStage :: Bool,
    kafkaConsumerCfg :: ConsumerConfig,
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
    url :: Maybe Text,
    smsCfg :: SmsConfig,
    passettoContext :: PassettoContext,
    redisStreamCfg :: Maybe RedisStreamCfg,
    ltsCfg :: LT.LocationTrackingeServiceConfig,
    eventStreamMap :: [EventStreamMap],
    eventRequestCounter :: EventCounterMetric,
    schedulerSetName :: Text,
    schedulerType :: SchedulerType,
    maxShards :: Int,
    jobInfoMap :: M.Map Text Bool,
    blackListedJobs :: [Text],
    shortDurationRetryCfg :: RetryCfg,
    cloudType :: Maybe CloudType,
    actorInfo :: Finance.ActorInfo
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
  ltsHedisEnv <- connectHedis ltsRedisCfg id
  secondaryLTSHedisEnv <-
    case secondaryLTSRedisCfg of
      Nothing -> pure Nothing
      Just cfg' ->
        Kernel.try (connectHedis cfg' id) >>= \case
          Left (e :: SomeException) -> do
            putStrLn $ "ERROR: Failed to connect to secondary LTS hedis: " ++ Kernel.Prelude.show e
            pure Nothing
          Right env -> pure (Just env)
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
  secondaryHedisClusterEnv <-
    Kernel.try (connectHedisCluster hedisSecondaryClusterCfg id) >>= \case
      Left (e :: SomeException) -> do
        putStrLn $ "ERROR: Failed to connect to secondary hedis cluster: " ++ Kernel.Prelude.show e
        pure Nothing
      Right env -> pure (Just env)
  loggerEnv <- prepareLoggerEnv loggerConfig hostname
  coreMetrics <- Metrics.registerCoreMetricsContainer
  eventRequestCounter <- registerEventRequestCounterMetric
  esqDBEnv <- prepareEsqDBEnv esqDBCfg loggerEnv
  esqDBReplicaEnv <- prepareEsqDBEnv esqDBReplicaCfg loggerEnv
  kafkaProducerTools <- buildKafkaProducerTools kafkaProducerCfg secondaryKafkaProducerCfg
  passettoContext <- uncurry mkPassettoContextAuto encTools.service
  isShuttingDown <- mkShutdown
  serviceClickhouseEnv <- createConn serviceClickhouseCfg
  kafkaClickhouseEnv <- createConn kafkaClickhouseCfg
  dashboardClickhouseEnv <- createConn dashboardClickhouseCfg
  inMemEnv <- IM.setupInMemEnv inMemConfig (Just hedisClusterEnv)
  let url = Nothing
  let cloudType = Nothing :: Maybe CloudType
  let actorInfo = Finance.ActorInfo {actorType = Finance.UNKNOWN, actorId = requestId}
  pure $ AppEnv {..}

releaseAppEnv :: AppEnv -> IO ()
releaseAppEnv AppEnv {..} = do
  releaseLoggerEnv loggerEnv
  disconnectHedis hedisEnv
  disconnectHedis ltsHedisEnv
  Kernel.Prelude.maybe (pure ()) disconnectHedis secondaryLTSHedisEnv
  Kernel.Prelude.maybe (pure ()) disconnectHedis secondaryHedisClusterEnv
