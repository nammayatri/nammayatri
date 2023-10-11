{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Environment where

import AWS.S3
import qualified Data.Text as T
import EulerHS.Prelude hiding (maybe, show)
import Kafka.Consumer hiding (OffsetReset (..), offsetReset)
import qualified Kafka.Consumer as Consumer
import qualified Kernel.Prelude as P (maybe, throwIO)
import Kernel.Storage.Esqueleto.Config (EsqDBConfig, EsqDBEnv, prepareEsqDBEnv)
import Kernel.Storage.Hedis.Config
import qualified Kernel.Streaming.Kafka.Producer.Types as KT
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Common (Tables)
import Kernel.Types.Error
import Kernel.Types.Flow (FlowR)
import Kernel.Types.SlidingWindowCounters
import qualified Kernel.Types.SlidingWindowCounters as SWC
import Kernel.Utils.App (lookupDeploymentVersion)
import Kernel.Utils.Common (CacheConfig)
import Kernel.Utils.Dhall
import Kernel.Utils.IOLogging
import Kernel.Utils.Servant.Client
import System.Environment (lookupEnv)
import Prelude (show)

data ConsumerConfig = ConsumerConfig
  { topicNames :: [Consumer.TopicName],
    offsetReset :: !(Maybe Consumer.OffsetReset),
    consumerProperties :: !Consumer.ConsumerProperties
  }

data OffsetResetConfig = Earliest | Latest | Default
  deriving (Generic, FromDhall)

castOffsetReset :: OffsetResetConfig -> Maybe Consumer.OffsetReset
castOffsetReset Earliest = Just Consumer.Earliest
castOffsetReset Latest = Just Consumer.Latest
castOffsetReset Default = Nothing

instance FromDhall ConsumerConfig where
  autoWith _ =
    record
      ( ConsumerConfig
          <$> field "topicNames" (map TopicName <$> list strictText)
          <*> field "offsetReset" (castOffsetReset <$> auto @OffsetResetConfig)
          <*> field "consumerProperties" customeDecoder
      )
    where
      customeDecoder =
        record $
          let cgId = field "groupId" strictText
              bs = field "brockers" (map BrokerAddress <$> list strictText)
              kc = field "kafkaCompression" (KT.castCompression <$> auto)
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

data ConsumerType
  = AVAILABILITY_TIME
  | BROADCAST_MESSAGE
  | PERSON_STATS
  | RIDER_BECKN_REQUEST
  | DRIVER_BECKN_REQUEST
  deriving (Generic, FromDhall, Read, Eq)

type ConsumerRecordD = ConsumerRecord (Maybe ByteString) (Maybe ByteString)

instance Show ConsumerType where
  show AVAILABILITY_TIME = "availability-time"
  show BROADCAST_MESSAGE = "broadcast-message"
  show PERSON_STATS = "person-stats"
  show RIDER_BECKN_REQUEST = "rider-beckn-request"
  show DRIVER_BECKN_REQUEST = "driver-beckn-request"

type Seconds = Integer

type Flow = FlowR AppEnv

data AppCfg = AppCfg
  { esqDBCfg :: EsqDBConfig,
    esqDBReplicaCfg :: EsqDBConfig,
    hedisCfg :: HedisCfg,
    hedisClusterCfg :: HedisCfg,
    hedisNonCriticalCfg :: HedisCfg,
    hedisNonCriticalClusterCfg :: HedisCfg,
    hedisMigrationStage :: Bool,
    cutOffHedisCluster :: Bool,
    dumpEvery :: Seconds,
    kafkaConsumerCfg :: ConsumerConfig,
    timeBetweenUpdates :: Seconds,
    availabilityTimeWindowOption :: SWC.SlidingWindowOptions,
    granualityPeriodType :: PeriodType,
    loggerConfig :: LoggerConfig,
    cacheConfig :: CacheConfig,
    httpClientOptions :: HttpClientOptions,
    enableRedisLatencyLogging :: Bool,
    enablePrometheusMetricLogging :: Bool,
    tables :: Tables,
    s3Config :: Maybe S3Config
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
    timeBetweenUpdates :: Seconds,
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
    s3Env :: S3Env Flow
  }
  deriving (Generic)

buildAppEnv :: AppCfg -> ConsumerType -> IO AppEnv
buildAppEnv AppCfg {..} consumerType = do
  hostname <- map T.pack <$> lookupEnv "POD_NAME"
  version <- lookupDeploymentVersion
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
  loggerEnv <- prepareLoggerEnv loggerConfig hostname
  coreMetrics <- Metrics.registerCoreMetricsContainer
  esqDBEnv <- prepareEsqDBEnv esqDBCfg loggerEnv
  esqDBReplicaEnv <- prepareEsqDBEnv esqDBReplicaCfg loggerEnv
  s3Env <-
    if consumerType `elem` [RIDER_BECKN_REQUEST, DRIVER_BECKN_REQUEST]
      then do
        P.maybe (P.throwIO $ InternalError "s3Config required for this consumer type") (pure . buildS3Env) s3Config
      else do
        whenJust s3Config $ const (P.throwIO $ InternalError "s3Config not required for this consumer type")
        pure
          S3Env
            { pathPrefix = "",
              getH = \_ -> P.throwIO $ InternalError "s3Config not provided for this consumer type",
              putH = \_ _ -> P.throwIO $ InternalError "s3Config not provided for this consumer type"
            }
  pure $ AppEnv {..}
