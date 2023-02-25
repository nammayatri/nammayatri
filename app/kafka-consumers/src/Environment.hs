module Environment where

import qualified Data.Text as T
import EulerHS.Prelude hiding (maybe, show)
import Kafka.Consumer
import Kernel.Storage.Esqueleto.Config (EsqDBConfig, EsqDBEnv, prepareEsqDBEnv)
import Kernel.Storage.Hedis.Config
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Flow (FlowR)
import Kernel.Types.SlidingWindowCounters
import qualified Kernel.Types.SlidingWindowCounters as SWC
import Kernel.Utils.Dhall
import Kernel.Utils.IOLogging
import Storage.CachedQueries.CacheConfig
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
              isAutoCommitM = shouldAutoCommit <$> field "autoCommit" (maybe integer)
           in (\a b c -> a <> logLevel KafkaLogInfo <> b <> c)
                . (groupId . ConsumerGroupId)
                <$> cgId
                <*> isAutoCommitM
                <*> (brokersList <$> bs)

      shouldAutoCommit = \case
        Nothing -> noAutoCommit
        Just v -> autoCommit (Millis $ fromIntegral v)

data ConsumerType = AVAILABILITY_TIME | BROADCAST_MESSAGE deriving (Generic, FromDhall, Read)

type ConsumerRecordD = ConsumerRecord (Maybe ByteString) (Maybe ByteString)

instance Show ConsumerType where
  show AVAILABILITY_TIME = "availability-time"
  show BROADCAST_MESSAGE = "broadcast-message"

type Seconds = Integer

type Flow = FlowR AppEnv

data AppCfg = AppCfg
  { esqDBCfg :: EsqDBConfig,
    esqDBReplicaCfg :: EsqDBConfig,
    hedisCfg :: HedisCfg,
    dumpEvery :: Seconds,
    kafkaConsumerCfg :: ConsumerConfig,
    timeBetweenUpdates :: Seconds,
    availabilityTimeWindowOption :: SWC.SlidingWindowOptions,
    granualityPeriodType :: PeriodType,
    loggerConfig :: LoggerConfig,
    cacheConfig :: CacheConfig
  }
  deriving (Generic, FromDhall)

data AppEnv = AppEnv
  { hedisCfg :: HedisCfg,
    consumerType :: ConsumerType,
    dumpEvery :: Seconds,
    hostname :: Maybe Text,
    hedisEnv :: HedisEnv,
    kafkaConsumerCfg :: ConsumerConfig,
    timeBetweenUpdates :: Seconds,
    availabilityTimeWindowOption :: SWC.SlidingWindowOptions,
    granualityPeriodType :: PeriodType,
    loggerConfig :: LoggerConfig,
    loggerEnv :: LoggerEnv,
    esqDBEnv :: EsqDBEnv,
    esqDBReplicaEnv :: EsqDBEnv,
    cacheConfig :: CacheConfig,
    coreMetrics :: Metrics.CoreMetricsContainer
  }
  deriving (Generic)

buildAppEnv :: AppCfg -> ConsumerType -> IO AppEnv
buildAppEnv AppCfg {..} consumerType = do
  hostname <- map T.pack <$> lookupEnv "POD_NAME"
  hedisEnv <- connectHedis hedisCfg id
  loggerEnv <- prepareLoggerEnv loggerConfig hostname
  coreMetrics <- Metrics.registerCoreMetricsContainer
  esqDBEnv <- prepareEsqDBEnv esqDBCfg loggerEnv
  esqDBReplicaEnv <- prepareEsqDBEnv esqDBReplicaCfg loggerEnv
  pure $ AppEnv {..}
