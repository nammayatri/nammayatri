module Environment where

import Beckn.Storage.Esqueleto.Config (EsqDBConfig, EsqDBEnv, prepareEsqDBEnv)
import Beckn.Storage.Hedis.Config
import Beckn.Types.SlidingWindowCounters
import Beckn.Utils.Dhall
import Beckn.Utils.IOLogging
import qualified Data.Text as T
import EulerHS.Prelude hiding (show)
import Kafka.Consumer
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
           in (\a b -> a <> logLevel KafkaLogInfo <> b)
                . (groupId . ConsumerGroupId)
                <$> cgId
                <*> ((<>) noAutoCommit . brokersList <$> bs)

data ConsumerType = AVAILABILITY_TIME | FEED_TO_CLICKHOUSE deriving (Generic, FromDhall, Read)

instance Show ConsumerType where
  show AVAILABILITY_TIME = "availability-time"
  show FEED_TO_CLICKHOUSE = "feed-to-clickhouse"

type Seconds = Integer

data AppCfg = AppCfg
  { esqDBCfg :: EsqDBConfig,
    hedisCfg :: HedisCfg,
    dumpEvery :: Seconds,
    kafkaConsumerCfg :: ConsumerConfig,
    timeBetweenUpdates :: Seconds,
    windowOptions :: SlidingWindowOptions,
    granualityPeriodType :: PeriodType,
    loggerConfig :: LoggerConfig
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
    windowOptions :: SlidingWindowOptions,
    granualityPeriodType :: PeriodType,
    loggerConfig :: LoggerConfig,
    loggerEnv :: LoggerEnv,
    esqDBEnv :: EsqDBEnv
  }
  deriving (Generic)

buildAppEnv :: AppCfg -> ConsumerType -> IO AppEnv
buildAppEnv AppCfg {..} consumerType = do
  hostname <- map T.pack <$> lookupEnv "POD_NAME"
  hedisEnv <- connectHedis hedisCfg id
  loggerEnv <- prepareLoggerEnv loggerConfig hostname
  esqDBEnv <- prepareEsqDBEnv esqDBCfg loggerEnv
  pure $ AppEnv {..}
