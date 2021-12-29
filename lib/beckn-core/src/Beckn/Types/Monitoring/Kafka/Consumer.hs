module Beckn.Types.Monitoring.Kafka.Consumer
  ( module Beckn.Types.Monitoring.Kafka.Consumer,
    module Reexport,
  )
where

import Beckn.Types.Error
import Beckn.Types.Monitoring.Kafka.Commons as Reexport
import Beckn.Utils.Dhall (FromDhall)
import EulerHS.Prelude
import GHC.Records.Extra (HasField)
import Kafka.Consumer as Consumer hiding (groupId)
import qualified Kafka.Consumer as Consumer (groupId)

type KafkaCGroupId = Text

class KafkaConsumer a m where
  receiveMessage :: m a

data KafkaConsumerCfg = KafkaConsumerCfg
  { groupId :: KafkaCGroupId,
    brokers :: KafkaBrokersList,
    topic :: KafkaTopic
  }
  deriving (Generic, FromDhall)

newtype KafkaConsumerTools a = KafkaConsumerTools
  { consumer :: Consumer.KafkaConsumer
  }
  deriving (Generic)

consumerProps :: KafkaConsumerCfg -> ConsumerProperties
consumerProps kafkaConsumerCfg =
  brokersList castBrokers
    <> Consumer.groupId (ConsumerGroupId kafkaConsumerCfg.groupId)
    <> logLevel KafkaLogDebug
  where
    castBrokers = BrokerAddress <$> kafkaConsumerCfg.brokers

consumerSub :: KafkaConsumerCfg -> Subscription
consumerSub kafkaConsumerCfg =
  Consumer.topics [castTopic]
    <> offsetReset Earliest
  where
    castTopic = TopicName kafkaConsumerCfg.topic

buildKafkaConsumerTools :: KafkaConsumerCfg -> IO (KafkaConsumerTools a)
buildKafkaConsumerTools kafkaConsumerCfg = do
  consumer <-
    newConsumer (consumerProps kafkaConsumerCfg) (consumerSub kafkaConsumerCfg)
      >>= either (throwM . KafkaUnableToBuildTools) return
  return $ KafkaConsumerTools {..}

releaseKafkaConsumerTools :: KafkaConsumerTools a -> IO ()
releaseKafkaConsumerTools kafkaConsumerTools =
  closeConsumer kafkaConsumerTools.consumer
    >>= flip whenJust (throwM . KafkaUnableToReleaseTools)