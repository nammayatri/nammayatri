module Beckn.Streaming.Kafka.Consumer.Types
  ( KafkaConsumerCfg (..),
    KafkaConsumerTools,
    buildKafkaConsumerTools,
    releaseKafkaConsumerTools,
    module Reexport,
  )
where

import Beckn.Streaming.Kafka.Commons as Reexport
import Beckn.Streaming.Kafka.HasKafkaTopics
import Beckn.Types.Error
import Beckn.Utils.Dhall (FromDhall)
import EulerHS.Prelude
import GHC.Records.Extra (HasField)
import Kafka.Consumer as Consumer hiding (groupId)

newtype KafkaConsumerCfg = KafkaConsumerCfg
  { brokers :: KafkaBrokersList
  }
  deriving (Generic, FromDhall)

newtype KafkaConsumerTools a = KafkaConsumerTools
  { consumer :: Consumer.KafkaConsumer
  }
  deriving (Generic)

consumerProps :: KafkaConsumerCfg -> ConsumerProperties
consumerProps kafkaConsumerCfg =
  brokersList castBrokers
    <> logLevel KafkaLogDebug
  where
    castBrokers = BrokerAddress <$> kafkaConsumerCfg.brokers

consumerSub :: [KafkaTopic] -> Subscription
consumerSub topicList =
  Consumer.topics castTopics
    <> offsetReset Earliest
  where
    castTopics = TopicName <$> topicList

buildKafkaConsumerTools :: forall a. HasKafkaTopics a => KafkaConsumerCfg -> IO (KafkaConsumerTools a)
buildKafkaConsumerTools kafkaConsumerCfg = do
  consumer <-
    newConsumer (consumerProps kafkaConsumerCfg) (consumerSub $ getTopics @a)
      >>= either (throwM . KafkaUnableToBuildTools) return
  return $ KafkaConsumerTools {..}

releaseKafkaConsumerTools :: KafkaConsumerTools a -> IO ()
releaseKafkaConsumerTools kafkaConsumerTools =
  closeConsumer kafkaConsumerTools.consumer
    >>= flip whenJust (throwM . KafkaUnableToReleaseTools)
