module Beckn.Types.Monitoring.Kafka where

import Beckn.Utils.Dhall (FromDhall)
import EulerHS.Prelude
import GHC.Records.Extra (HasField)
import Kafka.Producer

type HasKafka r = HasField "kafkaTools" r KafkaTools

type KafkaBrokerAddress = Text

type KafkaBrokersList = [KafkaBrokerAddress]

type KafkaTopic = Text

type KafkaKey = ByteString

type KafkaHostName = Text

type KafkaServiceName = Text

data KafkaToolsConfig = KafkaToolsConfig
  { brokers :: KafkaBrokersList,
    serviceName :: KafkaServiceName,
    targetTopic :: KafkaTopic
  }
  deriving (Generic, FromDhall)

data KafkaTools = KafkaTools
  { producer :: KafkaProducer,
    targetTopic :: KafkaTopic
  }
  deriving (Generic)

class Kafka m where
  produceMessage :: ToJSON a => Maybe KafkaKey -> a -> m ()
