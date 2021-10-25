module Beckn.Types.Monitoring.Kafka where

import Beckn.Utils.Dhall (FromDhall)
import Data.Aeson (Value)
import Data.Time (UTCTime)
import EulerHS.Prelude
import GHC.Records.Extra (HasField)
import Kafka.Producer

data KafkaToolsConfig = KafkaToolsConfig
  { brokers :: KafkaBrokersList,
    serviceName :: KafkaServiceName,
    targetTopic :: KafkaTopic
  }
  deriving (Generic, FromDhall)

data KafkaTools = KafkaTools
  { producer :: KafkaProducer,
    serviceName :: KafkaServiceName,
    hostName :: KafkaHostName,
    targetTopic :: KafkaTopic
  }
  deriving (Generic)

class Kafka m where
  produceMessage :: BusinessEvent -> m ()

type KafkaBrokerAddress = Text

type KafkaBrokersList = [KafkaBrokerAddress]

type HasKafka r = HasField "kafkaTools" r KafkaTools

type KafkaTopic = Text

type KafkaKey = ByteString

type KafkaEventName = Text

type KafkaHostName = Text

type KafkaEventHostName = Text

type KafkaServiceName = Text

type KafkaMetadata = Value

type KafkaPayload = Value

data BusinessEvent = BusinessEvent
  { timestamp :: UTCTime,
    eventName :: KafkaEventName,
    hostName :: KafkaHostName,
    serviceName :: KafkaServiceName, -- mobility BAP | BPP
    metadata :: KafkaMetadata,
    payload :: KafkaPayload
  }
  deriving (Generic, Show, FromJSON, ToJSON)