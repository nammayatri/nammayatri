module Beckn.Types.Monitoring.Kafka.Producer where

import EulerHS.Prelude
import GHC.Records.Extra (HasField)
import Kafka.Producer as Producer

type HasKafkaProducer r = HasField "kafkaProducerTools" r KafkaProducerTools

type KafkaBrokerAddress = Text

type KafkaBrokersList = [KafkaBrokerAddress]

type KafkaTopic = Text

type KafkaKey = ByteString

type KafkaHostName = Maybe Text

type KafkaServiceName = Text

newtype KafkaProducerTools = KafkaProducerTools
  { producer :: Producer.KafkaProducer
  }
  deriving (Generic)

class KafkaProducer m where
  produceMessage :: ToJSON a => KafkaTopic -> Maybe KafkaKey -> a -> m ()
