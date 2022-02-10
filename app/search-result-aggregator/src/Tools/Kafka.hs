module Tools.Kafka where

import Beckn.Prelude
import Beckn.Streaming.Kafka.Consumer.Types
import Beckn.Streaming.Kafka.Topic.PublicTransportQuoteList
import Beckn.Utils.Dhall (FromDhall)

newtype KafkaConsumerCfgs = KafkaConsumerCfgs
  { publicTransportStation :: KafkaConsumerCfg
  }
  deriving (Generic, FromDhall)

newtype KafkaConsumerEnv = KafkaConsumerEnv
  { publicTransportStation :: KafkaConsumerTools PublicTransportQuoteList
  }
  deriving (Generic)

buildKafkaConsumerEnv :: KafkaConsumerCfgs -> IO KafkaConsumerEnv
buildKafkaConsumerEnv cfgs = do
  publicTransportStation <- buildKafkaConsumerTools @PublicTransportQuoteList cfgs.publicTransportStation
  return KafkaConsumerEnv {..}

releaseKafkaConsumerEnv :: KafkaConsumerEnv -> IO ()
releaseKafkaConsumerEnv KafkaConsumerEnv {..} = do
  releaseKafkaConsumerTools publicTransportStation
