module Tools.Streaming.Kafka where

import Beckn.Prelude
import Beckn.Streaming.Kafka.Consumer.Types
import Beckn.Streaming.Kafka.Topic.PublicTransportSearch
import Beckn.Utils.Dhall (FromDhall)

newtype KafkaConsumerCfgs = KafkaConsumerCfgs
  { publicTransportSearch :: KafkaConsumerCfg
  }
  deriving (Generic, FromDhall)

newtype KafkaConsumerEnv = KafkaConsumerEnv
  { publicTransportSearch :: KafkaConsumerTools PublicTransportSearch
  }
  deriving (Generic)

buildKafkaConsumerEnv :: KafkaConsumerCfgs -> IO KafkaConsumerEnv
buildKafkaConsumerEnv cfgs = do
  publicTransportSearch <- buildKafkaConsumerTools @PublicTransportSearch cfgs.publicTransportSearch
  return KafkaConsumerEnv {..}

releaseKafkaConsumerEnv :: KafkaConsumerEnv -> IO ()
releaseKafkaConsumerEnv KafkaConsumerEnv {..} = do
  releaseKafkaConsumerTools publicTransportSearch
