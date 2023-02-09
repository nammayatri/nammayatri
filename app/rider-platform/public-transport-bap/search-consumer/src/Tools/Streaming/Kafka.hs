module Tools.Streaming.Kafka where

import Kernel.Prelude
import Kernel.Streaming.Kafka.Consumer.Types
import Kernel.Streaming.Kafka.Topic.PublicTransportSearch
import Kernel.Utils.Dhall (FromDhall)

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
