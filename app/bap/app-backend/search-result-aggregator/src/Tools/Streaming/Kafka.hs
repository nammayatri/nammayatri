module Tools.Streaming.Kafka where

import Beckn.Prelude
import Beckn.Streaming.Kafka.Consumer.Types
import Beckn.Streaming.Kafka.Topic.PublicTransportQuoteList
import Beckn.Utils.Dhall (FromDhall)

newtype KafkaConsumerCfgs = KafkaConsumerCfgs
  { publicTransportQuotes :: KafkaConsumerCfg
  }
  deriving (Generic, FromDhall)

newtype KafkaConsumerEnv = KafkaConsumerEnv
  { publicTransportQuotes :: KafkaConsumerTools PublicTransportQuoteList
  }
  deriving (Generic)

buildKafkaConsumerEnv :: KafkaConsumerCfgs -> IO KafkaConsumerEnv
buildKafkaConsumerEnv cfgs = do
  publicTransportQuotes <- buildKafkaConsumerTools @PublicTransportQuoteList cfgs.publicTransportQuotes
  return KafkaConsumerEnv {..}

releaseKafkaConsumerEnv :: KafkaConsumerEnv -> IO ()
releaseKafkaConsumerEnv KafkaConsumerEnv {..} = do
  releaseKafkaConsumerTools publicTransportQuotes
