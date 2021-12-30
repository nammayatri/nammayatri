module Tools.Streaming.Kafka.Environment
  ( module Tools.Streaming.Kafka.Environment,
    module Reexport,
  )
where

import Beckn.Streaming.Kafka.Producer.Types as Reexport
import Beckn.Streaming.Kafka.Topic.BusinessEvent.Environment
import Beckn.Streaming.Kafka.Topic.BusinessEvent.Types as Reexport
import EulerHS.Prelude

newtype BPPKafkaEnvs = BPPKafkaEnvs
  { businessEventEnv :: KafkaBEEnv
  }
  deriving (Generic)

buildBPPKafkaEnvs :: IO BPPKafkaEnvs
buildBPPKafkaEnvs = do
  businessEventEnv <- buildKafkaBEEnv "BPP"
  return $ BPPKafkaEnvs {..}
