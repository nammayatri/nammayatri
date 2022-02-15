module Tools.Streaming.Kafka.Environment
  ( module Tools.Streaming.Kafka.Environment,
    module Reexport,
  )
where

import Beckn.Streaming.Kafka.Producer.Types as Reexport
import Beckn.Streaming.Kafka.Topic.BusinessEvent.Environment
import Beckn.Streaming.Kafka.Topic.BusinessEvent.Types as Reexport
import EulerHS.Prelude

newtype BAPKafkaEnvs = BAPKafkaEnvs
  { businessEventEnv :: KafkaBEEnv
  }
  deriving (Generic)

buildBAPKafkaEnvs :: IO BAPKafkaEnvs
buildBAPKafkaEnvs = do
  businessEventEnv <- buildKafkaBEEnv "BAP"

  return $ BAPKafkaEnvs {..}
