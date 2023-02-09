module Tools.Streaming.Kafka
  ( module Tools.Streaming.Kafka,
    module Reexport,
  )
where

import EulerHS.Prelude
import Kernel.Streaming.Kafka.Producer.Types as Reexport
import Kernel.Streaming.Kafka.Topic.BusinessEvent.Environment
import Kernel.Streaming.Kafka.Topic.BusinessEvent.Types as Reexport

newtype BAPKafkaEnvs = BAPKafkaEnvs
  { businessEventEnv :: KafkaBEEnv
  }
  deriving (Generic)

buildBAPKafkaEnvs :: IO BAPKafkaEnvs
buildBAPKafkaEnvs = do
  businessEventEnv <- buildKafkaBEEnv "BAP"
  return $ BAPKafkaEnvs {..}
