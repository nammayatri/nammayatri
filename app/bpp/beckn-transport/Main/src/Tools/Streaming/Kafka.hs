module Tools.Streaming.Kafka
  ( module Tools.Streaming.Kafka,
    module Reexport,
  )
where

import EulerHS.Prelude
import Kernel.Streaming.Kafka.Producer.Types as Reexport
import Kernel.Streaming.Kafka.Topic.BusinessEvent.Environment
import Kernel.Streaming.Kafka.Topic.BusinessEvent.Types as Reexport

newtype BPPKafkaEnvs = BPPKafkaEnvs
  { businessEventEnv :: KafkaBEEnv
  }
  deriving (Generic)

buildBPPKafkaEnvs :: IO BPPKafkaEnvs
buildBPPKafkaEnvs = do
  businessEventEnv <- buildKafkaBEEnv "BPP"
  return $ BPPKafkaEnvs {..}
