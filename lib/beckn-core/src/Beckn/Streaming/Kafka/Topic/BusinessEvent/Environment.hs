module Beckn.Streaming.Kafka.Topic.BusinessEvent.Environment where

import Beckn.Streaming.Kafka.Producer.Types
import Beckn.Utils.App (getPodName)
import EulerHS.Prelude
import GHC.Records.Extra (HasField)

type HasKafkaBE r kafkaEnvs = (HasField "kafkaEnvs" r kafkaEnvs, HasField "businessEventEnv" kafkaEnvs KafkaBEEnv)

data KafkaBEEnv = KafkaBEEnv
  { hostName :: KafkaHostName,
    serviceName :: KafkaServiceName
  }
  deriving (Generic)

buildKafkaBEEnv :: KafkaServiceName -> IO KafkaBEEnv
buildKafkaBEEnv serviceName = do
  hostName <- getPodName
  return $
    KafkaBEEnv
      { ..
      }