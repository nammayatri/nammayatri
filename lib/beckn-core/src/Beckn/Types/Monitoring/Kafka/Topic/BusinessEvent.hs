module Beckn.Types.Monitoring.Kafka.Topic.BusinessEvent where

import Beckn.Types.Monitoring.Kafka.Producer
import Beckn.Utils.App (getPodName)
import Beckn.Utils.Dhall (FromDhall)
import Data.Aeson (Value)
import Data.Time (UTCTime)
import EulerHS.Prelude
import GHC.Records.Extra (HasField)

type KafkaBEName = Text

type KafkaBEMetadata = Value

type KafkaBEPayload = Value

type HasKafkaBE r kafkaEnvs = (HasField "kafkaEnvs" r kafkaEnvs, HasField "businessEventEnv" kafkaEnvs KafkaBEEnv)

data KafkaBECfg = KafkaBECfg
  { serviceName :: KafkaServiceName,
    targetTopic :: KafkaTopic
  }
  deriving (Generic, FromDhall)

data KafkaBEEnv = KafkaBEEnv
  { hostName :: KafkaHostName,
    serviceName :: KafkaServiceName,
    targetTopic :: KafkaTopic
  }
  deriving (Generic)

data BusinessEvent = BusinessEvent
  { timestamp :: UTCTime,
    eventName :: KafkaBEName,
    hostName :: KafkaHostName,
    serviceName :: KafkaServiceName, -- mobility BAP | BPP
    metadata :: KafkaBEMetadata,
    payload :: KafkaBEPayload
  }
  deriving (Generic, Show, FromJSON, ToJSON)

buildKafkaBEEnv :: KafkaBECfg -> IO KafkaBEEnv
buildKafkaBEEnv KafkaBECfg {..} = do
  hostName <- getPodName
  return $ KafkaBEEnv {..}
