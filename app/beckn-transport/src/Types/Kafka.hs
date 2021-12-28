module Types.Kafka
  ( module Types.Kafka,
    module Reexport,
  )
where

import Beckn.Types.Monitoring.Kafka.Producer as Reexport
import Beckn.Types.Monitoring.Kafka.Topic.BusinessEvent as Reexport
import Beckn.Utils.Dhall (FromDhall)
import EulerHS.Prelude
import GHC.Records.Extra (HasField)

newtype BPPKafkaEnvConfigs = BPPKafkaEnvConfigs
  { businessEventCfg :: KafkaBECfg
  }
  deriving (Generic, FromDhall)

newtype BPPKafkaEnvs = BPPKafkaEnvs
  { businessEventEnv :: KafkaBEEnv
  }
  deriving (Generic)

buildBPPKafkaEnvs :: BPPKafkaEnvConfigs -> IO BPPKafkaEnvs
buildBPPKafkaEnvs BPPKafkaEnvConfigs {..} = do
  businessEventEnv <- buildKafkaBEEnv businessEventCfg
  return $ BPPKafkaEnvs {..}
