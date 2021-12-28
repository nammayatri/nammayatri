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

newtype BAPKafkaEnvConfigs = BAPKafkaEnvConfigs
  { businessEventCfg :: KafkaBECfg
  }
  deriving (Generic, FromDhall)

newtype BAPKafkaEnvs = BAPKafkaEnvs
  { businessEventEnv :: KafkaBEEnv
  }
  deriving (Generic)

buildBAPKafkaEnvs :: BAPKafkaEnvConfigs -> IO BAPKafkaEnvs
buildBAPKafkaEnvs BAPKafkaEnvConfigs {..} = do
  businessEventEnv <- buildKafkaBEEnv businessEventCfg
  return $ BAPKafkaEnvs {..}
