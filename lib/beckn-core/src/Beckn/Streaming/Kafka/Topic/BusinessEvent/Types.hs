module Beckn.Streaming.Kafka.Topic.BusinessEvent.Types where

import Beckn.Streaming.Kafka.HasKafkaTopics (HasKafkaTopics (..))
import qualified Beckn.Streaming.Kafka.Producer as Prod
import Beckn.Streaming.Kafka.Producer.Types
import Beckn.Streaming.Kafka.Topic.BusinessEvent.Environment
import Beckn.Streaming.MonadProducer (MonadProducer (..))
import Beckn.Types.Flow (FlowR)
import Beckn.Types.Logging
import Beckn.Types.Time
import Data.Aeson (Value)
import Data.Time (UTCTime)
import EulerHS.Prelude
import GHC.Records.Extra (HasField)

type KafkaBEName = Text

type KafkaBEMetadata = Value

type KafkaBEPayload = Value

data BusinessEvent = BusinessEvent
  { timestamp :: UTCTime,
    eventName :: KafkaBEName,
    hostName :: KafkaHostName,
    serviceName :: KafkaServiceName, -- mobility BAP | BPP
    metadata :: KafkaBEMetadata,
    payload :: KafkaBEPayload
  }
  deriving (Generic, Show, FromJSON, ToJSON)

buildBusinessEvent :: (Log m, MonadTime m, MonadReader r m, HasKafkaBE r kafkaEnvs, ToJSON a, ToJSON b) => KafkaBEName -> a -> b -> m BusinessEvent
buildBusinessEvent eventName metadata payload = do
  kafkaBEEnv <- asks (.kafkaEnvs.businessEventEnv)
  currTime <- getCurrentTime
  return $
    BusinessEvent
      { timestamp = currTime,
        eventName,
        hostName = kafkaBEEnv.hostName,
        serviceName = kafkaBEEnv.serviceName,
        metadata = toJSON metadata,
        payload = toJSON payload
      }

instance HasKafkaTopics BusinessEvent where
  getTopics = ["beckn_business_events"]

instance (Log (FlowR r), HasKafkaProducer r, HasKafkaBE r kafkaEnvs) => MonadProducer BusinessEvent (FlowR r) where
  type Args BusinessEvent = (Maybe KafkaKey)
  produceMessage key value = mapM_ ($ value) (Prod.produceMessage <$> map (,key) (getTopics @BusinessEvent))