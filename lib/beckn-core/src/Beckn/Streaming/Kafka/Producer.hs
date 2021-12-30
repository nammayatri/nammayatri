module Beckn.Streaming.Kafka.Producer
  ( buildKafkaProducerTools,
    Beckn.Streaming.Kafka.Producer.produceMessage,
    releaseKafkaProducerTools,
    (..=),
    A.Value (Object),
    A.emptyObject,
  )
where

import Beckn.Streaming.Kafka.Producer.Types
import Beckn.Types.Error
import Beckn.Types.Logging
import Beckn.Utils.Error.Throwing (throwError)
import Data.Aeson (encode)
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Lazy as HM
import EulerHS.Prelude
import Kafka.Producer as KafkaProd

produceMessage :: (Log m, MonadThrow m, MonadIO m, MonadReader r m, HasKafkaProducer r, ToJSON a) => (KafkaTopic, Maybe KafkaKey) -> a -> m ()
produceMessage (topic, key) event = do
  kafkaProducerTools <- asks (.kafkaProducerTools)
  when (null topic) $ throwM KafkaTopicIsEmptyString
  mbErr <- KafkaProd.produceMessage kafkaProducerTools.producer message
  whenJust mbErr (throwError . KafkaUnableToProduceMessage)
  where
    message =
      ProducerRecord
        { prTopic = TopicName topic,
          prPartition = UnassignedPartition,
          prKey = key,
          prValue = Just . LBS.toStrict $ encode event
        }

(..=) :: ToJSON a => Text -> a -> HM.HashMap Text A.Value
(..=) = (A..=)
