module Beckn.Utils.Monitoring.Kafka.Producer
  ( buildKafkaProducerTools,
    Beckn.Utils.Monitoring.Kafka.Producer.produceMessage,
    releaseKafkaProducerTools,
    (..=),
    A.Value (Object),
    A.emptyObject,
  )
where

import Beckn.Types.Error
import Beckn.Types.Logging (Log)
import Beckn.Types.Monitoring.Kafka.Producer
import Beckn.Types.Time (MonadTime)
import Beckn.Utils.Error.Throwing (throwError)
import Data.Aeson (encode)
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Lazy as HM
import EulerHS.Prelude
import Kafka.Producer as KafkaProd

producerProps :: KafkaBrokersList -> ProducerProperties
producerProps brokers =
  brokersList castBrokers
    <> logLevel KafkaLogDebug
  where
    castBrokers = BrokerAddress <$> brokers

buildKafkaProducerTools :: KafkaBrokersList -> IO KafkaProducerTools
buildKafkaProducerTools kafkaBrokersList = do
  -- when (null targetTopic) $ throwM KafkaTopicIsEmptyString
  producer <- newProducer (producerProps kafkaBrokersList) >>= either (\err -> throwM (KafkaUnableToBuildTools $ show err)) return
  return $
    KafkaProducerTools
      { ..
      }

produceMessage :: (MonadIO m, MonadThrow m, Log m, MonadTime m, MonadReader r m, HasKafkaProducer r, ToJSON a) => KafkaTopic -> Maybe KafkaKey -> a -> m ()
produceMessage topic key event = do
  kafkaProducerTools <- asks (.kafkaProducerTools)
  mbErr <- KafkaProd.produceMessage kafkaProducerTools.producer message
  whenJust mbErr $ \err -> throwError (KafkaUnableToProduceMessage $ show err)
  where
    message =
      ProducerRecord
        { prTopic = TopicName topic,
          prPartition = UnassignedPartition,
          prKey = key,
          prValue = Just . LBS.toStrict $ encode event
        }

releaseKafkaProducerTools :: KafkaProducerTools -> IO ()
releaseKafkaProducerTools kafkaProducerTools = closeProducer kafkaProducerTools.producer

(..=) :: ToJSON a => Text -> a -> HM.HashMap Text A.Value
(..=) = (A..=)
