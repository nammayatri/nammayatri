module Beckn.Utils.Monitoring.Kafka
  ( buildKafkaTools,
    Beckn.Utils.Monitoring.Kafka.produceMessage,
    releaseKafkaTools,
    (..=),
    A.Value (Object),
    A.emptyObject,
  )
where

import Beckn.Types.Error
import Beckn.Types.Logging (Log)
import Beckn.Types.Monitoring.Kafka
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

buildKafkaTools :: KafkaToolsConfig -> IO KafkaTools
buildKafkaTools KafkaToolsConfig {..}  = do
  when (null targetTopic) $ throwM KafkaTopicIsEmptyString
  producer <- newProducer (producerProps brokers) >>= either (\err -> throwM (KafkaUnableToBuildTools $ show err)) return
  return $
    KafkaTools
      { ..
      }

produceMessage :: (MonadIO m, MonadThrow m, Log m, MonadReader r m, HasKafka r, ToJSON a) => 
  Maybe KafkaKey -> a -> m ()
produceMessage key event = do
  kafkaTools <- asks (.kafkaTools)
  when (null kafkaTools.targetTopic) $ throwError KafkaTopicIsEmptyString
  mbErr <- KafkaProd.produceMessage kafkaTools.producer (message kafkaTools)
  whenJust mbErr $ \err -> throwError (KafkaUnableToProduceMessage $ show err)
  where
    message kafkaTools =
      ProducerRecord
        { prTopic = TopicName kafkaTools.targetTopic,
          prPartition = UnassignedPartition,
          prKey = key,
          prValue = Just . LBS.toStrict $ encode event
        }

releaseKafkaTools :: KafkaTools -> IO ()
releaseKafkaTools kafkaTools = closeProducer kafkaTools.producer

(..=) :: ToJSON a => Text -> a -> HM.HashMap Text A.Value
(..=) = (A..=)
