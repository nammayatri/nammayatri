module Beckn.Utils.Monitoring.Kafka
  ( buildKafkaTools,
    Beckn.Utils.Monitoring.Kafka.produceMessage,
    buildBusinessEvent,
    releaseKafkaTools,
  )
where

import Beckn.Types.Error
import Beckn.Types.Logging (Log)
import Beckn.Types.Monitoring.Kafka
import Beckn.Types.Time (MonadTime, getCurrentTime)
import Beckn.Utils.Error.Throwing (throwError)
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as LBS
import EulerHS.Prelude
import Kafka.Producer as KafkaProd

producerProps :: KafkaBrokersList -> ProducerProperties
producerProps brokers =
  brokersList castBrokers
    <> logLevel KafkaLogDebug
  where
    castBrokers = BrokerAddress <$> brokers

buildKafkaTools :: KafkaToolsConfig -> Maybe KafkaHostName -> IO KafkaTools
buildKafkaTools KafkaToolsConfig {..} mbHostName = do
  let hostName = fromMaybe "null" mbHostName
  producer <- newProducer (producerProps brokers) >>= either (\err -> throwM (KafkaUnableToBuildTools $ show err)) return
  return $
    KafkaTools
      { ..
      }

produceMessage ::
  (MonadIO m, MonadThrow m, Log m, MonadReader r m, HasKafka r) =>
  BusinessEvent ->
  m ()
produceMessage event = do
  kafkaTools <- asks (.kafkaTools)
  when (null kafkaTools.targetTopic) $ throwError KafkaTopicIsEmptyString
  mbErr <- KafkaProd.produceMessage kafkaTools.producer (message kafkaTools)
  whenJust mbErr $ \err -> throwError (KafkaUnableToProduceMessage $ show err)
  where
    message kafkaTools =
      ProducerRecord
        { prTopic = TopicName kafkaTools.targetTopic,
          prPartition = UnassignedPartition,
          prKey = Just $ encodeUtf8 event.eventName,
          prValue = Just . LBS.toStrict $ encode event
        }

releaseKafkaTools :: KafkaTools -> IO ()
releaseKafkaTools kafkaTools = closeProducer kafkaTools.producer

buildBusinessEvent ::
  (MonadIO m, MonadThrow m, Log m, MonadTime m, MonadReader r m, HasKafka r) =>
  KafkaEventName ->
  KafkaMetadata ->
  KafkaPayload ->
  m BusinessEvent
buildBusinessEvent eventName metadata payload = do
  kafkaTools <- asks (.kafkaTools)
  currTime <- getCurrentTime
  return $
    BusinessEvent
      { timestamp = currTime,
        eventName,
        hostName = kafkaTools.hostName,
        serviceName = kafkaTools.serviceName,
        metadata,
        payload
      }