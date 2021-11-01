module Beckn.Utils.Monitoring.Kafka.BusinessEvent
  ( buildBusinessEvent,
    produceBusinessEventMessage,
    (..=),
    A.Value (Object),
    A.emptyObject,
  )
where

import Beckn.Types.Logging (Log)
import Beckn.Types.Monitoring.Kafka
import Beckn.Types.Monitoring.Kafka.BusinessEvent
import Beckn.Types.Time (MonadTime, getCurrentTime)
import Beckn.Utils.Monitoring.Kafka ((..=))
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import EulerHS.Prelude

buildBusinessEvent ::
  ( MonadIO m,
    MonadThrow m,
    Log m,
    MonadTime m,
    MonadReader r m,
    HasKafkaBE r kafkaEnvs,
    ToJSON a,
    ToJSON b
  ) =>
  KafkaBEName ->
  a ->
  b ->
  m BusinessEvent
buildBusinessEvent eventName metadata payload = do
  kafkaBEEnv <- asks (.kafkaEnvs.kafkaBEEnv)
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

produceBusinessEventMessage ::
  ( MonadIO m,
    MonadThrow m,
    Log m,
    MonadTime m,
    MonadReader r m,
    HasKafkaBE r kafkaEnvs,
    Kafka m,
    ToJSON a,
    ToJSON b
  ) =>
  KafkaBEName ->
  a ->
  b ->
  m ()
produceBusinessEventMessage eventName eventMeta eventPayload =
  produceMessage (Just $ encodeUtf8 eventName) =<< buildBusinessEvent eventName eventMeta eventPayload