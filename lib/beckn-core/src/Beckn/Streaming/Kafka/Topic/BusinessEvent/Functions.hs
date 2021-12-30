module Beckn.Streaming.Kafka.Topic.BusinessEvent.Functions where

import Beckn.Streaming.Kafka.Topic.BusinessEvent.Environment
import Beckn.Streaming.Kafka.Topic.BusinessEvent.Types
import Beckn.Streaming.MonadProducer (MonadProducer (..))
import Beckn.Types.Logging
import Beckn.Types.Time
import EulerHS.Prelude

produceBusinessEventMessage ::
  ( MonadProducer BusinessEvent m,
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
  m ()
produceBusinessEventMessage eventName metadata payload = do
  event <- buildBusinessEvent eventName metadata payload
  produceMessage (Just $ encodeUtf8 event.eventName) event
