{-# LANGUAGE BangPatterns #-}

-- | Kafka transport. Two entry points:
--
--   * runPerEvent — fire one Flow action per decoded message. Used by consumers
--     whose processor is shaped @event -> Flow ()@.
--
--   * runBatch — accumulate up to @kafkaReadBatchSize@ messages, dedupe by key,
--     fire one Flow action per batch. Used by consumers that push aggregates
--     downstream (e.g. LOCATION_UPDATE pipes a batch into Redis).
module Transporter.Kafka.Flow
  ( runPerEvent,
    runBatch,
    getConfigNameFromConsumertype,
    newKafkaConsumer,
  )
where

import Control.Error.Util
import qualified Data.Aeson as A
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Function
import qualified Data.List as DL
import Environment
import qualified EulerHS.Runtime as L
import qualified Kafka.Consumer as Consumer
import Kernel.Prelude
import Kernel.Types.Flow
import Kernel.Utils.Common hiding (id)
import qualified Streamly.Internal.Data.Fold as SF
import qualified Streamly.Internal.Data.Stream.IsStream as S
import Streamly.Internal.Data.Stream.Serial (SerialT)

------------------------------------------------------------
-- Per-event consumer
------------------------------------------------------------

-- | Drain messages one at a time. The handler receives the decoded value and
-- the message key as Text (drivers/persons/ride ids etc).
runPerEvent ::
  (FromJSON event) =>
  L.FlowRuntime ->
  AppEnv ->
  Consumer.KafkaConsumer ->
  (event -> Text -> Flow ()) ->
  IO ()
runPerEvent flowRt appEnv kafkaConsumer handler =
  readMessages kafkaConsumer
    & S.mapM (\(value, key, _cr) -> runFlowR flowRt appEnv $ withTaggedFlow key $ handler value key)
    & S.drain

------------------------------------------------------------
-- Batch consumer
------------------------------------------------------------

-- | Accumulate @kafkaReadBatchSize@ messages, dedupe by message key, hand the
-- list to the processor. Whole batch succeeds or fails together.
runBatch ::
  (FromJSON event) =>
  L.FlowRuntime ->
  AppEnv ->
  Consumer.KafkaConsumer ->
  Int -> -- batch size override; falls back to appEnv.kafkaReadBatchSize when 0
  ([(event, Text)] -> Flow ()) ->
  IO ()
runBatch flowRt appEnv kafkaConsumer batchSize handler = do
  let effectiveBatch = if batchSize > 0 then batchSize else appEnv.kafkaReadBatchSize
  readMessages kafkaConsumer
    & S.chunksOf effectiveBatch dedupByKey
    & S.mapM (runFlowR flowRt appEnv . withTaggedFlow "kafka-batch" . handler)
    & S.drain
  where
    dedupByKey = SF.mkFold step start extract
      where
        step !acc (!val, !key, _) = SF.Partial ((val, key) : acc)
        start = SF.Partial []
        extract = reverse . DL.nubBy ((==) `on` snd)

------------------------------------------------------------
-- Kafka helpers
------------------------------------------------------------

withTaggedFlow :: Text -> Flow a -> Flow a
withTaggedFlow tag action =
  withLogTag tag $ generateGUID >>= flip withLogTag action

readMessages ::
  (FromJSON message, ConvertUtf8 messageKey ByteString) =>
  Consumer.KafkaConsumer ->
  SerialT IO (message, messageKey, ConsumerRecordD)
readMessages kafkaConsumer = do
  let eitherRecords = S.bracket (pure kafkaConsumer) Consumer.closeConsumer pollMessageR
  let records = S.mapMaybe hush eitherRecords
  S.mapMaybe (removeMaybeFromTuple . decodeRecord) records
  where
    pollMessageR kc = S.repeatM (Consumer.pollMessage kc (Consumer.Timeout 500))
    decodeRecord =
      (A.decode . LBS.fromStrict <=< Consumer.crValue)
        &&& (pure . decodeUtf8 <=< Consumer.crKey)
        &&& id
    removeMaybeFromTuple (mbMessage, (mbMessageKey, cr)) =
      (\message messageKey -> (message, messageKey, cr)) <$> mbMessage <*> mbMessageKey

newKafkaConsumer :: AppEnv -> IO Consumer.KafkaConsumer
newKafkaConsumer appEnv =
  either (error . ("Unable to open a kafka consumer: " <>) . show) id
    <$> Consumer.newConsumer
      (appEnv.kafkaConsumerCfg.consumerProperties)
      (Consumer.topics appEnv.kafkaConsumerCfg.topicNames)

------------------------------------------------------------
-- Consumer-type → Dhall config name
------------------------------------------------------------

getConfigNameFromConsumertype :: ConsumerType -> IO String
getConfigNameFromConsumertype = \case
  BROADCAST_MESSAGE -> pure "broadcast-message"
  LOCATION_UPDATE -> pure "location-update"
  FLEET_COMMUNICATION_DISPATCH -> pure "fleet-communication-dispatch"
  RIDE_EVENTS_CONSUMER -> pure "ride-events-consumer"
  DOCUMENT_AUDIT_CONSUMER -> pure "document-audit-consumer"
