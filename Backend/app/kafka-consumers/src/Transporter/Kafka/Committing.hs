-- | Kafka loop for consumers whose output must not lose or reorder messages
-- (DRIVER_POOL_STATE). Unlike 'Transporter.Kafka.Flow':
--
--   * messages are handled one at a time, in order, and a failing message is
--     retried until it succeeds, so nothing is skipped;
--   * offsets are committed explicitly, only after messages are handled;
--   * a watermark (epoch ms) is reported per owned partition, about once a
--     second: the current time when the partition is caught up (consumed up to
--     the broker's high watermark), else the last handled event time on it.
--     Readers take the minimum over every partition of the topic, so with
--     several replicas none can report for partitions it does not own.
module Transporter.Kafka.Committing
  ( runOrderedCommitting,
    isRdKafkaError,
  )
where

import qualified Control.Concurrent as CC
import qualified Control.Exception as E
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import Environment
import qualified EulerHS.Runtime as L
import qualified Kafka.Consumer as Consumer
import Kafka.Metadata (WatermarkOffsets (..), watermarkOffsets)
import Kafka.Types (KafkaError)
import Kernel.Prelude
import Kernel.Types.Flow
import Kernel.Utils.Common hiding (id)
import qualified Prelude as P

-- | Commit and report the watermark at most this often (ms).
progressEveryMs :: Integer
progressEveryMs = 1000

-- | Broker round trips for the watermark (high offsets, committed offsets).
brokerTimeout :: Consumer.Timeout
brokerTimeout = Consumer.Timeout 5000

runOrderedCommitting ::
  (FromJSON event) =>
  L.FlowRuntime ->
  AppEnv ->
  Consumer.KafkaConsumer ->
  -- | event time, epoch ms
  (event -> Integer) ->
  (event -> Flow ()) ->
  -- | report the watermark: the topic's partition count, and epoch ms per
  -- owned partition (a partition with no known mark is left out)
  (Int -> M.Map Int Integer -> Flow ()) ->
  IO ()
runOrderedCommitting flowRt appEnv kc eventTime handler reportWatermark = go M.empty 0
  where
    flow = runFlowR flowRt appEnv
    -- lastByPartition: the event time of the last message handled per partition.
    go lastByPartition lastProgress = do
      polled <- Consumer.pollMessage kc (Consumer.Timeout 500)
      lastByPartition' <- case polled of
        Left err | isRdKafkaError "RdKafkaRespErrTimedOut" err -> pure lastByPartition
        Left err -> do
          flow $ logError $ "kafka poll error: " <> show err
          pure lastByPartition
        Right cr -> case A.decode . LBS.fromStrict =<< Consumer.crValue cr of
          Nothing -> do
            flow $ logError $ "skipping undecodable message at offset " <> show (Consumer.crOffset cr)
            pure lastByPartition
          Just event -> do
            handleWithRetry event
            pure (M.insert (Consumer.unPartitionId (Consumer.crPartition cr)) (eventTime event) lastByPartition)
      nowMs <- round . (* 1000) <$> getPOSIXTime
      if nowMs - lastProgress >= progressEveryMs
        then progress nowMs lastByPartition' >> go lastByPartition' nowMs
        else go lastByPartition' lastProgress

    handleWithRetry event =
      flow (handler event) `E.catch` \(e :: E.SomeException) -> do
        flow $ logError $ "handler failed, retrying in 1s: " <> show e
        CC.threadDelay 1000000
        handleWithRetry event

    progress nowMs lastByPartition = do
      commitErr <- Consumer.commitAllOffsets Consumer.OffsetCommit kc
      case commitErr of
        Just err | isRdKafkaError "RdKafkaRespErrNoOffset" err -> pure () -- nothing consumed since the last commit
        Just err -> flow $ logError $ "offset commit failed: " <> show err
        Nothing -> pure ()
      marks <- partitionMarks nowMs lastByPartition `E.catch` \(e :: E.SomeException) -> pure (Left (show e))
      case marks of
        Left err -> flow $ logError $ "watermark not reported: " <> err
        Right Nothing -> pure () -- no partitions assigned
        Right (Just (partitionCount, byPartition)) ->
          flow (reportWatermark partitionCount byPartition) `E.catch` \(e :: E.SomeException) ->
            flow $ logError $ "watermark update failed: " <> show e

    -- @nowMs@ is taken before the high offsets are read, so a caught-up
    -- partition has handled every message that reached the broker by then.
    partitionMarks :: Integer -> M.Map Int Integer -> IO (Either Text (Maybe (Int, M.Map Int Integer)))
    partitionMarks nowMs lastByPartition =
      Consumer.assignment kc >>= \case
        Left err -> pure (Left (show err))
        Right assigned -> case M.toList (M.filter (not . null) assigned) of
          [] -> pure (Right Nothing)
          [(topic, parts)] -> do
            highsE <- sequence <$> watermarkOffsets kc brokerTimeout topic
            positionsE <- Consumer.position kc [(topic, p) | p <- parts]
            case (highsE, positionsE) of
              (Left err, _) -> pure (Left ("high offsets: " <> show err))
              (_, Left err) -> pure (Left ("positions: " <> show err))
              (Right highs, Right positions) -> do
                let consumed = offsetsByPartition positions
                    unconsumed = [(topic, p) | p <- parts, not (M.member (Consumer.unPartitionId p) consumed)]
                -- Nothing consumed yet on a partition since the assignment: fall back to its committed offset.
                committedE <- if null unconsumed then pure (Right []) else Consumer.committed kc brokerTimeout unconsumed
                pure $ case committedE of
                  Left err -> Left ("committed offsets: " <> show err)
                  Right committedTps ->
                    let consumedTo = M.union consumed (offsetsByPartition committedTps)
                        highBy = M.fromList [(Consumer.unPartitionId (woPartitionId wo), Consumer.unOffset (woHighWatermark wo)) | wo <- highs]
                        -- No position and no committed offset: a new group starts at
                        -- the latest offset (librdkafka's default), so it is caught up.
                        caughtUp p = maybe True (>= M.findWithDefault 0 p highBy) (M.lookup p consumedTo)
                        mark p = if caughtUp p then Just nowMs else M.lookup p lastByPartition
                     in Right (Just (length highs, M.fromList [(p, m) | Consumer.PartitionId p <- parts, Just m <- [mark p]]))
          _ -> pure (Left "runOrderedCommitting expects a single topic")

    offsetsByPartition tps = M.fromList [(Consumer.unPartitionId (Consumer.tpPartition tp), o) | tp <- tps, Consumer.PartitionOffset o <- [Consumer.tpOffset tp]]

-- | The librdkafka error codes live in hw-kafka-client's hidden
-- Kafka.Internal.RdKafka module, so match on the code's shown name instead:
-- 'pollMessage' returns @KafkaResponseError RdKafkaRespErrTimedOut@ when nothing
-- arrived within the timeout.
isRdKafkaError :: Text -> KafkaError -> Bool
isRdKafkaError code err = code `T.isInfixOf` T.pack (P.show err)
