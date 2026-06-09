{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NumericUnderscores #-}

-- | Streamly-based Redis-Stream consumer.
--
-- Each shard is a single Streamly pipeline that interleaves two sources:
--
--   * XREADGROUP > poll loop — blocking reads of new entries, with built-in
--     backpressure (the next read only fires once downstream catches up).
--   * XPENDING + XCLAIM tick loop — periodic recovery of entries idle longer
--     than @claimMinIdleMs@.
--
-- 'S.parallel' merges both sources concurrently into one chunk stream. Each
-- chunk (list of decoded records) is handed to the configured 'Deliver'
-- callback which decides what to XACK (per-event independent acks vs
-- all-or-nothing batch ack).
--
-- Retry budget is read directly from XPENDING's @numTimesDelivered@ — no
-- in-process counter, no DLQ. When delivery count exceeds @maxDeliveries@ the
-- entry is XACKed with an error log and an operator must redrive manually.
module Transporter.RedisStream.Flow
  ( run,
    runBatch,
    ensureConsumerGroups,
  )
where

import Control.Concurrent.Async (mapConcurrently_)
import qualified Data.Aeson as A
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.List (partition)
import Environment
import qualified EulerHS.Runtime as R
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import qualified Kernel.Tools.Metrics.CoreMetrics as CoreMetrics
import Kernel.Types.Common (Seconds (..))
import Kernel.Types.Flow (runFlowR)
import Kernel.Utils.Common (logError, logInfo, logWarning, withLogTag, withTryCatch)
import qualified Streamly.Internal.Data.Stream.IsStream as S
import Transporter.RedisStream.Types

------------------------------------------------------------
-- Public entry points
------------------------------------------------------------

-- | Per-event: handler fires once per decoded entry, entries XACKed
-- independently as they succeed.
run ::
  FromJSON event =>
  R.FlowRuntime ->
  AppEnv ->
  RedisStreamCfg ->
  Text -> -- consumer instance name (typically POD_NAME)
  (event -> Flow ()) ->
  IO ()
run flowRt appEnv cfg instanceName processor =
  runInternal flowRt appEnv cfg instanceName (perEventDeliver processor)

-- | Per-batch: handler receives all decoded events in one chunk; XACK happens
-- only if the whole batch succeeds. Malformed entries (JSON decode failure)
-- are XACKed immediately and dropped — they can never succeed.
runBatch ::
  FromJSON event =>
  R.FlowRuntime ->
  AppEnv ->
  RedisStreamCfg ->
  Text ->
  ([event] -> Flow ()) ->
  IO ()
runBatch flowRt appEnv cfg instanceName batchProcessor =
  runInternal flowRt appEnv cfg instanceName (perBatchDeliver batchProcessor)

------------------------------------------------------------
-- Internal scaffolding
------------------------------------------------------------

-- | Given a chunk of records pulled from the stream (either freshly read or
-- claimed from the PEL), return the entry IDs that should be XACKed. Records
-- whose IDs aren't returned stay in the PEL for the next claim cycle.
type Deliver = [Hedis.StreamsRecord] -> Flow [BS.ByteString]

runInternal :: R.FlowRuntime -> AppEnv -> RedisStreamCfg -> Text -> Deliver -> IO ()
runInternal flowRt appEnv cfg instanceName deliver = do
  let shards = mkShards cfg
  runFlowR flowRt appEnv $ do
    ensureConsumerGroups cfg shards
    logInfo $
      "redis-stream consumer starting: group="
        <> cfg.consumerGroupName
        <> " instance="
        <> instanceName
        <> " shards="
        <> show cfg.shardCount
  mapConcurrently_ (runShard flowRt appEnv cfg instanceName deliver) shards

mkShards :: RedisStreamCfg -> [(Int, Text)]
mkShards cfg = [(i, cfg.streamPrefix <> show i) | i <- [0 .. cfg.shardCount - 1]]

-- | Idempotent group creation. xGroupCreate logs and swallows BUSYGROUP,
-- so calling on every startup is safe.
ensureConsumerGroups :: RedisStreamCfg -> [(Int, Text)] -> Flow ()
ensureConsumerGroups cfg shards =
  forM_ shards $ \(_, streamName) ->
    Hedis.xGroupCreate streamName cfg.consumerGroupName "$"

-- | One shard = one Streamly pipeline. Reads and claims run concurrently via
-- 'S.parallel'; their output is merged into a single chunk stream that gets
-- delivered + XACKed.
runShard ::
  R.FlowRuntime ->
  AppEnv ->
  RedisStreamCfg ->
  Text ->
  Deliver ->
  (Int, Text) ->
  IO ()
runShard flowRt appEnv cfg instanceName deliver (shardId, streamName) =
  S.parallel (readChunkStream tagRead) (claimChunkStream tagClaim)
    & S.mapM (deliverAndAck flowRt appEnv cfg streamName deliver)
    & S.drain
  where
    tagRead = "rs-read-shard-" <> show shardId
    tagClaim = "rs-claim-shard-" <> show shardId
    readChunkStream tag =
      S.repeatM $
        runFlowR flowRt appEnv $
          withLogTag tag $ readNewEntries cfg instanceName streamName
    -- 'S.delay' spaces consecutive emissions by claimIntervalSeconds.
    claimChunkStream tag =
      S.delay (fromIntegral (getSeconds cfg.claimIntervalSeconds)) $
        S.repeatM $
          runFlowR flowRt appEnv $
            withLogTag tag $ claimEligibleEntries cfg shardId instanceName streamName

-- | Drain one chunk through 'deliver' and XACK whatever it returns.
deliverAndAck ::
  R.FlowRuntime ->
  AppEnv ->
  RedisStreamCfg ->
  Text ->
  Deliver ->
  [Hedis.StreamsRecord] ->
  IO ()
deliverAndAck flowRt appEnv cfg streamName deliver records
  | null records = pure ()
  | otherwise = runFlowR flowRt appEnv $ do
    ackIds <- deliver records
    unless (null ackIds) $
      void $ Hedis.xAck streamName cfg.consumerGroupName ackIds

------------------------------------------------------------
-- Sources
------------------------------------------------------------

-- | XREADGROUP > one round. When paused, sleeps and returns [].
readNewEntries :: RedisStreamCfg -> Text -> Text -> Flow [Hedis.StreamsRecord]
readNewEntries cfg instanceName streamName = do
  paused <- checkPaused cfg
  if paused
    then do
      liftIO $ threadDelay (getSeconds cfg.pauseSleepSeconds * 1_000_000)
      pure []
    else do
      mbResp <-
        Hedis.xReadGroupOpts
          cfg.consumerGroupName
          instanceName
          [(streamName, ">")]
          (Just (fromIntegral cfg.readBlockMilliseconds))
          (Just (fromIntegral cfg.readBatchSize))
      pure $ maybe [] (concatMap (.records)) mbResp

-- | Scan XPENDING for entries idle longer than @claimMinIdleMs@. Split into
-- "budget exhausted" (XACK and log, no processing) and "still in budget"
-- (XCLAIM and feed through downstream).
claimEligibleEntries :: RedisStreamCfg -> Int -> Text -> Text -> Flow [Hedis.StreamsRecord]
claimEligibleEntries cfg shardId instanceName streamName = do
  paused <- checkPaused cfg
  if paused
    then pure []
    else do
      -- Sample the stream-depth gauges once per claim tick (reusing the XPENDING
      -- summary we need anyway, plus a cheap XLEN).
      streamLen <- Hedis.xLen streamName
      mbSummary <- Hedis.xPendingSummary streamName cfg.consumerGroupName
      CoreMetrics.setRedisStreamLength shardId streamLen
      CoreMetrics.setRedisStreamPending shardId (maybe 0 (\Hedis.XPendingSummaryResponse {numPendingMessages} -> numPendingMessages) mbSummary)
      case mbSummary of
        Just (Hedis.XPendingSummaryResponse {numPendingMessages})
          | numPendingMessages > 0 -> do
            details <-
              Hedis.xPendingDetail
                streamName
                cfg.consumerGroupName
                "-"
                "+"
                (fromIntegral cfg.readBatchSize)
                Nothing
            let stuck = filter (isStuck cfg) details
                (exhausted, retryable) = partition (isBudgetExhausted cfg) stuck
            forM_ exhausted (exhaustEntry cfg streamName)
            if null retryable
              then pure []
              else
                Hedis.xClaim
                  streamName
                  cfg.consumerGroupName
                  instanceName
                  (fromIntegral cfg.claimMinIdleMs)
                  Hedis.defaultXClaimOpts
                  (map (\Hedis.XPendingDetailRecord {messageId} -> messageId) retryable)
        _ -> pure []

------------------------------------------------------------
-- Pause / exhaust helpers
------------------------------------------------------------

-- | Killswitch. Any value present in @pauseFlagKey@ means "pause".
checkPaused :: RedisStreamCfg -> Flow Bool
checkPaused cfg = isJust <$> Hedis.tryGetFromCluster cfg.pauseFlagKey

isStuck :: RedisStreamCfg -> Hedis.XPendingDetailRecord -> Bool
isStuck cfg Hedis.XPendingDetailRecord {millisSinceLastDelivered} =
  millisSinceLastDelivered > fromIntegral cfg.claimMinIdleMs

isBudgetExhausted :: RedisStreamCfg -> Hedis.XPendingDetailRecord -> Bool
isBudgetExhausted cfg Hedis.XPendingDetailRecord {numTimesDelivered} =
  numTimesDelivered >= fromIntegral cfg.maxDeliveries

exhaustEntry :: RedisStreamCfg -> Text -> Hedis.XPendingDetailRecord -> Flow ()
exhaustEntry cfg streamName Hedis.XPendingDetailRecord {messageId, numTimesDelivered} = do
  let entryId = decodeUtf8 messageId
  withLogTag ("entry-" <> entryId) $ do
    logError $
      "delivery budget exhausted (numTimesDelivered="
        <> show numTimesDelivered
        <> "); XACKing without processing"
    void $ Hedis.xAck streamName cfg.consumerGroupName [messageId]

------------------------------------------------------------
-- Delivery strategies
------------------------------------------------------------

perEventDeliver :: FromJSON event => (event -> Flow ()) -> Deliver
perEventDeliver processor records =
  fmap catMaybes . forM records $ \rec -> do
    let entryId = decodeUtf8 rec.recordId
    withLogTag ("entry-" <> entryId) $
      case decodePayload rec.keyValues of
        Nothing -> do
          logError "decode failed; XACKing (malformed entry, cannot retry)"
          pure (Just rec.recordId)
        Just event -> do
          result <- withTryCatch ("rs:" <> entryId) (processor event)
          case result of
            Right () -> do
              CoreMetrics.incrementRedisStreamProcessed
              pure (Just rec.recordId)
            Left e -> do
              logWarning $ "process failed; entry stays pending; err=" <> show e
              pure Nothing

perBatchDeliver :: FromJSON event => ([event] -> Flow ()) -> Deliver
perBatchDeliver batchProcessor records = do
  let decoded = mapMaybe (\rec -> (rec.recordId,) <$> decodePayload rec.keyValues) records
      validIds = map fst decoded
      events = map snd decoded
      malformedIds =
        [ rec.recordId
          | rec <- records,
            isNothing (decodePayloadDummy rec.keyValues)
        ]
  forM_ malformedIds $ \rid ->
    withLogTag ("entry-" <> decodeUtf8 rid) $
      logError "decode failed; XACKing (malformed entry, cannot retry)"
  if null events
    then pure malformedIds
    else do
      result <- withTryCatch "rs-batch" (batchProcessor events)
      case result of
        Right () -> do
          replicateM_ (length validIds) CoreMetrics.incrementRedisStreamProcessed
          pure (malformedIds <> validIds)
        Left e -> do
          logWarning $ "batch process failed; valid entries stay pending; err=" <> show e
          pure malformedIds
  where
    -- Only used to detect malformed entries; we don't care about the typed result.
    decodePayloadDummy :: [(ByteString, ByteString)] -> Maybe A.Value
    decodePayloadDummy = decodePayload

------------------------------------------------------------
-- Payload decode
------------------------------------------------------------

-- | Publisher convention: each stream entry has a single field "payload"
-- containing the JSON-encoded event body.
decodePayload :: FromJSON a => [(ByteString, ByteString)] -> Maybe a
decodePayload kvs = lookup "payload" kvs >>= A.decodeStrict
