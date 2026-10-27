-- | Shared between the DRIVER_POOL_STATE writer and the SESSIONIZER: the event
-- the driver app publishes, the Redis layout of the sessionizer's state store,
-- and the small per-driver state the sessionizer tags pings with.
module Processor.DriverPoolState.Types
  ( DriverPoolStateEvent (..),
    DriverStateSnapshot (..),
    snapshotFromFields,
    encodeMember,
    decodeMember,
    currentKey,
    historyKey,
    writerWatermarkKey,
    writerPartitionsField,
    writerPartitionField,
    writerWatermarkFromHash,
    stateBufferMs,
    awaitStateWriter,
    sessionizerWatermarkKey,
    stateHorizonKey,
    stateExpiry,
  )
where

import qualified Control.Concurrent as CC
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as AKM
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Database.Redis as R
import Kernel.Prelude

-- | One message on the driver-pool-state topic: only the fields that changed,
-- under their DriverPoolData names.
data DriverPoolStateEvent = DriverPoolStateEvent
  { driverId :: Text,
    ts :: Integer,
    changes :: A.Object
  }
  deriving (Generic, Show, FromJSON)

-- | The fields the sessionizer tags pings with, plus @state@: the current
-- hash's values for the driver_analytics_5m state columns. A new history
-- entry is written only when one of these changes.
data DriverStateSnapshot = DriverStateSnapshot
  { mode :: Maybe Text,
    active :: Bool,
    onRide :: Bool,
    enabled :: Bool,
    blocked :: Bool,
    subscribed :: Bool,
    tiers :: [Text],
    state :: A.Object
  }
  deriving (Generic, Show, ToJSON)

-- | lastUpdatedAt changes with every event, so it is ignored: otherwise each
-- event would write a history entry.
instance Eq DriverStateSnapshot where
  a == b =
    (a.mode, a.active, a.onRide, a.enabled, a.blocked, a.subscribed, a.tiers) == (b.mode, b.active, b.onRide, b.enabled, b.blocked, b.subscribed, b.tiers)
      && AKM.delete "lastUpdatedAt" a.state == AKM.delete "lastUpdatedAt" b.state

-- | Members written before @state@ existed decode with an empty one.
instance FromJSON DriverStateSnapshot where
  parseJSON = A.withObject "DriverStateSnapshot" $ \o ->
    DriverStateSnapshot
      <$> o A..:? "mode"
      <*> o A..: "active"
      <*> o A..: "onRide"
      <*> o A..: "enabled"
      <*> o A..: "blocked"
      <*> o A..: "subscribed"
      <*> o A..: "tiers"
      <*> o A..:? "state" A..!= AKM.empty

-- | Build the snapshot from the driver's current-state hash (field -> JSON value).
-- Missing fields fall back to the most conservative value. @stateKeys@: the
-- fields copied into @state@ (those present).
snapshotFromFields :: [Text] -> HM.HashMap Text A.Value -> DriverStateSnapshot
snapshotFromFields stateKeys m =
  DriverStateSnapshot
    { mode = textField "mode",
      active = boolField "active",
      onRide = boolField "onRide",
      enabled = boolField "enabled",
      blocked = boolField "blocked",
      subscribed = boolField "subscribed",
      tiers = case HM.lookup "selectedServiceTiers" m of
        Just (A.Array xs) -> [t | A.String t <- toList xs]
        _ -> [],
      state = AKM.fromList [(AK.fromText k, v) | k <- stateKeys, Just v <- [HM.lookup k m]]
    }
  where
    textField k = case HM.lookup k m of
      Just (A.String t) -> Just t
      _ -> Nothing
    boolField k = case HM.lookup k m of
      Just (A.Bool b) -> b
      _ -> False

-- | Hash: DriverPoolData field -> JSON value, the merged current state.
currentKey :: Text -> Text
currentKey did = "driver_state:current:" <> did

-- | Sorted set: score = event ts (epoch ms), member = "<ts>:<snapshot json>".
historyKey :: Text -> Text
historyKey did = "driver_state:history:" <> did

-- | Hash: how far the writer has applied events, per partition of the
-- driver-pool-state topic ('writerPartitionField' -> epoch ms), plus the
-- topic's partition count ('writerPartitionsField'). Each writer replica
-- writes only the partitions it owns; the watermark is the minimum over all
-- partitions, so a fast replica can't speak for a slow one.
writerWatermarkKey :: Text
writerWatermarkKey = "state_writer:partition_watermarks"

writerPartitionsField :: ByteString
writerPartitionsField = "partitions"

writerPartitionField :: Int -> ByteString
writerPartitionField p = "p" <> encodeUtf8 (show p :: Text)

-- | The writer watermark from the hash's fields: the minimum over every
-- partition, or Nothing until every partition has reported.
writerWatermarkFromHash :: [(ByteString, ByteString)] -> Maybe Integer
writerWatermarkFromHash fields = do
  let readInt :: Read a => ByteString -> Maybe a
      readInt = readMaybe . T.unpack . TE.decodeUtf8
  n :: Int <- lookup writerPartitionsField fields >>= readInt
  marks <- forM [0 .. n - 1] $ \p -> lookup (writerPartitionField p) fields >>= readInt
  if null marks then Nothing else Just (minimum marks)

-- | Events are looked up only once the writer is this far past them, so a
-- state change published late (producer batching, retries) or stamped by a
-- pod whose clock is behind still lands before the lookup.
stateBufferMs :: Integer
stateBufferMs = 30 * 1000

-- | Block until the writer has applied every partition up to @needed@ +
-- 'stateBufferMs' (epoch ms) and return its watermark. @needed@ is capped at
-- the time of the call, so an event stamped ahead of the clock waits only
-- the buffer. No timeout: tagging an event with a state the writer has not
-- applied yet would be silently wrong, while waiting only delays (Kafka holds
-- the backlog). @warn@ is called every 'stallWarnEveryMs' of waiting.
awaitStateWriter :: (Text -> IO ()) -> R.Connection -> Integer -> IO Integer
awaitStateWriter warn conn needed = do
  callMs <- round . (* 1000) <$> getPOSIXTime
  go (min callMs needed + stateBufferMs) 0
  where
    go :: Integer -> Integer -> IO Integer
    go target waitedMs = do
      reply <- (Right <$> R.runRedis conn (R.hgetall (TE.encodeUtf8 writerWatermarkKey))) `catchAny` (pure . Left . show)
      let mbWm = either (const Nothing) (either (const Nothing) writerWatermarkFromHash) reply
      case mbWm of
        Just wm | wm >= target -> pure wm
        _ -> do
          when (waitedMs > 0 && waitedMs `mod` stallWarnEveryMs == 0) $
            warn $ "waiting " <> show waitedMs <> " ms for the state writer to reach " <> show target <> "; it is at " <> maybe "no watermark (unreadable, or not every partition reported)" show mbWm <> either (\e -> " (read failed: " <> e <> ")") (const "") reply
          CC.threadDelay (fromIntegral writerPollMs * 1000)
          go target (waitedMs + writerPollMs)
    writerPollMs, stallWarnEveryMs :: Integer
    writerPollMs = 200
    stallWarnEveryMs = 30 * 1000

-- | Epoch ms up to which the sessionizer has committed pings (T in the plan).
sessionizerWatermarkKey :: Text
sessionizerWatermarkKey = "sessionizer:watermark"

-- | Epoch ms before which DRIVER_PERFORMANCE no longer looks up states,
-- published after each of its flushes (no TTL).
stateHorizonKey :: Text
stateHorizonKey = "perf:state_horizon"

-- | A driver with no state change for this long drops out; their next event
-- re-seeds them from the DB.
stateExpiry :: Int
stateExpiry = 30 * 24 * 60 * 60

-- | Member text "<ts>:<snapshot json>". The ts prefix keeps two identical
-- snapshots at different times as separate entries.
encodeMember :: Integer -> DriverStateSnapshot -> Text
encodeMember t s = show t <> ":" <> decodeUtf8 (LBS.toStrict (A.encode s))

decodeMember :: ByteString -> Maybe (Integer, DriverStateSnapshot)
decodeMember raw = do
  memberText :: Text <- A.decode (LBS.fromStrict raw)
  let (tsPart, rest) = T.breakOn ":" memberText
  t <- readMaybe (T.unpack tsPart)
  s <- A.decode (LBS.fromStrict (encodeUtf8 (T.drop 1 rest)))
  pure (t, s)
