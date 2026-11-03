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
    seededField,
    historyKey,
    writerWatermarkKey,
    writerPartitionsField,
    writerPartitionField,
    writerWatermarkFromHash,
    stateBufferMs,
    readWriterWatermark,
    sessionizerWatermarkKey,
    flushedOffsetKey,
    stateExpiry,
  )
where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as AKM
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
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

-- | The current hash's values for the driver_analytics_5m state columns. A new
-- history entry is written only when one of them changes.
newtype DriverStateSnapshot = DriverStateSnapshot {state :: A.Object}
  deriving (Generic, Show, ToJSON, FromJSON)

-- | lastUpdatedAt changes with every event, so it is ignored: otherwise each
-- event would write a history entry.
instance Eq DriverStateSnapshot where
  a == b = AKM.delete "lastUpdatedAt" a.state == AKM.delete "lastUpdatedAt" b.state

-- | Build the snapshot from the driver's current-state hash (field -> JSON value).
-- @stateKeys@: the fields copied into @state@ (those present).
snapshotFromFields :: [Text] -> HM.HashMap Text A.Value -> DriverStateSnapshot
snapshotFromFields stateKeys m =
  DriverStateSnapshot {state = AKM.fromList [(AK.fromText k, v) | k <- stateKeys, Just v <- [HM.lookup k m]]}

-- | Hash: DriverPoolData field -> JSON value, the merged current state.
currentKey :: Text -> Text
currentKey did = "driver_state:current:" <> did

-- | Marker in the current-state hash: set once the driver has been seeded from the DB.
seededField :: Text
seededField = "_seededFromDb"

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

-- | The writer's watermark: Nothing until every partition has reported.
readWriterWatermark :: R.Connection -> IO (Either Text (Maybe Integer))
readWriterWatermark conn = do
  reply <- (Right <$> R.runRedis conn (R.hgetall (TE.encodeUtf8 writerWatermarkKey))) `catchAny` (pure . Left . show)
  pure (either Left (either (Left . show) (Right . writerWatermarkFromHash)) reply)

-- | Epoch ms of the oldest ping the sessionizer has not flushed yet.
sessionizerWatermarkKey :: Text
sessionizerWatermarkKey = "sessionizer:watermark"

-- | Last offset of this partition whose rows the sessionizer has delivered to
-- driver-analytics-5m (no TTL): records up to it are skipped on a replay.
flushedOffsetKey :: Text -> Int -> Text
flushedOffsetKey topic partition = "sessionizer:flushed:" <> topic <> ":" <> show partition

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
