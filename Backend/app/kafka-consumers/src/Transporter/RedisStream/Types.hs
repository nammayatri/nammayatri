-- | Generic config for the Redis-Stream transport. Decoupled from any specific
-- event type so the same transport can serve multiple processors.
module Transporter.RedisStream.Types
  ( RedisStreamCfg (..),
  )
where

import Kernel.Prelude
import Kernel.Types.Common (Seconds)
import Kernel.Utils.Dhall (FromDhall)

data RedisStreamCfg = RedisStreamCfg
  { -- | Stream key prefix; per-shard key is "<prefix><shardId>".
    streamPrefix :: Text,
    -- | Fixed at the time the producer starts writing; cannot change without
    -- stranding in-flight entries on old shards.
    shardCount :: Int,
    consumerGroupName :: Text,
    -- | Max entries returned per XREADGROUP call.
    readBatchSize :: Int,
    -- | XREADGROUP BLOCK timeout in milliseconds.
    readBlockMilliseconds :: Int,
    -- | Entries idle (no XACK) longer than this are eligible to be re-claimed
    -- by any consumer. Should be larger than the slowest expected processing
    -- time, otherwise live entries get stolen.
    claimMinIdleMs :: Int,
    -- | How often each shard scans XPENDING for stuck entries.
    claimIntervalSeconds :: Seconds,
    -- | Hard cap on delivery attempts (XPENDING's numTimesDelivered). When
    -- exceeded, the entry is XACKed with an error log — no DLQ.
    maxDeliveries :: Int,
    -- | If this Redis key has any value, all polling pauses. Operators flip
    -- it to drain without redeploying.
    pauseFlagKey :: Text,
    pauseSleepSeconds :: Seconds
  }
  deriving (Generic, FromDhall)
