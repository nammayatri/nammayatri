module Utils.SlowQueryLog
  ( withQueryTiming,
    withQueryTimingThreshold,
    timedRunDB,
    defaultSlowQueryThresholdSec,
  )
where

import Data.Time.Clock (diffUTCTime)
import Kernel.Prelude
import Kernel.Utils.Common (MonadFlow, getCurrentTime, logWarning)
import qualified Lib.SessionizerMetrics.Prometheus.Metrics as Metrics

-- | Default threshold above which a query is considered slow (100ms).
-- Override per-call via 'withQueryTimingThreshold'.
defaultSlowQueryThresholdSec :: Double
defaultSlowQueryThresholdSec = 0.1

-- | Wrap any monadic query action with latency measurement using a custom threshold.
-- Logs a warning for queries exceeding the threshold and records a Prometheus histogram observation.
withQueryTimingThreshold :: (MonadFlow m, MonadIO m) => Double -> Text -> Text -> m a -> m a
withQueryTimingThreshold thresholdSec tableName operation action = do
  startTime <- getCurrentTime
  result <- action
  endTime <- getCurrentTime
  let durationSec = realToFrac (diffUTCTime endTime startTime) :: Double
  Metrics.observeDbQueryLatency tableName operation durationSec
  when (durationSec > thresholdSec) $ do
    Metrics.incrementSlowQueryCounter tableName operation
    logWarning $
      "SLOW_QUERY table=" <> tableName
        <> " op=" <> operation
        <> " duration_ms=" <> show (round (durationSec * 1000) :: Int)
  pure result

-- | Wrap any monadic query action with latency measurement using the default 100ms threshold.
withQueryTiming :: (MonadFlow m, MonadIO m) => Text -> Text -> m a -> m a
withQueryTiming = withQueryTimingThreshold defaultSlowQueryThresholdSec

-- | Timed wrapper for L.runDB calls. Measures latency, logs slow queries,
-- and records the Prometheus histogram using the default 100ms threshold.
--
-- Usage:
-- @
--   res <- timedRunDB "booking" "count" $ L.runDB dbConf $ L.findRows $ B.select $ ...
-- @
timedRunDB :: (MonadFlow m, MonadIO m) => Text -> Text -> m a -> m a
timedRunDB = withQueryTiming
