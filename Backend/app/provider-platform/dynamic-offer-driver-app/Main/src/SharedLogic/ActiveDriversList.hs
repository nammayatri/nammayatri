module SharedLogic.ActiveDriversList
  ( mkActiveDriversSetKey,
    mkActiveDriversSetKeyForShard,
    getActiveDriversForShard,
    getShardNumsForFanOut,
    addDriverToActiveList,
    localDateStr,
    getNextDriverBatch,
  )
where

import qualified Data.Text as Text
import Data.Time hiding (getCurrentTime, secondsToNominalDiffTime)
import qualified Domain.Types.Person as DP
import EulerHS.Prelude
import Kernel.Prelude hiding (null)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common

formatDate :: Day -> Text
formatDate day =
  let (year, month, dayOfMonth) = toGregorian day
   in Text.pack $ show year <> "-" <> padZero month <> "-" <> padZero dayOfMonth
  where
    padZero n = if n < 10 then "0" <> show n else show n

localDateStr :: UTCTime -> NominalDiffTime -> Text
localDateStr referenceTime _diffTimeFromUTC = formatDate (utctDay referenceTime)

mkActiveDriversSetKey :: MonadTime m => Int -> Text -> NominalDiffTime -> m Text
mkActiveDriversSetKey shards driverId diffTimeFromUTC = do
  now <- getCurrentTime
  let localTime = addUTCTime diffTimeFromUTC now
  let dateStr = formatDate (utctDay localTime)
  pure $ "ActiveDrivers:Set:" <> dateStr <> ":" <> Redis.shardHashTag shards driverId

mkActiveDriversSetKeyForShard :: Int -> UTCTime -> NominalDiffTime -> Text
mkActiveDriversSetKeyForShard shardNum referenceTime diffTimeFromUTC =
  "ActiveDrivers:Set:" <> localDateStr referenceTime diffTimeFromUTC <> ":{shard-" <> show shardNum <> "}"

getActiveDriversForShard :: (Redis.HedisFlow m r, TryException m) => Int -> UTCTime -> NominalDiffTime -> m [Text]
getActiveDriversForShard shardNum referenceTime diffTimeFromUTC =
  Redis.runInMasterCloudRedisCell $
    Redis.sMembers (mkActiveDriversSetKeyForShard shardNum referenceTime diffTimeFromUTC)

getShardNumsForFanOut :: forall r m. (MonadReader r m, HasField "activeDriversListKeyShards" r Int, HasField "enableDriverFeeShardedFanOut" r Bool) => m [Maybe Int]
getShardNumsForFanOut = do
  n <- asks @r (.activeDriversListKeyShards)
  shardingEnabled <- asks @r (.enableDriverFeeShardedFanOut)
  pure $ if shardingEnabled && n > 0 then Just <$> [0 .. n - 1] else [Nothing]

getTTLForActiveDriversList :: MonadTime m => NominalDiffTime -> m Int
getTTLForActiveDriversList diffTimeFromUTC = do
  now <- getCurrentTime
  let localTime = addUTCTime diffTimeFromUTC now
  let localDate = utctDay localTime
  let tomorrowLocalDate = addDays 2 localDate
  let tomorrowLocalEnd = UTCTime tomorrowLocalDate (secondsToDiffTime 0)
  let tomorrowUTC = addUTCTime (negate diffTimeFromUTC) tomorrowLocalEnd
  pure $ ceiling (realToFrac (diffUTCTime tomorrowUTC now) :: Double)

addDriverToActiveList ::
  forall r m.
  (MonadThrow m, Log m, MonadTime m, Redis.HedisFlow m r, TryException m, HasField "activeDriversListKeyShards" r Int) =>
  Id DP.Person ->
  NominalDiffTime ->
  m ()
addDriverToActiveList driverId diffTimeFromUTC = do
  shards <- asks @r (.activeDriversListKeyShards)
  ttl <- getTTLForActiveDriversList diffTimeFromUTC
  key <- mkActiveDriversSetKey shards (driverId.getId) diffTimeFromUTC
  void $ Redis.runInMasterCloudRedisCell $ Redis.sAddExp key [driverId.getId] ttl

mkDriverQueueKey :: Text -> Int -> UTCTime -> NominalDiffTime -> Text
mkDriverQueueKey jobKeyPrefix shardNum referenceTime diffTimeFromUTC =
  jobKeyPrefix <> ":DriverQueue:" <> localDateStr referenceTime diffTimeFromUTC <> ":" <> Text.pack (formatTime defaultTimeLocale "%H:%M:%S" referenceTime) <> ":{shard-" <> show shardNum <> "}"

mkDriverQueuePopulatedFlagKey :: Text -> Int -> UTCTime -> NominalDiffTime -> Text
mkDriverQueuePopulatedFlagKey jobKeyPrefix shardNum referenceTime diffTimeFromUTC =
  jobKeyPrefix <> ":DriverQueue:Populated:" <> localDateStr referenceTime diffTimeFromUTC <> ":" <> Text.pack (formatTime defaultTimeLocale "%H:%M:%S" referenceTime) <> ":{shard-" <> show shardNum <> "}"

mkDriverQueueLockKey :: Text -> Int -> UTCTime -> NominalDiffTime -> Text
mkDriverQueueLockKey jobKeyPrefix shardNum referenceTime diffTimeFromUTC =
  jobKeyPrefix <> ":DriverQueue:Lock:" <> localDateStr referenceTime diffTimeFromUTC <> ":" <> Text.pack (formatTime defaultTimeLocale "%H:%M:%S" referenceTime) <> ":{shard-" <> show shardNum <> "}"

-- | Drains up to `limit` driverIds from a job-specific queue, scoped per (job, merchant, city,
--   service, shard, day) via `jobKeyPrefix`. The queue is populated exactly once per scope from
--   the shard's ActiveDrivers set, guarded by a companion flag key — safe to populate once because
--   every caller only runs for a window whose day has already closed by the time it fires
--   (midnight+offset for the previous day's window), so the source ActiveDrivers set for that day
--   is frozen by then. The whole populate-and-drain sequence runs under a blocking Redis lock
--   (`withWaitAndLockRedis`, not the non-blocking try-lock used elsewhere): most callers of this
--   function (Notification/Execution/SendOverlay/SendManualPaymentLink) have no outer per-shard
--   lock of their own, so without this, a caller could see the "already populated" flag set by a
--   concurrent caller still mid-populate, and drain an empty, not-yet-filled queue — silently
--   treating an unprocessed shard as fully drained.
getNextDriverBatch ::
  (Redis.HedisFlow m r, MonadMask m, TryException m) =>
  Text ->
  Int ->
  UTCTime ->
  NominalDiffTime ->
  Int ->
  m [Text]
getNextDriverBatch jobKeyPrefix shardNum referenceTime diffTimeFromUTC limit =
  Redis.runInMasterCloudRedisCell $
    Redis.withWaitAndLockRedis (mkDriverQueueLockKey jobKeyPrefix shardNum referenceTime diffTimeFromUTC) 30 100000 $ do
      let queueKey = mkDriverQueueKey jobKeyPrefix shardNum referenceTime diffTimeFromUTC
          flagKey = mkDriverQueuePopulatedFlagKey jobKeyPrefix shardNum referenceTime diffTimeFromUTC
      isFirstCall <- Redis.setNxExpire flagKey (2 * 24 * 3600) True
      when isFirstCall $ do
        activeDriverIds <- getActiveDriversForShard shardNum referenceTime diffTimeFromUTC
        unless (null activeDriverIds) $
          Redis.rPushExp queueKey activeDriverIds (2 * 24 * 3600)
      batch <- Redis.lRange queueKey 0 (fromIntegral limit - 1)
      Redis.lTrim queueKey (fromIntegral limit) (-1)
      pure batch
