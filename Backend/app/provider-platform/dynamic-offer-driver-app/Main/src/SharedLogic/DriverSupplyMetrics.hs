{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Driver-supply metrics: how many drivers are online, receiving search requests,
-- accepting them, and taking rides — per merchant × operating city.
--
-- These are DISTINCT-DRIVER counts, which Prometheus counters cannot express
-- (counters count events; the same driver pinged 40 times must count once). So:
--
--   * recording: event call sites SADD the driver id into a Redis set keyed by a
--     10-minute window (TTL 2.5 windows); the kernel's sAddExp already catches and
--     logs Redis errors, so recording can never break a business flow.
--   * publishing: a single forked loop (started at app boot) sets the gauges —
--     drivers-online every tick from the Redis counter in SharedLogic.DriverSupplyCounter
--     (recounted from driver_information on a cache miss or the 10-minute reconcile), and
--     the three windowed uniques once per COMPLETED window (the kernel exposes no
--     SCARD, so cardinality = length of sMembers; publishing once per window keeps
--     that to 3 set-reads per city per 10 minutes).
--
-- Every pod runs the publisher; the online recount is single-flighted behind a per-window
-- lock so only one pod queries Postgres.
module SharedLogic.DriverSupplyMetrics
  ( recordDriversPinged,
    recordDriverAccepted,
    recordDriverOnRide,
    runDriverSupplyMetricsPublisher,
  )
where

import Data.Bifunctor (second)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.DriverSupplyCounter (onlineCountKey)
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.DriverInformation as QDI
import Tools.Metrics.DriverSupplyMetrics.Types

windowSecs :: Int
windowSecs = 600

windowTtlSecs :: Int
windowTtlSecs = 1500 -- 2.5 windows: previous window must survive until published

publishTickSecs :: Int
publishTickSecs = 60

supplyKey :: Text -> Text -> Int -> Text
supplyKey kind cityId bucket = "driverSupply:" <> kind <> ":" <> cityId <> ":" <> show bucket

currentBucket :: UTCTime -> Int
currentBucket now = floor (utcTimeToPOSIXSeconds now) `div` windowSecs

-- Members are "<dim>|...|<dim>|<driverId>" so one set per (city, window) still yields
-- distinct counts per dimension tuple — no extra Redis reads, and no need to enumerate
-- tiers, buckets or pooling versions. Dimension values are metric labels, which never
-- contain "|", and driver ids are UUIDs, so the last segment is unambiguous.
supplyMember :: ([Text], Id DP.Person) -> Text
supplyMember (dims, driverId) = T.intercalate "|" (dims <> [driverId.getId])

reconcileLockKey :: Int -> Text
reconcileLockKey bucket = "driverSupply:onlineCount:reconcile:" <> show bucket

reconcileSecs :: Int
reconcileSecs = 600

recordSupplyEvent :: (Redis.HedisFlow m r, MonadFlow m) => Text -> Id DMOC.MerchantOperatingCity -> [([Text], Id DP.Person)] -> m ()
recordSupplyEvent kind cityId entries =
  unless (null entries) $ do
    now <- getCurrentTime
    Redis.sAddExp (supplyKey kind cityId.getId (currentBucket now)) (map supplyMember entries) windowTtlSecs

-- | The whole batch shares one search request, so its funnel labels are passed once;
-- the tier varies per driver.
recordDriversPinged :: (Redis.HedisFlow m r, MonadFlow m) => Id DMOC.MerchantOperatingCity -> (Text, Text, Text) -> [(Text, Id DP.Person)] -> m ()
recordDriversPinged cityId (distanceBucket, poolingLogicV, poolingConfigV) entries =
  recordSupplyEvent "pinged" cityId $ map (\(tier, driverId) -> ([tier, distanceBucket, poolingLogicV, poolingConfigV], driverId)) entries

recordDriverAccepted :: (Redis.HedisFlow m r, MonadFlow m) => Id DMOC.MerchantOperatingCity -> Text -> (Text, Text, Text) -> Id DP.Person -> m ()
recordDriverAccepted cityId tier (distanceBucket, poolingLogicV, poolingConfigV) driverId =
  recordSupplyEvent "accepted" cityId [([tier, distanceBucket, poolingLogicV, poolingConfigV], driverId)]

-- | No pooling versions: a booking does not carry them.
recordDriverOnRide :: (Redis.HedisFlow m r, MonadFlow m) => Id DMOC.MerchantOperatingCity -> Text -> Text -> Id DP.Person -> m ()
recordDriverOnRide cityId tier distanceBucket driverId =
  recordSupplyEvent "onride" cityId [([tier, distanceBucket], driverId)]

-- | Distinct drivers per dimension tuple for one window. Malformed members (no
-- separator) are dropped rather than counted under a bogus label.
countWindowByDims :: (Redis.HedisFlow m r, MonadFlow m) => Text -> Id DMOC.MerchantOperatingCity -> Int -> m [([Text], Int)]
countWindowByDims kind cityId bucket = do
  (members :: [Text]) <- Redis.sMembers (supplyKey kind cityId.getId bucket)
  let parsed = mapMaybe parseMember members
      byDims = Map.fromListWith Set.union [(dims, Set.singleton driverId) | (dims, driverId) <- parsed]
  pure $ map (second Set.size) (Map.toList byDims)
  where
    parseMember m = case reverse (T.splitOn "|" m) of
      (driverId : revDims) | not (null revDims) -> Just (reverse revDims, driverId)
      _ -> Nothing

-- | Authoritative recount from driver_information, only on a cache miss or on the
-- periodic reconcile -- never per tick. Uses `set`, not a read-then-incrby delta:
-- a delta is not idempotent across pods, so a cache miss would leave the key at
-- (pods x online).
rebuildOnlineCount :: (CacheFlow m r, EsqDBFlow m r, Redis.HedisFlow m r, MonadFlow m) => Id DMOC.MerchantOperatingCity -> m Int
rebuildOnlineCount cityId = do
  online <- QDI.countOnlineByCity cityId
  Redis.set (onlineCountKey cityId.getId) online
  pure online

-- | Redis is the fast path; Postgres only on a cache miss or when this pod won the reconcile lock.
currentOnlineCount ::
  (CacheFlow m r, EsqDBFlow m r, Redis.HedisFlow m r, MonadFlow m) =>
  Bool ->
  Id DMOC.MerchantOperatingCity ->
  m Int
currentOnlineCount shouldReconcile cityId
  | shouldReconcile = rebuildOnlineCount cityId
  | otherwise = do
    mbCount <- Redis.get (onlineCountKey cityId.getId)
    case mbCount of
      -- DECR on a missing key creates it at -1, so "exists" is not enough.
      Just count | count >= 0 -> pure count
      _ -> do
        logWarning $ "online count missing or negative for city " <> cityId.getId <> ", rebuilding from driver_information"
        rebuildOnlineCount cityId

-- | Forked once at app boot; must never die, so each tick is exception-guarded.
runDriverSupplyMetricsPublisher ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    Redis.HedisFlow m r,
    HasDriverSupplyMetrics m r,
    MonadFlow m
  ) =>
  m ()
runDriverSupplyMetricsPublisher = withLogTag "DriverSupplyMetrics" $ go (-1) (-1)
  where
    go lastPublishedBucket lastReconciledBucket = do
      res <- try @_ @SomeException (publishTick lastPublishedBucket lastReconciledBucket)
      (newLast, newReconciled) <- case res of
        Left err -> do
          logError $ "publish tick failed: " <> show err
          pure (lastPublishedBucket, lastReconciledBucket)
        Right published -> pure published
      liftIO $ threadDelay (publishTickSecs * 1000000)
      go newLast newReconciled

    publishTick lastPublishedBucket lastReconciledBucket = do
      supplyMetrics <- asks (.driverSupplyMetrics)
      now <- getCurrentTime
      -- One pod per window recounts from Postgres so drift cannot accumulate.
      let reconcileBucket = floor (utcTimeToPOSIXSeconds now) `div` reconcileSecs
      shouldReconcile <-
        if reconcileBucket == lastReconciledBucket
          then pure False
          else Redis.setNxExpire (reconcileLockKey reconcileBucket) reconcileSecs True
      when shouldReconcile $ logInfo "won reconcile lock, recounting online drivers from driver_information"
      let prevBucket = currentBucket now - 1
          -- Publish every completed-but-unpublished window, ascending so gauges end
          -- on the newest. Capped at 3 windows back (beyond the Redis TTL the sets
          -- are empty anyway); the strictly-increasing range also guards against a
          -- backward clock step republishing an already-expired bucket as 0.
          bucketsToPublish
            | lastPublishedBucket < 0 = [prevBucket]
            | otherwise = [max (lastPublishedBucket + 1) (prevBucket - 2) .. prevBucket]
      when (length bucketsToPublish > 1) $
        logWarning $ "publishing " <> show (length bucketsToPublish) <> " windows at once - earlier ticks were missed"
      -- Release the lock if this tick dies partway, so the next one can retry.
      res <- try @_ @SomeException $ do
        providers <- CQM.loadAllProviders
        forM_ providers $ \merchant -> do
          cities <- CQMOC.findAllByMerchantId merchant.id
          forM_ cities $ \city -> do
            let merchantLabel = merchant.shortId.getShortId
                cityLabel = show city.city
            online <- currentOnlineCount shouldReconcile city.id
            setDriverSupplyGauge supplyMetrics.driversOnlineGauge merchantLabel cityLabel online
            forM_ bucketsToPublish $ \bucket -> do
              publishFunnelGauge supplyMetrics.driversReceivingGauge "pinged" merchantLabel cityLabel city.id bucket
              publishFunnelGauge supplyMetrics.driversAcceptingGauge "accepted" merchantLabel cityLabel city.id bucket
              publishRideGauge supplyMetrics.driversOnRideGauge "onride" merchantLabel cityLabel city.id bucket
        pure (max lastPublishedBucket prevBucket, if shouldReconcile then reconcileBucket else lastReconciledBucket)
      case res of
        Right published -> pure published
        Left err -> do
          logError $ "publish tick failed mid-publish: " <> show err
          when shouldReconcile $ Redis.del (reconcileLockKey reconcileBucket)
          pure (lastPublishedBucket, lastReconciledBucket)

    publishFunnelGauge gaugeVec kind merchantLabel cityLabel cityId bucket = do
      perDims <- countWindowByDims kind cityId bucket
      forM_ perDims $ \(dims, n) -> case dims of
        [tier, distanceBucket, poolingLogicV, poolingConfigV] -> setDriverSupplyFunnelGauge gaugeVec merchantLabel cityLabel tier distanceBucket poolingLogicV poolingConfigV n
        _ -> logWarning $ "unexpected dimensions for " <> kind <> ": " <> show dims

    publishRideGauge gaugeVec kind merchantLabel cityLabel cityId bucket = do
      perDims <- countWindowByDims kind cityId bucket
      forM_ perDims $ \(dims, n) -> case dims of
        [tier, distanceBucket] -> setDriverSupplyRideGauge gaugeVec merchantLabel cityLabel tier distanceBucket n
        _ -> logWarning $ "unexpected dimensions for " <> kind <> ": " <> show dims
