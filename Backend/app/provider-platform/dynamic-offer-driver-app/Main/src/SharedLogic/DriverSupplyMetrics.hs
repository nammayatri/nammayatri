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
--     (seeded from ClickHouse only when the key is absent), and the three windowed
--     uniques once per COMPLETED window (the kernel exposes no SCARD, so cardinality =
--     length of sMembers; publishing once per window keeps that to 3 set-reads per city
--     per 10 minutes).
--
-- No Postgres on this path: the merchant/city list is resolved once at boot, and the
-- only non-Redis read is the ClickHouse seed on a counter miss.
module SharedLogic.DriverSupplyMetrics
  ( recordDriversPinged,
    recordDriverAccepted,
    recordDriverOnRide,
    runDriverSupplyMetricsPublisher,
  )
where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import Kernel.Prelude
import Kernel.Storage.Clickhouse.Config (ClickhouseFlow)
import qualified Kernel.Storage.ClickhouseV2 as CH
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.DriverSupplyCounter (onlineCountKey)
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Clickhouse.DriverInformation as CHDI
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

-- | Seeds the counter from ClickHouse. Only runs when the key is absent, so in practice
-- once per city per Redis lifetime; the incr/decr from the writers of
-- driver_information.active carry it from there. `set`, not a read-then-incrby delta:
-- a delta is not idempotent across pods, so a miss would leave the key at (pods x online).
seedOnlineCount ::
  (CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m, ClickhouseFlow m r, Redis.HedisFlow m r, MonadFlow m) =>
  Id DMOC.MerchantOperatingCity ->
  m Int
seedOnlineCount cityId = do
  online <- CHDI.countOnlineByCity cityId
  Redis.set (onlineCountKey cityId.getId) online
  pure online

-- | Redis is the only read on the tick path; ClickHouse is touched solely on a miss.
currentOnlineCount ::
  (CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m, ClickhouseFlow m r, Redis.HedisFlow m r, MonadFlow m) =>
  Id DMOC.MerchantOperatingCity ->
  m Int
currentOnlineCount cityId = do
  -- safeGet, not get: incr/decr write a raw Redis integer while `set` writes JSON, and
  -- the same key is written by both. safeGet degrades an undecodable value to Nothing,
  -- which re-seeds and logs below, rather than throwing and freezing the gauge forever.
  -- Same combination as Pass.hs / PassDetails.hs, which also mix set + incr on one key.
  mbCount <- Redis.safeGet (onlineCountKey cityId.getId)
  case mbCount of
    -- DECR on a missing key creates it at -1, so "exists" is not enough.
    Just count | count >= 0 -> pure count
    _ -> do
      logWarning $ "online count missing or negative for city " <> cityId.getId <> ", seeding from ClickHouse"
      seedOnlineCount cityId

-- | Forked once at app boot; must never die, so each tick is exception-guarded.
runDriverSupplyMetricsPublisher ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m,
    ClickhouseFlow m r,
    Redis.HedisFlow m r,
    HasDriverSupplyMetrics m r,
    MonadFlow m
  ) =>
  m ()
runDriverSupplyMetricsPublisher = withLogTag "DriverSupplyMetrics" $ do
  -- Merchants and their operating cities change only through config, so they are
  -- resolved once here rather than on every tick. A city added later is picked up
  -- on the next deploy.
  targets <- loadTargets
  logInfo $ "publishing driver supply for " <> show (length targets) <> " merchant-city pairs"
  go targets (-1)
  where
    loadTargets = do
      providers <- CQM.loadAllProviders
      fmap concat $
        forM providers $ \merchant -> do
          cities <- CQMOC.findAllByMerchantId merchant.id
          pure $ map (\city -> (merchant.shortId.getShortId, show city.city, city.id)) cities

    go targets lastPublishedBucket = do
      res <- try @_ @SomeException (publishTick targets lastPublishedBucket)
      newLast <- case res of
        Left err -> do
          logError $ "publish tick failed: " <> show err
          pure lastPublishedBucket
        Right published -> pure published
      liftIO $ threadDelay (publishTickSecs * 1000000)
      go targets newLast

    publishTick targets lastPublishedBucket = do
      supplyMetrics <- asks (.driverSupplyMetrics)
      now <- getCurrentTime
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
      forM_ targets $ \(merchantLabel, cityLabel, cityId) -> do
        online <- currentOnlineCount cityId
        setDriverSupplyGauge supplyMetrics.driversOnlineGauge merchantLabel cityLabel online
        forM_ bucketsToPublish $ \bucket -> do
          publishFunnelGauge supplyMetrics.driversReceivingGauge "pinged" merchantLabel cityLabel cityId bucket
          publishFunnelGauge supplyMetrics.driversAcceptingGauge "accepted" merchantLabel cityLabel cityId bucket
          publishRideGauge supplyMetrics.driversOnRideGauge "onride" merchantLabel cityLabel cityId bucket
      pure (max lastPublishedBucket prevBucket)

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
