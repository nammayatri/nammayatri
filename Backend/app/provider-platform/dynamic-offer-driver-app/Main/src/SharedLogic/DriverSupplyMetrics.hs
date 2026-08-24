{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Driver-supply gauges: how many drivers are online, and how many are on a ride,
-- per merchant × operating city.
--
-- Both are plain Redis counters, maintained by the events that change the underlying
-- state (SharedLogic.DriverSupplyCounter):
--
--   * online  -- incr/decr when a driver goes online or offline
--   * on-ride -- incr on ride start, decr on ride completion/cancellation
--
-- A counter, not a set of driver ids: a set large enough to hold every online driver
-- lands on one Redis shard and grows with the fleet. Per-driver uniqueness is not
-- needed here because a driver can only be online, or on a ride, once.
--
-- This loop does not count anything. It copies the counters into Prometheus gauges
-- once a tick, because a scrape can only read process memory. The only non-Redis read
-- is a one-off ClickHouse seed when a key is absent.
--
-- No backend_version label: every pod publishes, and during a rollout two versions
-- would export the same truth under different label sets, so any sum() would
-- double-count supply.
-- DASHBOARDS: always aggregate with max by (merchant, city), NEVER sum.
module SharedLogic.DriverSupplyMetrics
  ( runDriverSupplyMetricsPublisher,
  )
where

import qualified Domain.Types.MerchantOperatingCity as DMOC
import Kernel.Prelude
import Kernel.Storage.Clickhouse.Config (ClickhouseFlow)
import qualified Kernel.Storage.ClickhouseV2 as CH
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.DriverSupplyCounter (onRideCountKey, onlineCountKey)
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Clickhouse.DriverInformation as CHDI
import Tools.Metrics.DriverSupplyMetrics.Types

publishTickSecs :: Int
publishTickSecs = 60

-- Long enough to outlive a slow ClickHouse count, short enough that a pod dying
-- mid-seed does not block the next attempt for long.
seedLockSecs :: Int
seedLockSecs = 120

seedLockKey :: Text -> Text -> Text
seedLockKey kind cityId = "driverSupply:seed:" <> kind <> ":" <> cityId

seededMarkerKey :: Text -> Text -> Text
seededMarkerKey kind cityId = "driverSupply:seeded:" <> kind <> ":" <> cityId

type CounterFlow m r =
  (CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m, ClickhouseFlow m r, Redis.HedisFlow m r, MonadFlow m)

-- | Establishes a baseline for a counter whose key is absent. Single-flighted: without
-- the lock every pod would add the same baseline and leave the key at (pods x truth).
--
-- incrby, not set: a concurrent incr/decr between the ClickHouse read and the write is
-- a real state change, and `set` would silently discard it. Adding the baseline on top
-- of whatever the key holds keeps that delta -- including a negative, which is exactly
-- what a decr on a missing key leaves behind.
seedCounter :: CounterFlow m r => Text -> (Id DMOC.MerchantOperatingCity -> m Int) -> (Text -> Text) -> Id DMOC.MerchantOperatingCity -> m Int
seedCounter kind countFromCH mkKey cityId = do
  let key = mkKey cityId.getId
  wonLock <- Redis.setNxExpire (seedLockKey kind cityId.getId) seedLockSecs True
  if not wonLock
    then fromMaybe 0 <$> Redis.safeGet key -- another pod is seeding; publish what is there
    else do
      logInfo $ "seeding " <> kind <> " counter for city " <> cityId.getId <> " from ClickHouse"
      baseline <- countFromCH cityId
      void $ Redis.incrby key (fromIntegral baseline)
      Redis.set (seededMarkerKey kind cityId.getId) True
      fromMaybe baseline <$> Redis.safeGet key

isSeeded :: CounterFlow m r => Text -> Id DMOC.MerchantOperatingCity -> m Bool
isSeeded kind cityId = fromMaybe False <$> Redis.safeGet (seededMarkerKey kind cityId.getId)

-- | Redis is the only read on the tick path. safeGet, not get: incr/decr write a raw
-- Redis integer while the seed writes JSON, so an undecodable value degrades to a
-- re-seed rather than throwing and freezing the gauge.
currentCount :: CounterFlow m r => Text -> (Id DMOC.MerchantOperatingCity -> m Int) -> (Text -> Text) -> Id DMOC.MerchantOperatingCity -> m Int
currentCount kind countFromCH mkKey cityId = do
  mbCount <- Redis.safeGet (mkKey cityId.getId)
  seeded <- isSeeded kind cityId
  case mbCount of
    -- decr on a missing key creates it at -1, so "exists" is not enough.
    Just count | seeded && count >= 0 -> pure count
    _ -> seedCounter kind countFromCH mkKey cityId

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
  forever $ do
    void $ try @_ @SomeException (publishTick targets)
    liftIO $ threadDelay (publishTickSecs * 1000000)
  where
    loadTargets = do
      providers <- CQM.loadAllProviders
      fmap concat $
        forM providers $ \merchant -> do
          cities <- CQMOC.findAllByMerchantId merchant.id
          pure $ map (\city -> (merchant.shortId.getShortId, show city.city, city.id)) cities

    publishTick targets = do
      supplyMetrics <- asks (.driverSupplyMetrics)
      forM_ targets $ \(merchantLabel, cityLabel, cityId) -> do
        online <- currentCount "online" CHDI.countOnlineByCity onlineCountKey cityId
        onRide <- currentCount "onRide" CHDI.countOnRideByCity onRideCountKey cityId
        setDriverSupplyGauge supplyMetrics.driversOnlineGauge merchantLabel cityLabel online
        setDriverSupplyGauge supplyMetrics.driversOnRideGauge merchantLabel cityLabel onRide
