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
--     drivers-online every tick from driver_information on the read replica, and
--     the three windowed uniques once per COMPLETED window (the kernel exposes no
--     SCARD, so cardinality = length of sMembers; publishing once per window keeps
--     that to 3 set-reads per city per 10 minutes).
--
-- Every pod runs the publisher; all pods compute the same values from shared
-- sources of truth, so concurrent publishes are benign.
module SharedLogic.DriverSupplyMetrics
  ( recordDriversPinged,
    recordDriverAccepted,
    recordDriverOnRide,
    runDriverSupplyMetricsPublisher,
  )
where

import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
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

recordSupplyEvent :: (Redis.HedisFlow m r, MonadFlow m) => Text -> Id DMOC.MerchantOperatingCity -> [Id DP.Person] -> m ()
recordSupplyEvent kind cityId driverIds =
  unless (null driverIds) $ do
    now <- getCurrentTime
    Redis.sAddExp (supplyKey kind cityId.getId (currentBucket now)) (map (.getId) driverIds) windowTtlSecs

recordDriversPinged :: (Redis.HedisFlow m r, MonadFlow m) => Id DMOC.MerchantOperatingCity -> [Id DP.Person] -> m ()
recordDriversPinged = recordSupplyEvent "pinged"

recordDriverAccepted :: (Redis.HedisFlow m r, MonadFlow m) => Id DMOC.MerchantOperatingCity -> Id DP.Person -> m ()
recordDriverAccepted cityId driverId = recordSupplyEvent "accepted" cityId [driverId]

recordDriverOnRide :: (Redis.HedisFlow m r, MonadFlow m) => Id DMOC.MerchantOperatingCity -> Id DP.Person -> m ()
recordDriverOnRide cityId driverId = recordSupplyEvent "onride" cityId [driverId]

countWindow :: (Redis.HedisFlow m r, MonadFlow m) => Text -> Id DMOC.MerchantOperatingCity -> Int -> m Int
countWindow kind cityId bucket = do
  (members :: [Text]) <- Redis.sMembers (supplyKey kind cityId.getId bucket)
  pure $ length members

-- | Forked once at app boot; must never die, so each tick is exception-guarded.
runDriverSupplyMetricsPublisher ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    Redis.HedisFlow m r,
    HasDriverSupplyMetrics m r,
    MonadFlow m
  ) =>
  m ()
runDriverSupplyMetricsPublisher = withLogTag "DriverSupplyMetrics" $ go (-1)
  where
    go lastPublishedBucket = do
      res <- try @_ @SomeException (publishTick lastPublishedBucket)
      newLast <- case res of
        Left err -> do
          logError $ "publish tick failed: " <> show err
          pure lastPublishedBucket
        Right published -> pure published
      liftIO $ threadDelay (publishTickSecs * 1000000)
      go newLast

    publishTick lastPublishedBucket = do
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
      providers <- CQM.loadAllProviders
      forM_ providers $ \merchant -> do
        cities <- CQMOC.findAllByMerchantId merchant.id
        forM_ cities $ \city -> do
          let merchantLabel = merchant.shortId.getShortId
              cityLabel = show city.city
          online <- QDI.countOnlineByCity city.id
          setDriverSupplyGauge supplyMetrics.driversOnlineGauge merchantLabel cityLabel online
          forM_ bucketsToPublish $ \bucket -> do
            pinged <- countWindow "pinged" city.id bucket
            accepted <- countWindow "accepted" city.id bucket
            onRide <- countWindow "onride" city.id bucket
            setDriverSupplyGauge supplyMetrics.driversReceivingGauge merchantLabel cityLabel pinged
            setDriverSupplyGauge supplyMetrics.driversAcceptingGauge merchantLabel cityLabel accepted
            setDriverSupplyGauge supplyMetrics.driversOnRideGauge merchantLabel cityLabel onRide
      pure $ max lastPublishedBucket prevBucket
