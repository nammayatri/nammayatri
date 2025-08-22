module SharedLogic.FleetOperatorStats where

import qualified Domain.Types.FleetOperatorStats as DFS
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Ride as DR
import qualified Domain.Types.TransporterConfig as DTTC
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.FleetOperatorStats as QFleetOps

computeOperatorIncrements :: DR.Ride -> (Meters, HighPrecMoney, Int)
computeOperatorIncrements r =
  ( fromMaybe 0 r.chargeableDistance,
    fromMaybe 0.0 r.fare,
    1
  )

-- Helper: build initial FleetOperatorStats row for an operator from a ride
buildInitialFleetOperatorStats :: Text -> Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int -> Maybe Meters -> Maybe Int -> Maybe HighPrecMoney -> Maybe Currency -> Maybe DistanceUnit -> Maybe (Id DM.Merchant) -> Maybe (Id DMOC.MerchantOperatingCity) -> UTCTime -> DFS.FleetOperatorStats
buildInitialFleetOperatorStats operatorId totalRatingCount totalRatingScore driverFirstSubscription inspectionCompleted acceptationRequestCount totalRequestCount customerCancellationCount driverCancellationCount totalDistance totalCompletedRides totalEarning currency distanceUnit merchantId merchantOperatingCityId now =
  DFS.FleetOperatorStats
    { fleetOperatorId = operatorId,
      totalRatingCount = totalRatingCount,
      totalRatingScore = totalRatingScore,
      driverFirstSubscription = driverFirstSubscription,
      inspectionCompleted = inspectionCompleted,
      acceptationRequestCount = acceptationRequestCount,
      totalRequestCount = totalRequestCount,
      customerCancellationCount = customerCancellationCount,
      driverCancellationCount = driverCancellationCount,
      totalDistance = totalDistance,
      totalCompletedRides = totalCompletedRides,
      totalEarning = totalEarning,
      currency = currency,
      distanceUnit = distanceUnit,
      createdAt = now,
      updatedAt = now,
      merchantId = merchantId,
      merchantOperatingCityId = merchantOperatingCityId
    }

-- Common lock key for operator analytics mutations
makeOperatorMetricLockKey :: Text -> Text -> Text
makeOperatorMetricLockKey operatorId metricName = "FleetOperatorStats:Lock:" <> operatorId <> ":" <> metricName

incrementTotalRidesTotalDistAndTotalEarning :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> DR.Ride -> DTTC.TransporterConfig -> m ()
incrementTotalRidesTotalDistAndTotalEarning operatorId ride transporterConfig = do
  mbCurrent <- QFleetOps.findByPrimaryKey operatorId
  case mbCurrent of
    Just s -> do
      let (incDist, incEarn, incCount) = computeOperatorIncrements ride
          newTotalCompletedRides = Just (fromMaybe 0 s.totalCompletedRides + incCount)
          newTotalDistance = Just (fromMaybe 0 s.totalDistance + incDist)
          newTotalEarning = Just (fromMaybe 0 s.totalEarning + incEarn)
      QFleetOps.updateDistanceEarningAndCompletedRidesByFleetOperatorId newTotalDistance newTotalEarning newTotalCompletedRides operatorId
    Nothing -> do
      now <- getCurrentTime
      let dist = Just (fromMaybe 0 ride.chargeableDistance)
          earn = Just (fromMaybe 0.0 ride.fare)
      QFleetOps.create (buildInitialFleetOperatorStats operatorId Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing dist (Just 1) earn (Just transporterConfig.currency) (Just transporterConfig.distanceUnit) (Just transporterConfig.merchantId) (Just transporterConfig.merchantOperatingCityId) now)

incrementDriverCancellationCount :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> DTTC.TransporterConfig -> m ()
incrementDriverCancellationCount operatorId transporterConfig = do
  mbCurrent <- QFleetOps.findByPrimaryKey operatorId
  case mbCurrent of
    Just s -> QFleetOps.updateDriverCancellationCountByFleetOperatorId (Just (fromMaybe 0 s.driverCancellationCount + 1)) operatorId
    Nothing -> do
      now <- getCurrentTime
      QFleetOps.create (buildInitialFleetOperatorStats operatorId Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Just 1) Nothing Nothing Nothing (Just transporterConfig.currency) (Just transporterConfig.distanceUnit) (Just transporterConfig.merchantId) (Just transporterConfig.merchantOperatingCityId) now)

incrementAcceptationRequestCount :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> DTTC.TransporterConfig -> m ()
incrementAcceptationRequestCount operatorId transporterConfig = do
  mbCurrent <- QFleetOps.findByPrimaryKey operatorId
  case mbCurrent of
    Just s -> QFleetOps.updateAcceptationRequestCountByFleetOperatorId (Just (fromMaybe 0 s.acceptationRequestCount + 1)) operatorId
    Nothing -> do
      now <- getCurrentTime
      QFleetOps.create (buildInitialFleetOperatorStats operatorId Nothing Nothing Nothing Nothing (Just 1) Nothing Nothing Nothing Nothing Nothing Nothing (Just transporterConfig.currency) (Just transporterConfig.distanceUnit) (Just transporterConfig.merchantId) (Just transporterConfig.merchantOperatingCityId) now)

incrementTotalRequestCount :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> DTTC.TransporterConfig -> m ()
incrementTotalRequestCount operatorId transporterConfig = do
  mbCurrent <- QFleetOps.findByPrimaryKey operatorId
  case mbCurrent of
    Just s -> QFleetOps.updateTotalRequestCountByFleetOperatorId (Just (fromMaybe 0 s.totalRequestCount + 1)) operatorId
    Nothing -> do
      now <- getCurrentTime
      QFleetOps.create (buildInitialFleetOperatorStats operatorId Nothing Nothing Nothing Nothing Nothing (Just 1) Nothing Nothing Nothing Nothing Nothing (Just transporterConfig.currency) (Just transporterConfig.distanceUnit) (Just transporterConfig.merchantId) (Just transporterConfig.merchantOperatingCityId) now)

incrementTotalRatingCountAndTotalRatingScore :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> DTTC.TransporterConfig -> Int -> m ()
incrementTotalRatingCountAndTotalRatingScore operatorId transporterConfig ratingValue = do
  mbCurrent <- QFleetOps.findByPrimaryKey operatorId
  case mbCurrent of
    Just s -> do
      let newTotalRatingCount = Just (fromMaybe 0 s.totalRatingCount + 1)
          newTotalRatingScore = Just (fromMaybe 0 s.totalRatingScore + ratingValue)
      QFleetOps.updateTotalRatingCountAndTotalRatingScoreByFleetOperatorId newTotalRatingCount newTotalRatingScore operatorId
    Nothing -> do
      now <- getCurrentTime
      QFleetOps.create (buildInitialFleetOperatorStats operatorId (Just 1) (Just ratingValue) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Just transporterConfig.currency) (Just transporterConfig.distanceUnit) (Just transporterConfig.merchantId) (Just transporterConfig.merchantOperatingCityId) now)
