module SharedLogic.FleetOperatorStats where

import Data.Time hiding (getCurrentTime, secondsToNominalDiffTime)
import qualified Domain.Types.FleetOperatorDailyStats as DFODS
import qualified Domain.Types.FleetOperatorStats as DFS
import qualified Domain.Types.Ride as DR
import qualified Domain.Types.TransporterConfig as DTTC
import Kernel.Prelude
import Kernel.Utils.Common
import qualified Storage.Queries.FleetOperatorDailyStats as QFleetOpsDaily
import qualified Storage.Queries.FleetOperatorStats as QFleetOps

computeOperatorIncrements :: DR.Ride -> (Meters, HighPrecMoney, Int)
computeOperatorIncrements r =
  ( fromMaybe 0 r.chargeableDistance,
    fromMaybe 0.0 r.fare,
    1
  )

-- Helper: build initial FleetOperatorStats row for an operator from a ride
buildInitialFleetOperatorStats :: (MonadFlow m) => Text -> DTTC.TransporterConfig -> m DFS.FleetOperatorStats
buildInitialFleetOperatorStats fleetOperatorId transporterConfig = do
  now <- getCurrentTime
  pure
    DFS.FleetOperatorStats
      { fleetOperatorId = fleetOperatorId,
        totalRatingCount = Nothing,
        totalRatingScore = Nothing,
        driverFirstSubscription = Nothing,
        inspectionCompleted = Nothing,
        acceptationRequestCount = Nothing,
        totalRequestCount = Nothing,
        customerCancellationCount = Nothing,
        driverCancellationCount = Nothing,
        totalDistance = Nothing,
        totalCompletedRides = Nothing,
        totalEarning = Nothing,
        currency = Just transporterConfig.currency,
        distanceUnit = Just transporterConfig.distanceUnit,
        createdAt = now,
        updatedAt = now,
        merchantId = Just transporterConfig.merchantId,
        merchantOperatingCityId = Just transporterConfig.merchantOperatingCityId
      }

-- Common lock key for operator analytics mutations
makeFleetOperatorMetricLockKey :: Text -> KeyMetric -> Text
makeFleetOperatorMetricLockKey fleetOperatorId metricName = "FleetOperatorStats:Lock:" <> fleetOperatorId <> ":" <> show metricName

data KeyMetric = RIDE_METRICS | DRIVER_CANCEL | CUSTOMER_CANCEL | ACCEPTATION_REQUEST | TOTAL_REQUEST | REJECTED_REQUEST | PULLED_REQUEST | RATING_COUNT_AND_SCORE
  deriving (Show, Eq)

incrementTotalRidesTotalDistAndTotalEarning :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> DR.Ride -> DTTC.TransporterConfig -> m ()
incrementTotalRidesTotalDistAndTotalEarning fleetOperatorId ride transporterConfig = do
  mbCurrent <- QFleetOps.findByPrimaryKey fleetOperatorId
  case mbCurrent of
    Just s -> do
      let (incDist, incEarn, incCount) = computeOperatorIncrements ride
          newTotalCompletedRides = Just (fromMaybe 0 s.totalCompletedRides + incCount)
          newTotalDistance = Just (fromMaybe 0 s.totalDistance + incDist)
          newTotalEarning = Just (fromMaybe 0 s.totalEarning + incEarn)
      QFleetOps.updateDistanceEarningAndCompletedRidesByFleetOperatorId newTotalDistance newTotalEarning newTotalCompletedRides fleetOperatorId
    Nothing -> do
      let dist = Just (fromMaybe 0 ride.chargeableDistance)
          earn = Just (fromMaybe 0.0 ride.fare)
      initStats <- buildInitialFleetOperatorStats fleetOperatorId transporterConfig
      QFleetOps.create initStats {DFS.totalDistance = dist, DFS.totalCompletedRides = Just 1, DFS.totalEarning = earn}

incrementDriverCancellationCount :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> DTTC.TransporterConfig -> m ()
incrementDriverCancellationCount fleetOperatorId transporterConfig = do
  mbCurrent <- QFleetOps.findByPrimaryKey fleetOperatorId
  case mbCurrent of
    Just s -> QFleetOps.updateDriverCancellationCountByFleetOperatorId (Just (fromMaybe 0 s.driverCancellationCount + 1)) fleetOperatorId
    Nothing -> do
      initStats <- buildInitialFleetOperatorStats fleetOperatorId transporterConfig
      QFleetOps.create initStats {DFS.driverCancellationCount = Just 1}

incrementCustomerCancellationCount :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> DTTC.TransporterConfig -> m ()
incrementCustomerCancellationCount fleetOperatorId transporterConfig = do
  mbCurrent <- QFleetOps.findByPrimaryKey fleetOperatorId
  case mbCurrent of
    Just s -> QFleetOps.updateCustomerCancellationCountByFleetOperatorId (Just (fromMaybe 0 s.customerCancellationCount + 1)) fleetOperatorId
    Nothing -> do
      initStats <- buildInitialFleetOperatorStats fleetOperatorId transporterConfig
      QFleetOps.create initStats {DFS.customerCancellationCount = Just 1}

incrementAcceptationRequestCount :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> DTTC.TransporterConfig -> m ()
incrementAcceptationRequestCount fleetOperatorId transporterConfig = do
  mbCurrent <- QFleetOps.findByPrimaryKey fleetOperatorId
  case mbCurrent of
    Just s -> QFleetOps.updateAcceptationRequestCountByFleetOperatorId (Just (fromMaybe 0 s.acceptationRequestCount + 1)) fleetOperatorId
    Nothing -> do
      initStats <- buildInitialFleetOperatorStats fleetOperatorId transporterConfig
      QFleetOps.create initStats {DFS.acceptationRequestCount = Just 1}

incrementTotalRequestCount :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> DTTC.TransporterConfig -> m ()
incrementTotalRequestCount fleetOperatorId transporterConfig = do
  mbCurrent <- QFleetOps.findByPrimaryKey fleetOperatorId
  case mbCurrent of
    Just s -> QFleetOps.updateTotalRequestCountByFleetOperatorId (Just (fromMaybe 0 s.totalRequestCount + 1)) fleetOperatorId
    Nothing -> do
      initStats <- buildInitialFleetOperatorStats fleetOperatorId transporterConfig
      QFleetOps.create initStats {DFS.totalRequestCount = Just 1}

incrementTotalRatingCountAndTotalRatingScore :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> DTTC.TransporterConfig -> Int -> m ()
incrementTotalRatingCountAndTotalRatingScore fleetOperatorId transporterConfig ratingValue = do
  mbCurrent <- QFleetOps.findByPrimaryKey fleetOperatorId
  case mbCurrent of
    Just s -> do
      let newTotalRatingCount = Just (fromMaybe 0 s.totalRatingCount + 1)
          newTotalRatingScore = Just (fromMaybe 0 s.totalRatingScore + ratingValue)
      QFleetOps.updateTotalRatingCountAndTotalRatingScoreByFleetOperatorId newTotalRatingCount newTotalRatingScore fleetOperatorId
    Nothing -> do
      initStats <- buildInitialFleetOperatorStats fleetOperatorId transporterConfig
      QFleetOps.create initStats {DFS.totalRatingCount = Just 1, DFS.totalRatingScore = Just ratingValue}

-- Helper: build initial FleetOperatorDailyStats row
buildInitialFleetOperatorDailyStats :: (MonadFlow m) => Text -> Text -> Day -> DTTC.TransporterConfig -> UTCTime -> m DFODS.FleetOperatorDailyStats
buildInitialFleetOperatorDailyStats fleetOperatorId rowId merchantLocalDate transporterConfig now = do
  pure
    DFODS.FleetOperatorDailyStats
      { fleetOperatorId = fleetOperatorId,
        id = rowId,
        merchantLocalDate = merchantLocalDate,
        totalRatingCount = Nothing,
        totalRatingScore = Nothing,
        driverFirstSubscription = Nothing,
        inspectionCompleted = Nothing,
        rejectedRequestCount = Nothing,
        pulledRequestCount = Nothing,
        acceptationRequestCount = Nothing,
        totalRequestCount = Nothing,
        customerCancellationCount = Nothing,
        driverCancellationCount = Nothing,
        totalDistance = Nothing,
        totalCompletedRides = Nothing,
        totalEarning = Nothing,
        currency = Just transporterConfig.currency,
        distanceUnit = Just transporterConfig.distanceUnit,
        createdAt = now,
        updatedAt = now,
        merchantId = Just transporterConfig.merchantId,
        merchantOperatingCityId = Just transporterConfig.merchantOperatingCityId
      }

-- Daily: increment AcceptationRequestCount
incrementAcceptationRequestCountDaily :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> DTTC.TransporterConfig -> m ()
incrementAcceptationRequestCountDaily fleetOperatorId transporterConfig = do
  nowUTCTime <- getCurrentTime
  let now = addUTCTime (secondsToNominalDiffTime transporterConfig.timeDiffFromUtc) nowUTCTime
  let merchantLocalDate = utctDay now
  mbCurrent <- QFleetOpsDaily.findByFleetOperatorIdAndDate fleetOperatorId merchantLocalDate
  case mbCurrent of
    Just s -> QFleetOpsDaily.updateAcceptationRequestCountByFleetOperatorIdAndDate (Just (fromMaybe 0 s.acceptationRequestCount + 1)) fleetOperatorId merchantLocalDate
    Nothing -> do
      rowId <- generateGUIDText
      initStats <- buildInitialFleetOperatorDailyStats fleetOperatorId rowId merchantLocalDate transporterConfig nowUTCTime
      QFleetOpsDaily.create initStats {DFODS.acceptationRequestCount = Just 1}

-- Daily: increment TotalRequestCount
incrementTotalRequestCountDaily :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> DTTC.TransporterConfig -> m ()
incrementTotalRequestCountDaily fleetOperatorId transporterConfig = do
  nowUTCTime <- getCurrentTime
  let now = addUTCTime (secondsToNominalDiffTime transporterConfig.timeDiffFromUtc) nowUTCTime
  let merchantLocalDate = utctDay now
  mbCurrent <- QFleetOpsDaily.findByFleetOperatorIdAndDate fleetOperatorId merchantLocalDate
  case mbCurrent of
    Just s -> QFleetOpsDaily.updateTotalRequestCountByFleetOperatorIdAndDate (Just (fromMaybe 0 s.totalRequestCount + 1)) fleetOperatorId merchantLocalDate
    Nothing -> do
      rowId <- generateGUIDText
      initStats <- buildInitialFleetOperatorDailyStats fleetOperatorId rowId merchantLocalDate transporterConfig nowUTCTime
      QFleetOpsDaily.create initStats {DFODS.totalRequestCount = Just 1}

-- Daily: increment RejectedRequestCount
incrementRejectedRequestCountDaily :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> DTTC.TransporterConfig -> m ()
incrementRejectedRequestCountDaily fleetOperatorId transporterConfig = do
  nowUTCTime <- getCurrentTime
  let now = addUTCTime (secondsToNominalDiffTime transporterConfig.timeDiffFromUtc) nowUTCTime
  let merchantLocalDate = utctDay now
  mbCurrent <- QFleetOpsDaily.findByFleetOperatorIdAndDate fleetOperatorId merchantLocalDate
  case mbCurrent of
    Just s -> QFleetOpsDaily.updateRejectedRequestCountByFleetOperatorIdAndDate (Just (fromMaybe 0 s.rejectedRequestCount + 1)) fleetOperatorId merchantLocalDate
    Nothing -> do
      rowId <- generateGUIDText
      initStats <- buildInitialFleetOperatorDailyStats fleetOperatorId rowId merchantLocalDate transporterConfig nowUTCTime
      QFleetOpsDaily.create initStats {DFODS.rejectedRequestCount = Just 1}

-- Daily: increment PulledRequestCount
incrementPulledRequestCountDaily :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> DTTC.TransporterConfig -> m ()
incrementPulledRequestCountDaily fleetOperatorId transporterConfig = do
  nowUTCTime <- getCurrentTime
  let now = addUTCTime (secondsToNominalDiffTime transporterConfig.timeDiffFromUtc) nowUTCTime
  let merchantLocalDate = utctDay now
  mbCurrent <- QFleetOpsDaily.findByFleetOperatorIdAndDate fleetOperatorId merchantLocalDate
  case mbCurrent of
    Just s -> QFleetOpsDaily.updatePulledRequestCountByFleetOperatorIdAndDate (Just (fromMaybe 0 s.pulledRequestCount + 1)) fleetOperatorId merchantLocalDate
    Nothing -> do
      rowId <- generateGUIDText
      initStats <- buildInitialFleetOperatorDailyStats fleetOperatorId rowId merchantLocalDate transporterConfig nowUTCTime
      QFleetOpsDaily.create initStats {DFODS.pulledRequestCount = Just 1}

-- Daily: increment DriverCancellationCount
incrementDriverCancellationCountDaily :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> DTTC.TransporterConfig -> m ()
incrementDriverCancellationCountDaily fleetOperatorId transporterConfig = do
  nowUTCTime <- getCurrentTime
  let now = addUTCTime (secondsToNominalDiffTime transporterConfig.timeDiffFromUtc) nowUTCTime
  let merchantLocalDate = utctDay now
  mbCurrent <- QFleetOpsDaily.findByFleetOperatorIdAndDate fleetOperatorId merchantLocalDate
  case mbCurrent of
    Just s -> QFleetOpsDaily.updateDriverCancellationCountByFleetOperatorIdAndDate (Just (fromMaybe 0 s.driverCancellationCount + 1)) fleetOperatorId merchantLocalDate
    Nothing -> do
      rowId <- generateGUIDText
      initStats <- buildInitialFleetOperatorDailyStats fleetOperatorId rowId merchantLocalDate transporterConfig nowUTCTime
      QFleetOpsDaily.create initStats {DFODS.driverCancellationCount = Just 1}

-- Daily: increment CustomerCancellationCount
incrementCustomerCancellationCountDaily :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> DTTC.TransporterConfig -> m ()
incrementCustomerCancellationCountDaily fleetOperatorId transporterConfig = do
  nowUTCTime <- getCurrentTime
  let now = addUTCTime (secondsToNominalDiffTime transporterConfig.timeDiffFromUtc) nowUTCTime
  let merchantLocalDate = utctDay now
  mbCurrent <- QFleetOpsDaily.findByFleetOperatorIdAndDate fleetOperatorId merchantLocalDate
  case mbCurrent of
    Just s -> QFleetOpsDaily.updateCustomerCancellationCountByFleetOperatorIdAndDate (Just (fromMaybe 0 s.customerCancellationCount + 1)) fleetOperatorId merchantLocalDate
    Nothing -> do
      rowId <- generateGUIDText
      initStats <- buildInitialFleetOperatorDailyStats fleetOperatorId rowId merchantLocalDate transporterConfig nowUTCTime
      QFleetOpsDaily.create initStats {DFODS.customerCancellationCount = Just 1}

-- Daily: increment totals for earning, distance, and completed rides (aka ride completed)
incrementTotalEarningDistanceAndCompletedRidesDaily :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> DR.Ride -> DTTC.TransporterConfig -> m ()
incrementTotalEarningDistanceAndCompletedRidesDaily fleetOperatorId ride transporterConfig = do
  nowUTCTime <- getCurrentTime
  let now = addUTCTime (secondsToNominalDiffTime transporterConfig.timeDiffFromUtc) nowUTCTime
  let merchantLocalDate = utctDay now
  mbCurrent <- QFleetOpsDaily.findByFleetOperatorIdAndDate fleetOperatorId merchantLocalDate
  case mbCurrent of
    Just s -> do
      let newTotalCompletedRides = Just (fromMaybe 0 s.totalCompletedRides + 1)
          newTotalDistance = Just (fromMaybe 0 s.totalDistance + fromMaybe 0 ride.chargeableDistance)
          newTotalEarning = Just (fromMaybe 0 s.totalEarning + fromMaybe 0.0 ride.fare)
      QFleetOpsDaily.updateDistanceEarningAndCompletedRidesByFleetOperatorIdAndDate newTotalDistance newTotalEarning newTotalCompletedRides fleetOperatorId merchantLocalDate
    Nothing -> do
      rowId <- generateGUIDText
      initStats <- buildInitialFleetOperatorDailyStats fleetOperatorId rowId merchantLocalDate transporterConfig nowUTCTime
      QFleetOpsDaily.create
        initStats
          { DFODS.totalCompletedRides = Just 1,
            DFODS.totalDistance = Just (fromMaybe 0 ride.chargeableDistance),
            DFODS.totalEarning = Just (fromMaybe 0.0 ride.fare)
          }

-- Daily: increment total rating count and total rating score
incrementTotalRatingCountAndTotalRatingScoreDaily :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> DTTC.TransporterConfig -> Int -> m ()
incrementTotalRatingCountAndTotalRatingScoreDaily fleetOperatorId transporterConfig ratingValue = do
  nowUTCTime <- getCurrentTime
  let now = addUTCTime (secondsToNominalDiffTime transporterConfig.timeDiffFromUtc) nowUTCTime
  let merchantLocalDate = utctDay now
  mbCurrent <- QFleetOpsDaily.findByFleetOperatorIdAndDate fleetOperatorId merchantLocalDate
  case mbCurrent of
    Just s -> do
      let newTotalRatingCount = Just (fromMaybe 0 s.totalRatingCount + 1)
          newTotalRatingScore = Just (fromMaybe 0 s.totalRatingScore + ratingValue)
      QFleetOpsDaily.updateTotalRatingCountAndTotalRatingScoreByFleetOperatorIdAndDate newTotalRatingCount newTotalRatingScore fleetOperatorId merchantLocalDate
    Nothing -> do
      rowId <- generateGUIDText
      initStats <- buildInitialFleetOperatorDailyStats fleetOperatorId rowId merchantLocalDate transporterConfig nowUTCTime
      QFleetOpsDaily.create initStats {DFODS.totalRatingCount = Just 1, DFODS.totalRatingScore = Just ratingValue}
