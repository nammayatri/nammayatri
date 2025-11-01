module SharedLogic.FleetOperatorStats where

import Data.Time hiding (getCurrentTime, secondsToNominalDiffTime)
import qualified Domain.Types.FleetOperatorDailyStats as DFODS
import qualified Domain.Types.FleetOperatorStats as DFS
import qualified Domain.Types.Ride as DR
import qualified Domain.Types.TransporterConfig as DTTC
import Kernel.Prelude hiding (getField)
import Kernel.Utils.Common
import qualified Storage.Queries.FleetOperatorDailyStats as QFleetOpsDaily
import qualified Storage.Queries.FleetOperatorStats as QFleetOps

-- Generic helpers to reduce duplication for simple count increments
incrementDailyCount ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Text -> -- Fleet operator id
  DTTC.TransporterConfig -> -- Transporter config (for tz & merchant info)
  (DFODS.FleetOperatorDailyStats -> Maybe Int) -> -- Getter for the counter from existing row
  (Maybe Int -> Text -> Day -> m ()) -> -- Update action for that counter
  (DFODS.FleetOperatorDailyStats -> DFODS.FleetOperatorDailyStats) -> -- Setter for initial row (set counter = Just 1)
  m ()
incrementDailyCount fleetOperatorId transporterConfig getField updateCounter setInitField = do
  nowUTCTime <- getCurrentTime
  let now = addUTCTime (secondsToNominalDiffTime transporterConfig.timeDiffFromUtc) nowUTCTime
  let merchantLocalDate = utctDay now
  mbCurrent <- QFleetOpsDaily.findByFleetOperatorIdAndDate fleetOperatorId merchantLocalDate
  case mbCurrent of
    Just s -> updateCounter (Just (fromMaybe 0 (getField s) + 1)) fleetOperatorId merchantLocalDate
    Nothing -> do
      rowId <- generateGUIDText
      initStats <- buildInitialFleetOperatorDailyStats fleetOperatorId rowId merchantLocalDate transporterConfig nowUTCTime
      QFleetOpsDaily.create (setInitField initStats)

incrementOverallCount ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Text -> -- Fleet operator id
  DTTC.TransporterConfig -> -- Transporter config
  (DFS.FleetOperatorStats -> Maybe Int) -> -- Getter for the counter from existing row
  (Maybe Int -> Text -> m ()) -> -- Update action for that counter
  (DFS.FleetOperatorStats -> DFS.FleetOperatorStats) -> -- Setter for initial row (set counter = Just 1)
  m ()
incrementOverallCount fleetOperatorId transporterConfig getField updateCounter setInitField = do
  mbCurrent <- QFleetOps.findByPrimaryKey fleetOperatorId
  case mbCurrent of
    Just s -> updateCounter (Just (fromMaybe 0 (getField s) + 1)) fleetOperatorId
    Nothing -> do
      initStats <- buildInitialFleetOperatorStats fleetOperatorId transporterConfig
      QFleetOps.create (setInitField initStats)

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
incrementDriverCancellationCount fleetOperatorId transporterConfig =
  incrementOverallCount
    fleetOperatorId
    transporterConfig
    (.driverCancellationCount)
    QFleetOps.updateDriverCancellationCountByFleetOperatorId
    (\s -> s {DFS.driverCancellationCount = Just 1})

incrementCustomerCancellationCount :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> DTTC.TransporterConfig -> m ()
incrementCustomerCancellationCount fleetOperatorId transporterConfig =
  incrementOverallCount
    fleetOperatorId
    transporterConfig
    (.customerCancellationCount)
    QFleetOps.updateCustomerCancellationCountByFleetOperatorId
    (\s -> s {DFS.customerCancellationCount = Just 1})

incrementAcceptationRequestCount :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> DTTC.TransporterConfig -> m ()
incrementAcceptationRequestCount fleetOperatorId transporterConfig =
  incrementOverallCount
    fleetOperatorId
    transporterConfig
    (.acceptationRequestCount)
    QFleetOps.updateAcceptationRequestCountByFleetOperatorId
    (\s -> s {DFS.acceptationRequestCount = Just 1})

incrementTotalRequestCount :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> DTTC.TransporterConfig -> m ()
incrementTotalRequestCount fleetOperatorId transporterConfig =
  incrementOverallCount
    fleetOperatorId
    transporterConfig
    (.totalRequestCount)
    QFleetOps.updateTotalRequestCountByFleetOperatorId
    (\s -> s {DFS.totalRequestCount = Just 1})

incrementTotalRatingCountAndTotalRatingScore :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> DTTC.TransporterConfig -> Int -> Bool -> m ()
incrementTotalRatingCountAndTotalRatingScore fleetOperatorId transporterConfig ratingValue shouldIncrementCount = do
  mbCurrent <- QFleetOps.findByPrimaryKey fleetOperatorId
  case mbCurrent of
    Just s -> do
      let newTotalRatingCount = Just (fromMaybe 0 s.totalRatingCount + if shouldIncrementCount then 1 else 0)
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
incrementAcceptationRequestCountDaily fleetOperatorId transporterConfig =
  incrementDailyCount
    fleetOperatorId
    transporterConfig
    (.acceptationRequestCount)
    QFleetOpsDaily.updateAcceptationRequestCountByFleetOperatorIdAndDate
    (\s -> s {DFODS.acceptationRequestCount = Just 1})

-- Daily: increment TotalRequestCount
incrementTotalRequestCountDaily :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> DTTC.TransporterConfig -> m ()
incrementTotalRequestCountDaily fleetOperatorId transporterConfig =
  incrementDailyCount
    fleetOperatorId
    transporterConfig
    (.totalRequestCount)
    QFleetOpsDaily.updateTotalRequestCountByFleetOperatorIdAndDate
    (\s -> s {DFODS.totalRequestCount = Just 1})

-- Daily: increment RejectedRequestCount
incrementRejectedRequestCountDaily :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> DTTC.TransporterConfig -> m ()
incrementRejectedRequestCountDaily fleetOperatorId transporterConfig =
  incrementDailyCount
    fleetOperatorId
    transporterConfig
    (.rejectedRequestCount)
    QFleetOpsDaily.updateRejectedRequestCountByFleetOperatorIdAndDate
    (\s -> s {DFODS.rejectedRequestCount = Just 1})

-- Daily: increment PulledRequestCount
incrementPulledRequestCountDaily :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> DTTC.TransporterConfig -> m ()
incrementPulledRequestCountDaily fleetOperatorId transporterConfig =
  incrementDailyCount
    fleetOperatorId
    transporterConfig
    (.pulledRequestCount)
    QFleetOpsDaily.updatePulledRequestCountByFleetOperatorIdAndDate
    (\s -> s {DFODS.pulledRequestCount = Just 1})

-- Daily: increment DriverCancellationCount
incrementDriverCancellationCountDaily :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> DTTC.TransporterConfig -> m ()
incrementDriverCancellationCountDaily fleetOperatorId transporterConfig =
  incrementDailyCount
    fleetOperatorId
    transporterConfig
    (.driverCancellationCount)
    QFleetOpsDaily.updateDriverCancellationCountByFleetOperatorIdAndDate
    (\s -> s {DFODS.driverCancellationCount = Just 1})

-- Daily: increment CustomerCancellationCount
incrementCustomerCancellationCountDaily :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> DTTC.TransporterConfig -> m ()
incrementCustomerCancellationCountDaily fleetOperatorId transporterConfig =
  incrementDailyCount
    fleetOperatorId
    transporterConfig
    (.customerCancellationCount)
    QFleetOpsDaily.updateCustomerCancellationCountByFleetOperatorIdAndDate
    (\s -> s {DFODS.customerCancellationCount = Just 1})

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
incrementTotalRatingCountAndTotalRatingScoreDaily :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> DTTC.TransporterConfig -> Int -> Bool -> m ()
incrementTotalRatingCountAndTotalRatingScoreDaily fleetOperatorId transporterConfig ratingValue shouldIncrementCount = do
  nowUTCTime <- getCurrentTime
  let now = addUTCTime (secondsToNominalDiffTime transporterConfig.timeDiffFromUtc) nowUTCTime
  let merchantLocalDate = utctDay now
  mbCurrent <- QFleetOpsDaily.findByFleetOperatorIdAndDate fleetOperatorId merchantLocalDate
  case mbCurrent of
    Just s -> do
      let newTotalRatingCount = Just (fromMaybe 0 s.totalRatingCount + if shouldIncrementCount then 1 else 0)
          newTotalRatingScore = Just (fromMaybe 0 s.totalRatingScore + ratingValue)
      QFleetOpsDaily.updateTotalRatingCountAndTotalRatingScoreByFleetOperatorIdAndDate newTotalRatingCount newTotalRatingScore fleetOperatorId merchantLocalDate
    Nothing -> do
      rowId <- generateGUIDText
      initStats <- buildInitialFleetOperatorDailyStats fleetOperatorId rowId merchantLocalDate transporterConfig nowUTCTime
      QFleetOpsDaily.create initStats {DFODS.totalRatingCount = Just 1, DFODS.totalRatingScore = Just ratingValue}
