module SharedLogic.FleetOperatorStats where

import Data.Time hiding (getCurrentTime, secondsToNominalDiffTime)
import qualified Domain.Types.FleetOperatorDailyStats as DFODS
import qualified Domain.Types.FleetOperatorStats as DFS
import qualified Domain.Types.Ride as DR
import qualified Domain.Types.TransporterConfig as DTTC
import Kernel.Prelude hiding (getField)
import Kernel.Utils.Common
import qualified Storage.Queries.FareParameters as QFareParameters
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
      initStats <- buildInitialFleetOperatorDailyStats fleetOperatorId merchantLocalDate transporterConfig nowUTCTime
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
makeFleetOperatorMetricLockKey :: Text -> Text
makeFleetOperatorMetricLockKey fleetOperatorId = "FleetOperatorStats:Lock:" <> fleetOperatorId

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

incrementRequestCounts ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Text ->
  DTTC.TransporterConfig ->
  Bool ->
  Bool ->
  m ()
incrementRequestCounts fleetOperatorId transporterConfig incrementAcceptationCount incrementTotalCount = do
  mbCurrent <- QFleetOps.findByPrimaryKey fleetOperatorId
  case mbCurrent of
    Just s -> do
      let newAcceptationRequestCount = if incrementAcceptationCount then Just (fromMaybe 0 s.acceptationRequestCount + 1) else s.acceptationRequestCount
          newTotalRequestCount = if incrementTotalCount then Just (fromMaybe 0 s.totalRequestCount + 1) else s.totalRequestCount
      QFleetOps.updateRequestCountsByFleetOperatorId newAcceptationRequestCount newTotalRequestCount fleetOperatorId
    Nothing -> do
      initStats <- buildInitialFleetOperatorStats fleetOperatorId transporterConfig
      let updatedStats =
            initStats
              { DFS.acceptationRequestCount = if incrementAcceptationCount then Just 1 else Nothing,
                DFS.totalRequestCount = if incrementTotalCount then Just 1 else Nothing
              }
      QFleetOps.create updatedStats

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
buildInitialFleetOperatorDailyStats :: (MonadFlow m) => Text -> Day -> DTTC.TransporterConfig -> UTCTime -> m DFODS.FleetOperatorDailyStats
buildInitialFleetOperatorDailyStats fleetOperatorId merchantLocalDate transporterConfig now = do
  pure
    DFODS.FleetOperatorDailyStats
      { fleetOperatorId = fleetOperatorId,
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
        cashPlatformFees = Nothing,
        onlinePlatformFees = Nothing,
        onlineDuration = Nothing,
        currency = Just transporterConfig.currency,
        distanceUnit = Just transporterConfig.distanceUnit,
        createdAt = now,
        updatedAt = now,
        merchantId = Just transporterConfig.merchantId,
        merchantOperatingCityId = Just transporterConfig.merchantOperatingCityId
      }

incrementRequestCountsDaily ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Text ->
  DTTC.TransporterConfig ->
  Bool ->
  Bool ->
  Bool ->
  Bool ->
  m ()
incrementRequestCountsDaily fleetOperatorId transporterConfig incrementAcceptationCount incrementTotalCount incrementRejectedCount incrementPulledCount = do
  nowUTCTime <- getCurrentTime
  let now = addUTCTime (secondsToNominalDiffTime transporterConfig.timeDiffFromUtc) nowUTCTime
  let merchantLocalDate = utctDay now
  mbCurrent <- QFleetOpsDaily.findByFleetOperatorIdAndDate fleetOperatorId merchantLocalDate
  case mbCurrent of
    Just s -> do
      let newRejectedRequestCount = if incrementRejectedCount then Just (fromMaybe 0 s.rejectedRequestCount + 1) else s.rejectedRequestCount
          newPulledRequestCount = if incrementPulledCount then Just (fromMaybe 0 s.pulledRequestCount + 1) else s.pulledRequestCount
          newAcceptationRequestCount = if incrementAcceptationCount then Just (fromMaybe 0 s.acceptationRequestCount + 1) else s.acceptationRequestCount
          newTotalRequestCount = if incrementTotalCount then Just (fromMaybe 0 s.totalRequestCount + 1) else s.totalRequestCount
      QFleetOpsDaily.updateRequestCountsByFleetOperatorIdAndDate newRejectedRequestCount newPulledRequestCount newAcceptationRequestCount newTotalRequestCount fleetOperatorId merchantLocalDate
    Nothing -> do
      initStats <- buildInitialFleetOperatorDailyStats fleetOperatorId merchantLocalDate transporterConfig nowUTCTime
      let updatedStats =
            initStats
              { DFODS.rejectedRequestCount = if incrementRejectedCount then Just 1 else Nothing,
                DFODS.pulledRequestCount = if incrementPulledCount then Just 1 else Nothing,
                DFODS.acceptationRequestCount = if incrementAcceptationCount then Just 1 else Nothing,
                DFODS.totalRequestCount = if incrementTotalCount then Just 1 else Nothing
              }
      QFleetOpsDaily.create updatedStats

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

  -- Get platform fees from FareParameters if available
  platformFeeTotal <- case ride.fareParametersId of
    Just fareParamsId -> do
      mbFareParams <- QFareParameters.findById fareParamsId
      pure $ case mbFareParams of
        Just fareParams -> fromMaybe 0 fareParams.platformFee
        Nothing -> 0
    Nothing -> pure 0

  -- Determine cash vs online platform fees based on onlinePayment field
  let (cashPlatformFeeIncrement, onlinePlatformFeeIncrement) =
        if ride.onlinePayment
          then (0, platformFeeTotal)
          else (platformFeeTotal, 0)

  mbCurrent <- QFleetOpsDaily.findByFleetOperatorIdAndDate fleetOperatorId merchantLocalDate
  case mbCurrent of
    Just s -> do
      let newTotalCompletedRides = Just (fromMaybe 0 s.totalCompletedRides + 1)
          newTotalDistance = Just (fromMaybe 0 s.totalDistance + fromMaybe 0 ride.chargeableDistance)
          newTotalEarning = Just (fromMaybe 0 s.totalEarning + fromMaybe 0.0 ride.fare)
          newCashPlatformFees = Just (fromMaybe 0 s.cashPlatformFees + cashPlatformFeeIncrement)
          newOnlinePlatformFees = Just (fromMaybe 0 s.onlinePlatformFees + onlinePlatformFeeIncrement)
      QFleetOpsDaily.updateDistanceEarningAndCompletedRidesByFleetOperatorIdAndDate newTotalDistance newTotalEarning newTotalCompletedRides newCashPlatformFees newOnlinePlatformFees fleetOperatorId merchantLocalDate
    Nothing -> do
      initStats <- buildInitialFleetOperatorDailyStats fleetOperatorId merchantLocalDate transporterConfig nowUTCTime
      QFleetOpsDaily.create
        initStats
          { DFODS.totalCompletedRides = Just 1,
            DFODS.totalDistance = Just (fromMaybe 0 ride.chargeableDistance),
            DFODS.totalEarning = Just (fromMaybe 0.0 ride.fare),
            DFODS.cashPlatformFees = if cashPlatformFeeIncrement > 0 then Just cashPlatformFeeIncrement else Nothing,
            DFODS.onlinePlatformFees = if onlinePlatformFeeIncrement > 0 then Just onlinePlatformFeeIncrement else Nothing
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
      initStats <- buildInitialFleetOperatorDailyStats fleetOperatorId merchantLocalDate transporterConfig nowUTCTime
      QFleetOpsDaily.create initStats {DFODS.totalRatingCount = Just 1, DFODS.totalRatingScore = Just ratingValue}

-- Daily: increment online duration (in seconds)
updateOnlineDurationDaily :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> DTTC.TransporterConfig -> Seconds -> Maybe DFODS.FleetOperatorDailyStats -> Day -> m ()
updateOnlineDurationDaily fleetOperatorId transporterConfig newOnlineDuration mbFleetOperatorDailyStats merchantLocalDate = do
  case mbFleetOperatorDailyStats of
    Just _fleetOperatorDailyStats -> QFleetOpsDaily.updateOnlineDurationByFleetOperatorIdAndDate (Just newOnlineDuration) fleetOperatorId merchantLocalDate
    Nothing -> do
      nowUTCTime <- getCurrentTime
      initStats <- buildInitialFleetOperatorDailyStats fleetOperatorId merchantLocalDate transporterConfig nowUTCTime
      QFleetOpsDaily.create initStats {DFODS.onlineDuration = Just newOnlineDuration}
