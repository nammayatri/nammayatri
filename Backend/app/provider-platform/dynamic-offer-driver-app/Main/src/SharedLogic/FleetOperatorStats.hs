module SharedLogic.FleetOperatorStats where

import Data.Time hiding (getCurrentTime, secondsToNominalDiffTime)
import qualified Domain.Types.Booking as DBooking
import qualified Domain.Types.Extra.MerchantPaymentMethod as DMPM
import qualified Domain.Types.FleetOperatorDailyStats as DFODS
import qualified Domain.Types.FleetOperatorStats as DFS
import qualified Domain.Types.Ride as DR
import qualified Domain.Types.TransporterConfig as DTTC
import Kernel.Prelude hiding (getField)
import Kernel.Utils.Common
import qualified Storage.Queries.FareParameters as QFareParameters
import qualified Storage.Queries.FleetOperatorDailyStats as QFleetOpsDaily
import qualified Storage.Queries.FleetOperatorDailyStatsExtra as QFleetOpsDailyExtra
import qualified Storage.Queries.FleetOperatorStats as QFleetOps

-- Helper: Separate fleet and driver records from a list
-- Returns (Maybe FleetOperatorDailyStats, Maybe FleetOperatorDailyStats)
-- where first is fleet data (fleetDriverId == fleetOperatorId) and second is driver data
separateFleetAndDriverStats ::
  Text -> -- fleetOperatorId
  Text -> -- driverId
  [DFODS.FleetOperatorDailyStats] ->
  (Maybe DFODS.FleetOperatorDailyStats, Maybe DFODS.FleetOperatorDailyStats)
separateFleetAndDriverStats fleetOperatorId driverId statsList =
  let fleetStats = find (\s -> s.fleetDriverId == fleetOperatorId) statsList
      driverStats = find (\s -> s.fleetDriverId == driverId) statsList
   in (fleetStats, driverStats)

-- Helper: Update or create fleet/driver stats
-- Takes mbStats and handles both update and create cases
updateOrCreateFleetDriverStats ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Text -> -- fleetOperatorId
  Text -> -- fleetDriverId
  Day -> -- merchantLocalDate
  DTTC.TransporterConfig ->
  UTCTime ->
  Maybe DFODS.FleetOperatorDailyStats ->
  (DFODS.FleetOperatorDailyStats -> m ()) -> -- Update function that takes existing stats
  (DFODS.FleetOperatorDailyStats -> DFODS.FleetOperatorDailyStats) -> -- Modify function for initial stats
  m ()
updateOrCreateFleetDriverStats fleetOperatorId fleetDriverId merchantLocalDate transporterConfig nowUTCTime mbStats updateFn modifyInitFn =
  case mbStats of
    Just stats -> updateFn stats
    Nothing -> do
      initStats <- buildInitialFleetOperatorDailyStats fleetOperatorId fleetDriverId merchantLocalDate transporterConfig nowUTCTime
      QFleetOpsDaily.create (modifyInitFn initStats)

-- Generic helpers to reduce duplication for simple count increments
incrementDailyCount ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Text -> -- Fleet operator id
  Text -> -- Driver id
  DTTC.TransporterConfig -> -- Transporter config (for tz & merchant info)
  (DFODS.FleetOperatorDailyStats -> Maybe Int) -> -- Getter for the counter from existing row
  (Maybe Int -> Text -> Text -> Day -> m ()) -> -- Update action for that counter (value -> fleetOperatorId -> fleetDriverId -> merchantLocalDate -> m ())
  (DFODS.FleetOperatorDailyStats -> DFODS.FleetOperatorDailyStats) -> -- Setter for initial row (set counter = Just 1)
  m ()
incrementDailyCount fleetOperatorId driverId transporterConfig getField updateCounter setInitField = do
  nowUTCTime <- getCurrentTime
  let now = addUTCTime (secondsToNominalDiffTime transporterConfig.timeDiffFromUtc) nowUTCTime
  let merchantLocalDate = utctDay now
  -- Fetch records where fleetDriverId IN [fleetOperatorId, driverId]
  statsList <- QFleetOpsDailyExtra.findByFleetOperatorIdAndDateWithDriverIds fleetOperatorId driverId merchantLocalDate
  let (mbFleetStats, mbDriverStats) = separateFleetAndDriverStats fleetOperatorId driverId statsList

  -- Update or create fleet-level stats
  updateOrCreateFleetDriverStats
    fleetOperatorId
    fleetOperatorId
    merchantLocalDate
    transporterConfig
    nowUTCTime
    mbFleetStats
    (\fleetStats -> updateCounter (Just (fromMaybe 0 (getField fleetStats) + 1)) fleetOperatorId fleetOperatorId merchantLocalDate)
    setInitField

  -- Update or create driver-level stats
  updateOrCreateFleetDriverStats
    fleetOperatorId
    driverId
    merchantLocalDate
    transporterConfig
    nowUTCTime
    mbDriverStats
    (\driverStats -> updateCounter (Just (fromMaybe 0 (getField driverStats) + 1)) fleetOperatorId driverId merchantLocalDate)
    setInitField

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
buildInitialFleetOperatorDailyStats :: (MonadFlow m) => Text -> Text -> Day -> DTTC.TransporterConfig -> UTCTime -> m DFODS.FleetOperatorDailyStats
buildInitialFleetOperatorDailyStats fleetOperatorId fleetDriverId merchantLocalDate transporterConfig now = do
  pure
    DFODS.FleetOperatorDailyStats
      { fleetOperatorId = fleetOperatorId,
        fleetDriverId = fleetDriverId,
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
        onlineTotalEarning = Nothing,
        cashTotalEarning = Nothing,
        cashPlatformFees = Nothing,
        onlinePlatformFees = Nothing,
        onlineDuration = Nothing,
        rideDuration = Nothing,
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
  Text ->
  DTTC.TransporterConfig ->
  Bool ->
  Bool ->
  Bool ->
  Bool ->
  m ()
incrementRequestCountsDaily fleetOperatorId driverId transporterConfig incrementAcceptationCount incrementTotalCount incrementRejectedCount incrementPulledCount = do
  nowUTCTime <- getCurrentTime
  let now = addUTCTime (secondsToNominalDiffTime transporterConfig.timeDiffFromUtc) nowUTCTime
  let merchantLocalDate = utctDay now
  -- Fetch records where fleetDriverId IN [fleetOperatorId, driverId]
  statsList <- QFleetOpsDailyExtra.findByFleetOperatorIdAndDateWithDriverIds fleetOperatorId driverId merchantLocalDate
  let (mbFleetStats, mbDriverStats) = separateFleetAndDriverStats fleetOperatorId driverId statsList

  -- Helper function to compute new request counts
  let computeNewRequestCounts stats =
        let newRejectedRequestCount = if incrementRejectedCount then Just (fromMaybe 0 stats.rejectedRequestCount + 1) else stats.rejectedRequestCount
            newPulledRequestCount = if incrementPulledCount then Just (fromMaybe 0 stats.pulledRequestCount + 1) else stats.pulledRequestCount
            newAcceptationRequestCount = if incrementAcceptationCount then Just (fromMaybe 0 stats.acceptationRequestCount + 1) else stats.acceptationRequestCount
            newTotalRequestCount = if incrementTotalCount then Just (fromMaybe 0 stats.totalRequestCount + 1) else stats.totalRequestCount
         in (newRejectedRequestCount, newPulledRequestCount, newAcceptationRequestCount, newTotalRequestCount)

  -- Helper function to set initial request counts
  let setInitRequestCounts stats =
        stats
          { DFODS.rejectedRequestCount = if incrementRejectedCount then Just 1 else Nothing,
            DFODS.pulledRequestCount = if incrementPulledCount then Just 1 else Nothing,
            DFODS.acceptationRequestCount = if incrementAcceptationCount then Just 1 else Nothing,
            DFODS.totalRequestCount = if incrementTotalCount then Just 1 else Nothing
          }

  -- Update or create fleet-level stats
  updateOrCreateFleetDriverStats
    fleetOperatorId
    fleetOperatorId
    merchantLocalDate
    transporterConfig
    nowUTCTime
    mbFleetStats
    ( \fleetStats -> do
        let (newRejected, newPulled, newAcceptation, newTotal) = computeNewRequestCounts fleetStats
        QFleetOpsDaily.updateRequestCountsByFleetOperatorIdAndDate newRejected newPulled newAcceptation newTotal fleetOperatorId fleetOperatorId merchantLocalDate
    )
    setInitRequestCounts

  -- Update or create driver-level stats
  updateOrCreateFleetDriverStats
    fleetOperatorId
    driverId
    merchantLocalDate
    transporterConfig
    nowUTCTime
    mbDriverStats
    ( \driverStats -> do
        let (newRejected, newPulled, newAcceptation, newTotal) = computeNewRequestCounts driverStats
        QFleetOpsDaily.updateRequestCountsByFleetOperatorIdAndDate newRejected newPulled newAcceptation newTotal fleetOperatorId driverId merchantLocalDate
    )
    setInitRequestCounts

-- Daily: increment DriverCancellationCount
incrementDriverCancellationCountDaily :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> Text -> DTTC.TransporterConfig -> m ()
incrementDriverCancellationCountDaily fleetOperatorId driverId transporterConfig =
  incrementDailyCount
    fleetOperatorId
    driverId
    transporterConfig
    (.driverCancellationCount)
    QFleetOpsDaily.updateDriverCancellationCountByFleetOperatorIdAndDate
    (\s -> s {DFODS.driverCancellationCount = Just 1})

-- Daily: increment CustomerCancellationCount
incrementCustomerCancellationCountDaily :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> Text -> DTTC.TransporterConfig -> m ()
incrementCustomerCancellationCountDaily fleetOperatorId driverId transporterConfig =
  incrementDailyCount
    fleetOperatorId
    driverId
    transporterConfig
    (.customerCancellationCount)
    QFleetOpsDaily.updateCustomerCancellationCountByFleetOperatorIdAndDate
    (\s -> s {DFODS.customerCancellationCount = Just 1})

-- Daily: increment totals for earning, distance, and completed rides (aka ride completed)
incrementTotalEarningDistanceAndCompletedRidesDaily :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> DR.Ride -> DBooking.Booking -> DTTC.TransporterConfig -> m ()
incrementTotalEarningDistanceAndCompletedRidesDaily fleetOperatorId ride booking transporterConfig = do
  nowUTCTime <- getCurrentTime
  let now = addUTCTime (secondsToNominalDiffTime transporterConfig.timeDiffFromUtc) nowUTCTime
  let merchantLocalDate = utctDay now
  let driverId = ride.driverId.getId

  -- Get platform fees from FareParameters if available
  platformFeeTotal <- case ride.fareParametersId of
    Just fareParamsId -> do
      mbFareParams <- QFareParameters.findById fareParamsId
      pure $ case mbFareParams of
        Just fareParams -> fromMaybe 0 fareParams.platformFee
        Nothing -> 0
    Nothing -> pure 0

  -- Determine if payment is online or cash based on booking.paymentInstrument
  -- If booking.paymentInstrument is Cash, it's cash payment
  -- If booking.paymentInstrument is Nothing, check merchant.onlinePayment
  -- Otherwise, it's online payment
  let isOnlinePayment = case booking.paymentInstrument of
        Just DMPM.Cash -> False
        Nothing -> ride.onlinePayment
        _ -> True

  -- Determine cash vs online platform fees based on payment type
  let (cashPlatformFeeIncrement, onlinePlatformFeeIncrement) =
        if isOnlinePayment
          then (0, platformFeeTotal)
          else (platformFeeTotal, 0)

  -- Calculate ride duration from tripStartTime and tripEndTime
  let rideDuration = case (ride.tripStartTime, ride.tripEndTime) of
        (Just startTime, Just endTime) -> Just (Seconds (round $ diffUTCTime endTime startTime))
        _ -> Nothing

  -- Split earnings into online and cash based on payment type
  let rideFare = fromMaybe 0.0 ride.fare
      (onlineEarningIncrement, cashEarningIncrement) =
        if isOnlinePayment
          then (rideFare, 0.0)
          else (0.0, rideFare)

  -- Fetch records where fleetDriverId IN [fleetOperatorId, driverId]
  statsList <- QFleetOpsDailyExtra.findByFleetOperatorIdAndDateWithDriverIds fleetOperatorId driverId merchantLocalDate
  let (mbFleetStats, mbDriverStats) = separateFleetAndDriverStats fleetOperatorId driverId statsList

  -- Helper function to compute new earning/distance/rides
  let computeNewEarningDistanceRides stats =
        let newTotalCompletedRides = Just (fromMaybe 0 stats.totalCompletedRides + 1)
            newTotalDistance = Just (fromMaybe 0 stats.totalDistance + fromMaybe 0 ride.chargeableDistance)
            newOnlineTotalEarning = Just (fromMaybe 0 stats.onlineTotalEarning + onlineEarningIncrement)
            newCashTotalEarning = Just (fromMaybe 0 stats.cashTotalEarning + cashEarningIncrement)
            newCashPlatformFees = Just (fromMaybe 0 stats.cashPlatformFees + cashPlatformFeeIncrement)
            newOnlinePlatformFees = Just (fromMaybe 0 stats.onlinePlatformFees + onlinePlatformFeeIncrement)
            newRideDuration = case (stats.rideDuration, rideDuration) of
              (Just existingDuration, Just newDuration) -> Just (existingDuration + newDuration)
              (Nothing, Just newDuration) -> Just newDuration
              (Just existingDuration, Nothing) -> Just existingDuration
              (Nothing, Nothing) -> Nothing
         in (newTotalDistance, newOnlineTotalEarning, newCashTotalEarning, newTotalCompletedRides, newCashPlatformFees, newOnlinePlatformFees, newRideDuration)

  -- Helper function to set initial earning/distance/rides
  let setInitEarningDistanceRides stats =
        stats
          { DFODS.totalCompletedRides = Just 1,
            DFODS.totalDistance = Just (fromMaybe 0 ride.chargeableDistance),
            DFODS.onlineTotalEarning = if onlineEarningIncrement > 0 then Just onlineEarningIncrement else Nothing,
            DFODS.cashTotalEarning = if cashEarningIncrement > 0 then Just cashEarningIncrement else Nothing,
            DFODS.cashPlatformFees = if cashPlatformFeeIncrement > 0 then Just cashPlatformFeeIncrement else Nothing,
            DFODS.onlinePlatformFees = if onlinePlatformFeeIncrement > 0 then Just onlinePlatformFeeIncrement else Nothing,
            DFODS.rideDuration = rideDuration
          }

  -- Update or create fleet-level stats
  updateOrCreateFleetDriverStats
    fleetOperatorId
    fleetOperatorId
    merchantLocalDate
    transporterConfig
    nowUTCTime
    mbFleetStats
    ( \fleetStats -> do
        let (newDistance, newOnlineEarning, newCashEarning, newRides, newCash, newOnline, newRideDur) = computeNewEarningDistanceRides fleetStats
        QFleetOpsDaily.updateDistanceEarningAndCompletedRidesByFleetOperatorIdAndDate newDistance newOnlineEarning newCashEarning newRides newCash newOnline newRideDur fleetOperatorId fleetOperatorId merchantLocalDate
    )
    setInitEarningDistanceRides

  -- Update or create driver-level stats
  updateOrCreateFleetDriverStats
    fleetOperatorId
    driverId
    merchantLocalDate
    transporterConfig
    nowUTCTime
    mbDriverStats
    ( \driverStats -> do
        let (newDistance, newOnlineEarning, newCashEarning, newRides, newCash, newOnline, newRideDur) = computeNewEarningDistanceRides driverStats
        QFleetOpsDaily.updateDistanceEarningAndCompletedRidesByFleetOperatorIdAndDate newDistance newOnlineEarning newCashEarning newRides newCash newOnline newRideDur fleetOperatorId driverId merchantLocalDate
    )
    setInitEarningDistanceRides

-- Daily: increment total rating count and total rating score
incrementTotalRatingCountAndTotalRatingScoreDaily :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> Text -> DTTC.TransporterConfig -> Int -> Bool -> m ()
incrementTotalRatingCountAndTotalRatingScoreDaily fleetOperatorId driverId transporterConfig ratingValue shouldIncrementCount = do
  nowUTCTime <- getCurrentTime
  let now = addUTCTime (secondsToNominalDiffTime transporterConfig.timeDiffFromUtc) nowUTCTime
  let merchantLocalDate = utctDay now
  -- Fetch records where fleetDriverId IN [fleetOperatorId, driverId]
  statsList <- QFleetOpsDailyExtra.findByFleetOperatorIdAndDateWithDriverIds fleetOperatorId driverId merchantLocalDate
  let (mbFleetStats, mbDriverStats) = separateFleetAndDriverStats fleetOperatorId driverId statsList

  -- Helper function to compute new rating counts
  let computeNewRatingCounts stats =
        let newTotalRatingCount = Just (fromMaybe 0 stats.totalRatingCount + if shouldIncrementCount then 1 else 0)
            newTotalRatingScore = Just (fromMaybe 0 stats.totalRatingScore + ratingValue)
         in (newTotalRatingCount, newTotalRatingScore)

  -- Helper function to set initial rating counts
  let setInitRatingCounts stats = stats {DFODS.totalRatingCount = Just 1, DFODS.totalRatingScore = Just ratingValue}

  -- Update or create fleet-level stats
  updateOrCreateFleetDriverStats
    fleetOperatorId
    fleetOperatorId
    merchantLocalDate
    transporterConfig
    nowUTCTime
    mbFleetStats
    ( \fleetStats -> do
        let (newRatingCount, newRatingScore) = computeNewRatingCounts fleetStats
        QFleetOpsDaily.updateTotalRatingCountAndTotalRatingScoreByFleetOperatorIdAndDate newRatingCount newRatingScore fleetOperatorId fleetOperatorId merchantLocalDate
    )
    setInitRatingCounts

  -- Update or create driver-level stats
  updateOrCreateFleetDriverStats
    fleetOperatorId
    driverId
    merchantLocalDate
    transporterConfig
    nowUTCTime
    mbDriverStats
    ( \driverStats -> do
        let (newRatingCount, newRatingScore) = computeNewRatingCounts driverStats
        QFleetOpsDaily.updateTotalRatingCountAndTotalRatingScoreByFleetOperatorIdAndDate newRatingCount newRatingScore fleetOperatorId driverId merchantLocalDate
    )
    setInitRatingCounts

-- Daily: update online duration (in seconds)
updateOnlineDurationDaily :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> Text -> DTTC.TransporterConfig -> Seconds -> Seconds -> Maybe DFODS.FleetOperatorDailyStats -> Maybe DFODS.FleetOperatorDailyStats -> Day -> m ()
updateOnlineDurationDaily fleetOperatorId driverId transporterConfig newOnlineDurationFleet newOnlineDurationDriver mbFleetStats mbDriverStats merchantLocalDate = do
  nowUTCTime <- getCurrentTime

  -- Update or create fleet-level stats
  updateOrCreateFleetDriverStats
    fleetOperatorId
    fleetOperatorId
    merchantLocalDate
    transporterConfig
    nowUTCTime
    mbFleetStats
    (\_fleetStats -> QFleetOpsDaily.updateOnlineDurationByFleetOperatorIdAndDate (Just newOnlineDurationFleet) fleetOperatorId fleetOperatorId merchantLocalDate)
    (\stats -> stats {DFODS.onlineDuration = Just newOnlineDurationFleet})

  -- Update or create driver-level stats
  updateOrCreateFleetDriverStats
    fleetOperatorId
    driverId
    merchantLocalDate
    transporterConfig
    nowUTCTime
    mbDriverStats
    (\_driverStats -> QFleetOpsDaily.updateOnlineDurationByFleetOperatorIdAndDate (Just newOnlineDurationDriver) fleetOperatorId driverId merchantLocalDate)
    (\stats -> stats {DFODS.onlineDuration = Just newOnlineDurationDriver})
