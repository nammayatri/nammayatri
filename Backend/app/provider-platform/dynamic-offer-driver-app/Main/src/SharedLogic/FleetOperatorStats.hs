module SharedLogic.FleetOperatorStats where

import qualified Data.Map as Map
import Data.Time hiding (getCurrentTime, secondsToNominalDiffTime)
import qualified Domain.Types.Booking as DBooking
import qualified Domain.Types.Extra.MerchantPaymentMethod as DMPM
import qualified Domain.Types.Extra.TransporterConfig as DTCExtra
import qualified Domain.Types.FleetOperatorDailyStats as DFODS
import qualified Domain.Types.FleetOperatorStats as DFS
import qualified Domain.Types.Ride as DR
import qualified Domain.Types.TransporterConfig as DTTC
import Kernel.Prelude hiding (getField)
import qualified Kernel.Storage.Hedis as Redis
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

  -- Update or create driver-level stats (only when driver differs from fleet operator to avoid double-counting)
  when (driverId /= fleetOperatorId) $
    updateOrCreateFleetDriverStats
      fleetOperatorId
      driverId
      merchantLocalDate
      transporterConfig
      nowUTCTime
      mbDriverStats
      (\driverStats -> updateCounter (Just (fromMaybe 0 (getField driverStats) + 1)) fleetOperatorId driverId merchantLocalDate)
      setInitField

incrementApprovedDriverRequestsDaily :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> Text -> DTTC.TransporterConfig -> m ()
incrementApprovedDriverRequestsDaily fleetOperatorId driverId transporterConfig =
  incrementDailyCount
    fleetOperatorId
    driverId
    transporterConfig
    (.approvedDriverRequests)
    QFleetOpsDaily.updateApprovedDriverRequestsByFleetOperatorIdAndDate
    (\s -> s {DFODS.approvedDriverRequests = Just 1})

incrementApprovedVehicleRequests :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> Text -> DTTC.TransporterConfig -> m ()
incrementApprovedVehicleRequests fleetOperatorId driverId transporterConfig =
  incrementDailyCount
    fleetOperatorId
    driverId
    transporterConfig
    (.approvedVehicleRequests)
    QFleetOpsDaily.updateApprovedVehicleRequestsByFleetOperatorIdAndDate
    (\s -> s {DFODS.approvedVehicleRequests = Just 1})

incrementRejectedDriverRequestsDaily :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> Text -> DTTC.TransporterConfig -> m ()
incrementRejectedDriverRequestsDaily fleetOperatorId driverId transporterConfig =
  incrementDailyCount
    fleetOperatorId
    driverId
    transporterConfig
    (.rejectedDriverRequests)
    QFleetOpsDaily.updateRejectedDriverRequestsByFleetOperatorIdAndDate
    (\s -> s {DFODS.rejectedDriverRequests = Just 1})

incrementRejectedVehicleRequestsDaily :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> Text -> DTTC.TransporterConfig -> m ()
incrementRejectedVehicleRequestsDaily fleetOperatorId driverId transporterConfig =
  incrementDailyCount
    fleetOperatorId
    driverId
    transporterConfig
    (.rejectedVehicleRequests)
    QFleetOpsDaily.updateRejectedVehicleRequestsByFleetOperatorIdAndDate
    (\s -> s {DFODS.rejectedVehicleRequests = Just 1})

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

-- | What the completed-ride increments read from the ride and booking. Small and
-- free of customer data, so it can travel on the fleet analytics topic.
data CompletedRideStats = CompletedRideStats
  { rideId :: Text,
    driverId :: Text,
    distance :: Meters,
    fare :: HighPrecMoney,
    platformFee :: HighPrecMoney,
    -- GST or VAT in the fare (fare_parameters.govtCharges); isVatTax says which. Maybe so that
    -- events published before these fields existed still decode.
    taxAmount :: Maybe HighPrecMoney,
    isVatTax :: Maybe Bool,
    isOnlinePayment :: Bool,
    rideDuration :: Maybe Seconds
  }
  deriving (Generic, Show, ToJSON, FromJSON)

mkCompletedRideStats :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => DR.Ride -> DBooking.Booking -> m CompletedRideStats
mkCompletedRideStats ride booking = do
  mbFareParams <- join <$> traverse QFareParameters.findById ride.fareParametersId
  let fee = maybe 0 (\fareParams -> fromMaybe 0 fareParams.platformFee) mbFareParams
  let online = isOnlineRide ride booking
  pure
    CompletedRideStats
      { rideId = ride.id.getId,
        driverId = ride.driverId.getId,
        distance = fromMaybe 0 ride.chargeableDistance,
        fare = fromMaybe 0.0 ride.fare,
        platformFee = fee,
        taxAmount = mbFareParams >>= (.govtCharges),
        isVatTax = mbFareParams >>= (.isVatTaxType),
        isOnlinePayment = online,
        rideDuration = case (ride.tripStartTime, ride.tripEndTime) of
          (Just startTime, Just endTime) -> Just (Seconds (round $ diffUTCTime endTime startTime))
          _ -> Nothing
      }

-- | Whether a ride's money counts in the online (in-app) or cash columns. Cash and booth payments
-- are offline; with no instrument, fall back to the ride's flag. Every per-ride amount (fare, fees,
-- tax, TDS) uses this so they all land in the same bucket.
isOnlineRide :: DR.Ride -> DBooking.Booking -> Bool
isOnlineRide ride booking = case booking.paymentInstrument of
  Just DMPM.Cash -> False
  Just DMPM.BoothOnline -> False
  Nothing -> ride.onlinePayment
  _ -> True

-- | (cash, online) amounts to subtract from gross earnings to get net, per the city's
-- analyticsConfig.netEarningsDeductions. Each kind is counted once however often it is listed.
netEarningsDeductionAmounts ::
  [DTCExtra.EarningsDeduction] ->
  Maybe HighPrecMoney -> -- cash platform fees
  Maybe HighPrecMoney -> -- online platform fees
  QFleetOpsDailyExtra.DeductionSums ->
  (HighPrecMoney, HighPrecMoney)
netEarningsDeductionAmounts deductions cashPlatformFees onlinePlatformFees sums =
  let pick kind (cash, online) = if kind `elem` deductions then (fromMaybe 0 cash, fromMaybe 0 online) else (0, 0)
      parts =
        [ pick DTCExtra.DeductPlatformFees (cashPlatformFees, onlinePlatformFees),
          pick DTCExtra.DeductGST (sums.cashGstSum, sums.onlineGstSum),
          pick DTCExtra.DeductVAT (sums.cashVatSum, sums.onlineVatSum),
          pick DTCExtra.DeductTDS (sums.cashTdsSum, sums.onlineTdsSum)
        ]
   in (sum (map fst parts), sum (map snd parts))

-- Helper: build initial FleetOperatorStats row for an operator from a ride
buildInitialFleetOperatorStats :: (MonadFlow m) => Text -> DTTC.TransporterConfig -> m DFS.FleetOperatorStats
buildInitialFleetOperatorStats fleetOperatorId transporterConfig = do
  now <- getCurrentTime
  pure
    DFS.FleetOperatorStats
      { fleetOperatorId = fleetOperatorId,
        totalRatingCount = Nothing,
        totalRatingScore = Nothing,
        totalAssociatedDriver = Nothing,
        totalActiveDrivers = Nothing,
        totalEnabledDrivers = Nothing,
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

incrementTotalRidesTotalDistAndTotalEarning :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> CompletedRideStats -> DTTC.TransporterConfig -> m ()
incrementTotalRidesTotalDistAndTotalEarning fleetOperatorId rideStats transporterConfig = do
  mbCurrent <- QFleetOps.findByPrimaryKey fleetOperatorId
  case mbCurrent of
    Just s -> do
      let newTotalCompletedRides = Just (fromMaybe 0 s.totalCompletedRides + 1)
          newTotalDistance = Just (fromMaybe 0 s.totalDistance + rideStats.distance)
          newTotalEarning = Just (fromMaybe 0 s.totalEarning + rideStats.fare)
      QFleetOps.updateDistanceEarningAndCompletedRidesByFleetOperatorId newTotalDistance newTotalEarning newTotalCompletedRides fleetOperatorId
    Nothing -> do
      initStats <- buildInitialFleetOperatorStats fleetOperatorId transporterConfig
      QFleetOps.create initStats {DFS.totalDistance = Just rideStats.distance, DFS.totalCompletedRides = Just 1, DFS.totalEarning = Just rideStats.fare}

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
        cashGst = Nothing,
        onlineGst = Nothing,
        cashVat = Nothing,
        onlineVat = Nothing,
        cashTds = Nothing,
        onlineTds = Nothing,
        onlineDuration = Nothing,
        rideDuration = Nothing,
        currency = Just transporterConfig.currency,
        distanceUnit = Just transporterConfig.distanceUnit,
        createdAt = now,
        updatedAt = now,
        merchantId = Just transporterConfig.merchantId,
        merchantOperatingCityId = Just transporterConfig.merchantOperatingCityId,
        approvedDriverRequests = Nothing,
        approvedVehicleRequests = Nothing,
        rejectedDriverRequests = Nothing,
        rejectedVehicleRequests = Nothing
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
incrementTotalEarningDistanceAndCompletedRidesDaily :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> CompletedRideStats -> DTTC.TransporterConfig -> m ()
incrementTotalEarningDistanceAndCompletedRidesDaily fleetOperatorId rideStats transporterConfig = do
  nowUTCTime <- getCurrentTime
  let now = addUTCTime (secondsToNominalDiffTime transporterConfig.timeDiffFromUtc) nowUTCTime
  let merchantLocalDate = utctDay now
  let driverId = rideStats.driverId
      rideDuration = rideStats.rideDuration

  -- Determine cash vs online platform fees based on payment type
  let (cashPlatformFeeIncrement, onlinePlatformFeeIncrement) =
        if rideStats.isOnlinePayment
          then (0, rideStats.platformFee)
          else (rideStats.platformFee, 0)

  -- Split earnings into online and cash based on payment type
  let (onlineEarningIncrement, cashEarningIncrement) =
        if rideStats.isOnlinePayment
          then (rideStats.fare, 0.0)
          else (0.0, rideStats.fare)

  -- Fetch records where fleetDriverId IN [fleetOperatorId, driverId]
  statsList <- QFleetOpsDailyExtra.findByFleetOperatorIdAndDateWithDriverIds fleetOperatorId driverId merchantLocalDate
  let (mbFleetStats, mbDriverStats) = separateFleetAndDriverStats fleetOperatorId driverId statsList

  -- The ride's GST or VAT (whichever regime its fare used), split by payment type like the fare.
  let rideTax = fromMaybe 0 rideStats.taxAmount
      (gstIncrement, vatIncrement) = if fromMaybe False rideStats.isVatTax then (0, rideTax) else (rideTax, 0)
      (onlineGstIncrement, cashGstIncrement) = splitByPayment gstIncrement
      (onlineVatIncrement, cashVatIncrement) = splitByPayment vatIncrement
      splitByPayment amount = if rideStats.isOnlinePayment then (amount, 0) else (0, amount)
      addTo old increment = Just (fromMaybe 0 old + increment)
      initWith increment = if increment > 0 then Just increment else Nothing

  -- Helper function to add this ride onto an existing row
  let addRide stats =
        stats
          { DFODS.totalCompletedRides = Just (fromMaybe 0 stats.totalCompletedRides + 1),
            DFODS.totalDistance = Just (fromMaybe 0 stats.totalDistance + rideStats.distance),
            DFODS.onlineTotalEarning = addTo stats.onlineTotalEarning onlineEarningIncrement,
            DFODS.cashTotalEarning = addTo stats.cashTotalEarning cashEarningIncrement,
            DFODS.cashPlatformFees = addTo stats.cashPlatformFees cashPlatformFeeIncrement,
            DFODS.onlinePlatformFees = addTo stats.onlinePlatformFees onlinePlatformFeeIncrement,
            DFODS.cashGst = addTo stats.cashGst cashGstIncrement,
            DFODS.onlineGst = addTo stats.onlineGst onlineGstIncrement,
            DFODS.cashVat = addTo stats.cashVat cashVatIncrement,
            DFODS.onlineVat = addTo stats.onlineVat onlineVatIncrement,
            DFODS.rideDuration = case (stats.rideDuration, rideDuration) of
              (Just existingDuration, Just newDuration) -> Just (existingDuration + newDuration)
              (Nothing, Just newDuration) -> Just newDuration
              (Just existingDuration, Nothing) -> Just existingDuration
              (Nothing, Nothing) -> Nothing
          }

  -- Helper function to set initial earning/distance/rides
  let setInitEarningDistanceRides stats =
        stats
          { DFODS.totalCompletedRides = Just 1,
            DFODS.totalDistance = Just rideStats.distance,
            DFODS.onlineTotalEarning = initWith onlineEarningIncrement,
            DFODS.cashTotalEarning = initWith cashEarningIncrement,
            DFODS.cashPlatformFees = initWith cashPlatformFeeIncrement,
            DFODS.onlinePlatformFees = initWith onlinePlatformFeeIncrement,
            DFODS.cashGst = initWith cashGstIncrement,
            DFODS.onlineGst = initWith onlineGstIncrement,
            DFODS.cashVat = initWith cashVatIncrement,
            DFODS.onlineVat = initWith onlineVatIncrement,
            DFODS.rideDuration = rideDuration
          }

  let updateRow fleetDriverId stats = do
        let n = addRide stats
        QFleetOpsDaily.updateDistanceEarningAndCompletedRidesByFleetOperatorIdAndDate n.totalDistance n.onlineTotalEarning n.cashTotalEarning n.totalCompletedRides n.cashPlatformFees n.onlinePlatformFees n.cashGst n.onlineGst n.cashVat n.onlineVat n.rideDuration fleetOperatorId fleetDriverId merchantLocalDate

  -- Update or create fleet-level stats
  updateOrCreateFleetDriverStats fleetOperatorId fleetOperatorId merchantLocalDate transporterConfig nowUTCTime mbFleetStats (updateRow fleetOperatorId) setInitEarningDistanceRides

  -- Update or create driver-level stats
  updateOrCreateFleetDriverStats fleetOperatorId driverId merchantLocalDate transporterConfig nowUTCTime mbDriverStats (updateRow driverId) setInitEarningDistanceRides

-- | TDS for one ride, known only once the end-ride finance job has run. It lands on the ride's own
-- merchant-local date, so a job that runs late (or after midnight) still nets against that day's fare.
data TdsDeductedStats = TdsDeductedStats
  { rideId :: Text,
    driverId :: Text,
    tdsAmount :: HighPrecMoney,
    isOnlinePayment :: Bool,
    merchantLocalDate :: Day
  }
  deriving (Generic, Show, ToJSON, FromJSON)

-- Daily: add a ride's TDS to the fleet/operator row and the driver row of the ride's date
incrementTdsDaily :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> TdsDeductedStats -> DTTC.TransporterConfig -> m ()
incrementTdsDaily fleetOperatorId tdsStats transporterConfig = do
  nowUTCTime <- getCurrentTime
  let driverId = tdsStats.driverId
      merchantLocalDate = tdsStats.merchantLocalDate
      (onlineTdsIncrement, cashTdsIncrement) = if tdsStats.isOnlinePayment then (tdsStats.tdsAmount, 0) else (0, tdsStats.tdsAmount)
  statsList <- QFleetOpsDailyExtra.findByFleetOperatorIdAndDateWithDriverIds fleetOperatorId driverId merchantLocalDate
  let (mbFleetStats, mbDriverStats) = separateFleetAndDriverStats fleetOperatorId driverId statsList
      updateRow fleetDriverId stats =
        QFleetOpsDaily.updateTdsByFleetOperatorIdAndDate
          (Just (fromMaybe 0 stats.cashTds + cashTdsIncrement))
          (Just (fromMaybe 0 stats.onlineTds + onlineTdsIncrement))
          fleetOperatorId
          fleetDriverId
          merchantLocalDate
      setInitTds stats =
        stats
          { DFODS.cashTds = if cashTdsIncrement > 0 then Just cashTdsIncrement else Nothing,
            DFODS.onlineTds = if onlineTdsIncrement > 0 then Just onlineTdsIncrement else Nothing
          }
  updateOrCreateFleetDriverStats fleetOperatorId fleetOperatorId merchantLocalDate transporterConfig nowUTCTime mbFleetStats (updateRow fleetOperatorId) setInitTds
  updateOrCreateFleetDriverStats fleetOperatorId driverId merchantLocalDate transporterConfig nowUTCTime mbDriverStats (updateRow driverId) setInitTds

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

-- | Batch increment total request count for multiple fleet owners at once
-- Groups drivers by fleet owner and increments counts in batch
incrementTotalRequestCountBatch ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, Redis.HedisFlow m r) =>
  [(Text, Text)] -> -- List of (fleetOwnerId, driverId) pairs
  DTTC.TransporterConfig ->
  -- | Runs the Postgres writes, then the Redis counters, inside the entity lock.
  (Text -> m () -> m () -> m ()) ->
  -- | Redis counters. Runs after the overall and daily Postgres rows, with the driver count added.
  (Text -> Int -> m ()) ->
  m ()
incrementTotalRequestCountBatch fleetDriverPairs transporterConfig claimEntity afterOverallWrite = do
  nowUTCTime <- getCurrentTime
  let now = addUTCTime (secondsToNominalDiffTime transporterConfig.timeDiffFromUtc) nowUTCTime
  let merchantLocalDate = utctDay now

  -- Group by fleet owner
  let groupedByFleet = groupByFleetOwner fleetDriverPairs

  -- For each fleet owner, update stats under a Redis lock to avoid races
  forM_ groupedByFleet $ \(fleetOwnerId, driverIds) ->
    Redis.withWaitAndLockRedis (makeFleetOperatorMetricLockKey fleetOwnerId) 10 5000 $ do
      let driverCount = length driverIds
      claimEntity fleetOwnerId (writeRequestCounts fleetOwnerId driverIds driverCount nowUTCTime merchantLocalDate) (afterOverallWrite fleetOwnerId driverCount)
  where
    writeRequestCounts fleetOwnerId driverIds driverCount nowUTCTime merchantLocalDate = do
      -- Update or create overall stats (increment by number of drivers)
      mbFleetStats <- QFleetOps.findByPrimaryKey fleetOwnerId
      case mbFleetStats of
        Just s -> do
          let newTotalRequestCount = Just (fromMaybe 0 s.totalRequestCount + driverCount)
          QFleetOps.updateRequestCountsByFleetOperatorId s.acceptationRequestCount newTotalRequestCount fleetOwnerId
        Nothing -> do
          initStats <- buildInitialFleetOperatorStats fleetOwnerId transporterConfig
          QFleetOps.create initStats {DFS.totalRequestCount = Just driverCount}

      -- Fetch daily stats for fleet-level and all drivers
      let allFleetDriverIds = fleetOwnerId : driverIds
      existingDailyStats <- QFleetOpsDailyExtra.findByFleetOperatorIdAndDateWithDriverIdsIn fleetOwnerId allFleetDriverIds merchantLocalDate
      let existingStatsMap = Map.fromList [(s.fleetDriverId, s) | s <- existingDailyStats]

      -- Update fleet-level daily stats
      let mbFleetDailyStats = Map.lookup fleetOwnerId existingStatsMap
      case mbFleetDailyStats of
        Just fleetStats -> do
          let newTotal = Just (fromMaybe 0 fleetStats.totalRequestCount + driverCount)
          QFleetOpsDaily.updateRequestCountsByFleetOperatorIdAndDate fleetStats.rejectedRequestCount fleetStats.pulledRequestCount fleetStats.acceptationRequestCount newTotal fleetOwnerId fleetOwnerId merchantLocalDate
        Nothing -> do
          initStats <- buildInitialFleetOperatorDailyStats fleetOwnerId fleetOwnerId merchantLocalDate transporterConfig nowUTCTime
          QFleetOpsDaily.create initStats {DFODS.totalRequestCount = Just driverCount}

      -- Update each driver's daily stats
      forM_ driverIds $ \driverId -> do
        let mbDriverDailyStats = Map.lookup driverId existingStatsMap
        case mbDriverDailyStats of
          Just driverStats -> do
            let newTotal = Just (fromMaybe 0 driverStats.totalRequestCount + 1)
            QFleetOpsDaily.updateRequestCountsByFleetOperatorIdAndDate driverStats.rejectedRequestCount driverStats.pulledRequestCount driverStats.acceptationRequestCount newTotal fleetOwnerId driverId merchantLocalDate
          Nothing -> do
            initStats <- buildInitialFleetOperatorDailyStats fleetOwnerId driverId merchantLocalDate transporterConfig nowUTCTime
            QFleetOpsDaily.create initStats {DFODS.totalRequestCount = Just 1}
    groupByFleetOwner :: [(Text, Text)] -> [(Text, [Text])]
    groupByFleetOwner pairs =
      let grouped = Map.fromListWith (++) [(foid, [did]) | (foid, did) <- pairs]
       in Map.toList grouped
