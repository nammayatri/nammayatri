{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Clickhouse.FleetOperatorDailyStats where

import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Fleet.Driver as Common
import Data.Time.Calendar (Day)
import Kernel.Prelude
import Kernel.Storage.ClickhouseV2 as CH
import qualified Kernel.Storage.ClickhouseV2.Internal.Types as CH
import qualified Kernel.Storage.ClickhouseV2.UtilsTH as TH
import Kernel.Types.Common
import qualified Storage.Queries.FleetOperatorDailyStatsExtra as FODSE

-- Minimal ClickHouse mapping for the query we need
data FleetOperatorDailyStatsT f = FleetOperatorDailyStatsT
  { fleetOperatorId :: C f Text,
    fleetDriverId :: C f Text,
    merchantLocalDate :: C f Day,
    rejectedRequestCount :: C f (Maybe Int),
    pulledRequestCount :: C f (Maybe Int),
    acceptationRequestCount :: C f (Maybe Int),
    totalRequestCount :: C f (Maybe Int),
    customerCancellationCount :: C f (Maybe Int),
    driverCancellationCount :: C f (Maybe Int),
    totalDistance :: C f (Maybe Meters),
    totalCompletedRides :: C f (Maybe Int),
    onlineTotalEarning :: C f (Maybe HighPrecMoney),
    cashTotalEarning :: C f (Maybe HighPrecMoney),
    cashPlatformFees :: C f (Maybe HighPrecMoney),
    onlinePlatformFees :: C f (Maybe HighPrecMoney),
    onlineDuration :: C f (Maybe Seconds),
    totalRatingScore :: C f (Maybe Int),
    rideDuration :: C f (Maybe Seconds)
  }
  deriving (Generic)

deriving instance Show FleetOperatorDailyStats

fleetOperatorDailyStatsTTable :: FleetOperatorDailyStatsT (FieldModification FleetOperatorDailyStatsT)
fleetOperatorDailyStatsTTable =
  FleetOperatorDailyStatsT
    { fleetOperatorId = "fleet_operator_id",
      fleetDriverId = "fleet_driver_id",
      merchantLocalDate = "merchant_local_date",
      rejectedRequestCount = "rejected_request_count",
      pulledRequestCount = "pulled_request_count",
      acceptationRequestCount = "acceptation_request_count",
      totalRequestCount = "total_request_count",
      customerCancellationCount = "customer_cancellation_count",
      driverCancellationCount = "driver_cancellation_count",
      totalDistance = "total_distance",
      totalCompletedRides = "total_completed_rides",
      onlineTotalEarning = "online_total_earning",
      cashTotalEarning = "cash_total_earning",
      cashPlatformFees = "cash_platform_fees",
      onlinePlatformFees = "online_platform_fees",
      onlineDuration = "online_duration",
      totalRatingScore = "total_rating_score",
      rideDuration = "ride_duration"
    }

type FleetOperatorDailyStats = FleetOperatorDailyStatsT Identity

instance CH.ClickhouseValue Seconds where
  fromClickhouseValue = parseAsStringOrNumber @Seconds

$(TH.mkClickhouseInstances ''FleetOperatorDailyStatsT 'SELECT_FINAL_MODIFIER)

data DailyFleetMetricsAggregated = DailyFleetMetricsAggregated
  { totalEarningSum :: Maybe HighPrecMoney,
    totalCompletedRidesSum :: Maybe Int,
    totalDistanceSum :: Maybe Meters,
    totalRequestCountSum :: Maybe Int,
    rejectedRequestCountSum :: Maybe Int,
    pulledRequestCountSum :: Maybe Int,
    acceptationRequestCountSum :: Maybe Int,
    driverCancellationCountSum :: Maybe Int,
    customerCancellationCountSum :: Maybe Int
  }
  deriving (Show, Generic)

mkDailyFleetMetricsAggregated ::
  ( Maybe HighPrecMoney,
    Maybe HighPrecMoney,
    Maybe Int,
    Maybe Meters,
    Maybe Int,
    Maybe Int,
    Maybe Int,
    Maybe Int,
    Maybe Int,
    Maybe Int
  ) ->
  DailyFleetMetricsAggregated
mkDailyFleetMetricsAggregated (ote, cte, cr, td, tr, rr, pr, ar, dc, cc) =
  DailyFleetMetricsAggregated
    { totalEarningSum = FODSE.getTotalEarningSum ote cte,
      totalCompletedRidesSum = cr,
      totalDistanceSum = td,
      totalRequestCountSum = tr,
      rejectedRequestCountSum = rr,
      pulledRequestCountSum = pr,
      acceptationRequestCountSum = ar,
      driverCancellationCountSum = dc,
      customerCancellationCountSum = cc
    }

sumFleetMetricsByFleetOwnerIdAndDateRange ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  Text ->
  Day ->
  Day ->
  m DailyFleetMetricsAggregated
sumFleetMetricsByFleetOwnerIdAndDateRange fleetOwnerId fromDay toDay = do
  res <-
    CH.findAll $
      CH.select_
        ( \fos ->
            CH.aggregate
              ( CH.sum_ fos.onlineTotalEarning,
                CH.sum_ fos.cashTotalEarning,
                CH.sum_ fos.totalCompletedRides,
                CH.sum_ fos.totalDistance,
                CH.sum_ fos.totalRequestCount,
                CH.sum_ fos.rejectedRequestCount,
                CH.sum_ fos.pulledRequestCount,
                CH.sum_ fos.acceptationRequestCount,
                CH.sum_ fos.driverCancellationCount,
                CH.sum_ fos.customerCancellationCount
              )
        )
        $ CH.filter_
          ( \fos ->
              fos.fleetOperatorId CH.==. fleetOwnerId
                CH.&&. fos.fleetDriverId CH.==. fleetOwnerId
                CH.&&. fos.merchantLocalDate CH.>=. fromDay
                CH.&&. fos.merchantLocalDate CH.<=. toDay
          )
          (CH.all_ @CH.APP_SERVICE_CLICKHOUSE fleetOperatorDailyStatsTTable)
  pure $ maybe (DailyFleetMetricsAggregated Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) mkDailyFleetMetricsAggregated (listToMaybe res)

sumFleetEarningsByFleetOwnerIdAndDateRange ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  Text ->
  Day ->
  Day ->
  m FODSE.DailyFleetEarningsAggregated
sumFleetEarningsByFleetOwnerIdAndDateRange fleetOwnerId fromDay toDay = do
  res <-
    CH.findAll $
      CH.select_
        ( \fos ->
            CH.aggregate
              ( CH.sum_ fos.onlineTotalEarning,
                CH.sum_ fos.cashTotalEarning,
                CH.sum_ fos.cashPlatformFees,
                CH.sum_ fos.onlinePlatformFees,
                CH.sum_ fos.onlineDuration
              )
        )
        $ CH.filter_
          ( \fos ->
              fos.fleetOperatorId CH.==. fleetOwnerId
                CH.&&. fos.fleetDriverId CH.==. fleetOwnerId
                CH.&&. fos.merchantLocalDate CH.>=. fromDay
                CH.&&. fos.merchantLocalDate CH.<=. toDay
          )
          (CH.all_ @CH.APP_SERVICE_CLICKHOUSE fleetOperatorDailyStatsTTable)
  pure $ maybe (FODSE.DailyFleetEarningsAggregated Nothing Nothing Nothing Nothing) FODSE.mkDailyFleetEarningsAggregated (listToMaybe res)

sumDriverEarningsByFleetOwnerIdAndDriverIds ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  Text ->
  [Text] ->
  Day ->
  Day ->
  Int ->
  Int ->
  Maybe Bool ->
  Maybe Common.FleetDriverListStatsSortOn ->
  m [FODSE.DriverEarningsAggregated]
sumDriverEarningsByFleetOwnerIdAndDriverIds fleetOwnerId driverIds fromDay toDay limit offset mbSortDesc mbSortOn = do
  let sortOn _fos (_, _, onlineTotalEarning, cashTotalEarning, cashPlatformFees, onlinePlatformFees, onlineDuration) = case mbSortOn of
        Just Common.ONLINE_TOTAL_EARNING -> castSortBy mbSortDesc onlineTotalEarning
        Just Common.CASH_TOTAL_EARNING -> castSortBy mbSortDesc cashTotalEarning
        Just Common.CASH_PLATFORM_FEES -> castSortBy mbSortDesc cashPlatformFees
        Just Common.ONLINE_PLATFORM_FEES -> castSortBy mbSortDesc onlinePlatformFees
        Just Common.ONLINE_DURATION -> castSortBy mbSortDesc onlineDuration
        _ -> castSortBy mbSortDesc onlineTotalEarning

  res <-
    CH.findAll $
      CH.select_
        ( \fos -> do
            let onlineTotalEarning = CH.sum_ fos.onlineTotalEarning
                cashTotalEarning = CH.sum_ fos.cashTotalEarning
                cashPlatformFees = CH.sum_ fos.cashPlatformFees
                onlinePlatformFees = CH.sum_ fos.onlinePlatformFees
                onlineDuration = CH.sum_ fos.onlineDuration
            CH.groupBy (fos.fleetOperatorId, fos.fleetDriverId) $ \(fleetOperatorId, fleetDriverId) -> do
              (fleetOperatorId, fleetDriverId, onlineTotalEarning, cashTotalEarning, cashPlatformFees, onlinePlatformFees, onlineDuration)
        )
        $ CH.orderBy_ sortOn $
          CH.limit_ limit $
            CH.offset_ offset $
              CH.filter_
                ( \fos ->
                    fos.fleetOperatorId CH.==. fleetOwnerId
                      CH.&&. fos.fleetDriverId `in_` driverIds
                      CH.&&. fos.merchantLocalDate CH.>=. fromDay
                      CH.&&. fos.merchantLocalDate CH.<=. toDay
                )
                (CH.all_ @CH.APP_SERVICE_CLICKHOUSE fleetOperatorDailyStatsTTable)
  pure $ map (\(_fleetOperatorId, fleetDriverId, onlineTotalEarning, cashTotalEarning, cashPlatformFees, onlinePlatformFees, onlineDuration) -> FODSE.mkDriverEarningsAggregated (fleetDriverId, onlineTotalEarning, cashTotalEarning, cashPlatformFees, onlinePlatformFees, onlineDuration)) res

castSortBy :: forall ord. (CH.ClickhouseQuery ord, CH.IsOrderColumns ord) => Maybe Bool -> ord -> CH.OrderBy 'CH.ORDERED
castSortBy mbSortDesc = case mbSortDesc of
  Just True -> CH.desc @ord
  _ -> CH.asc @ord

mkDriverMetricsAggregated ::
  ( Text, -- fleetOperatorId
    Text,
    (Maybe HighPrecMoney, Maybe HighPrecMoney, Maybe Int, Maybe Meters),
    (Maybe Int, Maybe Int, Maybe Int, Maybe Int),
    (Maybe Int, Maybe Int, Maybe Seconds, Maybe Int),
    Maybe Seconds
  ) ->
  FODSE.DriverMetricsAggregated
mkDriverMetricsAggregated (_, driverId, (te, ct, cr, td), (tr, rr, pr, ar), (dc, cc, od, trs), rd) =
  FODSE.DriverMetricsAggregated
    { driverId = driverId,
      onlineTotalEarningSum = te,
      cashTotalEarningSum = ct,
      totalCompletedRidesSum = cr,
      totalDistanceSum = td,
      totalRequestCountSum = tr,
      rejectedRequestCountSum = rr,
      pulledRequestCountSum = pr,
      acceptationRequestCountSum = ar,
      driverCancellationCountSum = dc,
      customerCancellationCountSum = cc,
      onlineDurationSum = od,
      totalRatingScoreSum = trs,
      rideDurationSum = rd
    }

sumDriverMetricsByFleetOwnerIdAndDriverIds ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  Text ->
  [Text] ->
  Day ->
  Day ->
  Int ->
  Int ->
  Maybe Bool ->
  Maybe Common.FleetDriverListStatsSortOn ->
  m [FODSE.DriverMetricsAggregated]
sumDriverMetricsByFleetOwnerIdAndDriverIds fleetOwnerId driverIds fromDay toDay limit offset mbSortDesc mbSortOn = do
  let sortOn _fos (_, _, onlineTotalEarning, cashTotalEarning, completedRides, totalDistance, totalRequestCount, rejectedRequestCount, pulledRequestCount, acceptationRequestCount, driverCancellationCount, customerCancellationCount, onlineDuration, totalRatingScore, rideDuration) = case mbSortOn of
        Just Common.ONLINE_TOTAL_EARNING -> castSortBy mbSortDesc onlineTotalEarning
        Just Common.CASH_TOTAL_EARNING -> castSortBy mbSortDesc cashTotalEarning
        Just Common.TOTAL_COMPLETED_RIDES -> castSortBy mbSortDesc completedRides
        Just Common.TOTAL_DISTANCE -> castSortBy mbSortDesc totalDistance
        Just Common.TOTAL_REQUEST_COUNT -> castSortBy mbSortDesc totalRequestCount
        Just Common.REJECTED_REQUEST_COUNT -> castSortBy mbSortDesc rejectedRequestCount
        Just Common.PULLED_REQUEST_COUNT -> castSortBy mbSortDesc pulledRequestCount
        Just Common.ACCEPTATION_REQUEST_COUNT -> castSortBy mbSortDesc acceptationRequestCount
        Just Common.DRIVER_CANCELLATION_COUNT -> castSortBy mbSortDesc driverCancellationCount
        Just Common.CUSTOMER_CANCELLATION_COUNT -> castSortBy mbSortDesc customerCancellationCount
        Just Common.ONLINE_DURATION -> castSortBy mbSortDesc onlineDuration
        Just Common.TOTAL_RATING_SCORE -> castSortBy mbSortDesc totalRatingScore
        Just Common.RIDE_DURATION -> castSortBy mbSortDesc rideDuration
        _ -> castSortBy mbSortDesc completedRides

  res <-
    CH.findAll $
      CH.select_
        ( \fos -> do
            let onlineTotalEarning = CH.sum_ fos.onlineTotalEarning
                cashTotalEarning = CH.sum_ fos.cashTotalEarning
                completedRides = CH.sum_ fos.totalCompletedRides
                totalDistance = CH.sum_ fos.totalDistance
                totalRequestCount = CH.sum_ fos.totalRequestCount
                rejectedRequestCount = CH.sum_ fos.rejectedRequestCount
                pulledRequestCount = CH.sum_ fos.pulledRequestCount
                acceptationRequestCount = CH.sum_ fos.acceptationRequestCount
                driverCancellationCount = CH.sum_ fos.driverCancellationCount
                customerCancellationCount = CH.sum_ fos.customerCancellationCount
                onlineDuration = CH.sum_ fos.onlineDuration
                totalRatingScore = CH.sum_ fos.totalRatingScore
                rideDuration = CH.sum_ fos.rideDuration
            CH.groupBy (fos.fleetOperatorId, fos.fleetDriverId) $ \(fleetOperatorId, fleetDriverId) -> do
              (fleetOperatorId, fleetDriverId, onlineTotalEarning, cashTotalEarning, completedRides, totalDistance, totalRequestCount, rejectedRequestCount, pulledRequestCount, acceptationRequestCount, driverCancellationCount, customerCancellationCount, onlineDuration, totalRatingScore, rideDuration)
        )
        $ CH.orderBy_ sortOn $
          CH.limit_ limit $
            CH.offset_ offset $
              CH.filter_
                ( \fos ->
                    fos.fleetOperatorId CH.==. fleetOwnerId
                      CH.&&. fos.fleetDriverId `in_` driverIds
                      CH.&&. fos.merchantLocalDate CH.>=. fromDay
                      CH.&&. fos.merchantLocalDate CH.<=. toDay
                )
                (CH.all_ @CH.APP_SERVICE_CLICKHOUSE fleetOperatorDailyStatsTTable)
  pure $ map (\(_fleetOperatorId, driverId, te, ct, cr, td, tr, rr, pr, ar, dc, cc, od, trs, rd) -> mkDriverMetricsAggregated (_fleetOperatorId, driverId, (te, ct, cr, td), (tr, rr, pr, ar), (dc, cc, od, trs), rd)) res
