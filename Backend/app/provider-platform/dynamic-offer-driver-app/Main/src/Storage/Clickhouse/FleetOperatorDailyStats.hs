{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Clickhouse.FleetOperatorDailyStats where

import Data.Time.Calendar (Day)
import Kernel.Prelude
import Kernel.Storage.ClickhouseV2 as CH
import qualified Kernel.Storage.ClickhouseV2.UtilsTH as TH
import Kernel.Types.Common

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
    totalEarning :: C f (Maybe HighPrecMoney),
    cashPlatformFees :: C f (Maybe HighPrecMoney),
    onlinePlatformFees :: C f (Maybe HighPrecMoney),
    onlineDuration :: C f (Maybe Seconds)
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
      totalEarning = "total_earning",
      cashPlatformFees = "cash_platform_fees",
      onlinePlatformFees = "online_platform_fees",
      onlineDuration = "online_duration"
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
mkDailyFleetMetricsAggregated (te, cr, td, tr, rr, pr, ar, dc, cc) =
  DailyFleetMetricsAggregated
    { totalEarningSum = te,
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
              ( CH.sum_ fos.totalEarning,
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

data DailyFleetEarningsAggregated = DailyFleetEarningsAggregated
  { totalEarningSum :: Maybe HighPrecMoney,
    cashPlatformFeesSum :: Maybe HighPrecMoney,
    onlinePlatformFeesSum :: Maybe HighPrecMoney,
    onlineDurationSum :: Maybe Seconds
  }
  deriving (Show, Generic)

mkDailyFleetEarningsAggregated ::
  ( Maybe HighPrecMoney,
    Maybe HighPrecMoney,
    Maybe HighPrecMoney,
    Maybe Seconds
  ) ->
  DailyFleetEarningsAggregated
mkDailyFleetEarningsAggregated (te, cpf, opf, od) =
  DailyFleetEarningsAggregated
    { totalEarningSum = te,
      cashPlatformFeesSum = cpf,
      onlinePlatformFeesSum = opf,
      onlineDurationSum = od
    }

sumFleetEarningsByFleetOwnerIdAndDateRange ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  Text ->
  Day ->
  Day ->
  m DailyFleetEarningsAggregated
sumFleetEarningsByFleetOwnerIdAndDateRange fleetOwnerId fromDay toDay = do
  res <-
    CH.findAll $
      CH.select_
        ( \fos ->
            CH.aggregate
              ( CH.sum_ fos.totalEarning,
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
  pure $ maybe (DailyFleetEarningsAggregated Nothing Nothing Nothing Nothing) mkDailyFleetEarningsAggregated (listToMaybe res)
