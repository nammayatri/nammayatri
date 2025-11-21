{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Clickhouse.DailyStats where

import Data.Time.Calendar
import qualified Domain.Types.DailyStats as DStats
import qualified Domain.Types.Person as DP
import Kernel.Prelude
import Kernel.Storage.ClickhouseV2 as CH
import qualified Kernel.Storage.ClickhouseV2.UtilsTH as TH
import Kernel.Types.Common
import Kernel.Types.Id

data DailyStatsT f = DailyStatsT
  { id :: C f (Id DStats.DailyStats),
    driverId :: C f (Id DP.Person),
    merchantLocalDate :: C f Data.Time.Calendar.Day,
    totalEarnings :: C f Kernel.Types.Common.HighPrecMoney,
    totalDistance :: C f Kernel.Types.Common.Meters,
    numRides :: C f Int,
    cancellationCharges :: C f Kernel.Types.Common.HighPrecMoney,
    bonusEarnings :: C f Kernel.Types.Common.HighPrecMoney
  }
  deriving (Generic)

deriving instance Show DailyStats

dailyStatsTTable :: DailyStatsT (FieldModification DailyStatsT)
dailyStatsTTable =
  DailyStatsT
    { id = "id",
      driverId = "driver_id",
      merchantLocalDate = "merchant_local_date",
      totalEarnings = "total_earnings",
      totalDistance = "total_distance",
      numRides = "num_rides",
      cancellationCharges = "cancellation_charges",
      bonusEarnings = "bonus_earnings"
    }

type DailyStats = DailyStatsT Identity

$(TH.mkClickhouseInstances ''DailyStatsT 'SELECT_FINAL_MODIFIER)

data EarningsBar = EarningsBar
  { driverId' :: Id DP.Person,
    periodStartDate :: Day,
    earnings :: Kernel.Types.Common.HighPrecMoney,
    distance :: Kernel.Types.Common.Meters,
    rides :: Int,
    cancellationChargesReceived :: Kernel.Types.Common.HighPrecMoney,
    bonusEarningsReceived :: Kernel.Types.Common.HighPrecMoney
  }
  deriving (Show, Generic)

mkEarningsBar ::
  (Id DP.Person, Day, Kernel.Types.Common.HighPrecMoney, Kernel.Types.Common.Meters, Int, Kernel.Types.Common.HighPrecMoney, Kernel.Types.Common.HighPrecMoney) ->
  EarningsBar
mkEarningsBar (driverId, periodStartDate, earnings, distance, rides, cancellationChargesReceived, bonusEarningsReceived) =
  EarningsBar
    { driverId' = driverId,
      periodStartDate = periodStartDate,
      earnings = earnings,
      distance = distance,
      rides = rides,
      cancellationChargesReceived = cancellationChargesReceived,
      bonusEarningsReceived = bonusEarningsReceived
    }

data StatsPeriod
  = WeeklyStats Int
  | MonthlyStats

aggregatePeriodStatsWithBoundaries ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  Id DP.Person ->
  Day ->
  Day ->
  StatsPeriod ->
  m [EarningsBar]
aggregatePeriodStatsWithBoundaries driverId' fromDateU toDateU period = do
  res <-
    CH.findAll $
      CH.select_
        ( \ds ->
            let periodStart' = case period of
                  WeeklyStats weekStartMode -> CH.toStartOfWeek ds.merchantLocalDate (CH.valColumn weekStartMode)
                  MonthlyStats -> CH.toStartOfMonth ds.merchantLocalDate
                earnings = CH.sum_ ds.totalEarnings
                distance = CH.sum_ ds.totalDistance
                rides = CH.sum_ ds.numRides
                cancellationCharges = CH.sum_ ds.cancellationCharges
                bonusEarnings = CH.sum_ ds.bonusEarnings
             in CH.groupBy (ds.driverId, periodStart') $ \(driverId, periodStart) ->
                  (driverId, periodStart, earnings, distance, rides, cancellationCharges, bonusEarnings)
        )
        $ CH.filter_
          ( \ds ->
              ds.driverId ==. driverId'
                CH.&&. ds.merchantLocalDate >=. fromDateU
                CH.&&. ds.merchantLocalDate <=. toDateU
          )
          (CH.all_ @CH.APP_SERVICE_CLICKHOUSE dailyStatsTTable)
  pure $ mkEarningsBar <$> res

-- Count drivers whose total rides in [fromDate, toDate] exceeds a threshold
countDriversWithNumRidesGreaterThanBetween ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  [Id DP.Person] ->
  Day ->
  Day ->
  Int ->
  m Int
countDriversWithNumRidesGreaterThanBetween driverIds fromDateU toDateU threshold = do
  res <-
    CH.findAll $
      CH.select_
        (\ds -> CH.groupBy ds.driverId $ \_ -> CH.sum_ ds.numRides)
        $ CH.filter_
          ( \ds ->
              ds.driverId `CH.in_` driverIds
                CH.&&. ds.merchantLocalDate >=. fromDateU
                CH.&&. ds.merchantLocalDate <=. toDateU
          )
          (CH.all_ @CH.APP_SERVICE_CLICKHOUSE dailyStatsTTable)
  let totals = [ridesSum | ridesSum <- res, ridesSum > threshold]
  pure $ length totals

countDriversWithNumRidesGreaterThan1Between ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  [Id DP.Person] ->
  Day ->
  Day ->
  m Int
countDriversWithNumRidesGreaterThan1Between driverIds fromDateU toDateU =
  countDriversWithNumRidesGreaterThanBetween driverIds fromDateU toDateU 1

countDriversWithNumRidesGreaterThan10Between ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  [Id DP.Person] ->
  Day ->
  Day ->
  m Int
countDriversWithNumRidesGreaterThan10Between driverIds fromDateU toDateU =
  countDriversWithNumRidesGreaterThanBetween driverIds fromDateU toDateU 10

countDriversWithNumRidesGreaterThan50Between ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  [Id DP.Person] ->
  Day ->
  Day ->
  m Int
countDriversWithNumRidesGreaterThan50Between driverIds fromDateU toDateU =
  countDriversWithNumRidesGreaterThanBetween driverIds fromDateU toDateU 50
