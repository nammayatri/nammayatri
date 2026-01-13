{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Clickhouse.FleetRcDailyStats where

import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Fleet.Driver as Common
import Data.Time.Calendar (Day)
import Kernel.Prelude
import Kernel.Storage.ClickhouseV2 as CH
import qualified Kernel.Storage.ClickhouseV2.UtilsTH as TH
import qualified Kernel.Types.Common as Common
import Kernel.Utils.Common
import qualified Storage.Clickhouse.FleetOperatorDailyStats as FODS
import qualified Storage.Queries.FleetRcDailyStatsExtra as QFRDSExtra

data FleetRcDailyStatsT f = FleetRcDailyStatsT
  { fleetOwnerId :: C f Text,
    rcId :: C f Text,
    merchantLocalDate :: C f Day,
    totalCompletedRides :: C f (Maybe Int),
    totalEarnings :: C f (Maybe HighPrecMoney),
    rideDistance :: C f (Maybe Meters),
    rideDuration :: C f (Maybe Common.Seconds)
  }
  deriving (Generic)

type FleetRcDailyStats = FleetRcDailyStatsT Identity

deriving instance Show FleetRcDailyStats

fleetRcDailyStatsTTable :: FleetRcDailyStatsT (FieldModification FleetRcDailyStatsT)
fleetRcDailyStatsTTable =
  FleetRcDailyStatsT
    { fleetOwnerId = "fleet_owner_id",
      rcId = "rc_id",
      merchantLocalDate = "merchant_local_date",
      totalCompletedRides = "total_completed_rides",
      totalEarnings = "total_earnings",
      rideDistance = "ride_distance",
      rideDuration = "ride_duration"
    }

$(TH.mkClickhouseInstances ''FleetRcDailyStatsT 'SELECT_FINAL_MODIFIER)

aggerateVehicleStatsByFleetOwnerIdAndDateRange ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  Text ->
  Maybe Text ->
  Int ->
  Int ->
  Maybe Bool ->
  Maybe Common.FleetVehicleStatsSortOn ->
  Day ->
  Day ->
  m [QFRDSExtra.FleetRcDailyStatsAggregated]
aggerateVehicleStatsByFleetOwnerIdAndDateRange fleetOwnerId mbRcId limit offset mbSortDesc mbSortOn fromDay toDay = do
  let sortOn _fos (_, _, totalEarnings, totalCompletedRides, totalDistance, totalDuration) = case mbSortOn of
        Just Common.VEHICLE_TOTAL_EARNINGS -> FODS.castSortBy mbSortDesc totalEarnings
        Just Common.VEHICLE_COMPLETED_RIDES -> FODS.castSortBy mbSortDesc totalCompletedRides
        Just Common.VEHICLE_RIDE_DISTANCE -> FODS.castSortBy mbSortDesc totalDistance
        Just Common.VEHICLE_RIDE_DURATION -> FODS.castSortBy mbSortDesc totalDuration
        Just Common.VEHICLE_EARNING_PER_KM -> FODS.castSortBy mbSortDesc (CH.unsafeCoerceNum @(Maybe HighPrecMoney) @Double totalEarnings CH./. CH.unsafeCoerceNum @(Maybe Meters) @Double totalDistance)
        _ -> FODS.castSortBy mbSortDesc totalCompletedRides
  rows <-
    CH.findAll $
      CH.select_
        ( \fleetRcDailyStats ->
            let totalEarnings = CH.sum_ fleetRcDailyStats.totalEarnings
                totalCompletedRides = CH.sum_ fleetRcDailyStats.totalCompletedRides
                totalDistance = CH.sum_ fleetRcDailyStats.rideDistance
                totalDuration = CH.sum_ fleetRcDailyStats.rideDuration
             in CH.groupBy
                  ( fleetRcDailyStats.fleetOwnerId,
                    fleetRcDailyStats.rcId
                  )
                  $ \(fleetOwnerId', rcId') ->
                    ( fleetOwnerId',
                      rcId',
                      totalEarnings,
                      totalCompletedRides,
                      totalDistance,
                      totalDuration
                    )
        )
        $ CH.orderBy_ sortOn $
          CH.limit_ limit $
            CH.offset_ offset $
              CH.filter_
                ( \fleetRcDailyStats ->
                    fleetRcDailyStats.fleetOwnerId CH.==. fleetOwnerId
                      CH.&&. CH.whenJust_ mbRcId (\rcId -> fleetRcDailyStats.rcId CH.==. rcId)
                      CH.&&. fleetRcDailyStats.merchantLocalDate CH.>=. fromDay
                      CH.&&. fleetRcDailyStats.merchantLocalDate CH.<=. toDay
                )
                (CH.all_ @CH.APP_SERVICE_CLICKHOUSE fleetRcDailyStatsTTable)

  pure $
    fmap
      ( \(fleetOwnerId', rcId, totalEarnings, totalCompletedRides, totalDistance, totalDuration) ->
          QFRDSExtra.mkFleetRcDailyStatsAggregated
            fleetOwnerId'
            rcId
            (fromMaybe (HighPrecMoney 0) totalEarnings)
            (fromMaybe 0 totalCompletedRides)
            (fromMaybe (0 :: Meters) totalDistance)
            (fromMaybe (Common.Seconds 0) totalDuration)
      )
      rows
