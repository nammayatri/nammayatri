module Screens.DriverProfileScreen.Transformer where

import Prelude

import Data.Array (length)
import Data.Int (toNumber)
import Data.Maybe (fromMaybe, Maybe(..))
import Helpers.Utils (getPeriod, parseFloat)
import Language.Strings (getString)
import Language.Types (STR(..))
import Screens.Types (AnalyticsData, ChipRailData, VehicleP)
import Services.API (DriverMissedOpp(..), DriverProfileSummaryRes(..), DriverSummary(..))
import Components.ChooseVehicle (Config) as ChooseVehicle

getAnalyticsData :: DriverProfileSummaryRes -> AnalyticsData
getAnalyticsData (DriverProfileSummaryRes response) =
  let
    (DriverSummary summary) = response.driverSummary

    (DriverMissedOpp missedOpp) = response.missedOpp
  in
    { totalEarnings: show summary.totalEarnings
    , bonusEarned: show summary.bonusEarned
    , totalCompletedTrips: summary.totalCompletedTrips
    , totalUsersRated: response.totalUsersRated
    , rating: response.rating
    , lateNightTrips: summary.lateNightTrips
    , lastRegistered: summary.lastRegistered
    , badges: [] -- TODO implement Badges
    , missedEarnings: missedOpp.missedEarnings
    , ridesCancelled: missedOpp.ridesCancelled
    , cancellationRate: missedOpp.cancellationRate
    , totalRidesAssigned: response.totalRidesAssigned
    , totalDistanceTravelled: (parseFloat (toNumber response.totalDistanceTravelled / 1000.0) 2) <> "km"
    }

transformSelectedVehicles :: Array ChooseVehicle.Config -> Array VehicleP
transformSelectedVehicles vehicleDetails = map (\vehicleDetail -> transformSelectedVehicle vehicleDetail) vehicleDetails

transformSelectedVehicle :: ChooseVehicle.Config -> VehicleP
transformSelectedVehicle vehicleDetail = 
  { vehicleName : vehicleDetail.vehicleVariant,
    isSelected : vehicleDetail.isSelected
  }