module Screens.DriverProfileScreen.Transformer where

import Prelude

import Data.Array (length)
import Data.Int (toNumber)
import Data.Maybe (fromMaybe, Maybe(..))
import Helpers.Utils (getPeriod, parseFloat)
import Language.Strings (getString)
import Language.Types (STR(..))
import MerchantConfig.Utils (getValueFromConfig)
import Screens.Types (AnalyticsData, ChipRailData)
import Services.API (DriverMissedOpp(..), DriverProfileSummaryRes(..), DriverSummary(..))

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
