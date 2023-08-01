module Screens.DriverProfileScreen.Transformer where

import Prelude

import Data.Array (length)
import Helpers.Utils (getPeriod)
import Language.Strings (getString)
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
    , chipRailData: getChipRailArray response.driverSummary response.languagesSpoken
    , badges: [] -- TODO implement Badges
    , missedEarnings: missedOpp.missedEarnings
    , ridesCancelled: missedOpp.ridesCancelled
    , cancellationRate: missedOpp.cancellationRate
    , totalRidesAssigned: response.totalRidesAssigned
    }

getChipRailArray :: DriverSummary -> Array String -> Array ChipRailData
getChipRailArray (DriverSummary summary) lang =
  let
    alive = getPeriod summary.lastRegistered
  in
    ( if summary.lateNightTrips > 0 then
        [ { mainTxt: show summary.lateNightTrips
          , subTxt: "Late Night Trips"
          }
        ]
      else
        []
    )
      <> ( [ { mainTxt: if alive.periodType == "new" then "" else (show alive.period) <> " " <> alive.periodType
            , subTxt: "on Namma Yatri"
            }
          ]
        )
      <> ( if length lang > 0 then
            [ { mainTxt: show (length lang)
              , subTxt: "Languages Spoken"
              }
            ]
          else
            []
        )

-- getPeriodType :: String -> String TODO :: Add translation Part
-- getPeriodType = case _ of 
--   "new" -> "New"
--   "year" -> getString YEAR
