module Screens.DriverProfileScreen.Transformer where

import Prelude

import Data.Array (length)
import Data.Int (toNumber)
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
    , chipRailData: getChipRailArray response.driverSummary response.languagesSpoken (parseFloat (toNumber response.totalDistanceTravelled / 1000.0) 2)
    , badges: [] -- TODO implement Badges
    , missedEarnings: missedOpp.missedEarnings
    , ridesCancelled: missedOpp.ridesCancelled
    , cancellationRate: missedOpp.cancellationRate
    , totalRidesAssigned: response.totalRidesAssigned
    , totalDistanceTravelled: (parseFloat (toNumber response.totalDistanceTravelled / 1000.0) 2) <> "km"
    }

getChipRailArray :: DriverSummary -> Array String -> String -> Array ChipRailData
getChipRailArray (DriverSummary summary) lang totalDistanceTravelled =
  let
    alive = getPeriod summary.lastRegistered
  in
    ( if summary.lateNightTrips > 0 then
        [ { mainTxt: show summary.lateNightTrips
          , subTxt: getString LATE_NIGHT_TRIPS
          }
        ]
      else
        []
    ) <> 
    ( [ { mainTxt: if alive.periodType == "new" then "" else (show alive.period) <> " " <> alive.periodType
          , subTxt: "on " <> getValueFromConfig "clientName"
          }
        ]
    )<> 
    ( if length lang > 0 then
            [ { mainTxt: show (length lang)
              , subTxt: getString LANGUAGES_SPOKEN
              }
            ]
          else
            []
    )<>
    (
      [ { mainTxt: totalDistanceTravelled <>"km"
        , subTxt: getString TRAVELLED_ON_APP
        }
      ]
    )

-- getPeriodType :: String -> String TODO :: Add translation Part
-- getPeriodType = case _ of 
--   "new" -> "New"
--   "year" -> getString YEAR
