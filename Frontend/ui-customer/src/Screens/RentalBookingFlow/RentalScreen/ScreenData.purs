{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.RentalBookingFlow.RentalScreen.ScreenData where

import Components.ChooseVehicle.Controller (SearchType(..)) as CV
import Data.Maybe(Maybe(..))
import PrestoDOM (Margin(..))
import Screens.Types (RentalScreenState, RentalScreenStage(..))
import Screens.SearchLocationScreen.ScreenData (dummyLocationInfo)
import Screens.RentalBookingFlow.RideScheduledScreen.ScreenData as RideScheduledScreenData

initData :: RentalScreenState
initData = 
  { data : {
      rentalBookingData : { 
          selectedDate : ""
        , selectedTime : ""
        , baseDuration : 2
        , baseDistance : 20
        , finalDuration : 0
        , finalDistance : 0
        , startOdometer : ""
        , endOdometer : ""
        , estimatedFare : 0
        , finalFare : 0
        , nightCharge : ""
      }
    , startTimeUTC : ""
    , currentStage : RENTAL_SELECT_PACKAGE
    , quoteList : []
    , endOTP : Nothing
    , nextStop : Nothing
    , selectedDateTimeConfig : {
        year : 0
      , month : 0
      , day : 0
      , hour : 0
      , minute : 0
    }
    , pickUpLoc : dummyLocationInfo
    , dropLoc : Nothing
    }
  , props : {
      maxDuration : 12
    , minDuration : 2
    , minDistance : 20
    , maxDistance : 30
    , farePerKm : ""
    , maxDateBooking : 5
    , showRateCard : false
    }
  }