{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.RentalBookingFlow.RentalScreen.ScreenData where

import Components.ChooseVehicle.Controller (SearchResultType(..)) as CV
import Data.Maybe(Maybe(..))
import PrestoDOM (Margin(..))
import Screens.Types (RentalScreenState, RentalScreenStage(..))
import Screens.SearchLocationScreen.ScreenData (dummyLocationInfo)
import Screens.RentalBookingFlow.RideScheduledScreen.ScreenData as RideScheduledScreenData
import ConfigProvider

initData :: RentalScreenState
initData = 
  { data : {
      rentalBookingData : { 
          startTimeUTC : ""
        , baseDuration : 1
        , baseDistance : 10
        , finalDuration : 0
        , finalDistance : 0
        , startOdometer : ""
        , endOdometer : ""
        , nightCharge : "250"
        , rideStartedAt : ""
        , rideEndedAt : ""
        , extraDistanceFare : ""
        , extraTimeFare : ""
      }
    , selectedQuote : Nothing
    , config : getAppConfig appConfig
    , searchId : ""
    , bookingId : ""
    , startTimeUTC : ""
    , currentStage : RENTAL_SELECT_PACKAGE
    , rentalsQuoteList : []
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
    , minDuration : 1
    , minDistance : 10
    , maxDistance : 20
    , farePerKm : ""
    , maxDateBooking : 5
    , showRateCard : false
    , showShimmer : true
    , showPrimaryButton : true
    , showPopUpModal : false
    , showRentalPolicy : false
    , isSpecialZone : false
    }
  }