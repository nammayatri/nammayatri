{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.RideHistoryScreen.ScreenData where

import Data.Maybe (Maybe(..))
import Foreign.Object (empty)
import ConfigProvider
import Prelude ((-))
import Resource.Constants (tripDatesCount)
import Screens.Types (AnimationState(..), RideHistoryScreenState, TripType(..))

initData :: RideHistoryScreenState
initData = {
  shimmerLoader : AnimatingIn,
  recievedResponse: false,
  prestoListArrayItems : [],
  rideList: [],
  currentTab: "COMPLETED",
  loadMoreDisabled: false,
  selectedItem: {
    date : "",
    time : "",
    total_amount : 0,
    card_visibility : "",
    shimmer_visibility : "",
    rideDistance : "",
    status : "",
    vehicleModel : "",
    shortRideId : "",
    vehicleNumber : "",
    driverName : "",
    driverSelectedFare : 0,
    vehicleColor : "",
    id : "",
    updatedAt : "",
    source : "",
    destination : "",
    vehicleType : "",
    riderName : "",
    customerExtraFee : Nothing,
    purpleTagVisibility : false,
    gotoTagVisibility : false,
    spLocTagVisibility : false,
    specialZoneLayoutBackground : "",
    specialZoneImage : "",
    specialZoneText : "",
    specialZonePickup : false,
    tripType : OneWay,
    tollCharge : 0.0,
    rideType : "",
    tripStartTime : Nothing,
    tripEndTime : Nothing,
    acRide : Nothing,
    vehicleServiceTier : ""
  , parkingCharge : 0.0
  , stops : []
  },
  offsetValue: 0,
  loaderButtonVisibility: false,
  logField : empty ,
  datePickerState : {
    activeIndex : tripDatesCount - 1 -- based on no of dates we are showing
  , selectedItem : {
      date : 0
    , month : ""
    , year : 0
    , utcDate : ""
  }
  }
  , data : {
    pastDays : 30
  , paymentHistory : {
    paymentHistoryList : []
  }
  , config  : getAppConfig appConfig
  }
  , props : {
    showDatePicker : false
  , showPaymentHistory : false
  }
}