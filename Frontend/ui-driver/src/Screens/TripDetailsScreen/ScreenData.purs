{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.TripDetailsScreen.ScreenData where

import Data.Maybe (Maybe(..))
import Screens.Types as ST
import MerchantConfig.DefaultConfig as DC

initData :: ST.TripDetailsScreenState
initData = {
  data: {
      tripId : "",
      distance: "",
      message : "",
      timeTaken : "",
      rider : "",
      date : "",
      time : "" ,
      source : "" ,
      destination : "",
      totalAmount : 0,
      paymentMode : ST.CASH,
      status : "",
      vehicleType : "",
      customerExtraFee : Nothing,
      purpleTagVisibility : false,
      gotoTagVisibility : false,
      spLocTagVisibility : false,
      specialZoneLayoutBackground : "",
      specialZoneImage : "",
      specialZoneText : "",
      goBackTo : ST.Earning,
      config : DC.config,
      specialZonePickup : false ,
      tollCharge : 0.0,
      rideType : "",
      tripStartTime : Nothing,
      tripEndTime : Nothing,
      vehicleModel : "",
      acRide : Nothing,
      vehicleServiceTier : "",
      tripType : ST.Rental,
      parkingCharge : 0.0,
      stops : []
  },
  props: {
    rating : 4,
    reportIssue : false,
    issueReported : false
  }
}

