{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.RideCompletedScreen.ScreenData where

import Screens.Types as ST
import ConfigProvider
import Data.Maybe (Maybe(..))
import Constants.Configs (dummyPrice)
import Screens.Types 

initData :: ST.RideCompletedScreenState
initData = {
  data: {
    appConfig : getAppConfig appConfig
  },
  props : {
    isFareBreakDownVisible : false,
    selectedRating : ST.SEL_NONE,
    showCallSupportPopup : false,
    endRideData:
    { actualRideDuration: Nothing
    , actualRideDistance: Nothing
    , rideId: ""
    , zeroCommision: 0
    , tip: Nothing
    , finalAmount: 0
    , finalAmountWithCurrency: dummyPrice
    , riderName: ""
    , rating: 0
    , feedback: ""
    , disability: Nothing
    , payerVpa: ""
    , specialZonePickup: Nothing
    , capacity : Nothing
    , serviceTier : ""
    , tollAmbigous : false
    , tripStartTime : Nothing
    , tripEndTime : Nothing
    }
    , parking : initialParkingData
    , toll : initialTollState
  }
}


initialTollState :: TollState
initialTollState = { 
  showTollChargePopup: true
, showTollChargeAmbigousPopup: true
, finalCharge : 0.0
, tollAmbigous : false
, estimatedCharge : 0.0
}

initialParkingData :: ParkingData
initialParkingData = {
  estimatedCharge : Nothing
, finalCharge : Nothing
}
