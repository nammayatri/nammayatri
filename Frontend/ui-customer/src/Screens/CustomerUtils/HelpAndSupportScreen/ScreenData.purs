{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.HelpAndSupportScreen.ScreenData where

import Screens.Types (HelpAndSupportScreenState, DeleteStatus(..))
import Screens.MyRidesScreen.ScreenData (dummyIndividualCard)
import Screens.Types (DeleteStatus(..))
import MerchantConfig.DefaultConfig as DC
import Data.Maybe(Maybe(..))
import Foreign.Object (empty)

initData :: HelpAndSupportScreenState
initData = {
  data: {
    source : "",
    destination : "",
    date : "",
    time : "",
    rating : 0,
    driverName : "",
    totalAmount : "",
    isNull : true,
    status : "",
    rideStartTime : "",
    rideEndTime : "",
    vehicleNumber : "",
    rideId : "",
    tripId : "",
    bookingId : "",
    faresList : [],
    email : "",
    description : "",
    accountStatus : ACTIVE,
    config : DC.config,
    vehicleVariant : Nothing,
    logField : empty
  },
  props:{
    apiFailure : false
  , isCallConfirmation : false
  , showDeleteAccountView : false
  , btnActive : false
  }

}
