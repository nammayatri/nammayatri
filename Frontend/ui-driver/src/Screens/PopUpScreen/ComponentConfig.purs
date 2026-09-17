{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.PopUpScreen.ComponentConfig where

import Components.RideAllocationModal as RideAllocationModal
import Helpers.Utils as HU
import Screens.Types as ST

rideAllocationModalConfig :: ST.PopUpScreenState -> ST.Rides -> RideAllocationModal.Config
rideAllocationModalConfig state item = let 
  config = RideAllocationModal.config 
  rideAllocationModalConfig' = config {
    id = item.id,
    seconds = item.seconds,
    countDown {
      text = HU.toStringJSON(item.timer)
    },
    source {
      text = item.sourceAddress
    },
    destination {
      text = item.destinationAddress
    },
    totalPrice = item.totalAmount,
    basePrice = item.baseAmount,
    reducePrice = item.decreasePrice,
    increasePrice = item.increasePrice,
    journeyDistance = item.journeyDistance,
    pickupDistance = item.pickupDistance,
    destinationArea {
      text = item.destinationArea
    }
  }
  in rideAllocationModalConfig'