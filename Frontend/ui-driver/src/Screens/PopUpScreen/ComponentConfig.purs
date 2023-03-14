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
      text = HU.toString(item.timer)
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