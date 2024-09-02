module Screens.ParcelDeliveryFlow.ParcelDeliveryScreen.ScreenData where

import Data.Maybe(Maybe(..))
import Screens.Types as ST

initData :: ST.ParcelDeliveryScreenState
initData =
  { data : {
      currentStage : ST.DELIVERY_INSTRUCTIONS
    , sourceAddress : dummyAddress
    , destinationAddress : dummyAddress
    , sourceLat : 0.0
    , sourceLong : 0.0
    , destinationLat : 0.0
    , destinationLong : 0.0
    , route : Nothing
    , deliveryDetailsInfo : dummyDeliveryDetailsInfo
    },
    props : {}
  }

dummyAddress :: ST.Address
dummyAddress = 
  { "area"      : Nothing
  , "state"     : Nothing
  , "country"   : Nothing
  , "building"  : Nothing
  , "door"      : Nothing
  , "street"    : Nothing
  , "city"      : Nothing
  , "areaCode"  : Nothing
  , "ward"      : Nothing
  , "placeId"   : Nothing
  }

dummyDeliveryDetailsInfo :: ST.DeliveryDetailsInfo
dummyDeliveryDetailsInfo = {
  sendersDetails : dummyPersonAndLocationInfo
  , receiversDetails : dummyPersonAndLocationInfo
  , currentState : ST.SenderModal
  , initiatedAs : ST.Sender
}

dummyPersonAndLocationInfo = {
  name : ""
  , mobileNumber : ""
  , address : ""
  , instruction : Nothing
}