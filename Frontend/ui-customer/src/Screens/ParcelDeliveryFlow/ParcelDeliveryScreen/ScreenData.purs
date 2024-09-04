module Screens.ParcelDeliveryFlow.ParcelDeliveryScreen.ScreenData where

import Components.ChooseVehicle.Controller as ChooseVehicleController
import Data.Maybe(Maybe(..))
import Components.ChooseVehicle.Controller as ChooseVehicleController
import Screens.Types as ST
import Services.API as API

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
    , parcelQuoteList : ChooseVehicleController.config
    , deliveryDetailsInfo : dummyDeliveryDetailsInfo
    , selectedQuote : Nothing
    },
    props : {
      editDetails : dummyPersonAndLocationInfo
    }
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

dummyDeliveryDetailsInfo :: API.DeliveryDetails
dummyDeliveryDetailsInfo = API.DeliveryDetails {
  senderDetails : dummyPersonAndLocationInfo
  , receiverDetails : dummyPersonAndLocationInfo
  , initiatedAs : API.SomeoneElse
}

dummyPersonAndLocationInfo :: API.PersonLocationAndInstruction
dummyPersonAndLocationInfo = API.PersonLocationAndInstruction {
  name : ""
  , phoneNumber : ""
  , address : dummyAddress'
}

dummyAddress' :: API.InstructionAndAddress
dummyAddress' = API.InstructionAndAddress {
  instruction : Nothing
  , extras : ""
}
