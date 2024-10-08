module Screens.ParcelDeliveryFlow.ParcelDeliveryScreen.ScreenData where

import Components.ChooseVehicle.Controller as ChooseVehicleController
import Data.Maybe(Maybe(..))
import Common.Types.App as CTA
import Components.ChooseVehicle.Controller as ChooseVehicleController
import Screens.Types as ST
import MerchantConfig.Types (AppConfig)
import ConfigProvider
import Prelude ((/=))
import Services.API as API

initData :: ST.ParcelDeliveryScreenState
initData =
  let config = getAppConfig appConfig
  in
    { data : {
        currentStage : ST.SENDER_DETAILS
      , sourceAddress : dummyAddress
      , destinationAddress : dummyAddress
      , sourceLat : 0.0
      , sourceLong : 0.0
      , destinationLat : 0.0
      , destinationLong : 0.0
      , route : Nothing
      , parcelQuoteList : ChooseVehicleController.config
      , senderDetails : dummyPersonDeliveryDetails
      , receiverDetails : dummyPersonDeliveryDetails { name = " ", phone = " ", extras = " ", instructions = Just " " }
      , initiatedAs : API.SomeoneElse
      , selectedQuote : Nothing
      , rateCard : {
        additionalFare : 0,
        currentRateCardType : CTA.DefaultRateCard,
        onFirstPage:false,
        baseFare : 0,
        extraFare : [],
        serviceTierName : Nothing,
        createdTime : "",
        driverAdditions : [],
        fareInfoDescription : [],
        isNightShift : false,
        nightChargeTill : "",
        nightChargeFrom : "",
        waitingTimeInfo : { freeMinutes: "", charge: "" }
        }
      , config : getAppConfig appConfig
      , tipForDriver : Nothing
      },
      props : {
        editDetails : dummyPersonDeliveryDetails,
        showRateCard : false,
        isEditModal : false,
        focusField : "",
        isValidInputs : false
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
  instructions : Nothing
  , extras : ""
}

dummyPersonDeliveryDetails :: ST.PersonDeliveryDetails
dummyPersonDeliveryDetails = {
  name : ""
  , phone : ""
  , extras : ""
  , instructions : Nothing
}

