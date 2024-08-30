module Screens.ParcelDeliveryFlow.ParcelDeliveryScreen.ScreenData where

import Data.Maybe(Maybe(..))
import Common.Types.App as CTA
import Components.ChooseVehicle.Controller as ChooseVehicleController
import ConfigProvider
import MerchantConfig.Types (AppConfig)
import Screens.Types as ST

initData :: ST.ParcelDeliveryScreenState
initData =
  { data : {
      currentStage : ST.FINAL_DETAILS
    , sourceAddress : dummyAddress
    , destinationAddress : dummyAddress
    , sourceLat : 0.0
    , sourceLong : 0.0
    , destinationLat : 0.0
    , destinationLong : 0.0
    , route : Nothing
    , parcelQuoteList : ChooseVehicleController.config
    , selectedQuote : Nothing
    , showRateCard : false
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