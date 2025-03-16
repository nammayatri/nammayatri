module Screens.RideBookingFlow.MeterSelectDestinationScreen.ScreenData where

import Screens.Types (MeterSelectDestinationScreenState, SearchLocationModelType(..)) 
import Data.Maybe as Mb
import Halogen.VDom.DOM.Prop (PropValue)
import Services.API as API 
import RemoteConfig as RU 
import ConfigProvider (getAppConfig, appConfig)
import PrestoDOM (Visibility(..))
import Data.Maybe (Maybe(..))
import Screens.Types (Address)
import JBridge (Location)

initData :: MeterSelectDestinationScreenState
initData =
  { data: {
    searchString : Mb.Nothing,
    destination: "",
    destinationAddress: dummyAddress,
    locationList: [],
    voiceToText: "Please speak to get suggestions"
  }
  , props: {
      destinationLat : 0.0,
      destinationLng : 0.0,
      currentLocation : dummyLocation,
      destinationPlaceId : Nothing,
      isSearchLocation: NoView,
      showVoiceToText : false,
      searchLocationModelProps: {
        isAutoComplete : false,
        crossBtnDestVisibility : false
      },
      voiceToTextSuccess : false,
      confirmButtonText : "Confirm",
      voiceToTextSearchString : ""
  }
}

dummyAddress :: Address
dummyAddress = {
              "area" : Nothing
            , "state" : Nothing
            , "country" : Nothing
            , "building" : Nothing
            , "door" : Nothing
            , "street" : Nothing
            , "city" : Nothing
            , "areaCode" : Nothing
            , "ward" : Nothing
            , "placeId" : Nothing
            }

dummyLocation :: Location
dummyLocation = {
   place : "",
   lat : 0.0,
   lng : 0.0,
   address : Nothing,
   city : Nothing,
   isSpecialPickUp : Nothing
}
