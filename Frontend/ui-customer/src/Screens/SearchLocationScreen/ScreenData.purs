module Screens.SearchLocationScreen.ScreenData where

import Screens.Types (SearchLocationScreenState, SearchLocationStage(..), SearchLocationTextField(..), SearchLocationActionType(..), LocationInfo)
import ConfigProvider
import Screens (ScreenName(..), getScreen)
import Data.Maybe (Maybe(..))
import Services.API (PlaceName(..), LatLong(..))
import Components.LocationListItem.Controller (locationListStateObj, dummyAddress)

initData :: SearchLocationScreenState 
initData = {
  data : { srcLoc : Nothing
         , destLoc : Nothing
         , currentLoc : Just dummyLocationInfo{
            address = "Current Location"
         } 
         , locationList : []
         , fromScreen : getScreen HOME_SCREEN -- getScreen RENTAL_SCREEN
         , saveFavouriteCard : {
              address : ""
            , tag : ""
            , tagExists : false
            , selectedItem : locationListStateObj
            , isBtnActive : false
        }
        , latLonOnMap : dummyLocationInfo
        , defaultGate : ""
        , nearByGates : []
        , specialZoneCoordinates : ""
        , confirmLocCategory : ""
        , predictionSelectedFromHome : locationListStateObj
  } ,
  props : {
    searchLocStage : PredictionsStage ,
    focussedTextField : Nothing , 
    actionType : AddingStopAction ,
    showSaveFavCard : false ,
    areBothLocMandatory : false,
    canSelectFromFav : true,
    showLoader : false,
    canClearText : false,
    locUnserviceable : false,
    isAutoComplete : false,
    textFieldText : {
      pickUpLoc : "",
      dropLoc : ""
    },
    pickUpSelectedOnMap : false
  },
  appConfig : getAppConfig appConfig
}

dummyLocationName :: PlaceName
dummyLocationName = PlaceName {
  "formattedAddress" : "",
  "location" : LatLong{
    "lat" : 0.0,
    "lon" : 0.0
  },
  "plusCode" : Nothing,
  "addressComponents" : [],
  "placeId" : Nothing
}

dummyLocationInfo :: LocationInfo 
dummyLocationInfo = {
  lat : Nothing ,
  lon : Nothing ,
  placeId : Nothing,
  address : "",
  addressComponents : dummyAddress,
  city : Nothing 
}