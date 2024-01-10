module Screens.SearchLocationScreen.ScreenData where

import Screens.Types (SearchLocationScreenState, SearchLocationStage(..), SearchLocationTextField(..), SearchLocationActionType(..))
import ConfigProvider
import Screens (ScreenName(..))
import Data.Maybe (Maybe(..))
import Services.API (PlaceName(..), LatLong(..))
import Components.LocationListItem.Controller (locationListStateObj)

initData :: SearchLocationScreenState 
initData = {
  data : { srcLoc : Nothing
         , destLoc : Nothing
         , currentLoc : Nothing 
         , locationList : []
         , savedLocList : []
         , fromScreen : HOME_SCREEN
         , recentSearches : []
         , saveFavouriteCard : {
              address : ""
            , tag : ""
            , tagExists : false
            , selectedItem : locationListStateObj
            , isBtnActive : false
        }
  } ,
  props : {
    searchLocStage : PredictionsStage ,
    focussedTextField : Nothing , 
    actionType : AddingStopAction ,
    showSaveFavCard : false ,
    areBothLocMandatory : false,
    canSelectFromFav : true,
    showLoader : false,
    canClearText : false
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