module Screens.AddNewAddressScreen.ScreenData where

import Data.Maybe (Maybe(..))
import PrestoDOM (Visibility(..))
import Screens.HomeScreen.ScreenData (dummyAddress)
import Screens.Types (AddNewAddressScreenState, CardType(..))
import Services.API (Prediction(..))
 
initData :: AddNewAddressScreenState
initData = {
  data: {
    locationList : []
  , selectedItem : {
      prefixImageUrl : "" 
    , postfixImageUrl : ""
    , postfixImageVisibility : false
    , lat : Nothing 
    , lon : Nothing 
    , placeId : Nothing
    , subTitle : ""
    , title : ""
    , description : ""
    , tag : ""
    , tagType : Nothing
    , cardType : Nothing
    , address : ""
    , tagName : ""
    , isEditEnabled : true
    , savedLocation : ""
    , placeName : ""
    , isClickable : true
    , alpha : 1.0
    , fullAddress : dummyAddress
    , locationItemType : Nothing
  }
  , address : ""
  , activeIndex : (Just 2)
  , selectedTag : Just OTHER_TAG
  , editTag : ""
  , addressSavedAs : ""
  , placeName : ""
  , lat : 0.0
  , lon : 0.0
  , savedTags : []
  , savedLocations : []
  , locSelectedFromMap : ""
  , latSelectedFromMap : 0.0
  , lonSelectedFromMap : 0.0
  , existsAs : ""
  , currentLocation : ""
  , currLat : Nothing 
  , currLon : Nothing
  , recentSearchs : { predictionArray : []}
  , addressComponents : []
  },
  props: {
  showSavePlaceView : false
  , isBtnActive : false
  , editLocation : false
  , tagExists : false
  , placeNameExists : false
  , isLocateOnMap : false
  , isLocationServiceable : true
  , fromHome : false
  , selectFromCurrentOrMap : true
  , isSearchedLocationServiceable : true
  , editSavedLocation : false
  } 

}
