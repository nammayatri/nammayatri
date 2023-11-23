{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.AddNewAddressScreen.ScreenData where

import Data.Maybe (Maybe(..))
import PrestoDOM (Visibility(..))
import Screens.HomeScreen.ScreenData (dummyAddress)
import Screens.Types (AddNewAddressScreenState, CardType(..), Location)
import Services.API (Prediction(..))
import MerchantConfig.DefaultConfig as DC
 
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
    , distance : Nothing
    , showDistance : Just false
    , actualDistance : Nothing
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
  , polygonCoordinates : ""
  , nearByPickUpPoints : []
  , config : DC.config
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
  , isSpecialZone : false
  , defaultPickUpPoint : ""
  , isServiceable : false
  }

}

dummyLocation :: Location
dummyLocation = {
   place : "",
   lat : 0.0,
   lng : 0.0,
   address : Nothing,
   city : Nothing
 }
