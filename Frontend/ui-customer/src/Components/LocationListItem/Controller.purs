module Components.LocationListItem.Controller where

import Data.Maybe (Maybe(..))
import Screens.Types (LocationListItemState, Address)

data Action = OnClick LocationListItemState 
            | SelectedCurrentLocation String String String 
            | FavClick LocationListItemState


dummyLocationListState :: LocationListItemState
dummyLocationListState = { prefixImageUrl : "ny_ic_briefcase,https://assets.juspay.in/nammayatri/images/user/ny_ic_briefcase.png"
  , postfixImageUrl : "ny_ic_fav,https://assets.juspay.in/nammayatri/images/user/ny_ic_fav.png"
  , postfixImageVisibility : true
  , title : "Work"
  , subTitle : "KIAL Rd, Devanahalli, Bengaluru,  Karnataka"
  , placeId : Nothing
  , lat : Nothing
  , lon : Nothing
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