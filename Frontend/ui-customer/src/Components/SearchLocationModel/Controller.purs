module Components.SearchLocationModel.Controller where

import Components.LocationListItem as LocationListItem
import Components.LocationTagBar as LocationTagBarController
import Components.PrimaryButton as PrimaryButton
import Data.Maybe (Maybe(..))
import Prelude (show)
import PrestoDOM (Visibility(..))
import Screens.Types (SearchLocationModelType, LocationListItemState, LocItemType(..))

data Action = GoBack
            | NoAction
            | SourceChanged String
            | DestinationChanged String
            | SourceClear
            | UpdateSource String String String
            | DestinationClear
            | SetLocationOnMap 
            | SetCurrentLocation
            | EditTextFocusChanged String
            | LocationListItemActionController LocationListItem.Action
            | PrimaryButtonActionController PrimaryButton.Action
            | DebounceCallBack String
            | SavedAddressClicked LocationTagBarController.Action
            | UpdateCurrentLocation String String
            | RecenterCurrentLocation

type SearchLocationModelState = {
    isSearchLocation :: SearchLocationModelType
  , locationList :: Array LocationListItemState
  , savedlocationList :: Array LocationListItemState
  , isSource :: Maybe Boolean
  , source :: String
  , destination :: String
  , isSrcServiceable :: Boolean
  , isDestServiceable :: Boolean
  , isRideServiceable :: Boolean
}

dummy_data :: Array LocationListItemState
dummy_data = [
    { prefixImageUrl : "ny_ic_briefcase,https://assets.juspay.in/nammayatri/images/user/ny_ic_briefcase.png"
    , postfixImageUrl : "ny_ic_fav,https://assets.juspay.in/nammayatri/images/user/ny_ic_fav.png"
    , postfixImageVisibility : true
    , title : "Work"
    , subTitle : "KIAL Rd, Devanahalli, Bengaluru,  Karnataka"
    , placeId : Nothing
    , lat : Nothing
    , lon : Nothing
    , description : ""
    , tag : ""
    , tagType : Just (show LOC_LIST)
    , cardType : Nothing 
    , address : ""
    , tagName : ""
    , isEditEnabled : true
    , savedLocation : ""
    , placeName : ""
    , isClickable : true 
    , alpha : 1.0
    , fullAddress : LocationListItem.dummyAddress
    , locationItemType : Nothing
    }
  , { prefixImageUrl : "ny_ic_recent_search,https://assets.juspay.in/nammayatri/images/user/ny_ic_recent_search.png"
    , postfixImageUrl : "ny_ic_fav,https://assets.juspay.in/nammayatri/images/user/ny_ic_fav.png"
    , postfixImageVisibility : true
    , title : "Work"
    , subTitle : "KIAL Rd, Devanahalli, Bengaluru,  Karnataka"
    , placeId : Nothing
    , lat : Nothing
    , lon : Nothing
    , description : ""
    , tag : ""
    , tagType : Just (show LOC_LIST)
    , cardType : Nothing 
    , address : ""
    , tagName : ""
    , isEditEnabled : true
    , savedLocation : ""
    , placeName : ""
    , isClickable : true 
    , alpha : 1.0
    , fullAddress : LocationListItem.dummyAddress
    , locationItemType : Nothing
    }
  , { prefixImageUrl : "ny_ic_loc_grey,https://assets.juspay.in/nammayatri/images/user/ny_ic_loc_grey.png"
    , postfixImageUrl : "ny_ic_fav,https://assets.juspay.in/nammayatri/images/user/ny_ic_fav.png"
    , postfixImageVisibility : true
    , title : "Work"
    , subTitle : "KIAL Rd, Devanahalli, Bengaluru,  Karnataka"
    , placeId : Nothing
    , lat : Nothing
    , lon : Nothing
    , description : ""
    , tag : ""
    , tagType : Just (show LOC_LIST)
    , cardType : Nothing 
    , address : ""
    , tagName : ""
    , isEditEnabled : true
    , savedLocation : ""
    , placeName : ""
    , isClickable : true 
    , alpha : 1.0
    , fullAddress : LocationListItem.dummyAddress
    , locationItemType : Nothing
    }
]
