{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.SearchLocationModel.Controller where

import Components.LocationListItem as LocationListItem
import Components.LocationTagBar as LocationTagBarController
import Components.PrimaryButton as PrimaryButton
import Data.Maybe (Maybe(..))
import Prelude (show)
import PrestoDOM (Visibility(..))
import Screens.Types (RentalStage(..), SearchLocationModelType, LocationListItemState, LocItemType(..))
import MerchantConfig.Types (AppConfig)
import Helpers.Utils (getAssetStoreLink, getCommonAssetStoreLink)
import Common.Types.App (LazyCheck(..))
import Prelude ((<>))
import Foreign.Object (Object)
import Foreign (Foreign)

data Action = GoBack
            | NoAction
            | SourceChanged String
            | DestinationChanged String
            | SourceClear
            | UpdateSource Number Number String
            | DestinationClear
            | SetLocationOnMap
            | SetCurrentLocation
            | EditTextFocusChanged String
            | LocationListItemActionController LocationListItem.Action
            | PrimaryButtonActionController PrimaryButton.Action
            | DebounceCallBack String Boolean
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
  , homeScreenConfig :: AppConfig
  , logField :: Object Foreign
  , rentalStage :: RentalStage
}

dummy_data :: Array LocationListItemState
dummy_data = [
    { prefixImageUrl : "ny_ic_briefcase," <> (getAssetStoreLink FunctionCall) <> "ny_ic_briefcase.png"
    , postfixImageUrl : "ny_ic_fav," <> (getAssetStoreLink FunctionCall) <> "ny_ic_fav.png"
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
    , distance : Nothing
    , showDistance : Just false
    }
  , { prefixImageUrl : "ny_ic_recent_search," <> (getAssetStoreLink FunctionCall) <> "ny_ic_recent_search.png"
    , postfixImageUrl : "ny_ic_fav," <> (getAssetStoreLink FunctionCall) <> "ny_ic_fav.png"
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
    , distance : Nothing
    , showDistance : Just false
    }
  , { prefixImageUrl : "ny_ic_loc_grey," <> (getAssetStoreLink FunctionCall) <> "ny_ic_loc_grey.png"
    , postfixImageUrl : "ny_ic_fav," <> (getAssetStoreLink FunctionCall) <> "ny_ic_fav.png"
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
    , distance : Nothing
    , showDistance : Just false
    }
]
