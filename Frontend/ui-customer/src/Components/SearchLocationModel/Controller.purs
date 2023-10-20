{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.SearchLocationModel.Controller where

import Components.Calendar as Calendar
import Components.LocationListItem as LocationListItem
import Components.LocationTagBar as LocationTagBarController
import Components.PrimaryButton as PrimaryButton
import Data.Maybe (Maybe(..))
import Prelude (show)
import PrestoDOM (Visibility(..))
import Screens.Types (SearchLocationModelType, LocationListItemState, LocItemType(..), BookingStage(..), RentalConfig(..))
import MerchantConfig.Types (AppConfig)
import Helpers.Utils (getAssetStoreLink, getCommonAssetStoreLink)
import Common.Types.App (LazyCheck(..))
import Prelude ((<>))
import Foreign.Object (Object)
import Foreign (Foreign)
import Components.ChooseYourRide as ChooseYourRide
import Components.ChooseVehicle.Controller (SearchType(..)) as CV
import Components.ChooseVehicle.Controller as ChooseVehicle

data Action = GoBack
            | NoAction
            | SourceChanged String
            | DestinationChanged String
            | SourceClear
            | UpdateSource Number Number String
            | DestinationClear
            | SetLocationOnMap
            | SetCurrentLocation
            | EditTextFocusChanged String Boolean
            | LocationListItemActionController LocationListItem.Action
            | PrimaryButtonActionController PrimaryButton.Action
            | DebounceCallBack String Boolean
            | SavedAddressClicked LocationTagBarController.Action
            | UpdateCurrentLocation String String
            | RecenterCurrentLocation
            | TimePicker Int Int
            | CalendarAction Calendar.Action
            | FocusDateAndTime
            | ChooseVehicleAC ChooseVehicle.Action
            | RentalConfirmAndBookAction PrimaryButton.Action
            | EditDate
            | DecreaseRentalPackage
            | IncreaseRentalPackage
            | RentalPackageAC

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
  , appConfig :: AppConfig
  , logField :: Object Foreign
  , crossBtnSrcVisibility :: Boolean
  , crossBtnDestVisibility :: Boolean
  , isAutoComplete :: Boolean
  , showLoader :: Boolean
  , prevLocation :: String
  , bookingStage :: BookingStage
  , rentalData :: RentalConfig
  , config :: AppConfig
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
    , actualDistance : 0
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
    , actualDistance : 0
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
    , actualDistance : 0
    }
]

chooseYourRideConfig :: ChooseYourRide.Config
chooseYourRideConfig = ChooseYourRide.config
  {
    rideDistance = "abc",
    rideDuration = "def",
    quoteList = [ {
            activeIndex: 0
          , basePrice: 254
          , capacity: "Economical, 4 people"
          , id: "d1b6e3e0-6075-49f1-abb9-28bfb1a4b353"
          , index: 0
          , isBookingOption: false
          , isCheckBox: false
          , isEnabled: true
          , isSelected: false
          , maxPrice: 123
          , price: "₹254"
          , searchResultType: CV.QUOTES
          , showInfo: false
          , vehicleImage: "ny_ic_taxi_side,https://assets.juspay.in/beckn/jatrisaathi/jatrisaathicommon/images/ny_ic_taxi_side.png"
          , vehicleType: ""
          , vehicleVariant: "TAXI"
          },
          {
            activeIndex: 0
          , basePrice: 292
          , capacity: "Comfy, 4 people"
          , id: "cd47110c-a5f4-41a7-bee8-724242850a2d"
          , index: 1
          , isBookingOption: false
          , isCheckBox: false
          , isEnabled: true
          , isSelected: false
          , maxPrice: 123
          , price: "₹292"
          , searchResultType: CV.QUOTES
          , showInfo: false
          , vehicleImage: "ny_ic_sedan_ac_side,https://assets.juspay.in/beckn/jatrisaathi/jatrisaathicommon/images/ny_ic_sedan_ac_side.png"
          , vehicleType: ""
          , vehicleVariant: "TAXI_PLUS"
          },
          { activeIndex: 0
          , basePrice: 582
          , capacity: "Spacious · 6 people"
          , id: "39ea627a-a7e2-41f9-976a-545d34833c08"
          , index: 2
          , isBookingOption: false
          , isCheckBox: false
          , isEnabled: true
          , isSelected: false
          , maxPrice: 123
          , price: "₹582"
          , searchResultType: CV.QUOTES
          , showInfo: false
          , vehicleImage: "ny_ic_suv_ac_side,https://assets.juspay.in/beckn/jatrisaathi/jatrisaathicommon/images/ny_ic_suv_ac_side.png"
          , vehicleType: ""
          , vehicleVariant: "SUV"
          }
        ],
    showTollExtraCharges = false,
    nearByDrivers = Nothing
  }