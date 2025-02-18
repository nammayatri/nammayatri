{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.RideSelectionScreen.ScreenData where

import Data.Maybe (Maybe(..))
import Screens.Types (AnimationState(..), IndividualRideCardState, ZoneType(..), FareProductType (..))
import Services.API (BookingLocationAPIEntity(..))
import MerchantConfig.DefaultConfig as DC
import Foreign.Object (empty)
import Common.Types.App 
import Screens.Types
import Data.Maybe
import ConfigProvider (appConfig, getAppConfig)
import MerchantConfig.Types (AppConfig(..))

-- ############################### Types ###############################
type RideSelectionScreenState =
  {
    shimmerLoader :: AnimationState,
    prestoListArrayItems :: Array ItemState,
    itemsRides :: Array IndividualRideCardState,
    props :: RideSelectionScreenProps,
    data :: RideSelectionScreenData,
    selectedCategory :: CategoryListType,
    selectedItem :: Maybe IndividualRideCardState
  }  

type RideSelectionScreenData = {
    offsetValue :: Int,
    loadMoreText :: Boolean,
    isSrcServiceable :: Boolean,
    config :: AppConfig,
    selectedOptionId :: Maybe String,
    entryPoint :: RideSelectionScreenEntry
  }

type RideSelectionScreenProps = {
  loaderButtonVisibility :: Boolean,
  loadMoreDisabled :: Boolean,
  receivedResponse :: Boolean,
  apiFailure :: Boolean,
  fromNavBar :: Boolean,
  optionsVisibility :: Boolean
}

data RideSelectionScreenEntry = FaqScreenEntry | HelpAndSupportScreenEntry

-- ############################### Data ###############################

initData :: RideSelectionScreenState
initData =
  { shimmerLoader: AnimatingIn
  , itemsRides: []
  , props:
      { loaderButtonVisibility: false
      , loadMoreDisabled: true
      , receivedResponse: false
      , apiFailure: false
      , fromNavBar: true
      , optionsVisibility: false
      }
  , data:
      { offsetValue: 0
      , loadMoreText: true
      , isSrcServiceable : true
      , config : getAppConfig appConfig
      , selectedOptionId : Nothing
      , entryPoint : HelpAndSupportScreenEntry
      }
  , prestoListArrayItems: []
  , selectedCategory : {
        categoryName : ""
      , categoryImageUrl : Nothing
      , categoryAction : Nothing
      , categoryId : ""
      , isRideRequired : false
      , categoryType: ""
      , maxAllowedRideAge : Nothing
      , allowedRideStatuses : Nothing
    }
  , selectedItem : Nothing
  }

dummyBookingDetails :: BookingLocationAPIEntity
dummyBookingDetails =
  BookingLocationAPIEntity
    { area: Nothing
    , state: Nothing
    , country: Nothing
    , building: Nothing
    , door: Nothing
    , street: Nothing
    , lat: 0.0
    , city: Nothing
    , areaCode: Nothing
    , lon: 0.0
    , placeId : Nothing
    , ward : Nothing
    , extras : Nothing
    , instructions : Nothing
    }

dummyIndividualCard :: IndividualRideCardState
dummyIndividualCard = {
    date :  "",
    time : "",
    source : "",
    destination :  "",
    totalAmount : "",
    cardVisibility : "",
    shimmerVisibility : "",
    driverImage : "",
    isCancelled :  "",
    isSuccessfull :  "",
    isScheduled : "",
    rating : 0,
    driverName : "",
    driverPhoneNumber : Nothing,
    rideStartTime : "",
    rideEndTime : "",
    vehicleNumber : "",
    rideId : "",
    status : "" ,
    shortRideId : "",
    bookingId : "",
    rideEndTimeUTC : "",
    sourceLocation : dummyBookingDetails,
    destinationLocation : dummyBookingDetails,
    alpha : "",
    fareBreakUpList : {
      baseFare : "₹ 0"
    , pickupCharges : "₹ 0"
    , nominalFare : "₹ 0"
    , waitingCharges : "₹ 0"
    }
  , faresList : []
  , baseFare : "₹ 0"
  , pickupCharges : "₹ 0"
  , extraFare : "₹ 0"
  , waitingCharges : "₹ 0"
  , baseDistance : "0 km"
  , extraDistance : "0 km"
  , referenceString : ""
  , isSpecialZone : false
  , nightCharges : false
  , zoneType : NOZONE
  , isSrcServiceable : true
  , optionsVisibility : false
  , vehicleVariant : Nothing
  , merchantExoPhone : ""
  , serviceTierName : Nothing
  , totalTime : ""
  , vehicleModel : ""
  , rideStartTimeUTC : ""
  , providerName : ""
  , providerType : ONUS
  , showRepeatRide : ""
  , rideType : ONE_WAY
  , estimatedDistance : 0
  , estimatedDuration : 0
  , estimatedFare : 0
  , showDestination : ""
  , rideScheduledTime : ""
  , isAirConditioned : Nothing
  , rideCreatedAt : ""
  , rideStatus : ""
}
