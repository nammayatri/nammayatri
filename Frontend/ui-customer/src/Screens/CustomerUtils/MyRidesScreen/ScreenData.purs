{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.MyRidesScreen.ScreenData where

import Data.Maybe (Maybe(..))
import Screens.Types (AnimationState(..), MyRidesScreenState, IndividualRideCardState, ZoneType(..), FareProductType(..))
import Services.API (BookingLocationAPIEntity(..))
import ConfigProvider
import Prelude ((<>))
import Foreign.Object (empty)
import Common.Types.App as CTP

initData :: MyRidesScreenState
initData =
  { shimmerLoader: AnimatingIn
  , itemsRides: []
  , props:
      { loaderButtonVisibility: false
      , loadMoreDisabled: true
      , receivedResponse: false
      , apiFailure: false
      , fromNavBar: true
      , optionsVisibility: true
      , fromBanner : false
      , refreshLoader : false
      , scrollEnable : false
      }
  , data:
      { selectedItem: dummyIndividualCard
      , offsetValue: 0
      , loadMoreText: true
      , config: getAppConfig appConfig
      , logField : empty
      , isSrcServiceable: true
      }
  , prestoListArrayItems: []
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
      baseFare : "€ 0"
    , pickupCharges : "€ 0"
    , nominalFare : "€ 0"
    , waitingCharges : "€ 0"
    }
  , faresList : []
  , baseFare : "€ 0"
  , pickupCharges : "€ 0"
  , extraFare : "€ 0"
  , waitingCharges : "€ 0"
  , baseDistance : "0 km"
  , extraDistance : "0 km"
  , referenceString : ""
  , isSpecialZone : false
  , nightCharges : false
  , zoneType : NOZONE
  , vehicleVariant : Nothing
  , isSrcServiceable : true
  , optionsVisibility : false
  , merchantExoPhone : ""
  , serviceTierName : Nothing
  , totalTime : ""
  , vehicleModel : ""
  , rideStartTimeUTC : ""
  , providerName : ""
  , providerType : CTP.ONUS
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
