module Screens.MyRidesScreen.ScreenData where

import Data.Maybe (Maybe(..))
import Screens.Types (AnimationState(..), MyRidesScreenState, IndividualRideCardState)
import Services.API (BookingLocationAPIEntity(..))

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
      }
  , data:
      { selectedItem: dummyIndividualCard
      , offsetValue: 0
      , loadMoreText: "LoadMore"
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
    driverImage : "ny_ic_user,https://assets.juspay.in/nammayatri/images/user/ny_ic_user.png",
    isCancelled :  "",
    isSuccessfull :  "",
    rating : 0,
    driverName : "",
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
  , baseFare : "₹ 0"
  , pickupCharges : "₹ 0"
  , extraFare : "₹ 0"
  , waitingCharges : "₹ 0"
  , baseDistance : "0 km"
  , extraDistance : "0 km"
}
