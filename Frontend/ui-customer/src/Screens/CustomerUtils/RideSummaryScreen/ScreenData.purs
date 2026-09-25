module Screens.RideSummaryScreen.ScreenData where

import Prelude
import Screens.Types
import Services.API
import Common.Types.App as CTA
import Data.Maybe
import ConfigProvider
import MerchantConfig.Types (AppConfig)



type RideSummaryScreenState = {
  data :: RideSummaryScreenData,
  props :: RideSummaryScreenProps
}


type RideSummaryScreenData = {
  rideDetails :: BookingAPIEntity,
  extraFare :: Array CTA.FareList,
  bookingId :: Maybe String,
  config :: AppConfig,
  rideList :: Array RideAPIEntity,
  merchantExoPhone :: String,
  fromScreen :: String
}

type RideSummaryScreenProps = {
  termsAndConditionOpen :: Boolean,
  excludedChargesOpen :: Boolean,
  includedChargesOpen :: Boolean, 
  pickUpOpen :: Boolean,
  isBookingAccepted :: Boolean,
  isCancelRide :: Boolean,
  showCallPopUp :: Boolean,
  hasApiFailed :: Boolean,
  shimmerVisibility :: Boolean
}

dummyAPI :: BookingAPIEntity
dummyAPI = BookingAPIEntity{
  currency : INR,
  estimatedDistance : Nothing,
  estimatedDuration : Nothing,
  estimatedFare : "",
  fromLocation : LocationInformation{
    address : {
          area :  Nothing,
          areaCode :  Nothing,
          building :  Nothing,
          city :  Nothing,
          country : Nothing,
          state :  Nothing,
          street :  Nothing,
          door :  Nothing,
          ward : Nothing,
          placeId : Nothing
      },
    placeId  : "",
    fullAddress : ""
  },
  id : "",
  isScheduled : false,
  returnTime : Nothing,
  roundTrip : Nothing,
  isAirConditioned: Nothing,
  startTime : "",
  toLocation : Just (LocationInformation{
    address : {
          area :  Nothing,
          areaCode :  Nothing,
          building :  Nothing,
          city :  Nothing,
          country : Nothing,
          state :  Nothing,
          street :  Nothing,
          door :  Nothing,
          ward : Nothing,
          placeId : Nothing
    },
    placeId  : "",
    fullAddress :""
  }),
  tripCategory : CTA.TripCategory {
    contents : Just "",
    tag : CTA.InterCity
  },
  vehicleServiceTier : "COMFY",
  vehicleServiceTierAirConditioned : Nothing,
  vehicleServiceTierName : "Mini",
  vehicleServiceTierSeatingCapacity : Nothing
}


initData :: RideSummaryScreenState
initData = {
  data: {
    rideDetails : dummyAPI,
    extraFare : [],
    bookingId : Nothing,
    config : getAppConfig appConfig,
    rideList : [],
    merchantExoPhone : "",
    fromScreen : ""
  },
  props: {
    pickUpOpen : false,
    termsAndConditionOpen : false,
    excludedChargesOpen : true,
    includedChargesOpen : true,
    isBookingAccepted : false,
    isCancelRide : false,
    showCallPopUp : false,
    hasApiFailed : false,
    shimmerVisibility : true
  }
}

dummyRideAPIEntity :: RideAPIEntity
dummyRideAPIEntity = RideAPIEntity{
  computedPrice : Nothing,
  status : "",
  vehicleModel : "",
  createdAt : "",
  driverNumber : Nothing,
  shortRideId : "",
  driverRegisteredAt : Nothing,
  vehicleNumber : "",
  rideOtp : "",
  driverName : "",
  chargeableRideDistance : Nothing,
  vehicleVariant : "",
  driverRatings : Nothing,
  vehicleColor : "",
  id : "",
  updatedAt : "",
  rideStartTime : Nothing,
  rideEndTime : Nothing,
  rideRating : Nothing,
  driverArrivalTime : Nothing,
  bppRideId : "",
  endOtp : Nothing,
  startOdometerReading : Nothing,
  endOdometerReading : Nothing,
  tollConfidence : Nothing,
  allowedEditPickupLocationAttempts : Nothing,
  destinationReachedAt : Nothing
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