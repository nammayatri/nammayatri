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
  fareDetails :: Array RateCardItem,
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
  showCallPopUp :: Boolean
}

dummyAPI :: BookingAPIEntity
dummyAPI = BookingAPIEntity{
  currency : INR,
  estimatedDistance : Just 0,
  estimatedDuration : Just 0,
  estimatedFare : "",
  fromLocation : LocationInformation{
    address : {
          area : Just "382, 2nd B Cross Rd, KHB Colony",
          areaCode : Just "382, 2nd B Cross Rd, KHB Colony",
          building : Just "382, 2nd B Cross Rd, KHB Colony",
          city : Just "Bengaluru",
          country :Just "382, 2nd B Cross Rd, KHB Colony",
          state : Just "382, 2nd B Cross Rd, KHB Colony",
          street : Just "382, 2nd B Cross Rd, KHB Colony",
          door : Just "382, 2nd B Cross Rd, KHB Colony",
          ward : Just "",
          placeId : Nothing
      },
    id  : "",
    lat : 0.0,
    lon : 0.0,
    fullAddress : ""
  },
  id : "",
  isScheduled : true,
  maxEstimatedDistance : Just 0.0,
  returnTime : Just "2016-07-22T00:00:00Z",
  roundTrip : Just true,
  isAirConditioned: Just false,
  startTime : "2016-07-22T00:00:00Z",
  toLocation : Just (LocationInformation{
    address : {
          area : Just "382, 2nd B Cross Rd, KHB Colony",
          areaCode : Just "382, 2nd B Cross Rd, KHB Colony",
          building : Just "382, 2nd B Cross Rd, KHB Colony",
          city : Just "Bengaluru",
          country :Just "382, 2nd B Cross Rd, KHB Colony",
          state : Just "382, 2nd B Cross Rd, KHB Colony",
          street : Just "382, 2nd B Cross Rd, KHB Colony",
          door : Just "382, 2nd B Cross Rd, KHB Colony",
          ward : Just "",
          placeId : Nothing
    },
    id  : "",
    lat : 0.0,
    lon : 0.0,
    fullAddress :""
  }),
  tripCategory : CTA.TripCategory {
    contents : Just "",
    tag : CTA.InterCity
  },
  vehicleServiceTier : "COMFY",
  vehicleServiceTierAirConditioned : Just 0.0,
  vehicleServiceTierName : "Mini",
  vehicleServiceTierSeatingCapacity : Just 4,
  nightTimeStart : "10:00 PM",
  nightTimeEnd : "06:00 AM"
}


initData :: RideSummaryScreenState
initData = {
  data: {
    rideDetails : dummyAPI,
    fareDetails : [],
    extraFare : [],
    bookingId : Nothing,
    config : getAppConfig appConfig,
    rideList : [],
    merchantExoPhone : "",
    fromScreen : ""
  },
  props: {
    pickUpOpen : true,
    termsAndConditionOpen : false,
    excludedChargesOpen : true,
    includedChargesOpen : true,
    isBookingAccepted : false,
    isCancelRide : false,
    showCallPopUp : false
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
  endOdometerReading : Nothing
, tollConfidence : Nothing
, allowedEditPickupLocationAttempts : Nothing
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