module Screens.RideSummaryScreen.ScreenData where

import Prelude
import Screens.Types
import Services.API
import Common.Types.App as CTA
import Data.Maybe


type RideSummaryScreenState = {
  data :: RideSummaryScreenData,
  props :: RideSummaryScreenProps
}


type RideSummaryScreenData = {
  rideDetails :: BookingAPIEntity,
  fareDetails :: Array RateCardItem,
  extraFare :: Array CTA.FareList,
  bookingId :: Maybe String
}

type RideSummaryScreenProps = {
  termsAndConditionOpen :: Boolean,
  excludedChargesOpen :: Boolean,
  includedChargesOpen :: Boolean, 
  pickUpOpen :: Boolean,
  isBookingAccepted :: Boolean
}

dummyAPI :: BookingAPIEntity
dummyAPI = BookingAPIEntity{
  createdAt : "2016-07-22T00:00:00Z",
  currency : INR,
  disabilityTag : Just "",
  distanceToPickup : Just 0,
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
    createdAt : "",
    id  : "",
    lat : 0.0,
    lon : 0.0,
    updatedAt :"",
    fullAddress : ""
  },
  id : "",
  isScheduled : true,
  maxEstimatedDistance : Just 0.0,
  returnTime : Just "2016-07-22T00:00:00Z",
  roundTrip : Just true,
  isAirConditioned: Just false,
  specialZoneOtpCode : Just "",
  startTime : "2016-07-22T00:00:00Z",
  status : CTA.UPCOMING,
  stopLocationId : Just "",
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
    createdAt : "",
    id  : "",
    lat : 0.0,
    lon : 0.0,
    updatedAt :"",
    fullAddress :""
  }),
  tollNames : Just [""],
  tripCategory : CTA.TripCategory {
    contents : Just "",
    tag : CTA.InterCity
  },
  updatedAt : "2016-07-22T00:00:00Z",
  vehicleServiceTier : "COMFY",
  vehicleServiceTierAirConditioned : Just 0.0,
  vehicleServiceTierName : "Mini",
  vehicleServiceTierSeatingCapacity : Just "4",
  nightTimeStart : "10:00 PM",
  nightTimeEnd : "06:00 AM"
}


initData :: RideSummaryScreenState
initData = {
  data: {
    rideDetails : dummyAPI,
    fareDetails : [],
    extraFare : [],
    bookingId : Nothing
  },
  props: {
    pickUpOpen : true,
    termsAndConditionOpen : false,
    excludedChargesOpen : true,
    includedChargesOpen : true,
    isBookingAccepted : false
  }
}