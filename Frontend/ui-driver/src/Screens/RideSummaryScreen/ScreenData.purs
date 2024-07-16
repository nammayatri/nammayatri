module Screens.RideSummaryScreen.ScreenData where

import Prelude
import Screens.Types as S
import Services.API
import Common.Types.App as CTA
import Data.Maybe


type RideSummaryScreenState = {
  data :: RideSummaryScreenData,
  props :: RideSummaryScreenProps
}


type RideSummaryScreenData = {
  rideDetails :: BookingAPIEntity,
  fareDetails :: Array RateCardItem
}

type RideSummaryScreenProps = {
  termsAndConditionOpen :: Boolean,
  excludedChargesOpen :: Boolean,
  includedChargesOpen :: Boolean, 
  pickUpOpen :: Boolean 
}

dummyAPI :: BookingAPIEntity
dummyAPI = BookingAPIEntity{
  area: Default,
  createdAt : "2016-07-22T00:00:00Z",
  currency : INR,
  disabilityTag : Just "",
  distanceToPickup : Just 0,
  estimatedDistance : Just 0,
  estimatedDuration : Just 0,
  estimatedFare : 0.0,
  fareParams : FareParameters {
    baseFare : 0.0,
    congestionCharge : Just 0.0,
    currency : INR,
    customerCancellationDues : Just 0.0,
    customerExtraFee : Just 0.0,
    driverSelectedFare : Just 0.0,
    fareParametersDetails : FareParametersDetails{
      contents : Content {
          currency : INR,
          deadKmFare : Just 0,
          distBasedFare : Just 0,
          distanceUnit : Just "",
          extraDistance : Just 0, 
          extraDuration : Just 0,
          timeBasedFare : Just 0
      },
      tag : Just ""
    },
    govtCharges : Just 0.0,
    id : "",
    nightShiftCharge : Just 0.0,
    nightShiftRateIfApplies : Just 0.0,
    parkingCharge : Just 0.0,
    rideExtraTimeFare : Just 0.0,
    serviceCharge : Just 0.0,
    tollCharges : Just 0.0,
    updatedAt : "2016-07-22T00:00:00Z",
    waitingCharge : Just 0.0
  },
  fromLocation : Location{
    address : LocationAddress{
          area : Just "382, 2nd B Cross Rd, KHB Colony",
          areaCode : Just "382, 2nd B Cross Rd, KHB Colony",
          building : Just "382, 2nd B Cross Rd, KHB Colony",
          city : Just "Bengaluru",
          country :Just "382, 2nd B Cross Rd, KHB Colony",
          state : Just "382, 2nd B Cross Rd, KHB Colony",
          street : Just "382, 2nd B Cross Rd, KHB Colony",
          door : Just "382, 2nd B Cross Rd, KHB Colony",
          fullAddress : Just "382, 2nd B Cross Rd, KHB Colony, 5th Block, Koramangala, Bengaluru, Karnataka 560095"
    },
    createdAt : "",
    id  : "",
    lat : 0.0,
    lon : 0.0,
    updatedAt :""
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
  toLocation : Just (Location{
    address : LocationAddress{
          area : Just "382, 2nd B Cross Rd, KHB Colony",
          areaCode : Just "382, 2nd B Cross Rd, KHB Colony",
          building : Just "382, 2nd B Cross Rd, KHB Colony",
          city : Just "Bengaluru",
          country :Just "382, 2nd B Cross Rd, KHB Colony",
          state : Just "382, 2nd B Cross Rd, KHB Colony",
          street : Just "382, 2nd B Cross Rd, KHB Colony",
          door : Just "382, 2nd B Cross Rd, KHB Colony",
          fullAddress : Just "382, 2nd B Cross Rd, KHB Colony, 5th Block, Koramangala, Bengaluru, Karnataka 560095"
    },
    createdAt : "",
    id  : "",
    lat : 0.0,
    lon : 0.0,
    updatedAt :""
  }),
  tollNames : Just [""],
  tripCategory : CTA.TripCategory {
    contents : Just "",
    tag : CTA.InterCity
  },
  updatedAt : "2016-07-22T00:00:00Z",
  vehicleServiceTier : COMFY,
  vehicleServiceTierAirConditioned : Just 0.0,
  vehicleServiceTierName : "Mini",
  vehicleServiceTierSeatingCapacity : Just 4
}


initData :: RideSummaryScreenState
initData = {
  data: {
    rideDetails : dummyAPI,
    fareDetails : []
  },
  props: {
    pickUpOpen : true,
    termsAndConditionOpen : false,
    excludedChargesOpen : false,
    includedChargesOpen : false
  }
}