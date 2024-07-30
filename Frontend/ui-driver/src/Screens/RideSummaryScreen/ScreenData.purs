module Screens.RideSummaryScreen.ScreenData where

import Prelude
import Screens.Types as S
import Services.API
import Common.Types.App as CTA
import Data.Maybe
import JBridge as JB
import Data.Function.Uncurried (runFn2)
import Engineering.Helpers.Commons (screenWidth, convertUTCtoISC, getNewIDWithTag , getCurrentUTC)


type RideSummaryScreenState = {
  data :: RideSummaryScreenData,
  props :: RideSummaryScreenProps
}


type RideSummaryScreenData = {
  rideDetails :: BookingAPIEntity,
  fareDetails :: Array RateCardItem,
  cancelRidePopUpData :: S.CancelRidePopUpData,
  activeRideData :: S.ActiveRide
}

type RideSummaryScreenProps = {
  termsAndConditionOpen :: Boolean,
  excludedChargesOpen :: Boolean,
  includedChargesOpen :: Boolean, 
  pickUpOpen :: Boolean ,
  showPopUp :: Boolean,
  throughBanner :: Boolean,
  timer :: Int,
  scheduletimerID :: String,
  showGoTo :: Boolean
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

dummyActiveRideData :: S.ActiveRide 
dummyActiveRideData = {
  id : "",
  source : "",
   sourceArea : Nothing,
  destination : Nothing,
  destinationArea : Nothing,
  src_lat : 0.0,
  src_lon :0.0,
  dest_lat : 0.0,
  dest_lon :0.0,
  actualRideDistance : 0.0,
  status : INPROGRESS,
  distance : 0.0,
  exoPhone : "",
  duration : 0,
  riderName : "",
  estimatedFare : 0,
  waitTimerId : "",
  notifiedCustomer : false,
  waitTimeInfo : false,
  rideCreatedAt : "",
  specialLocationTag : Nothing,
  requestedVehicleVariant : Nothing,
  disabilityTag : Nothing,
  waitTimeSeconds : 0,
  enableFrequentLocationUpdates : false,
  tripScheduledAt : Nothing,
  tripType : S.OneWay,
  tripStartTime : Nothing,
  tripEndTime : Nothing,
  tripDuration : Nothing,
  actualRideDuration : Nothing,
  nextStopAddress :  Nothing,
  lastStopAddress : Nothing,
  nextStopLat : Nothing,
  nextStopLon : Nothing,
  tripActualDistance : Nothing,
  lastStopLat : Nothing,
  lastStopLon : Nothing,
  startOdometerReading : Nothing,
  endOdometerReading : Nothing,
  driverVehicle : "",
  serviceTier : "",
  capacity : Nothing,
  hasToll : false,
  estimatedTollCharge : Nothing,
  acRide : Nothing,
  bapName : "",
  bookingFromOtherPlatform : false
}


initData :: RideSummaryScreenState
initData = {
  data: {
    rideDetails : dummyAPI
    ,fareDetails : []
    , cancelRidePopUpData:
          { delayInSeconds: 5
          , timerID: ""
          , continueEnabled: false
          , enableTimer: true
        }
    , activeRideData :dummyActiveRideData
  },
  props: {
    pickUpOpen : true,
    termsAndConditionOpen : false,
    excludedChargesOpen : false,
    includedChargesOpen : false,
    showPopUp  :false,
    throughBanner : false,
    timer : 7800  ,
    scheduletimerID : "scheduledTimer",
    showGoTo : false

    
  }
}