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
  activeRideData :: S.ActiveRide,
  route :: Maybe Route
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
  showGoTo :: Boolean,
  shimmerVisibility :: Boolean,
  errorPopUp :: Boolean
}

dummyAPI :: BookingAPIEntity
dummyAPI = BookingAPIEntity{
  area: Default,
  createdAt : "",
  currency : INR,
  disabilityTag : Nothing,
  distanceToPickup : Nothing,
  estimatedDistance : Nothing,
  estimatedDuration : Nothing,
  estimatedFare : 0.0,
  fareParams : FareParameters {
    baseFare : 0.0,
    congestionCharge : Nothing,
    currency : INR,
    customerCancellationDues : Nothing,
    customerExtraFee : Nothing,
    driverSelectedFare : Nothing,
    fareParametersDetails : FareParametersDetails{
      contents : Content {
          currency : INR,
          deadKmFare : Nothing,
          distBasedFare : Nothing,
          distanceUnit : Nothing,
          extraDistance : Nothing, 
          extraDuration : Nothing,
          timeBasedFare : Nothing
      },
      tag : Nothing
    },
    govtCharges : Nothing,
    id : "",
    nightShiftCharge : Nothing,
    nightShiftRateIfApplies : Nothing,
    parkingCharge : Nothing,
    rideExtraTimeFare : Nothing,
    serviceCharge : Nothing,
    tollCharges : Nothing,
    updatedAt : "",
    waitingCharge : Nothing
  },
  fromLocation : Location{
    address : LocationAddress{
          area : Nothing,
          areaCode : Nothing,
          building :Nothing,
          city : Nothing,
          country :Nothing,
          state : Nothing,
          street : Nothing,
          door : Nothing,
          fullAddress : Nothing
    },
    createdAt : "",
    id  : "",
    lat : 0.0,
    lon : 0.0,
    updatedAt :""
  },
  id : "",
  isScheduled : true,
  maxEstimatedDistance : Nothing,
  returnTime : Nothing,
  roundTrip : Nothing,
  isAirConditioned: Nothing,
  specialZoneOtpCode : Nothing,
  startTime : "",
  status : CTA.UPCOMING,
  stopLocationId : Nothing,
  toLocation : Nothing,
  tollNames : Nothing,
  tripCategory : CTA.TripCategory {
    contents : Nothing,
    tag : CTA.InterCity
  },
  updatedAt : "",
  vehicleServiceTier : COMFY,
  vehicleServiceTierAirConditioned : Nothing,
  vehicleServiceTierName : "",
  vehicleServiceTierSeatingCapacity :Nothing
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
  estimatedTollCharges : 0.0,
  acRide : Nothing,
  bapName : "",
  bookingFromOtherPlatform : false,
  sourceCity : "",
  destinationCity : Nothing,
  roundTrip : false,
  returnTime : "",
  parkingCharge : 0.0,
  extraFromLocationInfo : Nothing,
  extraToLocationInfo : Nothing,
  senderInstructions : Nothing,
  receiverInstructions : Nothing,
  senderPersonDetails : Nothing,
  receiverPersonDetails : Nothing,
  notifiedReachedDestination : false,
  stops : []
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
    , route: Nothing
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
    showGoTo : false,
    shimmerVisibility : true,
    errorPopUp : false

    
  }
}