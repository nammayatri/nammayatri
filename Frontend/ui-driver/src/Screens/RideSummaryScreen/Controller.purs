module Screens.RideSummaryScreen.Controller where

import Debug
import Prelude
import Screens.RideSummaryScreen.ScreenData
import Components.SourceToDestination.Controller as SourceToDestinationController
import PrestoDOM (Eval, continue, exit, update , updateAndExit,updateWithCmdAndExit)
import PrestoDOM.Types.Core (class Loggable, defaultPerformLog)
import Screens (ScreenName(..), getScreen)
import Services.API
import Components.RideSummaryCard as RideSummaryCard
import Components.DropDownCard.Controller as DropDownCardController
import Screens.Types as ST
import Data.Maybe
import Common.Types.App as CTA
import Data.Int (round , toNumber)
import Data.Number as DN
import Components.PopUpModal as PopUpModal
import Timers as T
import Screens.RideSummaryScreen.ScreenData 
import Data.String(take)
import JBridge (animateCamera, enableMyLocation, firebaseLogEvent, getCurrentPosition, getHeightFromPercent, hideKeyboardOnNavigation, isLocationEnabled, isLocationPermissionEnabled, minimizeApp, openNavigation, removeAllPolylines, requestLocation, showDialer, showMarker, toast, firebaseLogEventWithTwoParams,sendMessage, stopChatListenerService, getSuggestionfromKey, scrollToEnd, getChatMessages, cleverTapCustomEvent, metaLogEvent, toggleBtnLoader, openUrlInApp, pauseYoutubeVideo, differenceBetweenTwoUTC, removeMediaPlayer, locateOnMapConfig, getKeyInSharedPrefKeys, defaultMarkerConfig)
import Engineering.Helpers.LogEvent (logEvent, logEventWithTwoParams, logEventWithMultipleParams)
import Storage (getValueToLocalStore, KeyStore(..))


instance showAction  ::  Show Action where
  show _ = ""
instance loggableAction :: Loggable Action where
  performLog = defaultPerformLog



data Action = BackPressed
              | AcceptClick
              | NoAction
              | SourceToDestinationActionController SourceToDestinationController.Action
              | RideSummaryCardActionController RideSummaryCard.Action
              | PickUp DropDownCardController.Action 
              | IncludedCharges DropDownCardController.Action 
              | ExcludedCharges DropDownCardController.Action 
              | Terms DropDownCardController.Action 
              | ShowMapInterCity Number Number Number Number String String String
              | ShowMapRental Number Number String String String
              | ShowMapRegular Number Number Number Number String String String
              | PopUpModalCancelConfirmationAction PopUpModal.Action
              | CancelClicked 
              | OnClick
              | Call
              | GoToMap Number Number
              | ScheduleTimer Int String String
              | Back
              

data ScreenOutput = GoBack 
                    | AcceptScheduleRide String 
                    | UpdateRouteInterCity Number Number Number Number 
                    | UpdateRouteRental Number Number 
                    | UpdateRouteRegular Number Number Number Number
                    | CancelRide String String String
                    | DoneButtonClicked 
                    | CallingCustomer RideSummaryScreenState String
                    | GoToOpenGoogleMap  RideSummaryScreenState
                    | PressedBack
                    

eval :: Action -> RideSummaryScreenState -> Eval Action ScreenOutput RideSummaryScreenState
eval BackPressed state = exit GoBack
eval AcceptClick state = do
                          let (BookingAPIEntity entity) = state.data.rideDetails
                              id = entity.id
                          exit $ AcceptScheduleRide id
eval (Terms DropDownCardController.OnClick) state = do
                                                      let old = state.props.termsAndConditionOpen
                                                      continue state{ props{termsAndConditionOpen = not old}}
eval (ExcludedCharges DropDownCardController.OnClick) state = do
                                                                let old = state.props.excludedChargesOpen
                                                                continue state{ props{excludedChargesOpen = not old}}
eval (IncludedCharges DropDownCardController.OnClick) state = do
                                                                let old = state.props.includedChargesOpen
                                                                continue state{ props{includedChargesOpen = not old}}
eval (PickUp DropDownCardController.OnClick) state = do
                                                      let old = state.props.pickUpOpen
                                                      continue state{ props{pickUpOpen = not old}}
eval (ShowMapInterCity slat slon dlat dlon key lat lon) state = exit $ UpdateRouteInterCity slat slon dlat dlon
eval (ShowMapRental slat slon key lat lon) state = exit $ UpdateRouteRental slat slon
eval (ShowMapRegular slat slon dlat dlon key lat lon) state = exit $ UpdateRouteRegular slat slon dlat dlon

eval (CancelClicked) state  = continue state {props {showPopUp = true}}
eval (OnClick) state  = exit $ DoneButtonClicked 
eval Back state  = exit $ PressedBack

eval (PopUpModalCancelConfirmationAction (PopUpModal.OnButton2Click)) state = do
  _ <- pure $ T.clearTimerWithId state.data.cancelRidePopUpData.timerID
  continue state { data{cancelRidePopUpData{timerID = "cancelConfirmationTimer" , continueEnabled=false, enableTimer=false}}, props{showPopUp = false}}

eval (PopUpModalCancelConfirmationAction (PopUpModal.OnButton1Click)) state = do 
   let (BookingAPIEntity entity) = state.data.rideDetails 
       _ = spy "id---->" entity.id
       id = entity.id
   updateAndExit state {data {cancelRidePopUpData{enableTimer = false}}} $ CancelRide id "" "" 

eval (PopUpModalCancelConfirmationAction (PopUpModal.CountDown seconds status timerID)) state = do
  if status == "EXPIRED" then do
    _ <- pure $ T.clearTimerWithId timerID
    continue state { data { cancelRidePopUpData{delayInSeconds = 0, timerID = "", continueEnabled = true}}}
    else do
      void $ pure $ spy "printing inside CountDown " seconds
      continue state { data {cancelRidePopUpData{delayInSeconds = seconds, timerID = timerID, continueEnabled = false}}}
eval (Call) state = do
  let exophoneNumber = if (take 1 state.data.activeRideData.exoPhone) == "0" then state.data.activeRideData.exoPhone else "0" <> state.data.activeRideData.exoPhone
  updateWithCmdAndExit state [ do
    void $ pure $ showDialer exophoneNumber false -- TODO: FIX_DIALER
    pure NoAction
    ] $ CallingCustomer state exophoneNumber
eval (GoToMap dstLt dstLn) state =  updateAndExit state  $ GoToOpenGoogleMap state

eval (ScheduleTimer seconds status timerID) state = do
  if status == "EXPIRED" then do
    void $ pure $ T.clearTimerWithId timerID
    update state{props{scheduletimerID = "" , showGoTo = true}}
  else update state{props{timer = seconds, scheduletimerID = timerID}}

eval _ state = continue state


transformer :: ST.ActiveRide -> BookingAPIEntity
transformer resp  = 
  let  src_lat = resp.src_lat
       src_lon = resp.src_lon
       dest_lat = resp.dest_lat
       dest_lon = resp.dest_lon
       exoPhone = resp.exoPhone
       tripStartTime = resp.tripStartTime
       tripEndTime = resp.tripEndTime
       estimatedFare = toNumber (resp.estimatedFare)
       estimatedDistance  = round (resp.distance)
       estimatedDuration =  fromMaybe 0  resp.tripDuration
       source  = resp.source
       destination  = resp.destination
       tripType = resp.tripType
       serviceTier = resp.serviceTier
       capacity  =  fromMaybe 0 resp.capacity
       id  = resp.id
       tripCategory = tripCtegoryTotripType resp.tripType
       driverVehicle =  driverVehicleToVechicleServiceTier  resp.driverVehicle
  in

  BookingAPIEntity
  {
    area: Default,
    createdAt : "2016-07-22T00:00:00Z",
    currency : INR,
    disabilityTag : Nothing,
    distanceToPickup : Nothing,
  estimatedDistance : Just estimatedDistance,
  estimatedDuration : Just estimatedDuration,
  estimatedFare : estimatedFare,
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
      tag :   Nothing
    },
    govtCharges : Nothing,
    id :"" ,
    nightShiftCharge : Nothing,
    nightShiftRateIfApplies : Nothing,
    parkingCharge : Nothing,
    rideExtraTimeFare : Nothing,
    serviceCharge : Nothing,
    tollCharges :Nothing,
    updatedAt : "2016-07-:00:00Z",
    waitingCharge : Just 0.0
  },
  fromLocation : Location{
    address : LocationAddress{
          area : Nothing,
          areaCode : Nothing,
          building : Nothing,
          city : Nothing,
          country :Nothing,
          state : Nothing,
          street : Nothing,
          door : Nothing,
          fullAddress : Just source
    },
    createdAt : "",
    id  : "",
    lat : src_lat,
    lon : src_lon,
    updatedAt :""
  },
  id :id ,
  isScheduled : true,
  maxEstimatedDistance : Just 0.0,
  returnTime : Nothing,
  roundTrip : Nothing,
  isAirConditioned: Just false,
  specialZoneOtpCode : Just "",
  startTime : "2016-07-22T00:00:00Z",
  status : CTA.UPCOMING,
  stopLocationId : Just "",
  toLocation : Just (Location{
    address : LocationAddress{
          area : Nothing,
          areaCode : Nothing,
          building : Nothing,
          city : Nothing,
          country :Nothing,
          state : Nothing,
          street : Nothing,
          door : Nothing,
          fullAddress :  destination
    },
    createdAt : "",
    id  : "",
    lat : dest_lat,
    lon : dest_lon,
    updatedAt :""
  }),
  tollNames : Nothing,
  tripCategory : tripCategory,
  updatedAt : "2016-07-22T00:00:00Z",
  vehicleServiceTier : COMFY,
  vehicleServiceTierAirConditioned : Just 0.0,
  vehicleServiceTierName : "Mini",
  vehicleServiceTierSeatingCapacity : Just capacity
}


tripCtegoryTotripType :: ST.TripType ->  CTA.TripCategory
tripCtegoryTotripType tripType = 
    case tripType of 
    ST.OneWay -> CTA.TripCategory {contents  : Just "" , tag : CTA.OneWay}
    ST.Rental -> CTA.TripCategory {contents : Just "" , tag : CTA.Rental}
    ST.Intercity -> CTA.TripCategory {contents : Just "" , tag : CTA.InterCity}  
    ST.RideShare -> CTA.TripCategory {contents : Just "" , tag : CTA.RideShare}
    ST.RoundTrip -> CTA.TripCategory {contents : Just "" , tag : CTA.CrossCity}





driverVehicleToVechicleServiceTier :: String -> ServiceTierType
driverVehicleToVechicleServiceTier vehicle = 
        case vehicle of 
        "SUV" -> SUV_TIER
        "Sedan" -> SEDAN_TIER
        "Non-AC Mini" -> TAXI_PLUS
        "AUTO"  ->  AUTO_RICKSHAW
        _ -> TAXI

  