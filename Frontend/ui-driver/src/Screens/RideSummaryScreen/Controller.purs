module Screens.RideSummaryScreen.Controller where

import Debug
import Prelude
import Screens.RideSummaryScreen.ScreenData
import Components.SourceToDestination.Controller as SourceToDestinationController
import PrestoDOM (Eval, continue, exit, update , updateAndExit,updateWithCmdAndExit ,continueWithCmd)
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
import Engineering.Helpers.LogEvent (logEvent, logEventWithTwoParams, logEventWithMultipleParams)
import Storage (getValueToLocalStore, KeyStore(..))
import Effect.Aff (launchAff)
import Engineering.Helpers.Commons as EHC
import Types.App (defaultGlobalState)
import Control.Monad.Except (runExceptT)
import Control.Transformers.Back.Trans (runBackT)
import Engineering.Helpers.BackTrack (getState, liftFlowBT)
import PrestoDOM.Core (getPushFn)
import Services.Backend as Remote
import Data.Array (union, (!!), filter, length, (:), foldl, drop,  replicate, updateAt, elemIndex, (..), last, find, catMaybes, sortBy, reverse)
import JBridge as JB
import Services.Backend (walkCoordinate, walkCoordinates)
import Screens.HomeScreen.ComponentConfig (mapRouteConfig)
import Constants.Configs (getPolylineAnimationConfig)
import Effect.Uncurried (runEffectFn1, runEffectFn5, runEffectFn2, runEffectFn3, runEffectFn9, runEffectFn10)
import Engineering.Helpers.RippleCircles 
import Helpers.Utils
import Data.Number (fromString) as Number
import Components.DropDownCard.Controller


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
              | DisableShimmer
              | UpdateRoute Route
              | Notification String ST.NotificationBody
              | PopUpModalErrorAction PopUpModal.Action
              | ApiError 
              

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
                    | FcmNotification String RideSummaryScreenState
                    | GotoRideRequestscreen RideSummaryScreenState
                    

eval :: Action -> RideSummaryScreenState -> Eval Action ScreenOutput RideSummaryScreenState
eval BackPressed state = exit GoBack
eval AcceptClick state = do
                          let (BookingAPIEntity entity) = state.data.rideDetails
                              id = entity.id
                          exit $ AcceptScheduleRide id

eval (Terms (DropDownCardController.OnClick dropDownCard) ) state = do
                                                      let old = state.props.termsAndConditionOpen
                                                      continue state{ props{termsAndConditionOpen = not old}}

eval (ExcludedCharges (DropDownCardController.OnClick dropDownCard)) state = do
                                                                let old = state.props.excludedChargesOpen
                                                                continue state{ props{excludedChargesOpen = not old}}

eval (IncludedCharges (DropDownCardController.OnClick dropDownCard)) state = do
                                                                let old = state.props.includedChargesOpen
                                                                continue state{ props{includedChargesOpen = not old}}

eval (PickUp (DropDownCardController.OnClick dropDownCard)) state = do
                                                      let old = state.props.pickUpOpen
                                                      continue state{ props{pickUpOpen = not old}}
                                                      
eval (UpdateRoute route) state = continue state{data{route = Just route}, props{shimmerVisibility = false}}


eval (ShowMapInterCity slat slon dlat dlon key lat lon) state = continueWithCmd state
   [ do
        _ <- case state.data.route of
              Just (Route route) -> do
                let srcMarkerConfig = JB.defaultMarkerConfig{ pointerIcon = "ny_ic_src_marker", primaryText = "" }
                let destMarkerConfig = JB.defaultMarkerConfig{ pointerIcon = "ny_ic_dest_marker", primaryText = "", anchorU = 0.5, anchorV = 1.0 }
                let coor = walkCoordinates route.points
                let normalRoute = JB.mkRouteConfig coor srcMarkerConfig destMarkerConfig Nothing "NORMAL" "LineString" true JB.DEFAULT (mapRouteConfig "" "" false getPolylineAnimationConfig) 
                JB.drawRoute [normalRoute] (EHC.getNewIDWithTag "DriverSavedLoc1")
              Nothing -> pure unit
        pure NoAction
    ]
  

eval (ShowMapRental slat slon key lat lon) state = continueWithCmd state  
    [ do 
         _ <- case state.data.route of
               Just (Route route) -> do
                  let center = {lat: slat, lng: slon}
                  let srcMarkerConfig = JB.defaultMarkerConfig{ pointerIcon = "ny_ic_hatchback_nav_on_map", primaryText = "" }
                  let destMarkerConfig = JB.defaultMarkerConfig{ pointerIcon = "ny_ic_src_marker", primaryText = "", anchorU = 0.5, anchorV = 1.0 }
                  let coor = walkCoordinates route.points
                  let normalRoute = JB.mkRouteConfig coor srcMarkerConfig destMarkerConfig Nothing "NORMAL" "LineString" true JB.DEFAULT (mapRouteConfig "" "" false getPolylineAnimationConfig) 
                  JB.drawRoute [normalRoute] (EHC.getNewIDWithTag "DriverSavedLoc1")
                  void $ runEffectFn1 addRippleCircle circleRippleConfig{center = center, fillColor="#1042B8BA"  , radius = 1200.0, toStrokeColor = "#8042B8BA" , fromStrokeColor = "#8042B8BA" , repeatMode = 0, count = 1}
               Nothing -> pure unit
         pure NoAction

    ]

eval (ShowMapRegular slat slon dlat dlon key lat lon) state  = continueWithCmd state 
     [ do 
          void $ launchAff $ EHC.flowRunner defaultGlobalState $ runExceptT $ runBackT do 
              _ <- case state.data.route of
                    Just (Route route) -> do
                      let srcMarkerConfig = JB.defaultMarkerConfig{ pointerIcon = "ny_ic_hatchback_nav_on_map", primaryText = "" }
                          destMarkerConfig = JB.defaultMarkerConfig{ pointerIcon = "ny_ic_dest_marker", primaryText = "", anchorU = 0.5, anchorV = 1.0 }
                          coor = walkCoordinates route.points
                      (LatLon lat lon _) <- getCurrentLocation 0.0 0.0 0.0 0.0 400 false true
                      let currentDriverLat = fromMaybe 0.0 $ Number.fromString $ lat
                          currentDriverLon = fromMaybe 0.0 $ Number.fromString $ lon
                          normalRoute = JB.mkRouteConfig coor srcMarkerConfig destMarkerConfig Nothing "NORMAL" "LineString" true JB.DEFAULT (mapRouteConfig "" "" false getPolylineAnimationConfig) 
                          markerConfig = JB.defaultMarkerConfig {markerId = "ny_ic_demo_location", pointerIcon = "ny_ic_demo_location"}
                      liftFlowBT $ JB.drawRoute [normalRoute] (EHC.getNewIDWithTag "DriverSavedLoc1")
                      void $ liftFlowBT $ JB.showMarker markerConfig slat slon 160 0.5 0.9 (EHC.getNewIDWithTag "DriverSavedLoc1")
                      pure unit
                    Nothing -> pure unit
              pure unit
          pure NoAction 

     ]

eval (CancelClicked) state  = continue state {props {showPopUp = true}}

eval (OnClick) state  = exit $ DoneButtonClicked 

eval Back state  = exit $ PressedBack

eval (PopUpModalCancelConfirmationAction (PopUpModal.OnButton2Click)) state = do
  _ <- pure $ T.clearTimerWithId state.data.cancelRidePopUpData.timerID
  continue state { data{cancelRidePopUpData{timerID = "cancelConfirmationTimer" , continueEnabled=false, enableTimer=false}}, props{showPopUp = false , errorPopUp  = false}}

eval (PopUpModalCancelConfirmationAction (PopUpModal.OnButton1Click)) state = do 
   let (BookingAPIEntity entity) = state.data.rideDetails 
       id = entity.id
   updateAndExit state {data {cancelRidePopUpData{enableTimer = false}}} $ CancelRide id "" "" 

eval (PopUpModalCancelConfirmationAction (PopUpModal.CountDown seconds status timerID)) state = do
  if status == "EXPIRED" then do
    _ <- pure $ T.clearTimerWithId timerID
    continue state { data { cancelRidePopUpData{delayInSeconds = 0, timerID = "", continueEnabled = true}}}
    else do
      continue state { data {cancelRidePopUpData{delayInSeconds = seconds, timerID = timerID, continueEnabled = false}}}

eval (PopUpModalErrorAction (PopUpModal.OnButton2Click)) state  = updateAndExit state {props{errorPopUp = false}} $ GotoRideRequestscreen state

eval (Call) state = do
  let exophoneNumber = if (take 1 state.data.activeRideData.exoPhone) == "0" then state.data.activeRideData.exoPhone else "0" <> state.data.activeRideData.exoPhone
  updateWithCmdAndExit state [ do
    void $ pure $ JB.showDialer exophoneNumber false
    pure NoAction
    ] $ CallingCustomer state exophoneNumber

eval (GoToMap dstLt dstLn) state =  updateAndExit state  $ GoToOpenGoogleMap state


eval (ScheduleTimer seconds status timerID) state = do
  if status == "EXPIRED" then do
    void $ pure $ T.clearTimerWithId timerID
    continue state{props{scheduletimerID = "" , showGoTo = true}}
  else continue state{props{timer = seconds, scheduletimerID = timerID}}

eval (DisableShimmer) state = continue state{props{shimmerVisibility = false}}

eval (Notification notificationType _) state = do
    if (checkNotificationType notificationType ST.DRIVER_ASSIGNMENT ) then do
      exit $ FcmNotification notificationType state
    else if (checkNotificationType notificationType ST.CANCELLED_PRODUCT ) then do 
      exit $ FcmNotification notificationType state
    else continue state

eval ApiError state = continue state{props{shimmerVisibility = false}}

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
       tripCategory = tripCategoryTotripType resp.tripType
       driverVehicle =  driverVehicleToVechicleServiceTier  resp.serviceTier
       tripScheduledAt  = resp.tripScheduledAt
       acRide  =  resp.acRide
       sourceCity  = resp.sourceCity
       destinationCity  = fromMaybe "" resp.destinationCity
       roundTrip = resp.roundTrip
       returnTime  = resp.returnTime
       sourceArea  = resp.sourceArea
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
          area : sourceArea,
          areaCode : Nothing,
          building : Nothing,
          city : Just sourceCity,
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
  maxEstimatedDistance : Nothing,
  returnTime : Just returnTime,
  roundTrip : Just roundTrip,
  isAirConditioned: acRide,
  specialZoneOtpCode : Just "",
  startTime :fromMaybe "" tripScheduledAt,
  status : CTA.UPCOMING,
  stopLocationId : Nothing,
  toLocation : Just $ Location {
    address : LocationAddress{
          area : Nothing,
          areaCode : Nothing,
          building : Nothing,
          city : Just destinationCity,
          country :Nothing,
          state : Nothing,
          street : Nothing,
          door : Nothing,
          fullAddress : destination
          
    },
    createdAt : "",
    id  : "",
    lat : dest_lat,
    lon : dest_lon,
    updatedAt :""
  },
  tollNames : Nothing,
  tripCategory : tripCategory,
  updatedAt : fromMaybe "" tripScheduledAt,
  vehicleServiceTier : driverVehicle,
  vehicleServiceTierAirConditioned : Just 0.0,
  vehicleServiceTierName : serviceTier,
  vehicleServiceTierSeatingCapacity : Just capacity
}


tripCategoryTotripType :: ST.TripType ->  CTA.TripCategory
tripCategoryTotripType tripType = 
    case tripType of 
    ST.OneWay -> CTA.TripCategory {contents  : Nothing , tag : CTA.OneWay}
    ST.Rental -> CTA.TripCategory {contents : Nothing , tag : CTA.Rental}
    ST.Intercity -> CTA.TripCategory {contents : Nothing , tag : CTA.InterCity}  
    ST.RideShare -> CTA.TripCategory {contents : Nothing , tag : CTA.RideShare}
    ST.RoundTrip -> CTA.TripCategory {contents : Nothing , tag : CTA.CrossCity}
    ST.Delivery -> CTA.TripCategory {contents : Nothing , tag : CTA.Delivery}
