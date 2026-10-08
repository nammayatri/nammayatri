{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.MyRidesScreen.Controller where

import ConfigProvider
import Debug
import Accessor (_amount, _computedPrice, _contents, _description, _driverName, _estimatedDistance, _id, _list, _rideRating, _toLocation, _vehicleNumber, _otpCode, _vehicleVariant, _stopLocation , _status ,_isScheduled)
import Common.Types.App as CTP
import Components.ErrorModal as ErrorModal
import Components.GenericHeader as GenericHeader
import Components.IndividualRideCard.Controller as IndividualRideCardController
import Components.PrimaryButton as PrimaryButton
import Data.Array (union, (!!), length, filter, unionBy, head, all, null, sortWith, reverse, any)
import Data.Function.Uncurried (runFn2)
import Data.Int (fromString, round, toNumber)
import Data.Lens ((^.))
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.String (Pattern(..), split)
import Effect.Unsafe (unsafePerformEffect)
import Engineering.Helpers.Commons (convertUTCtoISC,getCurrentUTC)
import Engineering.Helpers.Commons (strToBool, os)
import Engineering.Helpers.LogEvent (logEvent)
import Engineering.Helpers.Utils (getFixedTwoDecimals, showToast, getCityFromString, isAmbulance)
import Helpers.SpecialZoneAndHotSpots (getSpecialTag)
import Helpers.Utils (parseFloat, rotateArray, setEnabled, setRefreshing, isHaveFare, withinTimeRange, fetchImage, FetchImageFrom(..), isParentView, emitTerminateApp, getVehicleVariantImage, getAssetLink, getCityConfig)
import JBridge (firebaseLogEvent)
import JBridge (differenceBetweenTwoUTCInMinutes)
import Language.Strings (getString, getVarString)
import Language.Types (STR(..))
import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppScreenEvent)
import MerchantConfig.Utils (getMerchant, Merchant(..))
import Prelude (class Show, pure, unit, bind, map, discard, show, ($), (==), (&&), (+), (/=), (<>), (||), (-), (<), (/), (*), negate, (<<<), not, void, (<#>),(>))
import PrestoDOM (Eval, update, ScrollState(..), continue, continueWithCmd, exit, updateAndExit)
import PrestoDOM.List as PrestoList
import PrestoDOM.Types.Core (class Loggable, toPropValue)
import Resources.Constants (DecodeAddress(..), decodeAddress, getFaresList, getFareFromArray, getFilteredFares, getKmMeter, fetchVehicleVariant)
import Resources.LocalizableV2.Strings (getEN)
import Screens (ScreenName(..), getScreen)
import Screens.HomeScreen.Transformer (dummyRideAPIEntity, getFareProductType)
import Screens.MyRidesScreen.ScreenData (dummyBookingDetails)
import Screens.Types (AnimationState(..), FareComponent, Fares, IndividualRideCardState, ItemState, MyRidesScreenState, Stage(..), ZoneType(..), VehicleVariant(..), VehicleViewType(..),NotificationBody)
import Screens.Types (FareProductType(..)) as FPT
import Data.Array as DA
import Helpers.Utils as HU
import Services.API (FareBreakupAPIEntity(..), RideAPIEntity(..), RideBookingListRes, RideBookingRes(..), RideBookingAPIDetails(..))
import Storage (isLocalStageOn, getValueToLocalStore, KeyStore(..))
import Styles.Colors as Color

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    AfterRender -> trackAppScreenRender appId "screen" (getScreen MY_RIDES_SCREEN)
    BackPressed -> do
      trackAppBackPress appId (getScreen MY_RIDES_SCREEN)
      trackAppEndScreen appId (getScreen MY_RIDES_SCREEN)
    OnFadeComplete str -> trackAppActionClick appId (getScreen MY_RIDES_SCREEN) "in_screen" "on_fade"
    Refresh -> trackAppActionClick appId (getScreen MY_RIDES_SCREEN) "in_screen" "refresh"
    Loader -> trackAppActionClick appId (getScreen MY_RIDES_SCREEN) "in_screen" "loader"
    GenericHeaderActionController act -> case act of
      GenericHeader.PrefixImgOnClick -> do
        trackAppActionClick appId (getScreen MY_RIDES_SCREEN) "generic_header_action" "back_icon"
        trackAppEndScreen appId (getScreen MY_RIDES_SCREEN)
      GenericHeader.SuffixImgOnClick -> trackAppActionClick appId (getScreen MY_RIDES_SCREEN) "generic_header_action" "forward_icon"
    IndividualRideCardActionController act -> case act of
      IndividualRideCardController.OnClick index -> trackAppActionClick appId (getScreen MY_RIDES_SCREEN) "individual_ride_card" "individual_ride"
      IndividualRideCardController.RepeatRide index -> trackAppActionClick appId (getScreen MY_RIDES_SCREEN) "individual_ride_card" "repeat_ride"
      IndividualRideCardController.NoAction int -> trackAppActionClick appId (getScreen MY_RIDES_SCREEN) "individual_ride_card" "no_action"
      IndividualRideCardController.OnRideToastAC -> trackAppActionClick appId (getScreen MY_RIDES_SCREEN) "individual_ride_card" "on_ride_toast"
    ErrorModalActionController act -> case act of
      ErrorModal.PrimaryButtonActionController act -> case act of
        PrimaryButton.OnClick -> do
          trackAppActionClick appId (getScreen MY_RIDES_SCREEN) "error_modal_action" "book_now_primary_button"
          trackAppEndScreen appId (getScreen MY_RIDES_SCREEN)
        PrimaryButton.NoAction -> trackAppActionClick appId (getScreen MY_RIDES_SCREEN) "error_modal_action" "primary_button_no_action"
    APIFailureActionController act -> case act of
      ErrorModal.PrimaryButtonActionController  act -> case act of
        PrimaryButton.OnClick -> do
          trackAppActionClick appId (getScreen MY_RIDES_SCREEN) "api_failure_error_modal_action" "primary_button"
          trackAppEndScreen appId (getScreen MY_RIDES_SCREEN)
        PrimaryButton.NoAction -> trackAppActionClick appId (getScreen MY_RIDES_SCREEN) "api_failure_error_modal_action" "primary_button_no_action"
    Scroll str -> trackAppScreenEvent appId (getScreen MY_RIDES_SCREEN) "in_screen" "scroll"
    ScrollStateChanged scrollState -> trackAppScreenEvent appId (getScreen MY_RIDES_SCREEN) "in_screen" "scroll_state_changed"
    RideBookingListAPIResponseAction rideList status -> trackAppScreenEvent appId (getScreen MY_RIDES_SCREEN) "in_screen" "ride_booking_list"
    NoAction -> trackAppScreenEvent appId (getScreen MY_RIDES_SCREEN) "in_screen" "no_action"
    NotificationListener notification notificationBody -> trackAppScreenEvent appId (getScreen MY_RIDES_SCREEN) "in_screen" "notification_listener_action"
data ScreenOutput = GoBack MyRidesScreenState
  | MyRidesScreen MyRidesScreenState
  | GoToTripDetails MyRidesScreenState
  | LoaderOutput MyRidesScreenState
  | BookRide
  | RepeatRide MyRidesScreenState
  | GoToRideScheduledScreen MyRidesScreenState
  | NotificationListenerSO String NotificationBody

data Action = NoAction
            | OnFadeComplete String
            | Refresh
            | Loader
            | BackPressed
            | GenericHeaderActionController GenericHeader.Action
            | RideBookingListAPIResponseAction RideBookingListRes String
            | IndividualRideCardActionController IndividualRideCardController.Action
            | ErrorModalActionController ErrorModal.Action
            | APIFailureActionController ErrorModal.Action
            | Scroll String 
            | AfterRender
            | ScrollStateChanged ScrollState 
            | NotificationListener String NotificationBody

eval :: Action -> MyRidesScreenState -> Eval Action ScreenOutput MyRidesScreenState

eval BackPressed state = 
  if isParentView CTP.FunctionCall 
    then do 
      void $ pure $ emitTerminateApp Nothing true
      continue state
    else exit $ GoBack state

eval (ScrollStateChanged scrollState) state =  case scrollState of
           SCROLL_STATE_FLING ->  continue state { props {scrollEnable =true}}
           _ -> continue state
eval (GenericHeaderActionController (GenericHeader.PrefixImgOnClick )) state = continueWithCmd state [do pure BackPressed]

eval (IndividualRideCardActionController (IndividualRideCardController.OnRideToastAC)) state = do 
  void $ pure $ showToast $ getString ALREADY_HAVE_AN_ACTIVE_RIDE
  continue state

eval (OnFadeComplete _ ) state = do
                      if not state.props.receivedResponse then continue state
                      else continue state {
                                shimmerLoader = case state.shimmerLoader of
                                                  AnimatedIn ->AnimatedOut
                                                  AnimatingOut -> AnimatedOut
                                                  a -> a
                                      }


eval (Loader) state = updateAndExit state{shimmerLoader = AnimatedIn, props{loaderButtonVisibility = false}} $ LoaderOutput state

eval (Scroll value) state = do
  let firstIndex = fromMaybe 0 (fromString (fromMaybe "0"((split (Pattern ",")(value))!!0)))
  let visibleItems = fromMaybe 0 (fromString (fromMaybe "0"((split (Pattern ",")(value))!!1)))
  let totalItems = fromMaybe 0 (fromString (fromMaybe "0"((split (Pattern ",")(value))!!2)))
  let canScrollUp = fromMaybe true (strToBool (fromMaybe "true" ((split (Pattern ",")(value))!!3)))
  let loadMoreButton = if (totalItems == (firstIndex + visibleItems) && totalItems /= 0 && totalItems /= visibleItems) then true else false
  let  enableScroll =  (firstIndex /= 0)
  let _ =  state
  continue state { props{loaderButtonVisibility = loadMoreButton, scrollEnable = enableScroll}}

eval (IndividualRideCardActionController (IndividualRideCardController.OnClick index)) state = do 
  let selectedCard = state.itemsRides !! index
  case selectedCard of
    Just selectedRide -> do
      if (selectedRide.isScheduled == "visible") then exit $ GoToRideScheduledScreen state { data { selectedItem = selectedRide}}
        else exit $ GoToTripDetails state { data { selectedItem = selectedRide}}
    Nothing -> continue state

eval (IndividualRideCardActionController (IndividualRideCardController.RepeatRide index)) state = do
  let _ = unsafePerformEffect $ logEvent state.data.logField "ny_user_repeat_ride_btn_click"
  let selectedCard = state.itemsRides !! index
  case selectedCard of
    Just selectedRide -> do
      exit $ RepeatRide state { data { selectedItem = selectedRide}}
    Nothing -> continue state


eval (RideBookingListAPIResponseAction rideList status) state = do
  _ <- pure $ setRefreshing "2000031" false
  case status of
    "success" -> do
                  let 
                      filteredList = if state.props.fromBanner then filter (\(RideBookingRes item) -> do
                                  let rideScheduledTime = fromMaybe (getCurrentUTC "") item.rideScheduledTime
                                  (any (_ == item.status) ["CONFIRMED" , "TRIP_ASSIGNED"]) && (item.isScheduled ||(rideScheduledTime > (getCurrentUTC "")))) (rideList^._list) else rideList ^. _list
                      bufferCardDataPrestoList = myRideListTransformerProp filteredList
                      bufferCardData = myRideListTransformer state filteredList
                      loaderBtnDisabled = length (filteredList) == 0
                  _ <- pure $ setRefreshing "2000031" false
                  continue $ state {shimmerLoader = AnimatedOut ,prestoListArrayItems = union (state.prestoListArrayItems) (bufferCardDataPrestoList), itemsRides = unionBy matchRidebyId (state.itemsRides) (bufferCardData),props{loadMoreDisabled = loaderBtnDisabled, receivedResponse = true ,refreshLoader = false, apiFailure = false}}
    "listCompleted" -> continue state {data{loadMoreText = false},props  {refreshLoader = false, apiFailure = false}}
    _ -> continue state{props{receivedResponse = true, apiFailure = true, loadMoreDisabled = true}}

eval Refresh state = updateAndExit state{props{ receivedResponse = false, loaderButtonVisibility = false ,refreshLoader = true}} $  MyRidesScreen state

eval (ErrorModalActionController (ErrorModal.PrimaryButtonActionController PrimaryButton.OnClick)) state = exit $ BookRide

eval (APIFailureActionController (ErrorModal.PrimaryButtonActionController PrimaryButton.OnClick)) state = exit $ BookRide

eval (NotificationListener notification notificationBody) state = exit $ NotificationListenerSO notification notificationBody

eval _ state = update state

myRideListTransformerProp :: Array RideBookingRes  -> Array ItemState
myRideListTransformerProp listRes =
    filter (\item -> (any (_ == item.status) [(toPropValue "COMPLETED"), (toPropValue "CANCELLED"), (toPropValue "REALLOCATED"), (toPropValue "CONFIRMED"),(toPropValue "UPCOMING") ,(toPropValue "TRIP_ASSIGNED")])) (map (\(RideBookingRes ride) -> 
    let (RideBookingAPIDetails rideApiDetails) = ride.bookingDetails
        rideStartTime = fromMaybe ride.createdAt $ if any (_ == ride.status ) ["CONFIRMED","TRIP_ASSIGNED"] then ride.rideScheduledTime else ride.rideStartTime
        isScheduled =  (any( _ == ride.status ) ["CONFIRMED","TRIP_ASSIGNED"]) && (rideStartTime > (getCurrentUTC ""))
        destination = fromMaybe dummyBookingDetails $ if (getFareProductType rideApiDetails.fareProductType) == FPT.RENTAL then (ride.bookingDetails ^._contents^._stopLocation) else (ride.bookingDetails ^._contents^._toLocation)
        imageInfo = case fetchVehicleVariant ((fromMaybe dummyRideAPIEntity (ride.rideList !!0) )^._vehicleVariant)of
                    Just variant -> split (Pattern ",") (getVehicleVariantImage (show variant) RIGHT_VIEW)
                    Nothing -> if isJust ride.vehicleServiceTierType then split (Pattern ",") (getVehicleVariantImage (fromMaybe "" ride.vehicleServiceTierType) RIGHT_VIEW) else ["",""]
        imageName = fromMaybe "" $ imageInfo !!0
        imageUrl = fromMaybe "" $ imageInfo !!1
        rideType = getFareProductType rideApiDetails.fareProductType
        rideString = if rideType == FPT.INTER_CITY then (getString INTER_CITY_ <>" " <> getString RIDE)
                      else if rideType == FPT.RENTAL then (getString RENTAL_RIDE)
                      else (getString RIDE)
        rideTypeString = (if isScheduled then (getString SCHEDULED <> " ") else "" )<> (rideString)
        rideTypeBackground = if rideType  == FPT.INTER_CITY then Color.blue800
                            else if rideType == FPT.RENTAL then Color.blueGreen
                            else Color.blueGreen
    in
      {
        date : toPropValue (( (fromMaybe "" ((split (Pattern ",") (convertUTCtoISC rideStartTime "llll")) !!0 )) <> ", " <>  (convertUTCtoISC rideStartTime "Do MMM") )),
        time : toPropValue (convertUTCtoISC rideStartTime "h:mm A"),
        source : toPropValue (decodeAddress (Booking ride.fromLocation)),
        destination : toPropValue (decodeAddress $ Booking destination),
        totalAmount : toPropValue ((getCurrency appConfig) <> " " <> show (fromMaybe 0 ((fromMaybe dummyRideAPIEntity (ride.rideList !!0) )^. _computedPrice))),
        cardVisibility : toPropValue "visible",
        shimmerVisibility : toPropValue "gone",
        driverImage : toPropValue $ fetchImage FF_ASSET "ny_ic_user",
        isCancelled : toPropValue (if ride.status == "CANCELLED" || ride.status == "REALLOCATED" then "visible" else "gone"),
        isSuccessfull : toPropValue (if ride.status == "COMPLETED" then "visible" else "gone"),
        isScheduled : toPropValue (if isScheduled then "visible" else "gone"),
        rating : toPropValue (fromMaybe 0 ((fromMaybe dummyRideAPIEntity (ride.rideList !!0) )^. _rideRating)),
        driverName : toPropValue ((fromMaybe dummyRideAPIEntity (ride.rideList !!0) )^. _driverName),
        rideStartTime : toPropValue (convertUTCtoISC rideStartTime "h:mm A"),
        rideEndTime : toPropValue (convertUTCtoISC (fromMaybe "" ride.rideEndTime) "h:mm A"),
        vehicleNumber : toPropValue ((fromMaybe dummyRideAPIEntity (ride.rideList !!0) )^._vehicleNumber),
        rideId : toPropValue ((fromMaybe dummyRideAPIEntity (ride.rideList !!0) )^._id),
        status :toPropValue (if ride.status == "REALLOCATED" then "CANCELLED" else ride.status),
        rideEndTimeUTC : toPropValue (fromMaybe ride.createdAt ride.rideEndTime),
        alpha : toPropValue if isLocalStageOn HomeScreen then "1.0" else "0.5",
        zoneVisibility : toPropValue if ((getSpecialTag ride.specialLocationTag).priorityTag == METRO && not (rideType == FPT.RENTAL || rideType == FPT.INTER_CITY )) then "visible" else "gone",
        showRepeatRide : toPropValue if getFareProductType rideApiDetails.fareProductType == FPT.RENTAL || isScheduled then "gone" else "visible",
        showDestination : toPropValue if (decodeAddress $ Booking destination) == "" then "gone" else "visible",
        variantImage : toPropValue $ if os == "IOS" then "url->" <> imageUrl <> "," <> imageName else  "url->" <> imageUrl,
        showVariantImage : toPropValue "visible",
        itemRideType : toPropValue (rideTypeString),
        rideTypeVisibility : toPropValue (if (rideType == FPT.INTER_CITY || rideType == FPT.RENTAL) then "visible" else "gone"),
        rideTypeBackground : toPropValue $ rideTypeBackground,
        cornerRadius : toPropValue $ "40.0"
      }) $ reverse $ sortWith (\(RideBookingRes ride) ->fromMaybe ride.createdAt $ if any (_ == ride.status) ["CONFIRMED","TRIP_ASSIGNED"] then ride.rideScheduledTime else ride.rideStartTime) listRes)


myRideListTransformer :: MyRidesScreenState -> Array RideBookingRes -> Array IndividualRideCardState
myRideListTransformer state listRes = filter (\item -> (any (_ == item.status) ["COMPLETED", "CANCELLED", "REALLOCATED", "CONFIRMED", "UPCOMING", "TRIP_ASSIGNED" ])) (map (\(RideBookingRes ride) ->
  let
    (RideBookingAPIDetails rideApiDetails) = ride.bookingDetails
    fares = getFares ride.fareBreakup
    (RideAPIEntity rideDetails) = (fromMaybe dummyRideAPIEntity (ride.rideList !!0))
    baseDistanceVal = (getKmMeter (fromMaybe 0 (rideDetails.chargeableRideDistance)))
    timeVal = (convertUTCtoISC (fromMaybe ride.createdAt ride.rideStartTime) "HH:mm:ss")
    nightChargesVal = (withinTimeRange "22:00:00" "5:00:00" timeVal)
    updatedFareList = getFaresList ride.fareBreakup baseDistanceVal (rideApiDetails.fareProductType == "OneWaySpecialZoneAPIDetails")
    specialTags = getSpecialTag ride.specialLocationTag
    cityStr = getValueToLocalStore CUSTOMER_LOCATION
    city = getCityFromString cityStr
    nightChargeFrom = if city == CTP.Delhi then "11 PM" else "10 PM"
    nightChargeTill = "5 AM"
    startTime = fromMaybe "" ride.rideStartTime
    endTime = fromMaybe "" ride.rideEndTime
    rideStartTime = fromMaybe ride.createdAt $ if (ride.status == "CONFIRMED" || ride.status == "TRIP_ASSIGNED") then ride.rideScheduledTime else ride.rideStartTime
    isScheduled = (ride.status == "CONFIRMED" || ride.status == "TRIP_ASSIGNED" ) && (rideStartTime > (getCurrentUTC ""))
    destination = (fromMaybe dummyBookingDetails $ if (getFareProductType rideApiDetails.fareProductType) == FPT.RENTAL then (ride.bookingDetails ^._contents^._stopLocation) else (ride.bookingDetails ^._contents^._toLocation))
    cityConfig = getCityConfig state.data.config.cityConfig cityStr
    rideType = getFareProductType rideApiDetails.fareProductType
    rideStatus = fromMaybe "" (ride.rideList !! 0 <#> \(RideAPIEntity ride) -> ride.status)
    autoWaitingCharges = if rideType == FPT.RENTAL then cityConfig.rentalWaitingChargeConfig.auto else cityConfig.waitingChargeConfig.auto 
    cabsWaitingCharges = if rideType == FPT.RENTAL then cityConfig.rentalWaitingChargeConfig.cabs else cityConfig.waitingChargeConfig.cabs
    bikeWaitingCharges = if rideType == FPT.RENTAL then cityConfig.rentalWaitingChargeConfig.bike else cityConfig.waitingChargeConfig.bike
    ambulanceWaitingCharges = if rideType == FPT.RENTAL then cityConfig.rentalWaitingChargeConfig.ambulance else cityConfig.waitingChargeConfig.ambulance
    waitingCharges = 
      if any (_ == rideDetails.vehicleVariant) ["AUTO_RICKSHAW", "EV_AUTO_RICKSHAW"] then
          autoWaitingCharges
      else if any (_ == rideDetails.vehicleVariant) ["BIKE", "DELVIERY_BIKE"] then 
          bikeWaitingCharges
      else if isAmbulance rideDetails.vehicleVariant  then
          ambulanceWaitingCharges
      else 
         cabsWaitingCharges
     in {
    date : (( (fromMaybe "" ((split (Pattern ",") (convertUTCtoISC rideStartTime "llll")) !!0 )) <> ", " <>  (convertUTCtoISC rideStartTime "Do MMM") )),
    time :  (convertUTCtoISC rideStartTime "h:mm A"),
    source :  decodeAddress (Booking ride.fromLocation),
    destination : decodeAddress $ Booking destination,
    totalAmount :  ((getCurrency appConfig) <> " " <> show (fromMaybe (0) rideDetails.computedPrice)),
    cardVisibility :  "visible",
    shimmerVisibility :  "gone",
    driverImage : fetchImage FF_ASSET  "ny_ic_user",
    isCancelled :  (if ride.status == "CANCELLED" || ride.status == "REALLOCATED" then "visible" else "gone"),
    isSuccessfull :  (if ride.status == "COMPLETED" then "visible" else "gone"),
    isScheduled :  (if isScheduled then "visible" else "gone"),
    rating : (fromMaybe 0 rideDetails.rideRating),
    driverName : (rideDetails.driverName),
    driverPhoneNumber : rideDetails.driverNumber,
    rideStartTime : (convertUTCtoISC rideStartTime "h:mm A"),
    rideEndTime : (convertUTCtoISC endTime "h:mm A"),
    vehicleNumber : (rideDetails.vehicleNumber),
    rideId : (rideDetails.id),
    status : if ride.status == "REALLOCATED" then "CANCELLED" else ride.status,
    shortRideId : (rideDetails.shortRideId),
    bookingId : ride.id,
    rideEndTimeUTC : fromMaybe "" (ride.rideEndTime),
    sourceLocation : ride.fromLocation,
    destinationLocation : destination,
    alpha : if isLocalStageOn HomeScreen then "1.0" else "0.5"
  , fareBreakUpList : fares
  , faresList : updatedFareList
  , baseFare : fares.baseFare
  , pickupCharges : fares.pickupCharges
  , extraFare : (getCurrency appConfig) <> " " <> show (getFareFromArray ride.fareBreakup "EXTRA_DISTANCE_FARE")
  , waitingCharges : fares.waitingCharges
  , baseDistance : baseDistanceVal
  , extraDistance : getKmMeter $  (\a -> if a < 0 then - a else a) ((fromMaybe 0 (rideDetails.chargeableRideDistance)) - (fromMaybe 0 (((ride.bookingDetails)^._contents)^._estimatedDistance)))
  , referenceString : (if (nightChargesVal && (getMerchant CTP.FunctionCall) /= YATRI) then "1.5" <> (getEN $ DAYTIME_CHARGES_APPLICABLE_AT_NIGHT nightChargeFrom nightChargeTill) else "")
                        <> (if (isHaveFare "DRIVER_SELECTED_FARE" (updatedFareList)) then "\n\n" <> (getEN DRIVERS_CAN_CHARGE_AN_ADDITIONAL_FARE_UPTO) else "")
                        <> (if (isHaveFare "WAITING_OR_PICKUP_CHARGES" updatedFareList && (rideApiDetails.fareProductType == "OneWaySpecialZoneAPIDetails")) then "\n\n" <> if cityConfig.enableWaitingConfig  then   ( (getVarString WAITING_CHARGE_DESCRIPTION [show waitingCharges.freeMinutes, show waitingCharges.perMinCharges] ) ) else (getString ADDITIONAL_CHARGES_WILL_BE_APPLICABLE) else "")
                        <> (if (isHaveFare "EARLY_END_RIDE_PENALTY" (updatedFareList)) then "\n\n" <> (getEN EARLY_END_RIDE_CHARGES_DESCRIPTION) else "")
                        <> (if (isHaveFare "CUSTOMER_SELECTED_FARE" ((updatedFareList))) then "\n\n" <> (getEN CUSTOMER_TIP_DESCRIPTION) else "")
                        <> (if (isHaveFare "TOLL_CHARGES" updatedFareList) then "\n\n" <> "⁺" <>  (getString TOLL_CHARGES_DESC) else "")
                        <> (if (isHaveFare "PARKING_CHARGE" updatedFareList) then  "\n\n" <> "⁺" <> (getString PARKING_CHARGES_DESC)  else "")
  , nightCharges : nightChargesVal
  , isSpecialZone : (null ride.rideList || isJust (ride.bookingDetails ^._contents^._otpCode))
  , zoneType : specialTags.priorityTag
  , vehicleVariant : fetchVehicleVariant rideDetails.vehicleVariant
  , isSrcServiceable: state.data.isSrcServiceable
  , optionsVisibility : false
  , merchantExoPhone : ride.merchantExoPhone
  , serviceTierName : ride.serviceTierName
  , showRepeatRide : if rideType == FPT.RENTAL || isScheduled then "gone" else "visible"
  , rideType :rideType
  , estimatedDistance : fromMaybe 0 ride.estimatedDistance
  , estimatedDuration : fromMaybe 0 ride.estimatedDuration
  , estimatedFare : ride.estimatedFare
  , showDestination : if (decodeAddress $ Booking destination) == "" then "gone" else "visible" 
  , rideScheduledTime : fromMaybe "" ride.rideScheduledTime
  , totalTime : show (runFn2 differenceBetweenTwoUTCInMinutes endTime startTime) <> " min"
  , vehicleModel : if (rideDetails.vehicleModel `DA.elem` ["", "Unkown"]) then fromMaybe (HU.getVariantRideType rideDetails.vehicleVariant) ride.serviceTierName else rideDetails.vehicleModel
  , rideStartTimeUTC : fromMaybe "" ride.rideStartTime
  , isAirConditioned : ride.isAirConditioned
  , providerName : ride.agencyName
  , providerType : maybe CTP.ONUS (\valueAdd -> if valueAdd then CTP.ONUS else CTP.OFFUS) ride.isValueAddNP
  , rideCreatedAt : ride.createdAt
  , rideStatus : rideStatus
}) ( reverse $ sortWith (\(RideBookingRes ride) -> fromMaybe ride.createdAt $ if  any(_ == ride.status ) ["CONFIRMED","TRIP_ASSIGNED"] then ride.rideScheduledTime else ride.rideStartTime) listRes ))

matchRidebyId :: IndividualRideCardState -> IndividualRideCardState -> Boolean
matchRidebyId rideOne rideTwo = rideOne.bookingId == rideTwo.bookingId

getFares ∷ Array FareBreakupAPIEntity → Fares
getFares fares = {
  baseFare :(getCurrency appConfig) <>  " " <> getFixedTwoDecimals (((getFareFromArray fares "BASE_FARE") + (getFareFromArray fares "EXTRA_DISTANCE_FARE")) - 10.0)
, pickupCharges : (getCurrency appConfig) <> " 10.0"
, waitingCharges : (getCurrency appConfig) <> " " <> getFixedTwoDecimals (getFareFromArray fares "WAITING_CHARGES")
, nominalFare : (getCurrency appConfig) <> " " <> getFixedTwoDecimals (getFareFromArray fares "DRIVER_SELECTED_FARE")
}

