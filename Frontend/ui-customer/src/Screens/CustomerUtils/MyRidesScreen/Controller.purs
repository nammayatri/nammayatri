{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.MyRidesScreen.Controller where

import Accessor (_amount, _computedPrice, _contents, _description, _driverName, _estimatedDistance, _id, _list, _rideRating, _toLocation, _vehicleNumber, _otpCode)
import Components.ErrorModal as ErrorModal
import Components.GenericHeader as GenericHeader
import Components.IndividualRideCard.Controller as IndividualRideCardController
import Components.PrimaryButton as PrimaryButton
import Data.Array (union, (!!), length, filter, unionBy, head, all, null, sortWith, reverse)
import Data.Int (fromString, round, toNumber)
import Data.Lens ((^.))
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.String (Pattern(..), split)
import Engineering.Helpers.Commons (strToBool)
import Helpers.Utils (parseFloat, rotateArray, setEnabled, setRefreshing, isHaveFare, withinTimeRange, fetchImage, FetchImageFrom(..), isParentView, emitTerminateApp)
import Engineering.Helpers.Commons (convertUTCtoISC)
import JBridge (firebaseLogEvent)
import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppScreenEvent)
import Prelude (class Show, pure, unit, bind, map, discard, show, ($), (==), (&&), (+), (/=), (<>), (||), (-), (<), (/), (*), negate, (<<<), not, void)
import PrestoDOM (Eval, ScrollState(..), continue, continueWithCmd, exit, updateAndExit)
import PrestoDOM.Types.Core (class Loggable, toPropValue)
import Screens (ScreenName(..), getScreen)
import Screens.HomeScreen.Transformer (dummyRideAPIEntity, getSpecialTag)
import Screens.Types (AnimationState(..), FareComponent, Fares, IndividualRideCardState, ItemState, MyRidesScreenState, Stage(..), ZoneType(..), VehicleVariant(..))
import Services.API (FareBreakupAPIEntity(..), RideAPIEntity(..), RideBookingListRes, RideBookingRes(..))
import Storage (isLocalStageOn)
import Language.Strings (getString)
import Resources.Localizable.EN (getEN)
import Language.Types (STR(..))
import Resources.Constants (DecodeAddress(..), decodeAddress, getFaresList, getFareFromArray, getFilteredFares, getKmMeter, fetchVehicleVariant)
import Common.Types.App (LazyCheck(..))
import MerchantConfig.Utils (getMerchant, Merchant(..))
import Effect.Unsafe (unsafePerformEffect)
import Engineering.Helpers.LogEvent (logEvent)
import ConfigProvider
import JBridge (toast)

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

data ScreenOutput = GoBack MyRidesScreenState
  | MyRidesScreen MyRidesScreenState
  | GoToTripDetails MyRidesScreenState
  | LoaderOutput MyRidesScreenState
  | BookRide
  | RepeatRide MyRidesScreenState

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

eval :: Action -> MyRidesScreenState -> Eval Action ScreenOutput MyRidesScreenState

eval BackPressed state = 
  if isParentView FunctionCall 
    then do 
      void $ pure $ emitTerminateApp Nothing true
      continue state
    else exit $ GoBack state

eval (ScrollStateChanged scrollState) state = do
  _ <- case scrollState of
           SCROLL_STATE_FLING -> pure $ setEnabled "2000031" false
           _ -> pure unit
  continue state
eval (GenericHeaderActionController (GenericHeader.PrefixImgOnClick )) state = continueWithCmd state [do pure BackPressed]

eval (IndividualRideCardActionController (IndividualRideCardController.OnRideToastAC)) state = do 
  void $ pure $ toast $ getString ALREADY_HAVE_AN_ACTIVE_RIDE
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
  _ <- if canScrollUp then (pure $ setEnabled "2000031" false) else  (pure $ setEnabled "2000031" true)
  continue state { props{loaderButtonVisibility = loadMoreButton}}

eval (IndividualRideCardActionController (IndividualRideCardController.OnClick index)) state = do
  let selectedCard = state.itemsRides !! index
  case selectedCard of
    Just selectedRide -> do
      exit $ GoToTripDetails state { data { selectedItem = selectedRide}}
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
                  let bufferCardDataPrestoList = ((myRideListTransformerProp (rideList ^. _list)))
                  let bufferCardData = myRideListTransformer state (rideList  ^. _list)
                  _ <- pure $ setRefreshing "2000031" false
                  let loaderBtnDisabled = if(length (rideList ^. _list )== 0) then true else false
                  continue $ state {shimmerLoader = AnimatedOut ,prestoListArrayItems = union (state.prestoListArrayItems) (bufferCardDataPrestoList), itemsRides = unionBy matchRidebyId (state.itemsRides) (bufferCardData),props{loadMoreDisabled = loaderBtnDisabled, receivedResponse = true}}
    "listCompleted" -> continue state {data{loadMoreText = false}}
    _ -> continue state{props{receivedResponse = true, apiFailure = true, loadMoreDisabled = true}}

eval Refresh state = updateAndExit state{props{ receivedResponse = false, loaderButtonVisibility = false }} $  MyRidesScreen state

eval (ErrorModalActionController (ErrorModal.PrimaryButtonActionController PrimaryButton.OnClick)) state = exit $ BookRide

eval (APIFailureActionController (ErrorModal.PrimaryButtonActionController PrimaryButton.OnClick)) state = exit $ BookRide


eval _ state = continue state

myRideListTransformerProp :: Array RideBookingRes  -> Array ItemState
myRideListTransformerProp listRes =  filter (\item -> (item.status == (toPropValue "COMPLETED") || item.status == (toPropValue "CANCELLED"))) (map (\(RideBookingRes ride) -> {
    date : toPropValue (( (fromMaybe "" ((split (Pattern ",") (convertUTCtoISC (fromMaybe ride.createdAt ride.rideStartTime) "llll")) !!0 )) <> ", " <>  (convertUTCtoISC (fromMaybe ride.createdAt ride.rideStartTime) "Do MMM") )),
    time : toPropValue (convertUTCtoISC (fromMaybe ride.createdAt ride.rideStartTime) "h:mm A"),
    source : toPropValue (decodeAddress (Booking ride.fromLocation)),
    destination : toPropValue (decodeAddress (Booking (ride.bookingDetails ^._contents^._toLocation))),
    totalAmount : toPropValue ((getCurrency appConfig) <> " " <> show (fromMaybe 0 ((fromMaybe dummyRideAPIEntity (ride.rideList !!0) )^. _computedPrice))),
    cardVisibility : toPropValue "visible",
    shimmerVisibility : toPropValue "gone",
    driverImage : toPropValue $ fetchImage FF_ASSET "ny_ic_user",
    isCancelled : toPropValue (if ride.status == "CANCELLED" then "visible" else "gone"),
    isSuccessfull : toPropValue (if ride.status == "COMPLETED" then "visible" else "gone"),
    rating : toPropValue (fromMaybe 0 ((fromMaybe dummyRideAPIEntity (ride.rideList !!0) )^. _rideRating)),
    driverName : toPropValue ((fromMaybe dummyRideAPIEntity (ride.rideList !!0) )^. _driverName),
    rideStartTime : toPropValue (convertUTCtoISC (fromMaybe "" ride.rideStartTime) "h:mm A"),
    rideEndTime : toPropValue (convertUTCtoISC (fromMaybe "" ride.rideEndTime) "h:mm A"),
    vehicleNumber : toPropValue ((fromMaybe dummyRideAPIEntity (ride.rideList !!0) )^._vehicleNumber),
    rideId : toPropValue ((fromMaybe dummyRideAPIEntity (ride.rideList !!0) )^._id),
    status : toPropValue (ride.status),
    rideEndTimeUTC : toPropValue (fromMaybe ride.createdAt ride.rideEndTime),
    alpha : toPropValue if isLocalStageOn HomeScreen then "1.0" else "0.5",
    zoneVisibility : toPropValue if (getSpecialTag ride.specialLocationTag).priorityTag == METRO then "visible" else "gone"
}) ( reverse $ sortWith (\(RideBookingRes ride) -> ride.createdAt ) listRes ))


myRideListTransformer :: MyRidesScreenState -> Array RideBookingRes -> Array IndividualRideCardState
myRideListTransformer state listRes = filter (\item -> (item.status == "COMPLETED" || item.status == "CANCELLED")) (map (\(RideBookingRes ride) ->
  let
    fares = getFares ride.fareBreakup
    (RideAPIEntity rideDetails) = (fromMaybe dummyRideAPIEntity (ride.rideList !!0))
    baseDistanceVal = (getKmMeter (fromMaybe 0 (rideDetails.chargeableRideDistance)))
    timeVal = (convertUTCtoISC (fromMaybe ride.createdAt ride.rideStartTime) "HH:mm:ss")
    nightChargesVal = (withinTimeRange "22:00:00" "5:00:00" timeVal)
    updatedFareList = getFaresList ride.fareBreakup baseDistanceVal
    specialTags = getSpecialTag ride.specialLocationTag
     in {
    date : (( (fromMaybe "" ((split (Pattern ",") (convertUTCtoISC (fromMaybe ride.createdAt ride.rideStartTime) "llll")) !!0 )) <> ", " <>  (convertUTCtoISC (fromMaybe ride.createdAt ride.rideStartTime) "Do MMM") )),
    time :  (convertUTCtoISC (fromMaybe ride.createdAt ride.rideStartTime) "h:mm A"),
    source :  decodeAddress (Booking ride.fromLocation),
    destination : decodeAddress (Booking (ride.bookingDetails ^._contents^._toLocation)),
    totalAmount :  ((getCurrency appConfig) <> " " <> show (fromMaybe (0) rideDetails.computedPrice)),
    cardVisibility :  "visible",
    shimmerVisibility :  "gone",
    driverImage : fetchImage FF_ASSET  "ny_ic_user",
    isCancelled :  (if ride.status == "CANCELLED" then "visible" else "gone"),
    isSuccessfull :  (if ride.status == "COMPLETED" then "visible" else "gone"),
    rating : (fromMaybe 0 rideDetails.rideRating),
    driverName : (rideDetails.driverName),
    rideStartTime : (convertUTCtoISC (fromMaybe "" ride.rideStartTime) "h:mm A"),
    rideEndTime : (convertUTCtoISC (fromMaybe "" ride.rideEndTime) "h:mm A"),
    vehicleNumber : (rideDetails.vehicleNumber),
    rideId : (rideDetails.id),
    status : ride.status,
    shortRideId : (rideDetails.shortRideId),
    bookingId : ride.id,
    rideEndTimeUTC : fromMaybe "" (ride.rideEndTime),
    sourceLocation : ride.fromLocation,
    destinationLocation : ((ride.bookingDetails)^._contents)^._toLocation,
    alpha : if isLocalStageOn HomeScreen then "1.0" else "0.5"
  , fareBreakUpList : fares
  , faresList : updatedFareList
  , baseFare : fares.baseFare
  , pickupCharges : fares.pickupCharges
  , extraFare : (getCurrency appConfig) <> " " <> show (getFareFromArray ride.fareBreakup "EXTRA_DISTANCE_FARE")
  , waitingCharges : fares.waitingCharges
  , baseDistance : baseDistanceVal
  , extraDistance : getKmMeter $  (\a -> if a < 0 then - a else a) ((fromMaybe 0 (rideDetails.chargeableRideDistance)) - (fromMaybe 0 (((ride.bookingDetails)^._contents)^._estimatedDistance)))
  , referenceString : (if (nightChargesVal && (getMerchant FunctionCall) /= YATRI) then "1.5" <> (getEN DAYTIME_CHARGES_APPLICABLE_AT_NIGHT) else "")
                        <> (if (isHaveFare "DRIVER_SELECTED_FARE" (updatedFareList)) then "\n\n" <> (getEN DRIVERS_CAN_CHARGE_AN_ADDITIONAL_FARE_UPTO) else "")
                        <> (if (isHaveFare "WAITING_OR_PICKUP_CHARGES" updatedFareList) then "\n\n" <> (getEN WAITING_CHARGE_DESCRIPTION) else "")
                        <> (if (isHaveFare "EARLY_END_RIDE_PENALTY" (updatedFareList)) then "\n\n" <> (getEN EARLY_END_RIDE_CHARGES_DESCRIPTION) else "")
                        <> (if (isHaveFare "CUSTOMER_SELECTED_FARE" ((updatedFareList))) then "\n\n" <> (getEN CUSTOMER_TIP_DESCRIPTION) else "")
  , nightCharges : nightChargesVal
  , isSpecialZone : (null ride.rideList || isJust (ride.bookingDetails ^._contents^._otpCode))
  , zoneType : specialTags.priorityTag
  , vehicleVariant : fetchVehicleVariant rideDetails.vehicleVariant
  , isSrcServiceable: state.data.isSrcServiceable
  , optionsVisibility : true
  , merchantExoPhone : ride.merchantExoPhone
}) ( reverse $ sortWith (\(RideBookingRes ride) -> ride.createdAt ) listRes ))

dummyFareBreakUp :: FareBreakupAPIEntity
dummyFareBreakUp = FareBreakupAPIEntity{amount: 0,description: ""}

matchRidebyId :: IndividualRideCardState -> IndividualRideCardState -> Boolean
matchRidebyId rideOne rideTwo = rideOne.bookingId == rideTwo.bookingId

getFares ∷ Array FareBreakupAPIEntity → Fares
getFares fares = {
  baseFare :(getCurrency appConfig) <>  " " <> show (((getFareFromArray fares "BASE_FARE") + (getFareFromArray fares "EXTRA_DISTANCE_FARE")) - 10)
, pickupCharges : (getCurrency appConfig) <> " 10.0"
, waitingCharges : (getCurrency appConfig) <> " " <> show (getFareFromArray fares "WAITING_CHARGES")
, nominalFare : (getCurrency appConfig) <> " " <> show (getFareFromArray fares "DRIVER_SELECTED_FARE")
}

