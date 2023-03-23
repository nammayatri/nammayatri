{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.MyRidesScreen.Controller where

import Accessor (_amount, _computedPrice, _contents, _description, _driverName, _estimatedDistance, _id, _list, _rideRating, _toLocation, _vehicleNumber)
import Components.ErrorModal as ErrorModal
import Components.GenericHeader as GenericHeader
import Components.IndividualRideCard.Controller as IndividualRideCardController
import Components.PrimaryButton as PrimaryButton
import Data.Array (union, (!!), length, filter, unionBy, head)
import Data.Int (fromString, round, toNumber)
import Data.Lens ((^.))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), split)
import Engineering.Helpers.Commons (strToBool)
import Helpers.Utils (convertUTCtoISC, parseFloat, rotateArray, setEnabled, setRefreshing, toString)
import JBridge (firebaseLogEvent)
import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppScreenEvent)
import Prelude (class Show, pure, unit, bind, map, discard, show, ($), (==), (&&), (+), (/=), (<>), (||), (-), (<), (/), negate)
import PrestoDOM (Eval, ScrollState(..), continue, continueWithCmd, exit, updateAndExit)
import PrestoDOM.Types.Core (class Loggable, toPropValue)
import Resources.Constants (DecodeAddress(..), decodeAddress)
import Screens (ScreenName(..), getScreen)
import Screens.HomeScreen.Transformer (dummyRideAPIEntity)
import Screens.Types (AnimationState(..), FareComponent, FareTypes(..), Fares, IndividualRideCardState, ItemState, MyRidesScreenState, Stage(..))
import Services.API (FareBreakupAPIEntity(..), RideAPIEntity(..), RideBookingListRes, RideBookingRes(..))
import Storage (isLocalStageOn)

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

eval BackPressed state = exit $ GoBack state

eval (ScrollStateChanged scrollState) state = do
  _ <- if scrollState == (SCROLL_STATE_FLING ) then (pure $ setEnabled "2000031" false)
          else pure unit
  continue state
eval (GenericHeaderActionController (GenericHeader.PrefixImgOnClick )) state = continueWithCmd state [do pure BackPressed]

eval (OnFadeComplete _ ) state = do
                      if (state.props.receivedResponse == false) then continue state
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
  _ <- pure $ firebaseLogEvent "ny_user_repeat_ride_btn_click"
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
                  let bufferCardData = myRideListTransformer (rideList  ^. _list)
                  _ <- pure $ setRefreshing "2000031" false
                  let loaderBtnDisabled = if(length (rideList ^. _list )== 0) then true else false
                  continue $ state {shimmerLoader = AnimatedOut ,prestoListArrayItems = union (state.prestoListArrayItems) (bufferCardDataPrestoList), itemsRides = unionBy matchRidebyId (state.itemsRides) (bufferCardData),props{loadMoreDisabled = loaderBtnDisabled, receivedResponse = true}}
    "listCompleted" -> continue state {data{loadMoreText = "Completed"}}
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
    totalAmount : toPropValue ("₹ " <> show (fromMaybe 0 ((fromMaybe dummyRideAPIEntity (ride.rideList !!0) )^. _computedPrice))),
    cardVisibility : toPropValue "visible",
    shimmerVisibility : toPropValue "gone",
    driverImage : toPropValue "ny_ic_user,https://assets.juspay.in/nammayatri/images/user/ny_ic_user.png",
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
    alpha : toPropValue if isLocalStageOn HomeScreen then "1.0" else "0.5"
}) (listRes ))


myRideListTransformer :: Array RideBookingRes -> Array IndividualRideCardState
myRideListTransformer listRes = filter (\item -> (item.status == "COMPLETED" || item.status == "CANCELLED")) (map (\(RideBookingRes ride) -> 
  let 
    fares = getFares ride.fareBreakup
    (RideAPIEntity rideDetails) = (fromMaybe dummyRideAPIEntity (ride.rideList !!0))
     in {
    date : (( (fromMaybe "" ((split (Pattern ",") (convertUTCtoISC (fromMaybe ride.createdAt ride.rideStartTime) "llll")) !!0 )) <> ", " <>  (convertUTCtoISC (fromMaybe ride.createdAt ride.rideStartTime) "Do MMM") )),
    time :  (convertUTCtoISC (fromMaybe ride.createdAt ride.rideStartTime) "h:mm A"),
    source :  decodeAddress (Booking ride.fromLocation),
    destination : decodeAddress (Booking (ride.bookingDetails ^._contents^._toLocation)),
    totalAmount :  ("₹ " <> show (fromMaybe (0) rideDetails.computedPrice)),
    cardVisibility :  "visible",
    shimmerVisibility :  "gone",
    driverImage :  "ny_ic_user,https://assets.juspay.in/nammayatri/images/user/ny_ic_user.png",
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
  , faresList : getFaresList ride.fareBreakup
  , baseFare : fares.baseFare
  , pickupCharges : fares.pickupCharges
  , extraFare : "₹ " <> show (getFareFromArray ride.fareBreakup EXTRA_DISTANCE_FARE)
  , waitingCharges : fares.waitingCharges
  , baseDistance : getKmMeter (fromMaybe 0.0 (rideDetails.chargeableRideDistance))
  , extraDistance : getKmMeter $  (\a -> if a < 0.0 then - a else a) ((fromMaybe 0.0 (rideDetails.chargeableRideDistance)) - toNumber (fromMaybe 0 (((ride.bookingDetails)^._contents)^._estimatedDistance)))
}) (listRes))


dummyFareBreakUp :: FareBreakupAPIEntity
dummyFareBreakUp = FareBreakupAPIEntity{amount: 0.0,description: ""}

matchRidebyId :: IndividualRideCardState -> IndividualRideCardState -> Boolean
matchRidebyId rideOne rideTwo = rideOne.bookingId == rideTwo.bookingId

getFares ∷ Array FareBreakupAPIEntity → Fares
getFares fares = {
  baseFare : "₹ " <> show (((getFareFromArray fares BASE_FARE) + (getFareFromArray fares EXTRA_DISTANCE_FARE)) - (getFareFromArray fares DEAD_KILOMETER_FARE))
, pickupCharges : "₹ " <> show (getFareFromArray fares DEAD_KILOMETER_FARE)
, waitingCharges : "₹ " <> show (getFareFromArray fares WAITING_CHARGES)
, nominalFare : "₹ " <> show (getFareFromArray fares DRIVER_SELECTED_FARE)
}
getFareFromArray :: Array FareBreakupAPIEntity -> FareTypes -> Number
getFareFromArray fareBreakUp fareType = (fromMaybe dummyFareBreakUp (head (filter (\fare -> fare^._description == (show fareType)) fareBreakUp)))^._amount

getFareFromFareEntity :: String -> FareTypes
getFareFromFareEntity fareType = case fareType of 
  "BASE_FARE" -> BASE_FARE 
  "EXTRA_DISTANCE_FARE" -> EXTRA_DISTANCE_FARE 
  "DRIVER_SELECTED_FARE" -> DRIVER_SELECTED_FARE 
  "TOTAL_FARE" -> TOTAL_FARE 
  "PICKUP_CHARGES" -> PICKUP_CHARGES 
  "WAITING_CHARGES" -> WAITING_CHARGES
  "DEAD_KILOMETER_FARE" -> DEAD_KILOMETER_FARE
  _ -> BASE_FARE

getKmMeter :: Number -> String
getKmMeter distance = if (distance < 1000.0) then toString distance <> " m" else (parseFloat (distance / 1000.0)) 2 <> " km"

getFaresList :: Array FareBreakupAPIEntity -> Array FareComponent
getFaresList fares =
  map
    ( \(FareBreakupAPIEntity item) ->
        let
          fareComponentType = getFareFromFareEntity item.description
        in
          { fareType: fareComponentType
          , price: if fareComponentType == BASE_FARE then (item.amount + getFareFromArray fares EXTRA_DISTANCE_FARE) - (getFareFromArray fares DEAD_KILOMETER_FARE) else item.amount
          }
    )
    (getFilteredFares fares)
getFilteredFares :: Array FareBreakupAPIEntity -> Array FareBreakupAPIEntity
getFilteredFares = filter (\(FareBreakupAPIEntity item) -> (getFareFromFareEntity item.description) /= EXTRA_DISTANCE_FARE && (getFareFromFareEntity item.description) /= TOTAL_FARE)