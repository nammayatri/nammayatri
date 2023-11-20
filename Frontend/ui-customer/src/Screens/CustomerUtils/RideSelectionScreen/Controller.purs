{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.RideSelectionScreen.Controller where

import Accessor (_computedPrice, _contents, _driverName, _estimatedDistance, _id, _list, _rideRating, _toLocation, _vehicleNumber, _otpCode)
import Common.Types.App (LazyCheck(..))
import Components.ErrorModal as ErrorModal
import Components.GenericHeader as GenericHeader
import Components.IndividualRideCard.Controller as IndividualRideCardController
import Components.PrimaryButton as PrimaryButton
import Data.Array (union, (!!), length, filter, unionBy, null)
import Data.Int (fromString)
import Data.Lens ((^.))
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.String (Pattern(..), split)
import Engineering.Helpers.Commons (convertUTCtoISC)
import Engineering.Helpers.Commons (strToBool)
import Helpers.Utils (FetchImageFrom(..), fetchImage, isHaveFare, setEnabled, setRefreshing, withinTimeRange)
import Language.Strings (getString)
import Language.Types (STR(..))
import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppScreenEvent)
import MerchantConfig.Utils (getMerchant, Merchant(..))
import Prelude (class Show, pure, unit, bind, map, discard, show, ($), (==), (&&), (+), (/=), (<>), (||), (-), (<), (/), negate, void)
import PrestoDOM (Eval, ScrollState(..), continue, continueWithCmd, exit, updateAndExit)
import PrestoDOM.Types.Core (class Loggable, toPropValue)
import Resources.Constants (DecodeAddress(..), decodeAddress, getFaresList, getFareFromArray, getKmMeter, fetchVehicleVariant)
import Resources.Localizable.EN (getEN)
import Screens (ScreenName(..), getScreen)
import Screens.HomeScreen.Transformer (dummyRideAPIEntity, getSpecialTag)
import Screens.Types (AnimationState(..), Fares, IndividualRideCardState, ItemState, RideSelectionScreenState, Stage(..), ZoneType(..))
import Services.API (FareBreakupAPIEntity(..), RideAPIEntity(..), RideBookingListRes, RideBookingRes(..))
import Storage (isLocalStageOn)
import Data.Ord (abs)
import ConfigProvider
import Screens.RideSelectionScreen.Transformer (myRideListTransformer, myRideListTransformerProp, matchRidebyId)

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    DontKnowRide (PrimaryButton.OnClick) -> trackAppActionClick appId (getScreen RIDE_SELECTION_SCREEN) "in_screen" "dont_know_ride"
    AfterRender -> trackAppScreenRender appId "screen" (getScreen RIDE_SELECTION_SCREEN)
    BackPressed -> do
      trackAppBackPress appId (getScreen RIDE_SELECTION_SCREEN)
      trackAppEndScreen appId (getScreen RIDE_SELECTION_SCREEN)
    OnFadeComplete str -> trackAppActionClick appId (getScreen RIDE_SELECTION_SCREEN) "in_screen" "on_fade"
    Refresh -> trackAppActionClick appId (getScreen RIDE_SELECTION_SCREEN) "in_screen" "refresh"
    Loader -> trackAppActionClick appId (getScreen RIDE_SELECTION_SCREEN) "in_screen" "loader"
    GenericHeaderActionController act -> case act of
      GenericHeader.PrefixImgOnClick -> do
        trackAppActionClick appId (getScreen RIDE_SELECTION_SCREEN) "generic_header_action" "back_icon"
        trackAppEndScreen appId (getScreen RIDE_SELECTION_SCREEN)
      GenericHeader.SuffixImgOnClick -> trackAppActionClick appId (getScreen RIDE_SELECTION_SCREEN) "generic_header_action" "forward_icon"
    IndividualRideCardActionController act -> case act of
      IndividualRideCardController.OnClick index -> trackAppActionClick appId (getScreen RIDE_SELECTION_SCREEN) "individual_ride_card" "individual_ride"
      IndividualRideCardController.RepeatRide index -> trackAppActionClick appId (getScreen RIDE_SELECTION_SCREEN) "individual_ride_card" "repeat_ride"
      IndividualRideCardController.NoAction int -> trackAppActionClick appId (getScreen RIDE_SELECTION_SCREEN) "individual_ride_card" "no_action"
      IndividualRideCardController.OnRideToastAC -> trackAppActionClick appId (getScreen RIDE_SELECTION_SCREEN) "individual_ride_card" "on_ride_toast"
    ErrorModalActionController act -> case act of
      ErrorModal.PrimaryButtonActionController act -> case act of
        PrimaryButton.OnClick -> do
          trackAppActionClick appId (getScreen RIDE_SELECTION_SCREEN) "error_modal_action" "book_now_primary_button"
          trackAppEndScreen appId (getScreen RIDE_SELECTION_SCREEN)
        PrimaryButton.NoAction -> trackAppActionClick appId (getScreen RIDE_SELECTION_SCREEN) "error_modal_action" "primary_button_no_action"
    APIFailureActionController act -> case act of
      ErrorModal.PrimaryButtonActionController  act -> case act of
        PrimaryButton.OnClick -> do
          trackAppActionClick appId (getScreen RIDE_SELECTION_SCREEN) "api_failure_error_modal_action" "primary_button"
          trackAppEndScreen appId (getScreen RIDE_SELECTION_SCREEN)
        PrimaryButton.NoAction -> trackAppActionClick appId (getScreen RIDE_SELECTION_SCREEN) "api_failure_error_modal_action" "primary_button_no_action"
    Scroll str -> trackAppScreenEvent appId (getScreen RIDE_SELECTION_SCREEN) "in_screen" "scroll"
    ScrollStateChanged scrollState -> trackAppScreenEvent appId (getScreen RIDE_SELECTION_SCREEN) "in_screen" "scroll_state_changed"
    RideBookingListAPIResponseAction rideList status -> trackAppScreenEvent appId (getScreen RIDE_SELECTION_SCREEN) "in_screen" "ride_booking_list"
    NoAction -> trackAppScreenEvent appId (getScreen RIDE_SELECTION_SCREEN) "in_screen" "no_action"
    _     -> pure unit

data ScreenOutput = GoBack RideSelectionScreenState
                  | SelectRide RideSelectionScreenState
                  | LoaderOutput RideSelectionScreenState
                  | RefreshScreen RideSelectionScreenState

data Action = NoAction
            | OnFadeComplete String
            | Refresh
            | Loader
            | BackPressed
            | GenericHeaderActionController GenericHeader.Action
            | RideBookingListAPIResponseAction RideBookingListRes String
            | IndividualRideCardActionController IndividualRideCardController.Action
            | APIFailureActionController ErrorModal.Action
            | ErrorModalActionController   ErrorModal.Action
            | Scroll String
            | AfterRender
            | ScrollStateChanged ScrollState
            | DontKnowRide PrimaryButton.Action


eval :: Action -> RideSelectionScreenState -> Eval Action ScreenOutput RideSelectionScreenState

eval (IndividualRideCardActionController (IndividualRideCardController.OnClick index)) state = do
  let selectedCard = state.itemsRides !! index
  case selectedCard of
    Just selectedRide -> do
      exit $ SelectRide state  { selectedItem = Just selectedRide}
    Nothing -> continue state

eval (DontKnowRide (PrimaryButton.OnClick)) state = do
  exit $ SelectRide state {
      selectedItem = Nothing
  }

eval BackPressed state = exit $ GoBack state

eval (ScrollStateChanged scrollState) state = do
  case scrollState of
      SCROLL_STATE_FLING -> pure $ setEnabled "2000031" false
      _ -> pure unit
  continue state

eval (GenericHeaderActionController (GenericHeader.PrefixImgOnClick )) state = continueWithCmd state [do pure BackPressed]

eval (OnFadeComplete _ ) state = do
  if (state.props.receivedResponse == false) then continue state
  else  continue state {
          shimmerLoader = case state.shimmerLoader of
            AnimatedIn ->AnimatedOut
            AnimatingOut -> AnimatedOut
            a -> a
        }

eval (Loader) state = updateAndExit state{shimmerLoader = AnimatedIn, props{loaderButtonVisibility = false}} $ LoaderOutput state

eval (Scroll value) state = do
  let firstIndex = fromMaybe 0 (fromString (fromMaybe "0"((split (Pattern ",")(value))!!0)))
      visibleItems = fromMaybe 0 (fromString (fromMaybe "0"((split (Pattern ",")(value))!!1)))
      totalItems = fromMaybe 0 (fromString (fromMaybe "0"((split (Pattern ",")(value))!!2)))
      canScrollUp = fromMaybe true (strToBool (fromMaybe "true" ((split (Pattern ",")(value))!!3)))
      loadMoreButton = totalItems == (firstIndex + visibleItems) && totalItems /= 0 && totalItems /= visibleItems
  void $ if canScrollUp then (pure $ setEnabled "2000031" false) else  (pure $ setEnabled "2000031" true)
  continue state { props{loaderButtonVisibility = loadMoreButton}}

eval (RideBookingListAPIResponseAction rideList status) state = do
  void $ pure $ setRefreshing "2000031" false
  case status of
    "success" -> do
                  let bufferCardDataPrestoList = ((myRideListTransformerProp (rideList ^. _list)))
                  let bufferCardData = myRideListTransformer state (rideList  ^. _list)
                  void $ pure $ setRefreshing "2000031" false
                  let loaderBtnDisabled = length (rideList ^. _list )== 0
                  continue $ state {shimmerLoader = AnimatedOut ,prestoListArrayItems = union (state.prestoListArrayItems) (bufferCardDataPrestoList), itemsRides = unionBy matchRidebyId (state.itemsRides) (bufferCardData),props{loadMoreDisabled = loaderBtnDisabled, receivedResponse = true}}
    "listCompleted" -> continue state {data{loadMoreText = false}}
    _ -> continue state{props{receivedResponse = true, apiFailure = true, loadMoreDisabled = true}}

eval Refresh state = updateAndExit state{props{ receivedResponse = false, loaderButtonVisibility = false }} $  RefreshScreen state

eval _ state = continue state

getTitle :: String -> String
getTitle category  = 
  case category of
    "APP_RELATED"        -> getString APP_RELATED_ISSUE_PAGE_NAME
    "RIDE_RELATED"       -> getString RIDE_RELATED_ISSUE_PAGE_NAME
    "DRIVER_RELATED"     -> getString DRIVER_RELATED_ISSUE_PAGE_NAME
    "LOST_AND_FOUND"     -> getString LOST_AND_FOUND_ISSUE_PAGE_NAME
    "SOS"                -> getString SOS_ISSUE_PAGE_NAME
    "FARE_DISCREPANCY"    -> getString FARE_DISCREPANCIES_ISSUE_PAGE_NAME
    "PAYMENT_RELATED"    -> getString PAYMENT_RELATED_ISSUE_PAGE_NAME
    "ACCOUNT_RELATED"    -> getString ACCOUNT_RELATED_ISSUE_PAGE_NAME
    "OTHER"              -> getString OTHER_ISSUES
    "SAFETY"             -> getString SAFETY_ISSUE_PAGE_NAME
    _                    -> "Issue"
