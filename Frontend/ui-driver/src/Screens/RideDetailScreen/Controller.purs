{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.RideDetailScreen.Controller where

import Prelude(Unit, class Show, pure, unit, ($), (&&), bind, discard)
import PrestoDOM (Eval, continue, continueWithCmd, exit)
import Screens.Types (RideDetailScreenState, Location)
import PrestoDOM.Types.Core (class Loggable)
import Data.Maybe (fromMaybe)
import Data.Number as Number
import Effect (Effect)
import JBridge (animateCamera, getCurrentPosition, isLocationEnabled, isLocationPermissionEnabled, showMarker, requestLocation, launchInAppRatingPopup,showDialer)
import Effect.Timer (IntervalId, setInterval) as Timer
import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, printLog, trackAppScreenEvent,trackAppTextInput)
import Screens (ScreenName(..), getScreen)
import Components.RatingCard.Controller as RatingCard
import Components.PrimaryButton.Controller as PrimaryButtonController
import Components.PopUpModal.Controller as PopUpModal
import JBridge (firebaseLogEvent)
import Services.Config (getSupportNumber)
import Debug (spy)

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    AfterRender -> trackAppScreenRender appId "screen" (getScreen RIDE_DETAILS_SCREEN)
    BackPressed -> do
      trackAppBackPress appId (getScreen RIDE_DETAILS_SCREEN)
      trackAppEndScreen appId (getScreen RIDE_DETAILS_SCREEN)
    GoToHome -> do
      trackAppActionClick appId (getScreen RIDE_DETAILS_SCREEN) "in_screen" "go_to_home"
      trackAppEndScreen appId (getScreen RIDE_DETAILS_SCREEN)
    GoToRateCardView -> trackAppScreenEvent appId (getScreen RIDE_DETAILS_SCREEN) "in_screen" "ratecardview"
    RateCardAction act -> case act of
      RatingCard.Rating index -> trackAppActionClick appId (getScreen RIDE_DETAILS_SCREEN) "rating_card" "star"
      RatingCard.PrimaryButtonAC act -> case act of
        PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen RIDE_DETAILS_SCREEN) "rating_card" "primary_button"
        PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen RIDE_DETAILS_SCREEN) "rating_card" "primary_button_no_action"
      RatingCard.SkipButtonAC act -> case act of
        PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen RIDE_DETAILS_SCREEN) "rating_card" "skip_primary_button"
        PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen RIDE_DETAILS_SCREEN) "rating_card" "primary_button_no_action"
      RatingCard.FeedbackChanged value -> trackAppActionClick appId (getScreen RIDE_DETAILS_SCREEN) "rating_card" "feedback_changed"
      RatingCard.BackPressed -> trackAppScreenEvent appId (getScreen RIDE_DETAILS_SCREEN) "rating_card" "background_click"
      RatingCard.SourceToDestinationAC act -> trackAppScreenEvent appId (getScreen RIDE_DETAILS_SCREEN) "rating_card" "source_to_destination"
      RatingCard.OnClose -> trackAppActionClick appId (getScreen HOME_SCREEN) "rating_card" "skip_feedback"
      RatingCard.OnBackgroundClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "rating_card" "skip_feedback"
      RatingCard.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "rating_card" "no_action" 
    PopUpModalAction act -> case act of
      PopUpModal.OnButton1Click -> trackAppActionClick appId (getScreen RIDE_DETAILS_SCREEN) "popup_modal" "on_goback"
      PopUpModal.Tipbtnclick _ _ -> trackAppActionClick appId (getScreen RIDE_DETAILS_SCREEN) "popup_modal" "tip_button_click"
      PopUpModal.DismissPopup -> trackAppActionClick appId (getScreen RIDE_DETAILS_SCREEN) "popup_modal" "dismiss_popup"
      PopUpModal.OnButton2Click -> trackAppActionClick appId (getScreen RIDE_DETAILS_SCREEN) "popup_modal" "call_support"
      PopUpModal.NoAction -> trackAppActionClick appId (getScreen RIDE_DETAILS_SCREEN) "popup_modal_action" "no_action"
      PopUpModal.OnImageClick -> trackAppActionClick appId (getScreen RIDE_DETAILS_SCREEN) "popup_modal_action" "image"
      PopUpModal.ETextController act -> trackAppTextInput appId (getScreen RIDE_DETAILS_SCREEN) "popup_modal_action" "primary_edit_text"
      PopUpModal.CountDown arg1 arg2 arg3 arg4 -> trackAppScreenEvent appId (getScreen RIDE_DETAILS_SCREEN) "popup_modal_action" "countdown_updated"
    OnCallSupport -> trackAppScreenEvent appId (getScreen RIDE_DETAILS_SCREEN) "in_screen" "popup_modal"
    CountDown -> trackAppScreenEvent appId (getScreen RIDE_DETAILS_SCREEN) "in_screen" "lottie_view"
    OnRideDetailsAC -> trackAppScreenEvent appId (getScreen RIDE_DETAILS_SCREEN) "in_screen" "ride_details"
    OnBackgroundClick -> trackAppScreenEvent appId (getScreen RIDE_DETAILS_SCREEN) "in_screen" "on_background_click"
    NoAction -> trackAppScreenEvent appId (getScreen RIDE_DETAILS_SCREEN) "in_screen" "no_action"


data ScreenOutput = GoBack | GoToHomeScreen | GoToRideDetails RideDetailScreenState | SubmitRating RideDetailScreenState
data Action = BackPressed
            | AfterRender
            | NoAction
            | GoToHome
            | GoToRateCardView
            | RateCardAction RatingCard.Action
            | OnCallSupport
            | PopUpModalAction PopUpModal.Action
            | OnRideDetailsAC
            | CountDown
            | OnBackgroundClick

eval :: Action -> RideDetailScreenState -> Eval Action ScreenOutput RideDetailScreenState
eval AfterRender state = continue state
eval BackPressed state = do 
  _ <- pure $ spy "onBackPressed" state
  if state.props.rateCardView then continue state{props{rateCardView = false}} else continue state
eval GoToHome state = do
  _ <- pure $ launchInAppRatingPopup unit
  exit $ GoToHomeScreen
  
eval (RateCardAction (RatingCard.Rating index)) state = continue state { props { rating = index } }

eval (RateCardAction (RatingCard.PrimaryButtonAC PrimaryButtonController.OnClick)) state = exit $ SubmitRating state

eval (RateCardAction (RatingCard.FeedbackChanged value)) state = continue state { props { feedback = value } } 
eval GoToRateCardView state = continue state {props{rateCardView = true}}
eval OnCallSupport state = continue state {props{supportPopUpView = true}}
eval (PopUpModalAction (PopUpModal.OnButton1Click)) state = continue state{props{supportPopUpView=false}}
eval (PopUpModalAction (PopUpModal.OnButton2Click)) state = do
  _ <- pure $ showDialer (getSupportNumber "")
  continue state
eval NoAction state = continue state
eval OnRideDetailsAC state = exit $ GoToRideDetails state
eval (RateCardAction (RatingCard.OnBackgroundClick)) state = continue state{props{rateCardView = false}}
eval (RateCardAction (RatingCard.OnClose)) state = exit $ GoToHomeScreen 
eval CountDown state = continue state
eval _ state = continue state

checkPermissionAndUpdateDriverMarker :: Effect Unit 
checkPermissionAndUpdateDriverMarker = do 
  conditionA <- isLocationPermissionEnabled unit 
  conditionB <- isLocationEnabled unit 
  if conditionA && conditionB then do 
    _ <- getCurrentPosition (showDriverMarker "ic_vehicle_side") constructLatLong
    pure unit
    else do
      _ <- requestLocation unit
      pure unit

showDriverMarker :: String -> Location -> Effect Unit
showDriverMarker marker location = do
  _ <- showMarker marker location.lat location.lon 100 0.5 0.5
  --_ <- showMarker "ic_active_marker" location.lat location.lng 350 0.5 0.5
  animateCamera location.lat location.lon 15

constructLatLong :: String -> String -> Location
constructLatLong lat lng =
  { lat: fromMaybe 0.0 (Number.fromString lat)
  , lon : fromMaybe 0.0 (Number.fromString lng)
  , place : ""
  }

