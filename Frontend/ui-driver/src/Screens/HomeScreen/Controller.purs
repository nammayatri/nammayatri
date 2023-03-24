{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.HomeScreen.Controller where

import Common.Types.App (CancellationReasons)
import Components.BottomNavBar as BottomNavBar
import Components.CancelRide as CancelRide
import Components.InAppOtpModal as InAppOtpModal
import Components.PopUpModal as PopUpModal
import Components.PrimaryButton as PrimaryButtonController
import Components.RideActionModal as RideActionModal
import Components.ChatView as ChatView
import Components.StatsModel.Controller (Action) as StatsModelController
import Data.Array as Array
import Data.Int (round, toNumber)
import Data.Lens ((^.))
import EN (getEN)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (fromString)
import Data.String (drop, length, take, trim)
import Effect (Effect)
import Effect.Class (liftEffect)
import Engineering.Helpers.Commons (clearTimer)
import Global (readFloat)
import Helpers.Utils (convertUTCtoISC, currentPosition, differenceBetweenTwoUTC, getDistanceBwCordinates, parseFloat,  setText')
import JBridge (animateCamera, enableMyLocation, firebaseLogEvent, getCurrentPosition, getHeightFromPercent, hideKeyboardOnNavigation, isLocationEnabled, isLocationPermissionEnabled, minimizeApp, openNavigation, removeAllPolylines, requestLocation, showDialer, showMarker, toast, firebaseLogEventWithTwoParams, sendMessage, scrollToBottom, stopChatListenerService)
import Language.Strings (getString)
import Language.Types (STR(..))
import Log (printLog, trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppTextInput, trackAppScreenEvent)
import Prelude (class Show, Unit, bind, discard, map, not, pure, show, unit, void, ($), (&&), (*), (+), (-), (/), (/=), (<), (<>), (==), (>), (||), (>=))
import PrestoDOM (Eval, continue, continueWithCmd, exit, updateAndExit)
import PrestoDOM.Types.Core (class Loggable)
import Resource.Constants (decodeAddress)
import Screens (ScreenName(..), getScreen)
import Screens.Types as ST
import Services.APITypes (GetRidesHistoryResp, RidesInfo(..), Status(..))
import Services.Accessor (_lat, _lon)
import Services.Config (getCustomerNumber)
import Storage (KeyStore(..), deleteValueFromLocalStore, getValueToLocalNativeStore, getValueToLocalStore, setValueToLocalNativeStore, setValueToLocalStore)
import Engineering.Helpers.Commons (getNewIDWithTag)

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog action appId = case action of 
    AfterRender -> trackAppScreenRender appId "screen" (getScreen HOME_SCREEN)
    BackPressed -> do
      trackAppBackPress appId (getScreen HOME_SCREEN)
      trackAppEndScreen appId (getScreen HOME_SCREEN)
    ScreenClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "screen_click"
    Notification _ -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "notification_page"
    ChangeStatus status -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "change_status"
    GoOffline status -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "go_offline"
    CancelGoOffline -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "cancell_go_offline"
    ShowMap key lat lon -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "show_map"
    BottomNavBarAction (BottomNavBar.OnNavigate item) -> do
      trackAppActionClick appId (getScreen HOME_SCREEN) "bottom_nav_bar" "on_navigate"
      trackAppEndScreen appId (getScreen HOME_SCREEN)
    RideActionModalAction act -> case act of
      RideActionModal.StartRide -> trackAppActionClick appId (getScreen HOME_SCREEN) "ride_action_modal" "start_ride"
      RideActionModal.EndRide -> trackAppActionClick appId (getScreen HOME_SCREEN) "ride_action_modal" "end_ride"
      RideActionModal.CancelRide -> trackAppActionClick appId (getScreen HOME_SCREEN) "ride_action_modal" "cancel_ride"
      RideActionModal.OnNavigate -> trackAppActionClick appId (getScreen HOME_SCREEN) "ride_action_modal" "on_navigate"
      RideActionModal.CallCustomer -> trackAppActionClick appId (getScreen HOME_SCREEN) "ride_action_modal" "call_customer"
      RideActionModal.LocationTracking -> trackAppActionClick appId (getScreen HOME_SCREEN) "ride_action_modal" "location_tracking"
      RideActionModal.NotifyCustomer -> trackAppActionClick appId (getScreen HOME_SCREEN) "ride_action_modal" "notify_driver"
      RideActionModal.ButtonTimer seconds id status timerID -> trackAppActionClick appId (getScreen HOME_SCREEN) "ride_action_modal" "button_timer"
      RideActionModal.MessageCustomer -> trackAppActionClick appId (getScreen HOME_SCREEN) "ride_action_modal" "message_customer"
    InAppOtpModalAction act -> case act of
      InAppOtpModal.OnSelection key index -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_app_otp_modal" "on_selection"
      InAppOtpModal.OnClickBack text -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_app_otp_modal" "on_click_back"
      InAppOtpModal.OnclickTextBox index -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_app_otp_modal" "on_click_text_box"
      InAppOtpModal.BackPressed -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_app_otp_modal" "on_backpressed"
      InAppOtpModal.OnClickDone text -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_app_otp_modal" "on_click_done"
      InAppOtpModal.AfterRender -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_app_otp_modal" "after_render"
    CountDown seconds -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "count_down"
    PopUpModalAction act -> case act of
      PopUpModal.OnButton1Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_end_ride" "go_back_onclick"
      PopUpModal.OnButton2Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_end_ride" "end_ride_onclick"
      PopUpModal.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_end_ride" "no_action"
      PopUpModal.ETextController act-> trackAppTextInput appId (getScreen HOME_SCREEN) "popup_modal_end_ride_text_changed" "primary_edit_text"
      PopUpModal.CountDown seconds id status timerID -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_end_ride" "countdown_updated"
      PopUpModal.OnImageClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_end_ride" "image_onclick"
    PopUpModalCancelConfirmationAction act -> case act of
      PopUpModal.OnButton1Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_cancel_confirmation" "continue_onclick"
      PopUpModal.OnButton2Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_cancel_confirmation" "go_back_onclick"
      PopUpModal.CountDown seconds id status timerID -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_cancel_confirmation" "countdown_updated"
      PopUpModal.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_cancel_confirmation" "no_action"
      PopUpModal.ETextController act-> trackAppTextInput appId (getScreen HOME_SCREEN) "popup_modal_cancel_confirmation_text_changed" "primary_edit_text"
      PopUpModal.OnImageClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_cancel_confirmation" "image_onclick"
    CancelRideModalAction act -> case act of
      CancelRide.Button1 act -> case act of
        PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "cancel_ride" "primary_btn_go_back_onclick"
        PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "cancel_ride" "primary_btn_go_back_no_action" 
      CancelRide.Button2 act -> case act of
        PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "cancel_ride" "primary_btn_cancel_ride_onclick" 
        PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "cancel_ride" "primary_btn_cancel_ride_no_action"
      CancelRide.UpdateIndex indexValue -> trackAppActionClick appId (getScreen HOME_SCREEN) "cancel_ride" "update_index_onclick"
      CancelRide.TextChanged  valId newVal -> trackAppTextInput appId (getScreen HOME_SCREEN) "reason_text_changed" "cancel_ride"
      CancelRide.OnGoBack -> trackAppActionClick appId (getScreen HOME_SCREEN) "cancel_ride" "go_back_onclick" 
      CancelRide.ClearOptions -> trackAppActionClick appId (getScreen HOME_SCREEN) "cancel_ride" "clear_options_onclick" 
      CancelRide.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "cancel_ride" "no_action"
    RetryTimeUpdate -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "retry_time_update_onclick" 
    RideActiveAction activeRide -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "ride_active_action"
    StatsModelAction act -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "stats_model_action"
    TimeUpdate time lat lng -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "time_update"
    ModifyRoute lat lon -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "modify_route"
    SetToken id -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "set_token"
    Cancel -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "cancel"
    CurrentLocation lat lng -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "current_location"
    ActiveRideAPIResponseAction resp -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "active_ride_api_response"
    RecenterButtonAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "recenter_btn"
    NoAction -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "no_action"
    UpdateMessages msg sender timeStamp -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "update_messages"
    InitializeChat -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "initialize_chat"
    RemoveChat -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "remove_chat"
    UpdateInChat -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "update_in_chat"
    ChatViewActionController act -> case act of 
      ChatView.SendMessage -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_app_messaging" "send_message"
      ChatView.SendSuggestion suggestion -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_app_messaging" "send_suggestion"
      ChatView.BackPressed -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_app_messaging" "back_pressed"
      ChatView.TextChanged input -> trackAppTextInput appId (getScreen HOME_SCREEN) "in_app_messaging" "text_changed" 
      ChatView.Call -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_app_messaging" "call_driver" 
      ChatView.Navigate -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_app_messaging" "navigate_to_google_maps"
      ChatView.NoAction -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_app_messaging" "no_action"


data ScreenOutput =   Refresh ST.HomeScreenState
                    | GoToProfileScreen 
                    | GoToRidesScreen 
                    | GoToReferralScreen
                    | StartRide ST.HomeScreenState 
                    | EndRide ST.HomeScreenState 
                    | CancelRide ST.HomeScreenState 
                    | DriverAvailabilityStatus ST.HomeScreenState Boolean 
                    | UpdatedState ST.HomeScreenState
                    | UpdateRoute ST.HomeScreenState
                    | FcmNotification String ST.HomeScreenState
                    | NotifyDriverArrived ST.HomeScreenState
                    | UpdateStage ST.HomeScreenStage ST.HomeScreenState
                    | GoToNotifications

data Action = NoAction
            | BackPressed
            | ScreenClick
            | Notification String
            | ChangeStatus Boolean
            | GoOffline Boolean
            | CancelGoOffline
            | AfterRender
            | ShowMap String String String
            | BottomNavBarAction BottomNavBar.Action
            | RideActionModalAction RideActionModal.Action
            | InAppOtpModalAction InAppOtpModal.Action
            | CountDown Int
            | CurrentLocation String String
            | ActiveRideAPIResponseAction (Array RidesInfo)
            | PopUpModalAction PopUpModal.Action
            | PopUpModalCancelConfirmationAction PopUpModal.Action
            | CancelRideModalAction CancelRide.Action 
            | Cancel 
            | SetToken String
            | ModifyRoute String String
            | RetryTimeUpdate
            | TimeUpdate String String String
            | StatsModelAction StatsModelController.Action
            | RideActiveAction RidesInfo
            | RecenterButtonAction
            | ChatViewActionController ChatView.Action
            | UpdateMessages String String String 
            | InitializeChat
            | RemoveChat
            | UpdateInChat

eval :: Action -> ST.HomeScreenState -> Eval Action ScreenOutput ST.HomeScreenState
eval AfterRender state = do 
  continue state{props{mapRendered= true}}
eval BackPressed state = do
  if state.props.enterOtpModal then do
    continue state { props = state.props { rideOtp = "", enterOtpFocusIndex = 0, enterOtpModal = false, rideActionModal = true } }
    else if (state.props.currentStage == ST.ChatWithCustomer) then do 
      _ <- pure $ setValueToLocalStore LOCAL_STAGE (show ST.RideAccepted)
      continue state{props{currentStage = ST.RideAccepted}}
      else if state.props.cancelRideModalShow then do
        continue state { data { cancelRideModal {activeIndex = Nothing, selectedReasonCode = "", selectedReasonDescription = ""}} ,props{ cancelRideModalShow = false, cancelConfirmationPopup = false}}
          else if state.props.cancelConfirmationPopup then do
            _ <- pure $ clearTimer state.data.cancelRideConfirmationPopUp.timerID
            continue state {props{cancelConfirmationPopup = false}, data{cancelRideConfirmationPopUp{timerID = "" , continueEnabled=false, enableTimer=false}}}
              else do
                _ <- pure $ minimizeApp ""
                continue state


eval (ChangeStatus status) state =
  if (getValueToLocalStore IS_DEMOMODE_ENABLED == "true") then
        continueWithCmd state [ do 
          _ <- pure $ setValueToLocalStore IS_DEMOMODE_ENABLED "false"
          _ <- pure $ toast (getString DEMO_MODE_DISABLED)
          _ <- pure $  deleteValueFromLocalStore IS_DEMOMODE_ENABLED
          _ <- pure $  deleteValueFromLocalStore DEMO_MODE_PASSWORD
          _ <- getCurrentPosition (showDriverMarker state "ny_ic_auto") constructLatLong
          pure NoAction
          ]
    else if  status then exit (DriverAvailabilityStatus state status)
      else continue state { props { goOfflineModal = true }}

eval (Notification notificationType) state = do 
  _ <- pure $ printLog "notificationType" notificationType
  if (checkNotificationType notificationType ST.DRIVER_REACHED && state.props.currentStage == ST.RideAccepted && (not state.data.activeRide.notifiedCustomer)) then 
    continue state{data{activeRide{isDriverArrived = true}}}
    else if (Array.any ( _ == notificationType) [show ST.CANCELLED_PRODUCT, show ST.DRIVER_ASSIGNMENT, show ST.RIDE_REQUESTED, show ST.DRIVER_REACHED]) then do
      exit $ FcmNotification notificationType state 
      else continue state

eval CancelGoOffline state = do 
  continue state { props = state.props { goOfflineModal = false } }

eval (GoOffline status) state = exit (DriverAvailabilityStatus state { props = state.props { goOfflineModal = false }} status)

eval (ShowMap key lat lon) state = continueWithCmd state [ do 
  id <- checkPermissionAndUpdateDriverMarker state
  pure AfterRender
  ]
eval (BottomNavBarAction (BottomNavBar.OnNavigate item)) state = do 
  case item of
    "Rides" -> exit GoToRidesScreen
    "Profile" -> exit $ GoToProfileScreen
    "Alert" -> do 
      _ <- pure $ setValueToLocalNativeStore ALERT_RECEIVED "false"
      _ <- pure $ firebaseLogEvent "ny_driver_alert_click"
      exit $ GoToNotifications
    "Contest" -> do
      _ <- pure $ setValueToLocalNativeStore REFERRAL_ACTIVATED "false"
      exit $ GoToReferralScreen
    _ -> continue state

eval (InAppOtpModalAction (InAppOtpModal.OnSelection key index)) state = do 
  let 
    rideOtp = if (index + 1) > (length state.props.rideOtp) then ( take 4 (state.props.rideOtp <> key)) else (take index (state.props.rideOtp)) <> key <> (take 4 (drop (index+1) state.props.rideOtp))
    focusIndex = length rideOtp
  continue state { props = state.props { rideOtp = rideOtp, enterOtpFocusIndex = focusIndex } }
eval (InAppOtpModalAction (InAppOtpModal.OnClickBack text)) state = do
  let 
    rideOtp = (if length( text ) > 0 then (take (length ( text ) - 1 ) text) else "" )
    focusIndex = length rideOtp
  continue state { props = state.props { rideOtp = rideOtp, enterOtpFocusIndex = focusIndex, otpIncorrect = false } }
eval (InAppOtpModalAction (InAppOtpModal.OnclickTextBox index)) state = do 
  let focusIndex = if index > (length state.props.rideOtp) then (length state.props.rideOtp) else index 
  let rideOtp = take index state.props.rideOtp
  continue state { props = state.props { enterOtpFocusIndex = focusIndex, rideOtp = rideOtp, otpIncorrect = false } }
eval (InAppOtpModalAction (InAppOtpModal.BackPressed)) state = do 
  continue state { props = state.props { rideOtp = "", enterOtpFocusIndex = 0, enterOtpModal = false} }
eval (InAppOtpModalAction (InAppOtpModal.OnClickDone text)) state = do 
    exit $ StartRide state
eval (RideActionModalAction (RideActionModal.StartRide)) state = do 
  continue state { props = state.props { enterOtpModal = true, rideOtp = "", enterOtpFocusIndex = 0, otpIncorrect = false } }
eval (RideActionModalAction (RideActionModal.EndRide)) state = do 
  continue $ (state {props {endRidePopUp = true}, data {route = []}})
eval (RideActionModalAction (RideActionModal.OnNavigate)) state = do
  let lat = if (state.props.currentStage == ST.RideAccepted || state.props.currentStage == ST.ChatWithCustomer) then state.data.activeRide.src_lat else state.data.activeRide.dest_lat
      lon = if (state.props.currentStage == ST.RideAccepted || state.props.currentStage == ST.ChatWithCustomer) then state.data.activeRide.src_lon else state.data.activeRide.dest_lon
  void $ pure $ openNavigation 0.0 0.0 lat lon
  continue state
eval (RideActionModalAction (RideActionModal.CancelRide)) state = do
  continue state{ data {cancelRideConfirmationPopUp{delayInSeconds = 5,  continueEnabled=false}}, props{cancelConfirmationPopup = true}}
eval (RideActionModalAction (RideActionModal.CallCustomer)) state = continueWithCmd state [ do
  _ <-  pure $ showDialer (getCustomerNumber "")
  _ <- (firebaseLogEventWithTwoParams "call_customer" "trip_id" (state.data.activeRide.id) "user_id" (getValueToLocalStore DRIVER_ID))
  pure NoAction
  ]

eval (RideActionModalAction (RideActionModal.MessageCustomer)) state = do 
  _ <- pure $ setValueToLocalStore LOCAL_STAGE (show ST.ChatWithCustomer)
  _ <- pure $ setValueToLocalNativeStore READ_MESSAGES (show (Array.length state.data.messages))
  continue state{props{currentStage = ST.ChatWithCustomer, sendMessageActive = false, unReadMessages = false}}

eval (RideActionModalAction (RideActionModal.LocationTracking)) state = do 
  let newState = state {props {showDottedRoute = not state.props.showDottedRoute} }
  updateAndExit newState $ UpdateRoute newState

eval (RideActionModalAction (RideActionModal.NotifyCustomer)) state =do 
  _ <- pure $ setValueToLocalStore LOCAL_STAGE (show ST.ChatWithCustomer)
  let newState = state{props{currentStage = ST.ChatWithCustomer, sendMessageActive = false, unReadMessages = false}}
  updateAndExit newState $ NotifyDriverArrived newState

eval (RideActionModalAction (RideActionModal.ButtonTimer seconds id status timerID)) state = do 
  if status == "EXPIRED" 
    then do
      _ <- pure $ clearTimer timerID 
      continue state{data{activeRide{isDriverArrived = false}}}
    else 
      continue state

eval (PopUpModalAction (PopUpModal.OnButton1Click)) state = continue $ (state {props {endRidePopUp = false}})
eval (PopUpModalAction (PopUpModal.OnButton2Click)) state = do 
  _ <- pure $ removeAllPolylines ""
  updateAndExit state {props {endRidePopUp = false, rideActionModal = false}} $ EndRide state {props {endRidePopUp = false, rideActionModal = false}}

eval (CancelRideModalAction (CancelRide.UpdateIndex indexValue)) state = continue state { data = state.data { cancelRideModal  { activeIndex = Just indexValue, selectedReasonCode =  (fromMaybe {reasonCode : "", description : ""} (((state.data.cancelRideModal).cancelRideReasons)Array.!!indexValue) ).reasonCode } } }
eval (CancelRideModalAction (CancelRide.TextChanged  valId newVal)) state = continue state { data {cancelRideModal { selectedReasonDescription = newVal, selectedReasonCode = "OTHER"}}}
eval (CancelRideModalAction (CancelRide.Button1 PrimaryButtonController.OnClick)) state = do
  pure $ hideKeyboardOnNavigation true 
  continue state { data{cancelRideModal {activeIndex = Nothing, selectedReasonCode = "", selectedReasonDescription = ""}} ,props {cancelRideModalShow = false, cancelConfirmationPopup=false } }
eval (CancelRideModalAction (CancelRide.OnGoBack)) state = continue state { data { cancelRideModal {activeIndex = Nothing, selectedReasonCode = "", selectedReasonDescription = ""}} ,props{ cancelRideModalShow = false, cancelConfirmationPopup = false}}
eval (CancelRideModalAction (CancelRide.ClearOptions)) state = continue state {data {cancelRideModal {activeIndex = Nothing, selectedReasonCode = "", selectedReasonDescription = ""}}}
eval (CancelRideModalAction (CancelRide.Button2 PrimaryButtonController.OnClick)) state = do
    pure $ hideKeyboardOnNavigation true 
    let cancelReasonSelected = case state.data.cancelRideModal.activeIndex of 
                                  Just index -> (state.data.cancelRideModal.cancelRideReasons Array.!! (index))
                                  Nothing    -> Nothing
    _ <- pure $ printLog "cancelReasonSelected" cancelReasonSelected
    case cancelReasonSelected of 
      Just reason -> do 
        _ <- pure $ printLog "inside Just" reason.reasonCode
        if (reason.reasonCode == "OTHER") then exit $ CancelRide state { props = state.props { cancelRideModalShow = false , cancelConfirmationPopup = false } } else do
          let newState = state { data = state.data {cancelRideModal = state.data.cancelRideModal { selectedReasonCode = reason.reasonCode , selectedReasonDescription = reason.description  } }, props = state.props { cancelRideModalShow = false, otpAttemptsExceeded = false, cancelConfirmationPopup = false } }
          exit $ CancelRide newState
      Nothing -> do 
        _ <- pure $ printLog "inside Nothing" "."
        continue state
  

eval (PopUpModalCancelConfirmationAction (PopUpModal.OnButton2Click)) state = do 
  _ <- pure $ clearTimer state.data.cancelRideConfirmationPopUp.timerID
  continue state {props{cancelConfirmationPopup = false}, data{cancelRideConfirmationPopUp{timerID = "" , continueEnabled=false, enableTimer=false}}}

eval (PopUpModalCancelConfirmationAction (PopUpModal.OnButton1Click)) state = continue state {props {cancelRideModalShow = true},data {cancelRideConfirmationPopUp{enableTimer = false}, cancelRideModal {activeIndex=Nothing, selectedReasonCode="", cancelRideReasons = cancellationReasons "" }}}

eval (PopUpModalCancelConfirmationAction (PopUpModal.CountDown seconds id status timerID)) state = do 
  if status == "EXPIRED" && seconds == 0 then do 
    _ <- pure $ clearTimer timerID 
    continue state { data { cancelRideConfirmationPopUp{delayInSeconds = 0, timerID = "", continueEnabled = true}}}
    else continue state { data {cancelRideConfirmationPopUp{delayInSeconds = (seconds+1), timerID = timerID, continueEnabled = false}}}

eval (CancelRideModalAction CancelRide.NoAction) state = do 
  _ <- pure $ printLog "CancelRideModalAction NoAction" state.data.cancelRideModal.cancelRideReasons
  continue state
eval (SetToken id )state = do 
  _ <-  pure $ setValueToLocalNativeStore FCM_TOKEN  id
  continue state
eval (CurrentLocation lat lng) state = do 
  let newState = state{data{ currentDriverLat = getLastKnownLocValue ST.LATITUDE lat,  currentDriverLon = getLastKnownLocValue ST.LONGITUDE lng }}
  exit $ UpdatedState newState
eval (ModifyRoute lat lon) state = do 
  let newState = state { data = state.data {currentDriverLat = getLastKnownLocValue ST.LATITUDE lat, currentDriverLon = getLastKnownLocValue ST.LONGITUDE lon} }
  exit $ UpdateRoute newState
  
eval RetryTimeUpdate state = do
  _ <-  pure $ setValueToLocalNativeStore REGISTERATION_TOKEN (getValueToLocalStore REGISTERATION_TOKEN)
  (updateAndExit state { data = state.data { locationLastUpdatedTime = "" }, props = state.props {refreshAnimation = true}} $ Refresh state { data = state.data { locationLastUpdatedTime = "" }, props = state.props {refreshAnimation = true}})

eval (TimeUpdate time lat lng) state = do
  let isDriverNearBy = ((getDistanceBwCordinates (getLastKnownLocValue ST.LATITUDE lat) (getLastKnownLocValue ST.LONGITUDE lng)  state.data.activeRide.src_lat state.data.activeRide.src_lon) < 0.05) 
      newState = state { data = state.data { activeRide{isDriverArrived = if (state.props.currentStage == ST.RideAccepted && not state.data.activeRide.notifiedCustomer) then isDriverNearBy else state.data.activeRide.isDriverArrived},currentDriverLat= getLastKnownLocValue ST.LATITUDE lat,  currentDriverLon = getLastKnownLocValue ST.LONGITUDE lng, locationLastUpdatedTime = (convertUTCtoISC time "DD/MM/yyyy  hh:mm:ss a") }}
  _ <- pure $ setValueToLocalStore LOCATION_UPDATE_TIME (convertUTCtoISC time "DD/MM/yyyy  hh:mm:ss a")
  continueWithCmd newState [ do 
    _ <- if (getValueToLocalNativeStore IS_RIDE_ACTIVE == "false") then checkPermissionAndUpdateDriverMarker newState else pure unit
    pure AfterRender
    ]

eval (UpdateInChat) state = continue state {props{updatedArrivalInChat = true}}

eval (InitializeChat ) state = continue state {props{chatcallbackInitiated = true}}

eval RemoveChat state = do 
  continueWithCmd state {props{chatcallbackInitiated = false}} [ do
    _ <- stopChatListenerService
    _ <- pure $ setValueToLocalNativeStore READ_MESSAGES "0.0"
    pure $ NoAction
  ]

eval (UpdateMessages message sender timeStamp) state = do 
  let newMessage = [(ChatView.makeChatComponent message sender timeStamp)]
  let messages = state.data.messages <> [((ChatView.makeChatComponent (getMessage message) sender timeStamp))]
  case (Array.last newMessage) of
    Just value -> do
                  if (value.sentBy == "Driver") then 
                    updateMessagesWithCmd state { data { messages = messages, suggestionsList = []}}
                  else do 
                    let readMessages = fromMaybe 0.0 (fromString (getValueToLocalNativeStore READ_MESSAGES))
                    let unReadMessages = (if (readMessages == 0.0 && state.props.currentStage /= ST.ChatWithCustomer) then true else (if (readMessages < (toNumber (Array.length messages)) && state.props.currentStage /= ST.ChatWithCustomer) then true else false))
                    let suggestionsList = case value.message of
                                            "I'll be there in 2 min" -> primaryReplySuggestions ""
                                            "Are you coming?" ->  secondaryReplySuggestions ""
                                            _ -> []
                    updateMessagesWithCmd state { data {messages = messages, suggestionsList = suggestionsList}, props {unReadMessages = unReadMessages}}
    Nothing -> continue state

eval (ChatViewActionController (ChatView.TextChanged value)) state = do
  let sendMessageActive = if (length (trim value)) >= 1 then
                          true
                        else
                          false
  continue state{data{messageToBeSent = (trim value)},props{sendMessageActive = sendMessageActive}}

eval(ChatViewActionController (ChatView.Call)) state = continueWithCmd state [ do
  _ <- pure $  showDialer (getCustomerNumber "")
  _ <- (firebaseLogEventWithTwoParams "call_customer" "trip_id" (state.data.activeRide.id) "user_id" (getValueToLocalStore DRIVER_ID))
  pure NoAction
  ]

eval (ChatViewActionController (ChatView.SendMessage)) state = do
  if state.data.messageToBeSent /= ""
  then 
   continueWithCmd state{data{messageToBeSent = ""},props {sendMessageActive = false}} [do
      _ <- pure $ sendMessage state.data.messageToBeSent
      _ <- setText' (getNewIDWithTag "ChatInputEditText") ""
      pure NoAction
   ]
  else
    continue state

eval (ChatViewActionController (ChatView.SendSuggestion chatSuggestion)) state = do
  let suggestions = Array.filter (\item -> (getString item) == chatSuggestion) (chatSuggestionsList "")
  let message = map (\item -> (getEN item)) suggestions
  _ <- pure $ sendMessage (fromMaybe "" (message Array.!! 0))
  continue state

eval (ChatViewActionController (ChatView.BackPressed)) state = do
  _ <- pure $ hideKeyboardOnNavigation true
  continueWithCmd state [do 
      pure $ BackPressed 
    ]

eval (ChatViewActionController (ChatView.Navigate)) state = do
  _ <- pure $ hideKeyboardOnNavigation true
  continueWithCmd state [do
    pure $ RideActionModalAction (RideActionModal.OnNavigate)
  ]

eval (RideActiveAction activeRide) state = updateAndExit state { data {activeRide = activeRideDetail state activeRide}} $ UpdateStage ST.RideAccepted state { data {activeRide = activeRideDetail state activeRide}}

eval RecenterButtonAction state = continue state

eval _ state = continue state 


checkPermissionAndUpdateDriverMarker :: ST.HomeScreenState -> Effect Unit 
checkPermissionAndUpdateDriverMarker state = do 
  conditionA <- isLocationPermissionEnabled unit 
  conditionB <- isLocationEnabled unit 
  if conditionA && conditionB then do 
    _ <- pure $ printLog "update driver location" "."
    _ <- getCurrentPosition (showDriverMarker state "ny_ic_auto") constructLatLong
    pure unit
    else do 
      _ <- requestLocation unit
      pure unit
 
showDriverMarker :: ST.HomeScreenState -> String -> ST.Location -> Effect Unit 
showDriverMarker state marker location = do
  case (getValueToLocalStore DEMO_MODE_PASSWORD) of
    "7891234" -> updateAutoIcon 13.311895563147432 76.93981481869986
    "8917234" -> updateAutoIcon 13.260559676317829 76.4785809882692
    "9178234" -> updateAutoIcon 13.160550263780683 76.66727044721313
    "1789234" -> updateAutoIcon 12.522069908884921 76.89518072273476
    _ -> do
      _ <- pure $ enableMyLocation true 
      animateCamera location.lat location.lon 17

updateAutoIcon :: Number -> Number -> Effect Unit 
updateAutoIcon lat lng = do
  _ <- showMarker "ny_ic_auto" lat lng 100 0.5 0.5
  _ <- pure $ enableMyLocation true
  animateCamera lat lng 17

constructLatLong :: String -> String -> ST.Location
constructLatLong lat lon =
  { lat: readFloat lat
  , lon : readFloat lon
  , place : ""
  }

activeRideDetail :: ST.HomeScreenState -> RidesInfo -> ST.ActiveRide
activeRideDetail state (RidesInfo ride) = {
  id : ride.id,
  source : (decodeAddress ride.fromLocation ),
  destination : (decodeAddress ride.toLocation),
  src_lat :  ((ride.fromLocation) ^. _lat),
  src_lon :  ((ride.fromLocation) ^. _lon),
  dest_lat: ((ride.toLocation) ^. _lat),
  dest_lon: ((ride.toLocation) ^. _lon),
  actualRideDistance : fromMaybe 0.0 (fromString (parseFloat (toNumber( fromMaybe 0 ride.chargeableDistance)) 2)),
  status : case ride.status of
              "NEW" -> NEW
              "INPROGRESS" -> INPROGRESS
              "COMPLETED" -> COMPLETED
              "CANCELLED" -> CANCELLED
              _ -> COMPLETED,
  distance : (toNumber ride.estimatedDistance),
  duration : state.data.activeRide.duration,
  riderName : fromMaybe "" ride.riderName,
  estimatedFare : ride.driverSelectedFare + ride.estimatedBaseFare,
  isDriverArrived : state.data.activeRide.isDriverArrived,
  notifiedCustomer : if (differenceBetweenTwoUTC ride.updatedAt ride.createdAt) == 0 then false else true
}

cancellationReasons :: String -> Array CancellationReasons
cancellationReasons dummy = [
        {
          reasonCode: "VEHICLE_ISSUE"
        , description: (getString VEHICLE_ISSUE)
        },
        {
          reasonCode: "PICKUP_TOO_FAR"
        , description: (getString PICKUP_TOO_FAR)
        },
        {
          reasonCode: "CUSTOMER_NOT_PICKING_CALL"
        , description: (getString CUSTOMER_NOT_PICKING_CALL)
        },
        {
          reasonCode: "TRAFFIC_JAM"
        , description: (getString TRAFFIC_JAM)
        },
        {
          reasonCode: "CUSTOMER_WAS_RUDE"
        , description: (getString CUSTOMER_WAS_RUDE)
        },
        {
          reasonCode: "OTHER"
        , description: (getString OTHER)
        }
]

dummyCancelReason :: CancellationReasons
dummyCancelReason =  {
        reasonCode : ""
        , description :""
        }

getValueFromRange :: Int -> Int -> Int -> Int -> Int  -> Int
getValueFromRange inMin inMax outMin outMax percent = (percent - inMin) * (outMax - outMin + 1) / (inMax - inMin + 1) + outMin

checkNotificationType :: String -> ST.NotificationType -> Boolean
checkNotificationType currentNotification requiredNotification = (show requiredNotification) == currentNotification

type RideRequestPollingData = {
    duration :: Int ,
    delay :: Number 
  }

getLastKnownLocValue :: ST.LocationType -> String -> Number
getLastKnownLocValue lType val =
  let lastKnownValue = fromMaybe 0.0 $ fromString $ getValueToLocalNativeStore $ if lType == ST.LATITUDE then LAST_KNOWN_LAT else LAST_KNOWN_LON
      currentVal = if (fromMaybe 0.0 (fromString val)) == 0.0 then Nothing else (fromString val)
    in fromMaybe lastKnownValue currentVal


updateMessagesWithCmd :: ST.HomeScreenState -> Eval Action ScreenOutput ST.HomeScreenState
updateMessagesWithCmd state =
  continueWithCmd state [ do
    if(state.props.currentStage == ST.ChatWithCustomer) then do
      _ <- pure $ scrollToBottom (getNewIDWithTag "ChatScrollView")
      pure unit
    else
      pure unit
    pure NoAction
    ]

getMessage :: String -> String
getMessage message = case message of 
                        "I'll be there in 2 min" -> (getString I_WILL_BE_THERE_IN_2_MINS)
                        "I'm at the pickup location" -> (getString I_AM_AT_THE_PICKUP_LOCATION)
                        "Are you coming?" -> (getString ARE_YOU_COMING)
                        "I've Arrived" -> (getString I_HAVE_ARRIVED)
                        "I'm stuck in traffic" -> (getString I_AM_STUCK_IN_TRAFFIC)
                        "Yes, I am almost there" -> (getString YES_I_AM_ALMOST_THERE)
                        "I'm on the way" -> (getString I_AM_ON_THE_WAY)
                        "Delayed a bit; reaching in some time" -> (getString DELAYED_A_BIT_REACHING_IN_SOME_TIME)
                        "Okay" -> (getString OKAY)
                        "Please be quick, I am waiting" -> (getString PLEASE_BE_QUICK_I_AM_WAITING)
                        _ -> message

initialSuggestions :: String -> Array String
initialSuggestions _ = 
  [
    (getString I_AM_ON_THE_WAY),
    (getString I_AM_STUCK_IN_TRAFFIC)
  ]

pickupSuggestions :: String -> Array String
pickupSuggestions _ = 
  [
    (getString I_HAVE_ARRIVED)
  ]

primaryReplySuggestions :: String -> Array String
primaryReplySuggestions _ = 
  [ 
    (getString OKAY),
    (getString PLEASE_BE_QUICK_I_AM_WAITING)
  ]

secondaryReplySuggestions :: String -> Array String
secondaryReplySuggestions _ = 
  [
    (getString YES_I_AM_ALMOST_THERE),
    (getString DELAYED_A_BIT_REACHING_IN_SOME_TIME)
  ]

chatSuggestionsList :: String -> Array STR
chatSuggestionsList _ =
  [
    I_AM_ON_THE_WAY,
    I_AM_STUCK_IN_TRAFFIC,
    I_HAVE_ARRIVED,
    OKAY,
    PLEASE_BE_QUICK_I_AM_WAITING,
    YES_I_AM_ALMOST_THERE,
    DELAYED_A_BIT_REACHING_IN_SOME_TIME
  ]