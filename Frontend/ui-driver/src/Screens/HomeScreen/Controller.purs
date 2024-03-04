{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.HomeScreen.Controller where

import Constants
import Helpers.Utils
import Locale.Utils
import PrestoDOM.Core
import PrestoDOM.List
import Screens.HomeScreen.ComponentConfig
import Screens.SubscriptionScreen.Controller

import Common.Resources.Constants (zoomLevel)
import Common.Styles.Colors as Color
import Common.Types.App (OptionButtonList, LazyCheck(..), Paths(..)) as Common
import Components.Banner as Banner
import Components.BannerCarousel as BannerCarousel
import Components.BottomNavBar as BottomNavBar
import Components.ChatView as ChatView
import Components.ChatView as ChatView
import Components.ErrorModal.Controller as ErrorModalController
import Components.GoToLocationModal as GoToLocationModal
import Components.InAppKeyboardModal as InAppKeyboardModal
import Components.MakePaymentModal as MakePaymentModal
import Components.PopUpModal as PopUpModal
import Components.PrimaryButton as PrimaryButtonController
import Components.RateCard as RateCard
import Components.RatingCard as RatingCard
import Components.RequestInfoCard as RequestInfoCard
import Components.RideActionModal as RideActionModal
import Components.RideCompletedCard as RideCompletedCard
import Components.SelectListModal as SelectListModal
import Components.StatsModel.Controller as StatsModelController
import Control.Monad.Except (runExcept)
import Control.Monad.Except (runExceptT)
import Control.Monad.Except.Trans (lift)
import Control.Monad.State (state)
import Control.Transformers.Back.Trans (runBackT)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Function.Uncurried (runFn1, runFn2)
import Data.Function.Uncurried (runFn2)
import Data.Function.Uncurried as Uncurried
import Data.Int (round, toNumber, fromString, ceil)
import Data.Int as Int
import Data.Lens ((^.))
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing, maybe)
import Data.Number (fromString) as Number
import Data.String (Pattern(..), Replacement(..), drop, length, take, trim, replaceAll, toLower, null)
import Data.String as DS
import Domain.Payments (APIPaymentStatus(..), PaymentStatus(..)) as PP
import Effect (Effect)
import Effect.Aff (launchAff)
import Effect.Class (liftEffect)
import Effect.Uncurried (runEffectFn4, runEffectFn3)
import Effect.Unsafe (unsafePerformEffect)
import Engineering.Helpers.BackTrack (getState, liftFlowBT)
import Engineering.Helpers.Commons (flowRunner)
import Engineering.Helpers.Commons (getCurrentUTC, getNewIDWithTag, convertUTCtoISC, isPreviousVersion,liftFlow)
import Engineering.Helpers.Commons as EHC
import JBridge (animateCamera, enableMyLocation, firebaseLogEvent, getCurrentPosition, getHeightFromPercent, hideKeyboardOnNavigation, isLocationEnabled, isLocationPermissionEnabled, minimizeApp, openNavigation, removeAllPolylines, requestLocation, showDialer, showMarker, toast, firebaseLogEventWithTwoParams,sendMessage, stopChatListenerService, getSuggestionfromKey, scrollToEnd, getChatMessages, cleverTapCustomEvent, metaLogEvent, toggleBtnLoader, openUrlInApp, pauseYoutubeVideo, differenceBetweenTwoUTC, removeMediaPlayer)
import Engineering.Helpers.LogEvent (logEvent, logEventWithTwoParams, logEventWithMultipleParams)
import Engineering.Helpers.Suggestions (getMessageFromKey, getSuggestionsfromKey, chatSuggestion)
import Engineering.Helpers.Utils (saveObject)
import Foreign.Generic (class Decode, ForeignError, decode, decodeJSON, encode)
import Foreign (unsafeToForeign)
import Helpers.Utils as HU
import JBridge (animateCamera, enableMyLocation, firebaseLogEvent, getCurrentPosition, getHeightFromPercent, hideKeyboardOnNavigation, isLocationEnabled, isLocationPermissionEnabled, minimizeApp, openNavigation, removeAllPolylines, requestLocation, showDialer, showMarker, toast, firebaseLogEventWithTwoParams, sendMessage, stopChatListenerService, getSuggestionfromKey, scrollToEnd, getChatMessages, cleverTapCustomEvent, metaLogEvent, toggleBtnLoader, openUrlInApp, pauseYoutubeVideo, differenceBetweenTwoUTC, uploadFile, getCurrentLatLong, uploadMultiPartData)
import JBridge as JB
import Language.Strings (getString)
import Language.Types (STR(..))
import Log (printLog, trackAppActionClick, trackAppBackPress, trackAppEndScreen, trackAppScreenEvent, trackAppScreenRender, trackAppTextInput)
import MerchantConfig.Utils (getMerchant, Merchant(..))
import Prelude (class Show, Unit, bind, discard, map, not, pure, show, unit, void, ($), (&&), (*), (+), (-), (/), (/=), (<), (<>), (==), (>), (||), (<=), (>=), when, negate, (<<<), (>>=), (<$>))
import Presto.Core.Types.Language.Flow (Flow, delay, doAff)
import PrestoDOM (Eval, continue, continueWithCmd, exit, updateAndExit, updateWithCmdAndExit)
import PrestoDOM.Core (getPushFn)
import PrestoDOM.Types.Core (class Loggable)
import RemoteConfig as RC
import Resource.Constants (decodeAddress, getLocationInfoFromStopLocation, rideTypeConstructor, getHomeStageFromString)
import Screens (ScreenName(..), getScreen)
import Screens.Types as ST
import Services.API (GetRidesHistoryResp(..), RidesInfo(..), Status(..), GetCurrentPlanResp(..), PlanEntity(..), PaymentBreakUp(..), GetRouteResp(..), Route(..),StopLocation(..))
import Services.Accessor (_lat, _lon)
import Storage (KeyStore(..), deleteValueFromLocalStore, getValueToLocalNativeStore, getValueToLocalStore, setValueToLocalNativeStore, setValueToLocalStore)
import Types.App (FlowBT, GlobalState(..), HOME_SCREENOUTPUT(..), ScreenType(..))
import Types.ModifyScreenState (modifyScreenState)
import Helpers.Utils as HU
import Services.API (LocationInfo(..))
import JBridge as JB
import Effect.Uncurried (runEffectFn1,runEffectFn4)
import Constants 
import Data.Function.Uncurried (runFn1, runFn2)
import Screens.HomeScreen.ComponentConfig
import MerchantConfig.Utils (getMerchant, Merchant(..))
import Common.Resources.Constants (zoomLevel)
import Control.Monad.Except (runExceptT)
import Control.Transformers.Back.Trans (runBackT)
import Effect.Aff (launchAff)
import Engineering.Helpers.Commons (flowRunner)
import Types.App (defaultGlobalState)
import Services.API as API
import Services.Accessor (_lat, _lon)
import Services.Backend as Remote
import Services.Config as SC
import Services.EndPoints as EP
import Styles.Colors as Color
import PrestoDOM.Core
import PrestoDOM.List
import RemoteConfig as RC
import Locale.Utils
import Foreign (unsafeToForeign)
import SessionCache (getValueFromWindow)
import Timers as TF
import Data.Ord (abs)

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
    RideActionModalAction act -> pure unit-- case act of
      -- RideActionModal.StartRide -> trackAppActionClick appId (getScreen HOME_SCREEN) "ride_action_modal" "start_ride"
      -- RideActionModal.EndRide -> trackAppActionClick appId (getScreen HOME_SCREEN) "ride_action_modal" "end_ride"
      -- RideActionModal.SelectListModal -> trackAppActionClick appId (getScreen HOME_SCREEN) "ride_action_modal" "cancel_ride"
      -- RideActionModal.OnNavigate -> trackAppActionClick appId (getScreen HOME_SCREEN) "ride_action_modal" "on_navigate"
      -- RideActionModal.CallCustomer -> trackAppActionClick appId (getScreen HOME_SCREEN) "ride_action_modal" "call_customer"
      -- RideActionModal.LocationTracking -> trackAppActionClick appId (getScreen HOME_SCREEN) "ride_action_modal" "location_tracking"
      -- RideActionModal.MessageCustomer -> trackAppActionClick appId (getScreen HOME_SCREEN) "ride_action_modal" "message_customer"
      -- _ -> pure unit
    PopUpModalAccessibilityAction act -> pure unit
    InAppKeyboardModalAction act -> pure unit--case act of
      -- InAppKeyboardModal.OnSelection key index -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_app_otp_modal" "on_selection"
      -- InAppKeyboardModal.OnClickBack text -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_app_otp_modal" "on_click_back"
      -- InAppKeyboardModal.OnclickTextBox index -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_app_otp_modal" "on_click_text_box"
      -- InAppKeyboardModal.BackPressed -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_app_otp_modal" "on_backpressed"
      -- InAppKeyboardModal.OnClickDone text -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_app_otp_modal" "on_click_done"
      -- _ -> pure unit
    InAppKeyboardModalOdometerAction act -> pure unit
    CountDown seconds -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "count_down"
    PopUpModalAction act -> pure unit --case act of
      -- PopUpModal.OnButton1Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_end_ride" "go_back_onclick"
      -- PopUpModal.OnButton2Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_end_ride" "end_ride_onclick"
      -- PopUpModal.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_end_ride" "no_action"
      -- PopUpModal.ETextController act-> trackAppTextInput appId (getScreen HOME_SCREEN) "popup_modal_end_ride_text_changed" "primary_edit_text"
      -- PopUpModal.CountDown seconds id status timerID -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_end_ride" "countdown_updated"
      -- PopUpModal.OnImageClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_end_ride" "image_onclick"
      -- PopUpModal.Tipbtnclick arg1 arg2 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_action" "tip_clicked"
      -- PopUpModal.DismissPopup -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_action" "popup_dismissed"
    PopUpModalCancelConfirmationAction act -> pure unit -- case act of
      -- PopUpModal.OnButton1Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_cancel_confirmation" "continue_onclick"
      -- PopUpModal.OnButton2Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_cancel_confirmation" "go_back_onclick"
      -- PopUpModal.CountDown seconds id status timerID -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_cancel_confirmation" "countdown_updated"
      -- PopUpModal.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_cancel_confirmation" "no_action"
      -- PopUpModal.ETextController act-> trackAppTextInput appId (getScreen HOME_SCREEN) "popup_modal_cancel_confirmation_text_changed" "primary_edit_text"
      -- PopUpModal.OnImageClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_cancel_confirmation" "image_onclick"
      -- PopUpModal.Tipbtnclick arg1 arg2 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_action" "tip_clicked"
      -- PopUpModal.DismissPopup -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_action" "popup_dismissed"
    CancelRideModalAction act -> pure unit -- case act of
      -- SelectListModal.Button1 act -> case act of
      --   PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "cancel_ride" "primary_btn_go_back_onclick"
      --   PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "cancel_ride" "primary_btn_go_back_no_action"
      -- SelectListModal.Button2 act -> case act of
      --   PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "cancel_ride" "primary_btn_cancel_ride_onclick"
      --   PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "cancel_ride" "primary_btn_cancel_ride_no_action"
      -- SelectListModal.UpdateIndex indexValue -> trackAppActionClick appId (getScreen HOME_SCREEN) "cancel_ride" "update_index_onclick"
      -- SelectListModal.TextChanged  valId newVal -> trackAppTextInput appId (getScreen HOME_SCREEN) "reason_text_changed" "cancel_ride"
      -- SelectListModal.OnGoBack -> trackAppActionClick appId (getScreen HOME_SCREEN) "cancel_ride" "go_back_onclick"
      -- SelectListModal.ClearOptions -> trackAppActionClick appId (getScreen HOME_SCREEN) "cancel_ride" "clear_options_onclick"
      -- SelectListModal.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "cancel_ride" "no_action"
    GenderBannerModal act -> pure unit
    AutoPayBanner act -> pure unit
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
    HelpAndSupportScreen -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "help_and_support_btn"
    NoAction -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "no_action"
    UpdateMessages msg sender timeStamp size -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "update_messages"
    OpenChatScreen -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "open_chat"
    InitializeChat -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "initialize_chat"
    RemoveChat -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "remove_chat"
    UpdateInChat -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "update_in_chat"
    ScrollToBottom -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "scroll_to_bottom"
    GetMessages -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "get_messages"
    KeyboardCallback status -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "key_board_callback"
    ChatViewActionController act -> pure unit -- case act of
      -- ChatView.SendMessage -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_app_messaging" "send_message"
      -- ChatView.SendSuggestion suggestion -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_app_messaging" "send_suggestion"
      -- ChatView.BackPressed -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_app_messaging" "back_pressed"
      -- ChatView.TextChanged input -> trackAppTextInput appId (getScreen HOME_SCREEN) "in_app_messaging" "text_changed"
      -- ChatView.Call -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_app_messaging" "call_driver"
      -- ChatView.Navigate -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_app_messaging" "navigate_to_google_maps"
      -- ChatView.NoAction -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_app_messaging" "no_action"
    SwitchDriverStatus status -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "SwitchDriverStatus"
    GoToProfile -> do
      trackAppActionClick appId (getScreen HOME_SCREEN) "bottom_nav_bar" "on_navigate"
      trackAppEndScreen appId (getScreen HOME_SCREEN)
    LinkAadhaarPopupAC act -> pure unit --case act of
    PopUpModalSilentAction act -> pure unit --case act of
      -- PopUpModal.OnButton1Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_silent_confirmation" "go_offline_onclick"
      -- PopUpModal.OnButton2Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_silent_confirmation" "go_silent_onclick"
      -- PopUpModal.CountDown seconds id status timerID -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_silent_confirmation" "countdown_updated"
      -- PopUpModal.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_silent_confirmation" "no_action"
      -- PopUpModal.ETextController act-> trackAppTextInput appId (getScreen HOME_SCREEN) "popup_modal_cancel_confirmation_text_changed" "primary_edit_text"
      -- PopUpModal.OnImageClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_cancel_confirmation" "image_onclick"
      -- PopUpModal.Tipbtnclick arg1 arg2 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_action" "tip_clicked"
      -- PopUpModal.DismissPopup -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_action" "popup_dismissed"
    ClickAddAlternateButton -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "add-alternate_btn"
    ZoneOtpAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "zone_otp"
    TriggerMaps -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "trigger_maps"
    RemoveGenderBanner -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "gender_banner"
    RequestInfoCardAction act -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "request_info_card"
    _ -> pure unit
    WaitTimerCallback id min sec -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "wait_timer_callBack" 
    MakePaymentModalAC act -> pure unit
    RateCardAC act -> pure unit
    PaymentBannerAC act -> pure unit
    PaymentStatusAction _ -> pure unit
    RemovePaymentBanner -> pure unit
    OfferPopupAC _ -> pure unit
    RCDeactivatedAC _ -> pure unit
    FreeTrialEndingAC _ -> pure unit
    UploadImage -> pure unit
    CallBackImageUpload _ _ _ -> pure unit
    CallBackNewStop _ -> pure unit
    _ -> pure unit


data ScreenOutput =   Refresh ST.HomeScreenState
                    | GoToHelpAndSupportScreen ST.HomeScreenState
                    | GoToProfileScreen ST.HomeScreenState
                    | GoToRidesScreen ST.HomeScreenState
                    | GoToReferralScreen
                    | StartRide ST.HomeScreenState
                    | EndRide ST.HomeScreenState
                    | ArrivedAtStop ST.HomeScreenState
                    | SelectListModal ST.HomeScreenState
                    | DriverAvailabilityStatus ST.HomeScreenState ST.DriverStatus
                    | UpdatedState ST.HomeScreenState
                    | UpdateRoute ST.HomeScreenState
                    | FcmNotification String ST.HomeScreenState
                    | NotifyDriverArrived ST.HomeScreenState
                    | UpdateStage ST.HomeScreenStage ST.HomeScreenState
                    | GoToNotifications ST.HomeScreenState
                    | AddAlternateNumber ST.HomeScreenState
                    | StartZoneRide ST.HomeScreenState
                    | CallCustomer ST.HomeScreenState String
                    | GotoEditGenderScreen
                    | OpenPaymentPage ST.HomeScreenState
                    | AadhaarVerificationFlow ST.HomeScreenState
                    | SubscriptionScreen ST.HomeScreenState
                    | GoToVehicleDetailScreen ST.HomeScreenState
                    | GoToRideDetailsScreen ST.HomeScreenState
                    | PostRideFeedback ST.HomeScreenState
                    | ClearPendingDues ST.HomeScreenState
                    | EnableGoto ST.HomeScreenState String
                    | LoadGotoLocations ST.HomeScreenState
                    | DisableGoto ST.HomeScreenState
                    | ExitGotoLocation ST.HomeScreenState Boolean
                    | RefreshGoTo ST.HomeScreenState
                    | EarningsScreen ST.HomeScreenState Boolean
                    | FetchOdometerReading ST.HomeScreenState
                    | GoToNewStop ST.HomeScreenState

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
            | InAppKeyboardModalAction InAppKeyboardModal.Action
            | InAppKeyboardModalOdometerAction InAppKeyboardModal.Action
            | CountDown Int
            | CurrentLocation String String
            | ActiveRideAPIResponseAction (Array RidesInfo)
            | PopUpModalAction PopUpModal.Action
            | PopUpModalCancelConfirmationAction PopUpModal.Action
            | CancelRideModalAction SelectListModal.Action
            | Cancel
            | SetToken String
            | UpiQrRendered String
            | ModifyRoute String String
            | RetryTimeUpdate
            | TimeUpdate String String String
            | StatsModelAction StatsModelController.Action
            | RideActiveAction RidesInfo
            | RecenterButtonAction
            | ChatViewActionController ChatView.Action
            | UpdateMessages String String String String
            | InitializeChat
            | OpenChatScreen
            | RemoveChat
            | UpdateInChat
            | HelpAndSupportScreen
            | SwitchDriverStatus ST.DriverStatus
            | PopUpModalSilentAction PopUpModal.Action
            | LinkAadhaarPopupAC PopUpModal.Action
            | GoToProfile
            | ClickAddAlternateButton
            | ZoneOtpAction
            | TriggerMaps
            | GenderBannerModal Banner.Action
            | PaymentBannerAC Banner.Action
            | RemoveGenderBanner
            | RequestInfoCardAction RequestInfoCard.Action
            | ScrollToBottom
            | GoToButtonClickAC PrimaryButtonController.Action
            | GoToLocationModalAC GoToLocationModal.Action
            | CancelBackAC PrimaryButtonController.Action
            | ClickInfo
            | GotoKnowMoreAction PopUpModal.Action
            | OnClickChangeAction GoToLocationModal.Action 
            | WaitTimerCallback String String Int
            | MakePaymentModalAC MakePaymentModal.Action
            | RateCardAC RateCard.Action
            | PaymentStatusAction PP.APIPaymentStatus
            | RemovePaymentBanner
            | KeyboardCallback String
            | OfferPopupAC PopUpModal.Action
            | FreeTrialEndingAC PopUpModal.Action 
            | GetCurrentDuesAction GetCurrentPlanResp
            | GetCurrentDuesFailed
            | AutoPayBanner Banner.Action
            | RCDeactivatedAC PopUpModal.Action
            | PopUpModalAccessibilityAction PopUpModal.Action
            | RideCompletedAC RideCompletedCard.Action
            | RatingCardAC RatingCard.Action
            | PopUpModalChatBlockerAction PopUpModal.Action
            | StartEarningPopupAC PopUpModal.Action
            | GetMessages
            | PaymentPendingPopupAC PopUpModal.Action
            | AccessibilityBannerAction Banner.Action
            | GenericAccessibilityPopUpAction PopUpModal.Action
            | GotoRequestPopupAction PopUpModal.Action
            | GotoCancellationPreventionAction PopUpModal.Action 
            | GotoLocInRangeAction PopUpModal.Action
            | EnableGotoTimerAC PrimaryButtonController.Action
            | UpdateGoHomeTimer Int String String
            | AddLocation PrimaryButtonController.Action
            | ConfirmDisableGoto PopUpModal.Action
            | AccountBlockedAC PopUpModal.Action
            | UpdateAndNotify
            | UpdateWaitTime ST.TimerStatus
            | NotifyAPI
            | IsMockLocation String
            | ErrorModalActionController ErrorModalController.Action
            | AddNewLocation
            | AddGotoAC
            | LinkAadhaarAC
            | BannerCarousal BannerCarousel.Action
            | SetBannerItem ListItem
            | UpdateBanner
            | BannerChanged String
            | BannerStateChanged String
            | CoinsPopupAC PopUpModal.Action 
            | ToggleStatsModel
            | ToggleBonusPopup
            | GoToEarningsScreen Boolean
            | CallBackImageUpload String String String
            | UploadMultiPartDataCallback  String String
            | UploadImage
            | CallBackNewStop String
            | NewStopAdded Common.Paths (Maybe StopLocation) (Maybe StopLocation)
            | NewStopPopup PopUpModal.Action
            | CustomerSafetyPopupAC PopUpModal.Action
            | UpdateLastLoc Number Number Boolean
            | VehicleNotSupportedAC PopUpModal.Action
            

eval :: Action -> ST.HomeScreenState -> Eval Action ScreenOutput ST.HomeScreenState

eval (GoToButtonClickAC PrimaryButtonController.OnClick) state = do
  pure $ toggleBtnLoader "GotoClick" false
  if state.data.driverGotoState.isGotoEnabled then continue state { data { driverGotoState { confirmGotoCancel = true } }} 
  else if state.data.driverGotoState.gotoCount <=0 then continue state
  else do
    pure $ toggleBtnLoader "GotoClick" true
    updateAndExit state{ data { driverGotoState { savedLocationsArray = []}}} $ LoadGotoLocations state{ data { driverGotoState { savedLocationsArray = []}}}

eval ClickInfo state = continue state {data { driverGotoState {goToInfo = true}}}

eval (GotoKnowMoreAction PopUpModal.OnButton1Click) state = continue state { data { driverGotoState { goToInfo = false } }} 

eval (ConfirmDisableGoto PopUpModal.OnButton2Click) state = continue state { data { driverGotoState { confirmGotoCancel = false } }} 

eval (ConfirmDisableGoto PopUpModal.OnButton1Click) state = updateAndExit state{ data { driverGotoState { confirmGotoCancel = false } }}  $ DisableGoto state{ data { driverGotoState { confirmGotoCancel = false } }} 

eval (AccountBlockedAC PopUpModal.OnButton2Click) state = continue state { props { accountBlockedPopup = false } }

eval (AccountBlockedAC PopUpModal.OnButton1Click) state = do 
  void $ pure $ showDialer (SC.getSupportNumber "") false 
  continue state
  
eval UpdateBanner state = do
  if state.data.bannerData.bannerScrollState == "1" then continue state
  else do
    let nextBanner = state.data.bannerData.currentBanner + 1
        updatedIdx = if nextBanner >= Array.length (getBannerConfigs state) then 0 else nextBanner
        newState = state{data {bannerData{currentBanner = updatedIdx, currentPage = updatedIdx}}}
    continue newState

eval (BannerChanged item) state = do
  let currentBanner = fromString item
  case currentBanner of
    Just idx -> do 
        let newState = state{data {bannerData{currentBanner = idx}}}
        if state.data.bannerData.currentPage /= idx then void $ pure $ unsafePerformEffect $ processEvent "RestartAutoScroll" unit -- To stop and stop the new autosroll
          else pure unit
        continue newState
    Nothing  -> continue state

eval (BannerStateChanged item) state = do
  let newState = state{data {bannerData{bannerScrollState = item}}}
  continue newState


eval (GotoRequestPopupAction (PopUpModal.OnButton1Click)) state = 
  case state.data.driverGotoState.goToPopUpType of
    ST.VALIDITY_EXPIRED -> exit $ RefreshGoTo state { data { driverGotoState { goToPopUpType = ST.NO_POPUP_VIEW } }} 
    _ -> continue state { data { driverGotoState { goToPopUpType = ST.NO_POPUP_VIEW} }} 

eval (GotoLocInRangeAction (PopUpModal.OnButton1Click)) state = continue state { data { driverGotoState { gotoLocInRange = false } }} 

eval (GoToLocationModalAC (GoToLocationModal.CardClicked item)) state = continue state {data { driverGotoState {selectedGoTo = item.id}}}

eval (EnableGotoTimerAC PrimaryButtonController.OnClick)state = updateAndExit state $ EnableGoto state state.data.driverGotoState.selectedGoTo

eval AddGotoAC state = exit $ ExitGotoLocation state false

eval (UpdateGoHomeTimer seconds status timerID) state = do
  if status == "EXPIRED" then do 
    void $ pure $ TF.clearTimerWithId timerID
    void $ pure $ HU.setPopupType ST.VALIDITY_EXPIRED
    continue state { data { driverGotoState { goToPopUpType = ST.VALIDITY_EXPIRED, timerId = "" }}}
    else do
      let timeInMinutes = seconds/60 + 1
      continue state { data { driverGotoState { timerInMinutes = show timeInMinutes <> " " <>(getString MIN_LEFT), timerId = timerID} } }

eval (CancelBackAC PrimaryButtonController.OnClick) state = continue state { data { driverGotoState { showGoto = false}}}

eval (AddLocation PrimaryButtonController.OnClick) state = exit $ ExitGotoLocation state true

eval AddNewLocation state = exit $ ExitGotoLocation state true

eval AfterRender state = continue state { props { mapRendered = true}}

eval BackPressed state = do
  if state.props.showGenericAccessibilityPopUp then do 
    _ <- pure $ pauseYoutubeVideo unit
    continue state{props{showGenericAccessibilityPopUp = false}}
  else if state.props.showRideRating then continue state{props{showRideRating = false}}
  else if state.props.currentStage == ST.RideCompleted then do
    _ <- pure $ minimizeApp ""
    continue state
  else if state.props.enterOtpModal then continue state { props = state.props { rideOtp = "", enterOtpFocusIndex = 0, enterOtpModal = false, rideActionModal = true } }
  else if (state.props.currentStage == ST.ChatWithCustomer) then do
    previousStage <- pure $ getValueToLocalStore PREVIOUS_LOCAL_STAGE
    _ <- pure $ setValueToLocalStore PREVIOUS_LOCAL_STAGE $ show state.props.currentStage
    _ <- pure $ setValueToLocalStore LOCAL_STAGE previousStage
    continue state{props{currentStage = getHomeStageFromString previousStage}}

  else if state.props.cancelRideModalShow then continue state { data { cancelRideModal {activeIndex = Nothing, selectedReasonCode = "", selectedReasonDescription = ""}} ,props{ cancelRideModalShow = false, cancelConfirmationPopup = false}}
  else if state.props.cancelConfirmationPopup then do
    _ <- pure $ TF.clearTimerWithId state.data.cancelRideConfirmationPopUp.timerID
    continue state {props{cancelConfirmationPopup = false}, data{cancelRideConfirmationPopUp{timerID = "" , continueEnabled=false, enableTimer=false}}}
  else if state.props.showBonusInfo then continue state { props { showBonusInfo = false } }
  else if state.data.paymentState.showRateCard then continue state { data { paymentState{ showRateCard = false } } }
  else if state.props.endRidePopUp then continue state{props {endRidePopUp = false}}
  else if (state.props.showlinkAadhaarPopup && state.props.showAadharPopUp) then continue state {props{showAadharPopUp = false}}
  else if state.props.silentPopUpView then continue state { props {silentPopUpView = false}}
  else if state.data.paymentState.showBlockingPopup then continue state {data{paymentState{showBlockingPopup = false}}}
  else if state.props.subscriptionPopupType /= ST.NO_SUBSCRIPTION_POPUP then continue state {props{subscriptionPopupType = ST.NO_SUBSCRIPTION_POPUP}}
  else if state.data.driverGotoState.goToInfo then continue state{data {driverGotoState {goToInfo = false}}}
  else if state.data.driverGotoState.gotoLocInRange then continue state { data { driverGotoState { gotoLocInRange = false }}} 
  else if state.data.driverGotoState.confirmGotoCancel then continue state { data { driverGotoState { confirmGotoCancel = false }}} 
  else if state.data.driverGotoState.showGoto then continue state { data { driverGotoState { showGoto = false }}} 
  else if state.data.driverGotoState.goToPopUpType /= ST.NO_POPUP_VIEW then continue state { data { driverGotoState { goToPopUpType = ST.NO_POPUP_VIEW }}} 
  else if state.props.showContactSupportPopUp then continue state {props {showContactSupportPopUp = false}}
  else if state.props.accountBlockedPopup then continue state {props {accountBlockedPopup = false}}
  else if state.props.vehicleNSPopup then continue state { props { vehicleNSPopup = false}}
  else do
    _ <- pure $ minimizeApp ""
    continue state

eval TriggerMaps state = continueWithCmd state[ do
  let destLat = if (state.data.activeRide.tripType == ST.Rental) then fromMaybe state.data.activeRide.src_lat state.data.activeRide.nextStopLat else state.data.activeRide.dest_lat
  let destLon = if (state.data.activeRide.tripType == ST.Rental) then fromMaybe state.data.activeRide.src_lon state.data.activeRide.nextStopLon else state.data.activeRide.dest_lon
  _ <- pure $ openNavigation 0.0 0.0 destLat destLon "DRIVE"
  _ <- pure $ setValueToLocalStore TRIGGER_MAPS "false"
  pure NoAction
  ]

eval (KeyboardCallback keyBoardState) state = do 
  let isOpen = case keyBoardState of
                    "onKeyboardOpen" -> true
                    "onKeyboardClose" -> false
                    _ -> false 
  if state.props.currentStage == ST.ChatWithCustomer && isOpen then do
    void $ pure $ scrollToEnd (getNewIDWithTag "ChatScrollView") true
    continue state
  else if state.props.enterOtpModal && not isOpen then
    continue state{ props{ rideOtp = "", enterOtpFocusIndex = 0, enterOtpModal = false } }
  else if (state.props.enterOdometerReadingModal || state.props.endRideOdometerReadingModal) && not isOpen then
    continueWithCmd state [do 
    pure $ InAppKeyboardModalOdometerAction (InAppKeyboardModal.OnClickDone "")
    ]
  else
    continue state

eval (Notification notificationType) state = do
  _ <- pure $ printLog "notificationType" notificationType
  if (checkNotificationType notificationType ST.DRIVER_REACHED && (state.props.currentStage == ST.RideAccepted || state.props.currentStage == ST.ChatWithCustomer) && (not state.data.activeRide.notifiedCustomer)) then do
    let newState = state{props{showAccessbilityPopup = isJust state.data.activeRide.disabilityTag, safetyAudioAutoPlay = false}}
    void $ pure $ setValueToLocalStore IS_DRIVER_AT_PICKUP "true"
    continueWithCmd newState [ pure if (not state.data.activeRide.notifiedCustomer) then NotifyAPI else AfterRender]
  else if (Array.any ( _ == notificationType) [show ST.CANCELLED_PRODUCT, show ST.DRIVER_ASSIGNMENT, show ST.RIDE_REQUESTED, show ST.DRIVER_REACHED]) then do
      exit $ FcmNotification notificationType state
  else continue state

eval CancelGoOffline state = do
  continue state { props = state.props { goOfflineModal = false } }

eval (GoOffline status) state = exit (DriverAvailabilityStatus state { props = state.props { goOfflineModal = false }} ST.Offline)

eval (ShowMap key lat lon) state = continueWithCmd state [ do
  id <- checkPermissionAndUpdateDriverMarker state
  pure AfterRender
  ]

eval (UploadImage) state = do
  _ <- pure $ printLog "UploadImage" $ getValueToLocalNativeStore LOCAL_STAGE
  let stage = getValueToLocalNativeStore LOCAL_STAGE
  if state.props.odometerImageUploading || ( stage == "RideStarted" && state.props.enterOdometerReadingModal) || ( stage == "RideCompleted" && state.props.endRideOdometerReadingModal) then
    continue state {props {odometerImageUploading = false}}
  else if state.props.odometerUploadAttempts < 3 then do
    continueWithCmd (state {props {odometerUploadAttempts = state.props.odometerUploadAttempts + 1,odometerImageUploading = true}}) [do
      let _ = unsafePerformEffect $ logEvent state.data.logField "UPLOAD odometer reading"
      _ <- liftEffect $ uploadFile false
      pure NoAction
      ]
  else do
    let newState = state{props{odometerFileId = Nothing}}
        exitAction = if state.props.endRideOdometerReadingModal then
            EndRide newState
          else do
            StartRide newState
    updateAndExit newState exitAction

eval (CallBackImageUpload image imageName imagePath) state = do
    _ <- pure $ printLog ("value of image base 64 taking time" <> imageName <> " " <> imagePath) image
   
    newState <- if state.props.endRideOdometerReadingModal then do
                  void $ pure $ setValueToLocalStore RIDE_END_ODOMETER image
                  pure state{props{endRideOdometerImage =image, odometerImageUploading = false}} 
                else do 
                  void $ pure $ setValueToLocalStore RIDE_START_ODOMETER image 
                  pure state{props{startRideOdometerImage =image,currentStage= ST.RideStarted,odometerImageUploading = false}}
    
    continueWithCmd newState [do
        void $  runEffectFn3 uploadMultiPartData imagePath (EP.uploadOdometerImage "") "Image"
        pure NoAction
      ]

eval (UploadMultiPartDataCallback fileType fileId) state = do
  let newState = state{props{odometerFileId = Just fileId}}
      exitAction = if state.props.endRideOdometerReadingModal then
          EndRide newState
        else do
          StartRide newState
  updateAndExit newState exitAction

eval (CallBackNewStop stopStatus) state = do
  _ <- pure $ printLog "new Stop Status " stopStatus
  -- let action = continueWithCmd state [ do
  --       void $ launchAff $ flowRunner defaultGlobalState $ do
  --         push <- liftFlow $ getPushFn Nothing "HomeScreen"
  --         activeRideResponse <- Remote.getRideHistoryReq "1" "0" "true" "null" "null"
  --         case activeRideResponse of
  --           Right (GetRidesHistoryResp rideList) -> do
  --             case (rideList.list Array.!! 0) of
  --               Just (RidesInfo {nextStopLocation,lastStopLocation}) -> do
  --                 currentLocation <- doAff do liftEffect JB.getCurrentLatLong 
  --                 liftFlow $ push $ NewStopAdded currentLocation nextStopLocation lastStopLocation
  --               Nothing -> pure unit
  --           Left err -> pure unit
  --         pure unit
  --       pure NoAction]
  case stopStatus of
    "ADD_STOP"  -> exit $ GoToNewStop state
    "EDIT_STOP" -> exit $ GoToNewStop state
    _ -> continue state
  

eval (NewStopAdded currentLocationLatLong nextStopLocation lastStopLocation) state = do
  _ <- pure $ printLog "NewStopAdded Action " nextStopLocation
  -- let newState = state { data 
  --   { activeRide 
  --     { nextStopAddress = (\(StopLocation {address, lat, lon}) -> decodeAddress (getLocationInfoFromStopLocation address lat lon) true) <$> nextStopLocation ,
  --       src_lat = (currentLocationLatLong.lat),
  --       src_lon = (currentLocationLatLong.lng), 
  --       nextStopLat = (\loc -> loc ^. _lat) <$> nextStopLocation, 
  --       nextStopLon = (\loc -> loc ^. _lon) <$> nextStopLocation,
  --       lastStopAddress = (\(StopLocation {address, lat, lon}) -> decodeAddress (getLocationInfoFromStopLocation address lat lon) true) <$> lastStopLocation
  --     }
  --   }, 
  --   props
  --     {showDottedRoute = false, 
  --     arrivedAtStop = isNothing nextStopLocation,
  --     routeVisible = false,
  --     mapRendered = true
  --     }}
  -- updateAndExit newState $ Refresh newState {props{showNewStopPopup = false}}
  exit $ GoToNewStop state
  
eval (BottomNavBarAction (BottomNavBar.OnNavigate item)) state = do
  case item of
    "Rides" -> exit $ GoToRidesScreen state
    "Profile" -> exit $ GoToProfileScreen state
    "Earnings" ->  exit $ EarningsScreen state false
    "Alert" -> do
      _ <- pure $ setValueToLocalNativeStore ALERT_RECEIVED "false"
      let _ = unsafePerformEffect $ logEvent state.data.logField "ny_driver_alert_click"
      exit $ GoToNotifications state
    "Rankings" -> do
      void $ pure $ incrementValueOfLocalStoreKey TIMES_OPENED_NEW_BENEFITS
      _ <- pure $ setValueToLocalNativeStore REFERRAL_ACTIVATED "false"
      exit $ GoToReferralScreen
    "Join" -> do
      let driverSubscribed = getValueToLocalNativeStore DRIVER_SUBSCRIBED == "true"
      void $ pure $ incrementValueOfLocalStoreKey TIMES_OPENED_NEW_SUBSCRIPTION
      _ <- pure $ cleverTapCustomEvent if driverSubscribed then "ny_driver_myplan_option_clicked" else "ny_driver_plan_option_clicked"
      _ <- pure $ metaLogEvent if driverSubscribed then "ny_driver_myplan_option_clicked" else "ny_driver_plan_option_clicked"
      let _ = unsafePerformEffect $ firebaseLogEvent if driverSubscribed then "ny_driver_myplan_option_clicked" else "ny_driver_plan_option_clicked"
      exit $ SubscriptionScreen state
    _ -> continue state

eval (OfferPopupAC PopUpModal.OnButton1Click) state = do
  _ <- pure $ setValueToLocalNativeStore SHOW_JOIN_NAMMAYATRI "__failed"
  _ <- pure $ cleverTapCustomEvent "ny_driver_in_app_popup_join_now"
  _ <- pure $ metaLogEvent "ny_driver_in_app_popup_join_now"
  let _ = unsafePerformEffect $ firebaseLogEvent "ny_driver_in_app_popup_join_now"
  exit $ SubscriptionScreen state {props { showOffer = false }}

eval (OfferPopupAC PopUpModal.OptionWithHtmlClick) state = do
  _ <- pure $ setValueToLocalNativeStore SHOW_JOIN_NAMMAYATRI "__failed"
  continue state {props { showOffer = false }}

eval (PaymentPendingPopupAC PopUpModal.OnButton1Click) state = do
  if state.props.subscriptionPopupType == ST.GO_ONLINE_BLOCKER then do
    _ <- pure $ cleverTapCustomEvent "ny_driver_due_payment_settle_now"
    _ <- pure $ metaLogEvent "ny_driver_due_payment_settle_now"
    let _ = unsafePerformEffect $ firebaseLogEvent "ny_driver_due_payment_settle_now"
    exit $ ClearPendingDues state {props{subscriptionPopupType = ST.GO_ONLINE_BLOCKER}}
  else do
    if state.props.subscriptionPopupType == ST.SOFT_NUDGE_POPUP then do
      _ <- pure $ cleverTapCustomEvent "ny_driver_payment_pending_soft_nudge_plan"
      _ <- pure $ metaLogEvent "ny_driver_payment_pending_soft_nudge_plan"
      let _ = unsafePerformEffect $ firebaseLogEvent "ny_driver_payment_pending_soft_nudge_plan"
      pure unit
    else if state.props.subscriptionPopupType == ST.LOW_DUES_CLEAR_POPUP then do
        _ <- pure $ setValueToLocalStore APP_SESSION_TRACK_COUNT "shown"
        pure unit
    else pure unit
    exit $ SubscriptionScreen state

eval (PaymentPendingPopupAC PopUpModal.OptionWithHtmlClick) state = do
  if state.props.subscriptionPopupType == ST.GO_ONLINE_BLOCKER then do
    _ <- pure $ cleverTapCustomEvent "ny_driver_due_payment_view_details"
    _ <- pure $ metaLogEvent "ny_driver_due_payment_view_details"
    let _ = unsafePerformEffect $ firebaseLogEvent "ny_driver_due_payment_view_details"
    exit $ SubscriptionScreen state{props{ subscriptionPopupType = ST.NO_SUBSCRIPTION_POPUP}}
  else do
    _ <- pure $ cleverTapCustomEvent "ny_driver_payment_pending_soft_nudge_plan_go_online"
    _ <- pure $ metaLogEvent "ny_driver_payment_pending_soft_nudge_plan_go_online"
    let _ = unsafePerformEffect $ firebaseLogEvent "ny_driver_payment_pending_soft_nudge_plan_go_online"
    exit (DriverAvailabilityStatus state{props{ subscriptionPopupType = ST.NO_SUBSCRIPTION_POPUP}} ST.Online)

eval (PaymentPendingPopupAC PopUpModal.OnSecondaryTextClick) state = do
  continueWithCmd state [do
    _ <- openUrlInApp $ HU.splitBasedOnLanguage state.data.config.subscriptionConfig.overlayYoutubeLink
    pure NoAction
  ]
  
eval (PaymentPendingPopupAC PopUpModal.DismissPopup) state = do
  if (state.props.subscriptionPopupType == ST.SOFT_NUDGE_POPUP) then do
    _ <- pure $ cleverTapCustomEvent "ny_driver_payment_pending_soft_nudge_plan_dismiss"
    _ <- pure $ metaLogEvent "ny_driver_payment_pending_soft_nudge_plan_dismiss"
    let _ = unsafePerformEffect $ firebaseLogEvent "ny_driver_payment_pending_soft_nudge_plan_dismiss"
    pure unit
  else if (state.props.subscriptionPopupType == ST.LOW_DUES_CLEAR_POPUP) then do
    _ <- pure $ setValueToLocalStore APP_SESSION_TRACK_COUNT "shown"
    pure unit
  else pure unit
  continue state {props{ subscriptionPopupType = ST.NO_SUBSCRIPTION_POPUP}}

eval (FreeTrialEndingAC PopUpModal.OnButton1Click) state = do
  exit $ SubscriptionScreen state

eval (FreeTrialEndingAC PopUpModal.OptionWithHtmlClick) state = do
  _ <- pure $ setValueToLocalStore APP_SESSION_TRACK_COUNT "shown"
  continue state {props{ subscriptionPopupType = ST.NO_SUBSCRIPTION_POPUP}}

eval (InAppKeyboardModalAction (InAppKeyboardModal.OnSelection key index)) state = do
  let
    rideOtp = if (index + 1) > (length state.props.rideOtp) then ( take 4 (state.props.rideOtp <> key)) else (take index (state.props.rideOtp)) <> key <> (take 4 (drop (index+1) state.props.rideOtp))
    focusIndex = length rideOtp
    newState = state { props = state.props { rideOtp = rideOtp, enterOtpFocusIndex = focusIndex ,otpIncorrect = false} }
    exitAction = if state.props.endRideOtpModal then EndRide newState else if state.props.zoneRideBooking then StartZoneRide newState else StartRide newState
  if ((length rideOtp) >= 4  && (not state.props.otpAttemptsExceeded)) then 
      if state.data.activeRide.tripType == ST.Rental then 
        continue $ rentalNewState state
      else updateAndExit newState exitAction
  else continue newState
  where
    rentalNewState newState = (newState { props = newState.props {enterOdometerReadingModal =  newState.props.enterOtpModal, endRideOdometerReadingModal = newState.props.endRideOtpModal, enterOtpModal = false, endRideOtpModal = false }})  

eval (InAppKeyboardModalAction (InAppKeyboardModal.OnClickBack text)) state = do
  let
    rideOtp = (if length( text ) > 0 then (take (length ( text ) - 1 ) text) else "" )
    focusIndex = length rideOtp
  continue state { props = state.props { rideOtp = rideOtp, enterOtpFocusIndex = focusIndex, otpIncorrect = false } }
eval (InAppKeyboardModalAction (InAppKeyboardModal.OnclickTextBox index)) state = do
  let focusIndex = if index > (length state.props.rideOtp) then (length state.props.rideOtp) else index
  let rideOtp = take index state.props.rideOtp
  continue state { props = state.props { enterOtpFocusIndex = focusIndex, rideOtp = rideOtp, otpIncorrect = false } }
eval (InAppKeyboardModalAction (InAppKeyboardModal.BackPressed)) state = do
  void $ pure $ hideKeyboardOnNavigation true
  continue state { props = state.props { rideOtp = "", enterOtpFocusIndex = 0, enterOtpModal = false, endRideOtpModal= false} }
eval (InAppKeyboardModalAction (InAppKeyboardModal.OnClickDone otp)) state = do
    if state.data.activeRide.tripType == ST.Rental then do
      void $ pure $ hideKeyboardOnNavigation true
      continue state { data = state.data {
        odometerReading {digit0 = "", digit1 = "", digit2 = "", digit3 = "", digit4 = ""}
        }, 
        props = state.props { 
          rideOtp = otp,
          enterOtpModal = false,
          endRideOtpModal = false,
          otpIncorrect = false,
          enterOdometerReadingModal =  state.props.enterOtpModal, 
          endRideOdometerReadingModal = state.props.endRideOtpModal, 
          enterOdometerFocusIndex = 0,
          endRideOdometerReadingValidationFailed = false
          }
        }
    else 
      let newState = state { props { rideOtp = otp ,
          enterOdometerReadingModal = state.props.enterOtpModal, 
          enterOtpModal = false, 
          endRideOtpModal = false, 
          endRideOdometerReadingModal = state.props.endRideOtpModal
          }
        }
      in if state.props.currentStage == ST.RideStarted then
        updateAndExit newState $ EndRide newState
      else 
        if state.props.zoneRideBooking then
          updateAndExit newState $ StartZoneRide newState 
        else
          updateAndExit newState $ StartRide newState

eval (InAppKeyboardModalOdometerAction (InAppKeyboardModal.BackPressed)) state = do
  continue $ state { props = state.props { enterOtpModal = state.props.enterOdometerReadingModal, enterOdometerReadingModal = false,endRideOtpModal = state.props.endRideOdometerReadingModal,endRideOdometerReadingModal = false} }

eval (InAppKeyboardModalOdometerAction (InAppKeyboardModal.OnclickTextBox index)) state = do
  continue state { props = state.props { enterOdometerFocusIndex = index } }

eval (InAppKeyboardModalOdometerAction (InAppKeyboardModal.OnClickBack text)) state = do
  let odometerReading  = if length text > 0 then (take (length ( text ) - 1 ) text) else "" 
  _ <- pure $ printLog "InAppKeyboardModal.OnClickBack" odometerReading
  continue state { props = state.props { odometerValue = odometerReading } }

eval (InAppKeyboardModalOdometerAction (InAppKeyboardModal.OnClickDone odometerReading)) state = do
  let keyboardId = "OdometerKeyboard" <> show state.props.enterOdometerFocusIndex
  if length odometerReading < 4 then do
    continue state
  else do
    let newState = state{ props { odometerFileId = Nothing, enterOtpModal = false, endRideOtpModal = false, odometerValue = odometerReading, endRideOdometerReadingValidationFailed = false } }
    if (state.props.currentStage == ST.RideStarted)
      then do
        let startOdometerLength = length $ show state.data.activeRide.startOdometerReading
        let startOdometerValue = maybe "0000.0" show state.data.activeRide.startOdometerReading
        let startOdometerReading = if startOdometerLength < 4 then (if startOdometerLength == 0 then "0000" else if startOdometerLength == 1 then "000" else if startOdometerLength == 2 then "00" else if startOdometerLength == 3 then "0" else "") <> (startOdometerValue) else startOdometerValue
        let endOdometerReading = if (take 1 startOdometerReading == "9") && (take 1 (odometerReading) == "0") then
            "1" <> odometerReading
          else odometerReading
        if endOdometerReadingIsMoreThanStart endOdometerReading startOdometerReading && endOdometerReadingIsNotMoreThan500 endOdometerReading startOdometerReading then
          updateAndExit newState { props { odometerValue = endOdometerReading }} $ EndRide newState { props { odometerValue = endOdometerReading }}
        else do
          continue state { props { endRideOdometerReadingValidationFailed = true}}
      else do
        updateAndExit newState $ StartRide newState
  where
    endOdometerReadingIsNotMoreThan500 endOdometerReading startOdometerReading = (fromMaybe 0.0 $ Number.fromString endOdometerReading) - (fromMaybe 0.0 $ Number.fromString startOdometerReading) <= 500.0
    endOdometerReadingIsMoreThanStart endOdometerReading startOdometerReading = (fromMaybe 0.0 $ Number.fromString startOdometerReading) <= (fromMaybe 0.0 $ Number.fromString endOdometerReading)
    makeOdometerReading odometer = odometer.digit0 <> odometer.digit1 <> odometer.digit2 <> odometer.digit3 <> "." <> odometer.digit4
    isAnyOdometerDigitEmpty odometer = Array.any (_ == "") [odometer.digit0, odometer.digit1, odometer.digit2, odometer.digit3, odometer.digit4]
  
eval (InAppKeyboardModalOdometerAction (InAppKeyboardModal.OnSelection key index)) state = do 
  let odometerValue = if length state.props.odometerValue < 4 then state.props.odometerValue <> key else state.props.odometerValue
  continue state { props = state.props { odometerValue = odometerValue} }

eval (RideActionModalAction (RideActionModal.NoAction)) state = continue state {data{triggerPatchCounter = state.data.triggerPatchCounter + 1,peekHeight = getPeekHeight state}}
eval (RideActionModalAction (RideActionModal.StartRide)) state = do
  continue state { props = state.props { enterOtpModal = true, rideOtp = "", enterOtpFocusIndex = 0, enterOdometerFocusIndex = 0, otpIncorrect = false, zoneRideBooking = false, arrivedAtStop = isNothing state.data.activeRide.nextStopAddress } }
eval (RideActionModalAction (RideActionModal.EndRide)) state = do
  if state.data.activeRide.tripType == ST.Rental then continue state{props{endRideOtpModal = true,enterOtpFocusIndex = 0, enterOdometerFocusIndex = 0, otpIncorrect = false, rideOtp="",odometerValue=""}, data{route = []}}
  else if state.data.activeRide.tripType == ST.Intercity then continue state { props{ endRideOtpModal = true, enterOtpFocusIndex = 0, otpIncorrect = false, rideOtp = "" } }
  else continue $ (state {props {endRidePopUp = true}, data {route = []}})

eval (RideActionModalAction (RideActionModal.ArrivedAtStop)) state = do
  _ <- pure $ printLog "RideActionModalAction" "arrived at stop"
  exit $ ArrivedAtStop state

eval (RideActionModalAction (RideActionModal.OnNavigate)) state = do
  _ <- pure $ setValueToLocalStore TRIGGER_MAPS "false"
  let lat = if (state.props.currentStage == ST.RideAccepted || state.props.currentStage == ST.ChatWithCustomer) then state.data.activeRide.src_lat else if state.data.activeRide.tripType == ST.Rental then fromMaybe state.data.activeRide.src_lat state.data.activeRide.nextStopLat else state.data.activeRide.dest_lat
      lon = if (state.props.currentStage == ST.RideAccepted || state.props.currentStage == ST.ChatWithCustomer) then state.data.activeRide.src_lon else if state.data.activeRide.tripType == ST.Rental then fromMaybe state.data.activeRide.src_lon state.data.activeRide.nextStopLon else state.data.activeRide.dest_lon
  void $ pure $ openNavigation 0.0 0.0 lat lon "DRIVE"
  continue state
eval (RideActionModalAction (RideActionModal.CancelRide)) state = do
  continue state{ data {cancelRideConfirmationPopUp{delayInSeconds = 5,  continueEnabled=false}}, props{cancelConfirmationPopup = true}}
eval (RideActionModalAction (RideActionModal.CallCustomer)) state = do
  let exophoneNumber = if (take 1 state.data.activeRide.exoPhone) == "0" then state.data.activeRide.exoPhone else "0" <> state.data.activeRide.exoPhone
  updateWithCmdAndExit state [ do
    void $ pure $ showDialer exophoneNumber false -- TODO: FIX_DIALER
    _ <- logEventWithTwoParams state.data.logField "call_customer" "trip_id" (state.data.activeRide.id) "user_id" (getValueToLocalStore DRIVER_ID)
    pure NoAction
    ] $ CallCustomer state exophoneNumber

eval (RideActionModalAction (RideActionModal.SecondaryTextClick)) state = continue state{props{showAccessbilityPopup = true, safetyAudioAutoPlay = false}}
eval (RideActionModalAction (RideActionModal.RideStartTimer seconds status id)) state = do
  void $ pure $ printLog "RideActionModal rideStartTimer " seconds
  if (getValueToLocalStore RIDE_START_TIMER_ID) == id then do
    if status == "EXPIRED" then do
      continue state { props {rideStartTimer = 0}}
    else continue state { props {rideStartTimer = seconds}}
  else 
    continue state


eval (MakePaymentModalAC (MakePaymentModal.PrimaryButtonActionController PrimaryButtonController.OnClick)) state = updateAndExit state $ OpenPaymentPage state

eval (MakePaymentModalAC (MakePaymentModal.Cancel)) state = continue state{data { paymentState {makePaymentModal = false}}}

eval (MakePaymentModalAC (MakePaymentModal.Info)) state = continue state{data { paymentState {showRateCard = true}}}

eval (RateCardAC (RateCard.PrimaryButtonAC PrimaryButtonController.OnClick)) state = continue state{data { paymentState {showRateCard = false}}}

------------------------------- ChatService - Start --------------------------

eval (OpenChatScreen) state = do
  if not state.props.chatcallbackInitiated || state.data.activeRide.disabilityTag == Just ST.BLIND_AND_LOW_VISION then continue state else do
    continueWithCmd state{props{openChatScreen = false}} [do
      pure $ (RideActionModalAction (RideActionModal.MessageCustomer))
    ]

eval (RideActionModalAction (RideActionModal.MessageCustomer)) state = do
  if not state.props.chatcallbackInitiated then continue state else do
    _ <- pure $ setValueToLocalStore PREVIOUS_LOCAL_STAGE (show state.props.currentStage)
    _ <- pure $ setValueToLocalStore LOCAL_STAGE (show ST.ChatWithCustomer)
    _ <- pure $ setValueToLocalNativeStore READ_MESSAGES (show (Array.length state.data.messages))
    continueWithCmd state{props{currentStage = ST.ChatWithCustomer, sendMessageActive = false, unReadMessages = false}} [do
      pure $ (RideActionModalAction (RideActionModal.LoadMessages))
    ]

eval GetMessages state = do
  continueWithCmd state [do
    pure $ (RideActionModalAction (RideActionModal.LoadMessages))
  ]

eval (RideActionModalAction (RideActionModal.VisuallyImpairedCustomer)) state = continue state{props{showChatBlockerPopUp = true}}

eval (UpdateInChat) state = continue state {props{updatedArrivalInChat = true}}

eval (InitializeChat ) state = continue state {props{chatcallbackInitiated = true}}

eval RemoveChat state = do
  continueWithCmd state {props{chatcallbackInitiated = false}} [ do
    _ <- stopChatListenerService
    _ <- pure $ setValueToLocalNativeStore READ_MESSAGES "0"
    pure $ NoAction
  ]

eval (UpdateMessages message sender timeStamp size) state = do
  if not state.props.chatcallbackInitiated then continue state {props {canSendSuggestion = true}} else do
    continueWithCmd state{data{messagesSize = size}, props {canSendSuggestion = true}} [do
      pure $ (RideActionModalAction (RideActionModal.LoadMessages))
    ]
    
eval (RideActionModalAction (RideActionModal.LoadMessages)) state = do
  let allMessages = getChatMessages Common.FunctionCall
  case (Array.last allMessages) of
      Just value -> if value.message == "" then continue state {data { messagesSize = show (fromMaybe 0 (fromString state.data.messagesSize) + 1)}, props {canSendSuggestion = true}} else
                      if value.sentBy == "Driver" then updateMessagesWithCmd state {data {messages = allMessages, chatSuggestionsList = []}, props {canSendSuggestion = true}}
                      else do
                        let readMessages = fromMaybe 0 (fromString (getValueToLocalNativeStore READ_MESSAGES))
                        let unReadMessages = (if (readMessages == 0 && state.props.currentStage /= ST.ChatWithCustomer) then true else (if (readMessages < (Array.length allMessages) && state.props.currentStage /= ST.ChatWithCustomer) then true else false))
                        let suggestions = getDriverSuggestions state $ getSuggestionsfromKey chatSuggestion value.message
                        updateMessagesWithCmd state {data {messages = allMessages, chatSuggestionsList = suggestions }, props {unReadMessages = unReadMessages, canSendSuggestion = true}}
      Nothing -> continue state {props {canSendSuggestion = true}}

eval ScrollToBottom state = do
  _ <- pure $ scrollToEnd (getNewIDWithTag "ChatScrollView") true
  continue state

eval (ChatViewActionController (ChatView.TextChanged value)) state = continue state{data{messageToBeSent = (trim value)},props{sendMessageActive = (length (trim value)) >= 1}}

eval(ChatViewActionController (ChatView.Call)) state = continueWithCmd state [ do
  _ <- pure $ showDialer (if (take 1 state.data.activeRide.exoPhone) == "0" then state.data.activeRide.exoPhone else "0" <> state.data.activeRide.exoPhone) false -- TODO: FIX_DIALER
  _ <- logEventWithTwoParams state.data.logField "call_customer" "trip_id" state.data.activeRide.id "user_id" (getValueToLocalStore DRIVER_ID)
  pure NoAction
  ]

eval (ChatViewActionController (ChatView.SendMessage)) state = do
  if state.data.messageToBeSent /= ""
  then
   continueWithCmd state{data{messageToBeSent = ""},props {sendMessageActive = false}} [do
      _ <- pure $ sendMessage state.data.messageToBeSent
      _ <- pure $ setText (getNewIDWithTag "ChatInputEditText") ""
      pure NoAction
   ]
  else
    continue state

eval (ChatViewActionController (ChatView.SendSuggestion suggestions)) state = do
  if state.props.canSendSuggestion then do
    let message = if isPreviousVersion (getValueToLocalStore VERSION_NAME) (getPreviousVersion (getMerchant Common.FunctionCall)) then (getMessageFromKey chatSuggestion suggestions "EN_US") else suggestions
    _ <- pure $ sendMessage message
    let _ = unsafePerformEffect $ logEvent state.data.logField $ toLower $ (replaceAll (Pattern "'") (Replacement "") (replaceAll (Pattern ",") (Replacement "") (replaceAll (Pattern " ") (Replacement "_") suggestions)))
    continue state{data {chatSuggestionsList = []}, props {canSendSuggestion = false}}
  else continue state

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

------------------------------- ChatService - End --------------------------

eval (RideActionModalAction (RideActionModal.LocationTracking)) state = do
  let newState = state {props {showDottedRoute = not state.props.showDottedRoute} }
  updateAndExit newState $ UpdateRoute newState

eval (RideActionModalAction (RideActionModal.WaitingInfo)) state = do
  continue state {data{activeRide {waitTimeInfo = true }}}

eval (RideActionModalAction (RideActionModal.TimerCallback timerID timeInMinutes seconds)) state = continueWithCmd state [do pure $ (WaitTimerCallback timerID timeInMinutes seconds)]

eval (UpdateWaitTime status) state = do
  void $ pure $ setValueToLocalNativeStore WAITING_TIME_STATUS (show status)
  continue state { props { waitTimeStatus = status}, data {activeRide {notifiedCustomer = status /= ST.NoStatus}}}

eval (WaitTimerCallback timerID _ seconds) state = 
  continue state { data {activeRide {waitTimerId = timerID, waitTimeSeconds = seconds}}}

eval (PopUpModalAction (PopUpModal.OnButton1Click)) state = continue $ (state {props {endRidePopUp = false}})
eval (PopUpModalAction (PopUpModal.OnButton2Click)) state = do
  _ <- pure $ removeAllPolylines ""
  updateAndExit state {props {endRidePopUp = false, rideActionModal = false}} $ EndRide state {props {endRidePopUp = false, rideActionModal = false, zoneRideBooking = true}}

eval (CancelRideModalAction (SelectListModal.UpdateIndex indexValue)) state = continue state { data = state.data { cancelRideModal  { activeIndex = Just indexValue, selectedReasonCode = (fromMaybe dummyCancelReason $ state.data.cancelRideModal.selectionOptions Array.!!indexValue).reasonCode } } }
eval (CancelRideModalAction (SelectListModal.TextChanged  valId newVal)) state = continue state { data {cancelRideModal { selectedReasonDescription = newVal, selectedReasonCode = "OTHER"}}}
eval (CancelRideModalAction (SelectListModal.Button1 PrimaryButtonController.OnClick)) state = do
  pure $ hideKeyboardOnNavigation true
  continue state { data{cancelRideModal {activeIndex = Nothing, selectedReasonCode = "", selectedReasonDescription = ""}} ,props {cancelRideModalShow = false, cancelConfirmationPopup=false } }
eval (CancelRideModalAction (SelectListModal.OnGoBack)) state = continue state { data { cancelRideModal {activeIndex = Nothing, selectedReasonCode = "", selectedReasonDescription = ""}} ,props{ cancelRideModalShow = false, cancelConfirmationPopup = false}}
eval (CancelRideModalAction (SelectListModal.ClearOptions)) state = continue state {data {cancelRideModal {activeIndex = Nothing, selectedReasonCode = "", selectedReasonDescription = ""}}}
eval (CancelRideModalAction (SelectListModal.Button2 PrimaryButtonController.OnClick)) state = do
    pure $ hideKeyboardOnNavigation true
    let cancelReasonSelected = case state.data.cancelRideModal.activeIndex of
                                  Just index -> (state.data.cancelRideModal.selectionOptions Array.!! (index))
                                  Nothing    -> Nothing
    _ <- pure $ printLog "cancelReasonSelected" cancelReasonSelected
    case cancelReasonSelected of
      Just reason -> do
        _ <- pure $ printLog "inside Just" reason.reasonCode
        if (reason.reasonCode == "OTHER") then exit $ SelectListModal state { props = state.props { cancelRideModalShow = false , cancelConfirmationPopup = false, zoneRideBooking = true} } else do
          let newState = state { data = state.data {cancelRideModal = state.data.cancelRideModal { selectedReasonCode = reason.reasonCode , selectedReasonDescription = reason.description  } }, props = state.props { cancelRideModalShow = false, otpAttemptsExceeded = false, cancelConfirmationPopup = false, zoneRideBooking = true } }
          exit $ SelectListModal newState
      Nothing -> do
        _ <- pure $ printLog "inside Nothing" "."
        continue state


eval (PopUpModalCancelConfirmationAction (PopUpModal.OnButton2Click)) state = do
  _ <- pure $ TF.clearTimerWithId state.data.cancelRideConfirmationPopUp.timerID
  continue state {props{cancelConfirmationPopup = false}, data{cancelRideConfirmationPopUp{timerID = "" , continueEnabled=false, enableTimer=false}}}

eval (PopUpModalCancelConfirmationAction (PopUpModal.OnButton1Click)) state = continue state {props {cancelRideModalShow = true, cancelConfirmationPopup = false},data {cancelRideConfirmationPopUp{enableTimer = false}, cancelRideModal {activeIndex=Nothing, selectedReasonCode="", selectionOptions = cancellationReasons "" }}}

eval (PopUpModalCancelConfirmationAction (PopUpModal.CountDown seconds status timerID)) state = do
  if status == "EXPIRED" then do
    _ <- pure $ TF.clearTimerWithId timerID
    continue state { data { cancelRideConfirmationPopUp{delayInSeconds = 0, timerID = "", continueEnabled = true}}}
    else continue state { data {cancelRideConfirmationPopUp{delayInSeconds = seconds, timerID = timerID, continueEnabled = false}}}

eval (CancelRideModalAction SelectListModal.NoAction) state = do
  _ <- pure $ printLog "CancelRideModalAction NoAction" state.data.cancelRideModal.selectionOptions
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

eval (IsMockLocation isMock) state = do
  let val = false --isMock == "true" -- TODO:: Handle it properly @ashkiriti
      _ = unsafePerformEffect $ if val then  logEvent (state.data.logField) "ny_fakeGPS_enabled" else pure unit -- we are using unsafePerformEffect becasue without it we are not getting logs in firebase, since we are passing a parameter from state i.e. logField then the output will be inline and it will not be able to precompute so it's safe to use it here.
  continue state{props{isMockLocation = val}}

eval RetryTimeUpdate state = do
  _ <-  pure $ setValueToLocalNativeStore REGISTERATION_TOKEN (getValueToLocalStore REGISTERATION_TOKEN)
  (updateAndExit state { data = state.data { locationLastUpdatedTime = "" }, props = state.props {refreshAnimation = true}} $ Refresh state { data = state.data { locationLastUpdatedTime = "" }, props = state.props {refreshAnimation = true}})

eval (TimeUpdate time lat lng) state = do
  let driverLat = getLastKnownLocValue ST.LATITUDE lat
      driverLong = getLastKnownLocValue ST.LONGITUDE lng
      newState = state { data = state.data { currentDriverLat= driverLat,  currentDriverLon = driverLong, locationLastUpdatedTime = (convertUTCtoISC time "hh:mm a") }}
  void $ pure $ setValueToLocalStore IS_DRIVER_AT_PICKUP (show newState.data.activeRide.notifiedCustomer)
  void $ pure $ setValueToLocalStore LOCATION_UPDATE_TIME (convertUTCtoISC time "hh:mm a")
  continueWithCmd newState [ do
    void $ if (getValueToLocalNativeStore IS_RIDE_ACTIVE == "false") then checkPermissionAndUpdateDriverMarker newState else pure unit
    case state.data.config.waitTimeConfig.enableWaitTime, state.props.currentStage, state.data.activeRide.notifiedCustomer of
      true, ST.RideAccepted, false -> do
        let dist = getDistanceBwCordinates driverLat driverLong state.data.activeRide.src_lat state.data.activeRide.src_lon
            insideThreshold = dist <= state.data.config.waitTimeConfig.thresholdDist
        pure $ if insideThreshold then UpdateAndNotify else (UpdateLastLoc driverLat driverLong insideThreshold)
      _, _, _ -> pure $ UpdateLastLoc driverLat driverLong false
    ]

eval (UpdateLastLoc lat lon val) state = continue state {data { prevLatLon = Just {lat : lat, lon : lon, place : "", driverInsideThreshold : false}}}
  
eval (UpdateAndNotify) state =
  continueWithCmd state
        [ do
            void $ launchAff $ EHC.flowRunner defaultGlobalState $ runExceptT $ runBackT
              $ do
                  push <- liftFlowBT $ getPushFn Nothing "HomeScreen"
                  GetRouteResp routeApiResponse <- Remote.getRouteBT (Remote.makeGetRouteReq state.data.currentDriverLat state.data.currentDriverLon state.data.activeRide.src_lat state.data.activeRide.src_lon) "pickup"
                  let shortRoute = (routeApiResponse Array.!! 0)
                  liftFlowBT $ push $ case shortRoute of
                      Just (Route route) -> do
                          if route.distance <= state.data.config.waitTimeConfig.routeDistance then NotifyAPI 
                          else do
                            let dist = getDistanceBwCordinates state.data.currentDriverLat state.data.currentDriverLon state.data.activeRide.src_lat state.data.activeRide.src_lon
                            if dist <= state.data.config.waitTimeConfig.straightLineDist then NotifyAPI else UpdateLastLoc state.data.currentDriverLat state.data.currentDriverLon false
                      _ -> NoAction
                  pure unit
            pure NoAction
        ]

eval NotifyAPI state = updateAndExit state $ NotifyDriverArrived state 

eval (RideActiveAction activeRide) state = do
  let currActiveRideDetails = activeRideDetail state activeRide
      updatedState = state { data {activeRide = currActiveRideDetails}, props{showAccessbilityPopup = (isJust currActiveRideDetails.disabilityTag), safetyAudioAutoPlay = false}}
  updateAndExit updatedState $ UpdateStage ST.RideAccepted updatedState

eval RecenterButtonAction state = continue state

eval (SwitchDriverStatus status) state =
  if not state.data.isVehicleSupported && status /= ST.Offline then continue state { props{ vehicleNSPopup = true }}
  else if state.data.paymentState.driverBlocked && not state.data.paymentState.subscribed then continue state { props{ subscriptionPopupType = ST.GO_ONLINE_BLOCKER }}
  else if state.data.paymentState.driverBlocked then continue state { data{paymentState{ showBlockingPopup = true}}}
  else if not state.props.rcActive then do
    void $ pure $ toast $ getString PLEASE_ADD_RC
    exit (DriverAvailabilityStatus state { props = state.props { goOfflineModal = false }} ST.Offline)
  else if ((getValueToLocalStore IS_DEMOMODE_ENABLED) == "true") then do
    continueWithCmd state [ do
          _ <- pure $ setValueToLocalStore IS_DEMOMODE_ENABLED "false"
          _ <- pure $ toast (getString DEMO_MODE_DISABLED)
          _ <- pure $  deleteValueFromLocalStore DEMO_MODE_PASSWORD
          _ <- getCurrentPosition (showDriverMarker state "ny_ic_auto") constructLatLong
          pure NoAction
          ]
  else if state.props.driverStatusSet == status then continue state
    else do
      let maxDue = state.data.paymentState.totalPendingManualDues >= state.data.subsRemoteConfig.max_dues_limit
          lowDue = state.data.paymentState.totalPendingManualDues >= state.data.subsRemoteConfig.max_dues_limit
          showPopup = state.data.config.subscriptionConfig.enableSubscriptionPopups && (maxDue || lowDue)
          popup = if maxDue then ST.GO_ONLINE_BLOCKER else ST.SOFT_NUDGE_POPUP
          checkIfLastWasSilent = state.props.driverStatusSet == ST.Silent
      case status of
        ST.Offline -> continue state { props { goOfflineModal = checkIfLastWasSilent, silentPopUpView = not checkIfLastWasSilent }}
        _ -> if showPopup then continue state { props{ subscriptionPopupType = popup }} else exit (DriverAvailabilityStatus state status)

eval (PopUpModalSilentAction (PopUpModal.OnButton1Click)) state = exit (DriverAvailabilityStatus state{props{silentPopUpView = false}} ST.Offline)
eval (PopUpModalSilentAction (PopUpModal.OnButton2Click)) state = exit (DriverAvailabilityStatus state{props{silentPopUpView = false}} ST.Silent)

eval GoToProfile state =  do
  _ <- pure $ setValueToLocalNativeStore PROFILE_DEMO "false"
  _ <- pure $ hideKeyboardOnNavigation true
  exit $ GoToProfileScreen state

eval ClickAddAlternateButton state = do
    if state.props.showlinkAadhaarPopup then
      exit $ AadhaarVerificationFlow state
    else do
      let curr_time = getCurrentUTC ""
      let last_attempt_time = getValueToLocalStore SET_ALTERNATE_TIME
      let time_diff = runFn2 differenceBetweenTwoUTC curr_time last_attempt_time
      if(time_diff <= 600 && time_diff > 0) then do
        pure $ toast $ getString TOO_MANY_ATTEMPTS_PLEASE_TRY_AGAIN_LATER
        continue state
      else do
        exit $ AddAlternateNumber state

eval LinkAadhaarAC state = exit $ AadhaarVerificationFlow state

eval ZoneOtpAction state = do
  continue state { props = state.props { enterOtpModal = true, rideOtp = "", enterOtpFocusIndex = 0, otpIncorrect = false,zoneRideBooking = true } }

eval HelpAndSupportScreen state = exit $ GoToHelpAndSupportScreen state

eval (BannerCarousal (BannerCarousel.OnClick index)) state =
  continueWithCmd state [do
    let banners = getBannerConfigs state
    case Array.index banners index of
      Just config -> do
        let _ = runFn2 EHC.updatePushInIdMap "bannerCarousel" false
        case config.type of
          BannerCarousel.Gender -> pure (GenderBannerModal (Banner.OnClick))
          BannerCarousel.Disability -> pure (AccessibilityBannerAction (Banner.OnClick))
          BannerCarousel.AutoPay -> pure (AutoPayBanner (Banner.OnClick))
          BannerCarousel.Remote link -> do
            void $ openUrlInApp link
            pure NoAction
          _ -> pure NoAction
      Nothing -> pure NoAction
  ] 

eval (GenderBannerModal (Banner.OnClick)) state = do
  _ <- pure $ firebaseLogEvent "ny_driver_gender_banner_click"
  exit $ GotoEditGenderScreen

eval (AutoPayBanner (Banner.OnClick)) state = do
  let ctEvent = case state.props.autoPayBanner of
                  ST.CLEAR_DUES_BANNER -> "clear_dues_banner_clicked"
                  ST.SETUP_AUTOPAY_BANNER -> "setup_autopay_banner_clicked"
                  ST.FREE_TRIAL_BANNER -> "setup_autopay_trial_ends_banner_clicked"
                  _ -> ""
  _ <- pure $ cleverTapCustomEvent ctEvent
  exit $ SubscriptionScreen state

eval (AccessibilityBannerAction (Banner.OnClick)) state = continue state{props{showGenericAccessibilityPopUp = true}}

eval (ToggleBonusPopup) state = continue state { data {activeRide {waitTimeInfo =false}}, props { showBonusInfo = not state.props.showBonusInfo } }

eval (StatsModelAction StatsModelController.OnIconClick) state = continueWithCmd state [ pure $ ToggleBonusPopup]

eval (RequestInfoCardAction RequestInfoCard.Close) state = continue state { data {activeRide {waitTimeInfo =false}}, props { showBonusInfo = false } }

eval (RequestInfoCardAction RequestInfoCard.BackPressed) state = continue state { data {activeRide {waitTimeInfo =false}}, props { showBonusInfo = false } }

eval (RequestInfoCardAction RequestInfoCard.NoAction) state = continue state

eval RemovePaymentBanner state = if state.data.paymentState.blockedDueToPayment then
                                                  continue state else continue state {data { paymentState {paymentStatusBanner = false}}}
eval (LinkAadhaarPopupAC PopUpModal.OnButton1Click) state = exit $ AadhaarVerificationFlow state

eval (LinkAadhaarPopupAC PopUpModal.DismissPopup) state = continue state {props{showAadharPopUp = false}}
eval (PopUpModalAccessibilityAction PopUpModal.OnButton1Click) state = continueWithCmd state{props{showAccessbilityPopup = false, safetyAudioAutoPlay = false}} [ do 
  _ <- pure $ pauseYoutubeVideo unit
  void $ runEffectFn1 removeMediaPlayer ""
  pure NoAction
  ] 

eval (GenericAccessibilityPopUpAction PopUpModal.OnButton1Click) state = continueWithCmd state{props{showAccessbilityPopup = false, safetyAudioAutoPlay = false}} [ do 
  _ <- pure $ pauseYoutubeVideo unit
  void $ runEffectFn1 removeMediaPlayer ""
  pure NoAction
  ] 

eval (PopUpModalChatBlockerAction PopUpModal.OnButton2Click) state = continueWithCmd state{props{showChatBlockerPopUp = false}} [do
      pure $ RideActionModalAction (RideActionModal.MessageCustomer)
  ]

eval (StartEarningPopupAC PopUpModal.OnButton1Click) state = exit $ SubscriptionScreen state { data{paymentState {showBlockingPopup = false}}}

eval (VehicleNotSupportedAC PopUpModal.OnButton1Click) state = continue state { props { vehicleNSPopup = false}}

eval (StartEarningPopupAC (PopUpModal.OptionWithHtmlClick)) state = do
  _ <- pure $ showDialer state.data.config.subscriptionConfig.supportNumber false
  continue state

eval (PopUpModalChatBlockerAction PopUpModal.OnButton1Click) state = continueWithCmd state{props{showChatBlockerPopUp = false}} [do
      pure $ PopUpModalChatBlockerAction PopUpModal.DismissPopup
    ]

eval (PopUpModalChatBlockerAction PopUpModal.DismissPopup) state = continue state{props{showChatBlockerPopUp = false}}

eval RemoveGenderBanner state = do
  _ <- pure $ setValueToLocalStore IS_BANNER_ACTIVE "False"
  continue state { props = state.props{showGenderBanner = false}}

eval (PaymentStatusAction status) state =
  case status of
    PP.CHARGED -> continue state { data { paymentState { paymentStatusBanner = false}}}
    _ -> continue state { data { paymentState {
                  paymentStatus = PP.Failed,
                  bannerBG = Color.pearl,
                  bannerTitle = getString YOUR_PREVIOUS_PAYMENT_IS_PENDING,
                  bannerTitleColor = Color.dustyRed,
                  banneActionText = getString CONTACT_SUPPORT,
                  bannerImage = "ny_ic_payment_failed_banner," }}}
  
eval (RideCompletedAC (RideCompletedCard.UpiQrRendered id)) state = do
  continueWithCmd state [ do
                    runEffectFn4 generateQR ("upi://pay?pa=" <> state.data.endRideData.payerVpa <> "&am=" <> (show $ state.data.endRideData.finalAmount)) id 200 0
                    pure $ NoAction
                ]

eval (RideCompletedAC (RideCompletedCard.Support)) state = continue state {props {showContactSupportPopUp = true}}
eval (RideCompletedAC (RideCompletedCard.ContactSupportPopUpAC PopUpModal.OnButton1Click)) state = continue state {props {showContactSupportPopUp = false}}
eval (RideCompletedAC (RideCompletedCard.ContactSupportPopUpAC PopUpModal.OnButton2Click)) state =  do
                                                                                                      pure $ unsafePerformEffect $ contactSupportNumber "" -- unsafePerformEffect is temporary fix. Need to update this.
                                                                                                      continue state
eval (RideCompletedAC (RideCompletedCard.ContactSupportPopUpAC PopUpModal.DismissPopup)) state = continue state {props {showContactSupportPopUp = false}}

eval (RideCompletedAC (RideCompletedCard.RideDetails)) state = exit $ GoToRideDetailsScreen state
eval (RideCompletedAC (RideCompletedCard.SkipButtonActionController (PrimaryButtonController.OnClick))) state = continue state {props {showRideRating = true}}
eval (RideCompletedAC (RideCompletedCard.BannerAction (Banner.OnClick))) state = continueWithCmd state [pure $ AutoPayBanner (Banner.OnClick) ]


eval (RatingCardAC (RatingCard.Rating selectedRating)) state = continue state {data {endRideData { rating = selectedRating}}}
eval (RatingCardAC (RatingCard.FeedbackChanged feedback)) state = continue state {data {endRideData {feedback = feedback}}}
eval (RatingCardAC (RatingCard.BackPressed)) state = continue state {props {showRideRating = false}}
eval (RatingCardAC (RatingCard.PrimaryButtonAC PrimaryButtonController.OnClick)) state = exit $ PostRideFeedback state {props {showRideRating = false, showRideCompleted = false}}

eval (RCDeactivatedAC PopUpModal.OnButton1Click) state = exit $ GoToVehicleDetailScreen state 

eval (RCDeactivatedAC PopUpModal.OnButton2Click) state = continue state {props {rcDeactivePopup = false}}

eval (CoinsPopupAC PopUpModal.OnButton1Click) state = do
  let _ = unsafePerformEffect $ logEvent state.data.logField "ny_driver_coins_intro_popup_check_now_click"
  exit $ EarningsScreen state true

eval (CoinsPopupAC PopUpModal.OptionWithHtmlClick) state = do
  void $ pure $ setValueToLocalNativeStore COINS_POPUP_SHOWN_DATE (getCurrentUTC "")
  continue state {props {showCoinsPopup = false}}

eval (AccessibilityBannerAction (Banner.OnClick)) state = continue state{props{showGenericAccessibilityPopUp = true}}

eval (PaymentBannerAC (Banner.OnClick)) state = do
  _ <- pure $ showDialer state.data.config.subscriptionConfig.supportNumber false
  continue state

eval (SetBannerItem bannerItem) state = continue state{data{bannerData{bannerItem = Just bannerItem}}}

eval ToggleStatsModel state = continue state { props { isStatsModelExpanded = not state.props.isStatsModelExpanded } }

eval (GoToEarningsScreen showCoinsView) state = do
  let _ = unsafePerformEffect $ logEventWithMultipleParams state.data.logField  "ny_driver_coins_click_on_homescreen" $ [{key : "CoinBalance", value : unsafeToForeign state.data.coinBalance}]
  exit $ EarningsScreen state showCoinsView 

eval _ state = continue state

checkPermissionAndUpdateDriverMarker :: ST.HomeScreenState -> Effect Unit
checkPermissionAndUpdateDriverMarker state = do
  conditionA <- isLocationPermissionEnabled unit
  conditionB <- isLocationEnabled unit
  if conditionA && conditionB then do
    _ <- pure $ printLog "update driver location" "."
    _ <- getCurrentPosition (showDriverMarker state "ic_vehicle_side") constructLatLong
    pure unit
    else do
      _ <- requestLocation unit
      pure unit

showDriverMarker :: ST.HomeScreenState -> String -> ST.Location -> Effect Unit
showDriverMarker state marker location = do
  case getValueToLocalStore DEMO_MODE_PASSWORD of
    "7891234" -> updateDemoLocationIcon 13.311895563147432 76.93981481869986
    "8917234" -> updateDemoLocationIcon 13.260559676317829 76.4785809882692
    "9178234" -> updateDemoLocationIcon 13.160550263780683 76.66727044721313
    "1789234" -> updateDemoLocationIcon 12.522069908884921 76.89518072273476
    "7891789" -> updateDemoLocationIcon 23.06194031948526 88.7637073215878
    "7891788" -> updateDemoLocationIcon 24.338294091147212 88.1949706368274
    "7891567" -> updateDemoLocationIcon 9.869715234892222 76.37632251438302
    "7891678" -> updateDemoLocationIcon 9.955097514840311 76.37173322025349
    _ -> do
      _ <- pure $ enableMyLocation true
      animateCamera location.lat location.lon zoomLevel "ZOOM"

updateDemoLocationIcon :: Number -> Number -> Effect Unit
updateDemoLocationIcon lat lng = do
  _ <- showMarker "ny_ic_demo_location" lat lng 100 0.5 0.5
  _ <- pure $ enableMyLocation true
  animateCamera lat lng zoomLevel "ZOOM"

constructLatLong :: String -> String -> ST.Location
constructLatLong lat lon =
  { lat: fromMaybe 0.0 (Number.fromString lat)
  , lon : fromMaybe 0.0 (Number.fromString lon)
  , place : ""
  , driverInsideThreshold : false
  }

activeRideDetail :: ST.HomeScreenState -> RidesInfo -> ST.ActiveRide
activeRideDetail state (RidesInfo ride) = 
  let waitTimeSeconds = DS.split (DS.Pattern "<$>") (getValueToLocalStore TOTAL_WAITED)
      waitTime = maybe 0 (fromMaybe 0 <<< Int.fromString) $ waitTimeSeconds Array.!! 1
      isTimerValid = (fromMaybe "" (waitTimeSeconds Array.!! 0)) == ride.id
      isSafetyRide = isSafetyPeriod state ride.createdAt
      tripType = rideTypeConstructor ride.tripCategory
  in 
  {
  id : ride.id,
  source : (decodeAddress ride.fromLocation true),
  destination : (\toLocation -> decodeAddress toLocation true) <$> ride.toLocation,
  src_lat :  ((ride.fromLocation) ^. _lat),
  src_lon :  ((ride.fromLocation) ^. _lon),
  dest_lat: maybe ((ride.fromLocation) ^. _lat) (\toLocation -> toLocation ^. _lat) ride.toLocation,
  dest_lon: maybe ((ride.fromLocation) ^. _lon) (\toLocation -> toLocation ^. _lon) ride.toLocation,
  actualRideDistance : fromMaybe 0.0 (Number.fromString (parseFloat (toNumber( fromMaybe 0 ride.chargeableDistance)) 2)),
  status : case ride.status of
              "NEW" -> NEW
              "INPROGRESS" -> INPROGRESS
              "COMPLETED" -> COMPLETED
              "CANCELLED" -> CANCELLED
              _ -> COMPLETED,
  distance : (toNumber ride.estimatedDistance),
  duration : state.data.activeRide.duration,
  tripDuration : ride.estimatedDuration,
  actualRideDuration : ride.actualDuration,
  riderName : fromMaybe "" ride.riderName,
  estimatedFare : ride.driverSelectedFare + ride.estimatedBaseFare,
  notifiedCustomer : getValueToLocalStore WAITING_TIME_STATUS == (show ST.PostTriggered),
  exoPhone : ride.exoPhone,
  waitTimeSeconds :if ride.status == "INPROGRESS" && isTimerValid then waitTime else -1,
  rideCreatedAt : ride.createdAt,
  waitTimeInfo : state.data.activeRide.waitTimeInfo,
  requestedVehicleVariant : ride.requestedVehicleVariant,
  waitTimerId : state.data.activeRide.waitTimerId,
  specialLocationTag : if isJust ride.disabilityTag then Just "Accessibility"
                        else if isJust ride.driverGoHomeRequestId then Just "GOTO" 
                        else if isJust ride.specialLocationTag then ride.specialLocationTag
                        else if isSafetyRide then Just "Safety"
                        else ride.specialLocationTag, --  "None_SureMetro_PriorityDrop",--"GOTO",
  disabilityTag : case ride.disabilityTag of
              Just "BLIND_LOW_VISION" -> Just ST.BLIND_AND_LOW_VISION
              Just "HEAR_IMPAIRMENT" -> Just ST.HEAR_IMPAIRMENT
              Just "LOCOMOTOR_DISABILITY" -> Just ST.LOCOMOTOR_DISABILITY
              Just "OTHER" -> Just ST.OTHER_DISABILITY
              Just _ -> Just ST.OTHER_DISABILITY
              Nothing -> if isSafetyRide && (isNothing ride.specialLocationTag) 
                          then Just ST.SAFETY 
                          else Nothing,
  enableFrequentLocationUpdates : fromMaybe false ride.enableFrequentLocationUpdates,
  tripScheduledAt: ride.tripScheduledAt,
  tripType: tripType,
  tripStartTime: ride.tripStartTime,
  tripEndTime: ride.tripEndTime,
  tripActualDistance: ride.chargeableDistance,
  nextStopAddress : getAddressFromStopLocation ride.nextStopLocation,
  lastStopAddress : case ride.lastStopLocation of
    Just (API.StopLocation {address,lat,lon}) -> if lat == ((ride.fromLocation) ^. _lat) && lon == ((ride.fromLocation) ^. _lon) then Nothing else getAddressFromStopLocation ride.lastStopLocation
    Nothing -> Nothing,
  nextStopLat : (\a -> ( a ^. _lat)) <$> ride.nextStopLocation,
  nextStopLon : (\a -> ( a ^. _lon)) <$> ride.nextStopLocation,
  lastStopLat : (\a -> ( a ^. _lat)) <$> ride.lastStopLocation,
  lastStopLon : (\a -> ( a ^. _lon)) <$> ride.lastStopLocation,
  startOdometerReading : (\(API.OdometerReading {value}) -> value) <$> ride.startOdometerReading,
  endOdometerReading : (\(API.OdometerReading {value}) -> value) <$> ride.endOdometerReading
}
  where 
    getAddressFromStopLocation :: Maybe API.StopLocation -> Maybe String
    getAddressFromStopLocation  stopLocation = (\(API.StopLocation {address,lat,lon}) -> decodeAddress (getLocationInfoFromStopLocation address lat lon) true) <$>  stopLocation

cancellationReasons :: String -> Array Common.OptionButtonList
cancellationReasons dummy = [
        {
          reasonCode: "VEHICLE_ISSUE"
        , description: (getString VEHICLE_ISSUE)
        , textBoxRequired : false
        , subtext: Nothing
        },
        {
          reasonCode: "PICKUP_TOO_FAR"
        , description: (getString PICKUP_TOO_FAR)
        , textBoxRequired : false
        , subtext: Nothing
        },
        {
          reasonCode: "CUSTOMER_NOT_PICKING_CALL"
        , description: (getString CUSTOMER_NOT_PICKING_CALL)
        , textBoxRequired : false
        , subtext: Nothing
        },
        {
          reasonCode: "TRAFFIC_JAM"
        , description: (getString TRAFFIC_JAM)
        , textBoxRequired : false
        , subtext: Nothing
        },
        {
          reasonCode: "CUSTOMER_WAS_RUDE"
        , description: (getString CUSTOMER_WAS_RUDE)
        , textBoxRequired : false
        , subtext: Nothing
        },
        {
          reasonCode: "OTHER"
        , description: (getString OTHER)
        , textBoxRequired : true
        , subtext: Nothing
        }
]

dummyCancelReason :: Common.OptionButtonList
dummyCancelReason =  {
        reasonCode : ""
        , description :""
        , textBoxRequired : false
        , subtext : Nothing
        }

checkNotificationType :: String -> ST.NotificationType -> Boolean
checkNotificationType currentNotification requiredNotification = (show requiredNotification) == currentNotification

type RideRequestPollingData = {
    duration :: Int ,
    delay :: Number
  }

getLastKnownLocValue :: ST.LocationType -> String -> Number
getLastKnownLocValue lType val =
  let lastKnownValue = fromMaybe 0.0 $ Number.fromString $ getValueToLocalNativeStore $ if lType == ST.LATITUDE then LAST_KNOWN_LAT else LAST_KNOWN_LON
      currentVal = if (fromMaybe 0.0 (Number.fromString val)) == 0.0 then Nothing else (Number.fromString val)
    in fromMaybe lastKnownValue currentVal

updateMessagesWithCmd :: ST.HomeScreenState -> Eval Action ScreenOutput ST.HomeScreenState
updateMessagesWithCmd state =
  continueWithCmd state [ do
    if(state.props.currentStage == ST.ChatWithCustomer) then do
      _ <- pure $ setValueToLocalNativeStore READ_MESSAGES (show (Array.length state.data.messages))
      pure unit
    else
      pure unit
    pure NoAction
    ]


getPeekHeight :: ST.HomeScreenState -> Int
getPeekHeight state = 
  let headerLayout = runFn1 JB.getLayoutBounds $ getNewIDWithTag "rideActionHeaderLayout"
      labelLayout =  runFn1 JB.getLayoutBounds $ getNewIDWithTag "rideActionLabelLayout"
      contentLayout = runFn1 JB.getLayoutBounds $ getNewIDWithTag "rideActionLayout"
      pixels = runFn1 HU.getPixels ""
      density = (runFn1 HU.getDeviceDefaultDensity "")/  defaultDensity
      currentPeekHeight = headerLayout.height  + contentLayout.height + (if RideActionModal.isSpecialRide (rideActionModalConfig state) then (labelLayout.height + 6) else 0)
      requiredPeekHeight = ceil (((toNumber currentPeekHeight) /pixels) * density)
    in if requiredPeekHeight == 0 then 470 else requiredPeekHeight
  
getDriverSuggestions :: ST.HomeScreenState -> Array String-> Array String
getDriverSuggestions state suggestions = case (Array.length suggestions == 0) of
                                  true -> if (state.data.activeRide.notifiedCustomer) then getSuggestionsfromKey chatSuggestion "driverDefaultAP" else getSuggestionsfromKey chatSuggestion "driverDefaultBP"
                                  false -> suggestions

getPreviousVersion :: Merchant -> String
getPreviousVersion merchant = 
  case merchant of
    NAMMAYATRI -> "1.4.8"
    YATRI -> "2.3.0"
    YATRISATHI -> "0.1.8"
    _ -> "100.100.100"


getBannerConfigs :: ST.HomeScreenState -> Array (BannerCarousel.Config (BannerCarousel.Action -> Action))
getBannerConfigs state = 
  (if state.props.autoPayBanner /= ST.NO_SUBSCRIPTION_BANNER 
    then [autpPayBannerCarousel state BannerCarousal] 
    else [])
  -- <> (if getValueToLocalStore IS_BANNER_ACTIVE == "True" then [genderBannerConfig state BannerCarousal] else []) NOTE::- Deprecated the complete profile banner for now
  <> (if state.props.currentStage == ST.HomeScreen && state.data.config.purpleRideConfig.showPurpleVideos then [accessbilityBannerConfig state BannerCarousal] else [])
  <> getRemoteBannerConfigs
  where 
    getRemoteBannerConfigs :: Array (BannerCarousel.Config (BannerCarousel.Action -> Action))
    getRemoteBannerConfigs = do 
      let driverLocation = toLower $ getValueToLocalStore DRIVER_LOCATION
          language = getLanguage $ getLanguageLocale languageKey
          configName = "driver_carousel_banner" <> language
          datas = RC.carouselConfigData driverLocation configName "driver_carousel_banner_en" $ getValueFromWindow "DRIVER_ID"
      BannerCarousel.remoteConfigTransformer datas BannerCarousal
    getLanguage :: String -> String
    getLanguage lang = 
      let language = toLower $ take 2 lang
      in if not (null language) then "_" <> language else "_en"


isSafetyPeriod :: ST.HomeScreenState -> String -> Boolean
isSafetyPeriod state riseStartTime = 
  let timeStamp = EHC.convertUTCtoISC riseStartTime "HH:mm:ss"
  in JB.withinTimeRange state.data.config.safetyRide.startTime state.data.config.safetyRide.endTime timeStamp
