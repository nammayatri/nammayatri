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
import Common.Types.App (OptionButtonList, LazyCheck(..), Paths(..), UploadFileConfig(..)) as Common
import Components.Banner as Banner
import Components.BannerCarousel as BannerCarousel
import Components.BottomNavBar as BottomNavBar
import Components.ChatView as ChatView
import Components.ErrorModal.Controller as ErrorModalController
import Components.GoToLocationModal as GoToLocationModal
import Components.InAppKeyboardModal as InAppKeyboardModal
import Components.MakePaymentModal as MakePaymentModal
import Components.PlanCard.Controller as PlanCard
import Components.PopUpModal as PopUpModal
import Components.PrimaryButton as PrimaryButtonController
import Components.RateCard as RateCard
import Components.RatingCard as RatingCard
import Components.RequestInfoCard as RequestInfoCard
import Components.RideActionModal as RideActionModal
import Components.RideCompletedCard as RideCompletedCard
import Components.SelectListModal as SelectListModal
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
import Data.Int (round, toNumber, fromString, ceil, fromNumber)
import Data.Int as Int
import Data.Lens ((^.))
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing, maybe)
import Data.Number (fromString) as Number
import Data.String (Pattern(..), Replacement(..), drop, length, take, trim, replaceAll, toLower, null)
import Data.String as DS
import Domain.Payments (APIPaymentStatus(..), PaymentStatus(..)) as PP
import Data.Functor
import Effect (Effect)
import Effect.Aff (launchAff)
import Effect.Class (liftEffect)
import Effect.Uncurried (runEffectFn4, runEffectFn1, runEffectFn5, runEffectFn6)
import Effect.Unsafe (unsafePerformEffect)
import Engineering.Helpers.BackTrack (getState, liftFlowBT)
import Engineering.Helpers.Commons (flowRunner)
import Engineering.Helpers.Commons (getCurrentUTC, getNewIDWithTag, convertUTCtoISC, isPreviousVersion, getExpiryTime,liftFlow)
import Engineering.Helpers.Commons as EHC
import JBridge (animateCamera, enableMyLocation, firebaseLogEvent, getCurrentPosition, getHeightFromPercent, hideKeyboardOnNavigation, isLocationEnabled, isLocationPermissionEnabled, minimizeApp, openNavigation, removeAllPolylines, requestLocation, showDialer, showMarker, toast, firebaseLogEventWithTwoParams,sendMessage, stopChatListenerService, getSuggestionfromKey, scrollToEnd, getChatMessages, cleverTapCustomEvent, metaLogEvent, toggleBtnLoader, openUrlInApp, pauseYoutubeVideo, differenceBetweenTwoUTC, removeMediaPlayer, locateOnMapConfig, getKeyInSharedPrefKeys, defaultMarkerConfig, renderBase64Image)
import Engineering.Helpers.LogEvent (logEvent, logEventWithTwoParams, logEventWithMultipleParams)
import Engineering.Helpers.Suggestions (getMessageFromKey, getSuggestionsfromKey, chatSuggestion)
import Engineering.Helpers.Utils (saveObject)
import Engineering.Helpers.GeoHash (encodeGeohash, geohashNeighbours)
import Foreign.Generic (class Decode, ForeignError, decode, decodeJSON, encode)
import Foreign (unsafeToForeign)
import Helpers.Utils as HU
import JBridge as JB
import Helpers.API as HelpersAPI
import Debug (spy)
import Language.Strings (getString)
import Language.Types (STR(..)) as LT
import Log (printLog, trackAppActionClick, trackAppBackPress, trackAppEndScreen, trackAppScreenEvent, trackAppScreenRender, trackAppTextInput)
import MerchantConfig.Utils (getMerchant, Merchant(..))
import Prelude (class Show, Unit, bind, discard, map, not, pure, show, unit, void, ($), (&&), (*), (+), (-), (/), (/=), (<), (<>), (==), (>), (||), (<=), (>=), when, negate, (<<<), (>>=), (<$>), const, compare)
import Presto.Core.Types.Language.Flow (Flow, delay, doAff)
import PrestoDOM (Eval, update, continue, continueWithCmd, exit, updateAndExit, updateWithCmdAndExit)
import PrestoDOM.Core (getPushFn)
import PrestoDOM.Types.Core (class Loggable)
import RemoteConfig as RC
import RemoteConfig.Utils (getDriverVoipConfig)
import Resource.Constants (decodeAddress, getLocationInfoFromStopLocation, rideTypeConstructor, getHomeStageFromString)
import Screens (ScreenName(..), getScreen)
import Screens.Types as ST
import Services.API (GetRidesHistoryResp, RidesInfo(..), Status(..), GetCurrentPlanResp(..), PlanEntity(..), PaymentBreakUp(..), GetRouteResp(..), Route(..), StopLocation(..),DriverProfileStatsResp(..), BookingTypes(..) , ScheduledBookingListResponse(..) , ScheduleBooking(..) , BookingAPIEntity(..))
import Services.Accessor (_lat, _lon, _area, _extras, _instructions)
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
import DecodeUtil
import LocalStorage.Cache (getValueFromCache, setValueToCache)
import Components.SelectPlansModal as SelectPlansModal
import Screens.SubscriptionScreen.Transformer as SubscriptionTransformer
import Components.PlanCard.Controller as PlanCard
import Screens.RideSummaryScreen.ScreenData as  RSD
import Screens.HomeScreen.ScreenData as HSD
import Debug
import Data.Tuple(Tuple(..))
import Data.String (Pattern(..), contains)
import Resource.Localizable.TypesV2 as LT2
import Resource.Localizable.StringsV2 as StringsV2
import Components.PrimaryButton as PrimaryButton
import Services.API (DriverProfileDataRes(..))
import Components.DropDownCard.Controller as DropDownCard
import Components.SwitchButtonView as SwitchButtonView
import Mobility.Prelude (boolToInt)
import Constants.Configs (getPolylineAnimationConfig)
import Presto.Core.Types.API (ErrorResponse(..))
import Engineering.Helpers.Utils as EHU
import Data.Ordering (Ordering(..))
import Data.Newtype (unwrap)
import Data.Traversable (for_)
import Engineering.Helpers.RippleCircles as EHR

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog action appId = pure unit
    -- case action of
    -- AfterRender -> trackAppScreenRender appId "screen" (getScreen HOME_SCREEN)
    -- BackPressed -> do
    --   trackAppBackPress appId (getScreen HOME_SCREEN)
    --   trackAppEndScreen appId (getScreen HOME_SCREEN)
    -- ScreenClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "screen_click"
    -- Notification _ _ -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "notification_page"
    -- ChangeStatus status -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "change_status"
    -- GoOffline status -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "go_offline"
    -- CancelGoOffline -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "cancell_go_offline"
    -- ShowMap key lat lon -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "show_map"
    -- BottomNavBarAction (BottomNavBar.OnNavigate item) -> do
    --   trackAppActionClick appId (getScreen HOME_SCREEN) "bottom_nav_bar" "on_navigate"
    --   trackAppEndScreen appId (getScreen HOME_SCREEN)
    -- RideActionModalAction act -> pure unit-- case act of
    --   -- RideActionModal.StartRide -> trackAppActionClick appId (getScreen HOME_SCREEN) "ride_action_modal" "start_ride"
    --   -- RideActionModal.EndRide -> trackAppActionClick appId (getScreen HOME_SCREEN) "ride_action_modal" "end_ride"
    --   -- RideActionModal.SelectListModal -> trackAppActionClick appId (getScreen HOME_SCREEN) "ride_action_modal" "cancel_ride"
    --   -- RideActionModal.OnNavigate -> trackAppActionClick appId (getScreen HOME_SCREEN) "ride_action_modal" "on_navigate"
    --   -- RideActionModal.CallCustomer -> trackAppActionClick appId (getScreen HOME_SCREEN) "ride_action_modal" "call_customer"
    --   -- RideActionModal.LocationTracking -> trackAppActionClick appId (getScreen HOME_SCREEN) "ride_action_modal" "location_tracking"
    --   -- RideActionModal.MessageCustomer -> trackAppActionClick appId (getScreen HOME_SCREEN) "ride_action_modal" "message_customer"
    --   -- _ -> pure unit
    -- PopUpModalAccessibilityAction act -> pure unit
    -- PopUpRentalInfoAction act -> pure unit
    -- PopUpModalAdvancedRideAction act -> pure unit
    -- InAppKeyboardModalAction act -> pure unit--case act of
    --   -- InAppKeyboardModal.OnSelection key index -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_app_otp_modal" "on_selection"
    --   -- InAppKeyboardModal.OnClickBack text -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_app_otp_modal" "on_click_back"
    --   -- InAppKeyboardModal.OnclickTextBox index -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_app_otp_modal" "on_click_text_box"
    --   -- InAppKeyboardModal.BackPressed -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_app_otp_modal" "on_backpressed"
    --   -- InAppKeyboardModal.OnClickDone text -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_app_otp_modal" "on_click_done"
    --   -- _ -> pure unit
    -- InAppKeyboardModalOdometerAction act -> pure unit
    -- CountDown seconds -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "count_down"
    -- PopUpModalAction act -> pure unit --case act of
    --   -- PopUpModal.OnButton1Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_end_ride" "go_back_onclick"
    --   -- PopUpModal.OnButton2Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_end_ride" "end_ride_onclick"
    --   -- PopUpModal.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_end_ride" "no_action"
    --   -- PopUpModal.ETextController act-> trackAppTextInput appId (getScreen HOME_SCREEN) "popup_modal_end_ride_text_changed" "primary_edit_text"
    --   -- PopUpModal.CountDown seconds id status timerID -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_end_ride" "countdown_updated"
    --   -- PopUpModal.OnImageClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_end_ride" "image_onclick"
    --   -- PopUpModal.Tipbtnclick arg1 arg2 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_action" "tip_clicked"
    --   -- PopUpModal.DismissPopup -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_action" "popup_dismissed"
    -- PopUpModalCancelConfirmationAction act -> pure unit -- case act of
    --   -- PopUpModal.OnButton1Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_cancel_confirmation" "continue_onclick"
    --   -- PopUpModal.OnButton2Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_cancel_confirmation" "go_back_onclick"
    --   -- PopUpModal.CountDown seconds id status timerID -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_cancel_confirmation" "countdown_updated"
    --   -- PopUpModal.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_cancel_confirmation" "no_action"
    --   -- PopUpModal.ETextController act-> trackAppTextInput appId (getScreen HOME_SCREEN) "popup_modal_cancel_confirmation_text_changed" "primary_edit_text"
    --   -- PopUpModal.OnImageClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_cancel_confirmation" "image_onclick"
    --   -- PopUpModal.Tipbtnclick arg1 arg2 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_action" "tip_clicked"
    --   -- PopUpModal.DismissPopup -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_action" "popup_dismissed"
    -- CancelRideModalAction act -> pure unit -- case act of
    --   -- SelectListModal.Button1 act -> case act of
    --   --   PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "cancel_ride" "primary_btn_go_back_onclick"
    --   --   PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "cancel_ride" "primary_btn_go_back_no_action"
    --   -- SelectListModal.Button2 act -> case act of
    --   --   PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "cancel_ride" "primary_btn_cancel_ride_onclick"
    --   --   PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "cancel_ride" "primary_btn_cancel_ride_no_action"
    --   -- SelectListModal.UpdateIndex indexValue -> trackAppActionClick appId (getScreen HOME_SCREEN) "cancel_ride" "update_index_onclick"
    --   -- SelectListModal.TextChanged  valId newVal -> trackAppTextInput appId (getScreen HOME_SCREEN) "reason_text_changed" "cancel_ride"
    --   -- SelectListModal.OnGoBack -> trackAppActionClick appId (getScreen HOME_SCREEN) "cancel_ride" "go_back_onclick"
    --   -- SelectListModal.ClearOptions -> trackAppActionClick appId (getScreen HOME_SCREEN) "cancel_ride" "clear_options_onclick"
    --   -- SelectListModal.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "cancel_ride" "no_action"
    -- GenderBannerModal act -> pure unit
    -- AutoPayBanner act -> pure unit
    -- AdvancedRideBannerAction act -> pure unit
    -- RetryTimeUpdate -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "retry_time_update_onclick"
    -- TimeUpdate time lat lng -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "time_update"
    -- ModifyRoute lat lon -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "modify_route"
    -- SetToken id -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "set_token"
    -- Cancel -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "cancel"
    -- CurrentLocation lat lng -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "current_location"
    -- ActiveRideAPIResponseAction resp -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "active_ride_api_response"
    -- RecenterButtonAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "recenter_btn"
    -- HelpAndSupportScreen -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "help_and_support_btn"
    -- NoAction -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "no_action"
    -- UpdateMessages msg sender timeStamp size -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "update_messages"
    -- OpenChatScreen -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "open_chat"
    -- InitializeChat -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "initialize_chat"
    -- RemoveChat -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "remove_chat"
    -- UpdateInChat -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "update_in_chat"
    -- ScrollToBottom -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "scroll_to_bottom"
    -- GetMessages -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "get_messages"
    -- KeyboardCallback status -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "key_board_callback"
    -- ChatViewActionController act -> pure unit -- case act of
    --   -- ChatView.SendMessage -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_app_messaging" "send_message"
    --   -- ChatView.SendSuggestion suggestion -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_app_messaging" "send_suggestion"
    --   -- ChatView.BackPressed -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_app_messaging" "back_pressed"
    --   -- ChatView.TextChanged input -> trackAppTextInput appId (getScreen HOME_SCREEN) "in_app_messaging" "text_changed"
    --   -- ChatView.Call -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_app_messaging" "call_driver"
    --   -- ChatView.Navigate -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_app_messaging" "navigate_to_google_maps"
    --   -- ChatView.NoAction -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_app_messaging" "no_action"
    -- SwitchDriverStatus status -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "SwitchDriverStatus"
    -- GoToProfile -> do
    --   trackAppActionClick appId (getScreen HOME_SCREEN) "bottom_nav_bar" "on_navigate"
    --   trackAppEndScreen appId (getScreen HOME_SCREEN)
    -- LinkAadhaarPopupAC act -> pure unit --case act of
    -- PopUpModalSilentAction act -> pure unit --case act of
    --   -- PopUpModal.OnButton1Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_silent_confirmation" "go_offline_onclick"
    --   -- PopUpModal.OnButton2Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_silent_confirmation" "go_silent_onclick"
    --   -- PopUpModal.CountDown seconds id status timerID -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_silent_confirmation" "countdown_updated"
    --   -- PopUpModal.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_silent_confirmation" "no_action"
    --   -- PopUpModal.ETextController act-> trackAppTextInput appId (getScreen HOME_SCREEN) "popup_modal_cancel_confirmation_text_changed" "primary_edit_text"
    --   -- PopUpModal.OnImageClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_cancel_confirmation" "image_onclick"
    --   -- PopUpModal.Tipbtnclick arg1 arg2 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_action" "tip_clicked"
    --   -- PopUpModal.DismissPopup -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_action" "popup_dismissed"
    -- ZoneOtpAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "zone_otp"
    -- TriggerMaps -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "trigger_maps"
    -- RemoveGenderBanner -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "gender_banner"
    -- RequestInfoCardAction act -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "request_info_card"
    -- _ -> pure unit
    -- WaitTimerCallback id min sec -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "wait_timer_callBack" 
    -- MakePaymentModalAC act -> pure unit
    -- RateCardAC act -> pure unit
    -- PaymentBannerAC act -> pure unit
    -- PaymentStatusAction _ -> pure unit
    -- RemovePaymentBanner -> pure unit
    -- OfferPopupAC _ -> pure unit
    -- RCDeactivatedAC _ -> pure unit
    -- FreeTrialEndingAC _ -> pure unit
    -- DriverStats _ -> pure unit
    -- UploadImage -> pure unit
    -- CallBackImageUpload _ _ _ -> pure unit
    -- CallBackNewStop _ -> pure unit
    -- _ -> pure unit


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
                    | FcmNotification String ST.NotificationBody ST.HomeScreenState
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
                    | GoToCompleteProfile ST.HomeScreenState
                    | DisableGoto ST.HomeScreenState
                    | ExitGotoLocation ST.HomeScreenState Boolean
                    | RefreshGoTo ST.HomeScreenState
                    | EarningsScreen ST.HomeScreenState Boolean
                    | DriverStatsUpdate DriverProfileStatsResp ST.HomeScreenState
                    | UpdateSpecialLocationList ST.HomeScreenState
                    | FetchOdometerReading ST.HomeScreenState
                    | GoToNewStop ST.HomeScreenState
                    | UpdateAirConditioned ST.HomeScreenState Boolean
                    | GoToBookingPreferences ST.HomeScreenState
                    | GoToRideReqScreen ST.HomeScreenState
                    | UpdateRouteOnStageSwitch ST.HomeScreenState
                    | CustomerReferralTrackerScreen ST.HomeScreenState
                    | BenefitsScreen ST.HomeScreenState
                    | GotoAddUPIScreen ST.HomeScreenState
                    | VerifyManualUPI ST.HomeScreenState
                    | SwitchPlan ST.PlanCardState ST.HomeScreenState
                    | GotoHotspotScreen ST.HomeScreenState
                    | GoToRideSummary ST.HomeScreenState
                    | GoToRideSummaryScreen  ST.HomeScreenState 
                    | UploadParcelImage ST.HomeScreenState
                    | NotifyDriverReachedDestination ST.HomeScreenState
                    | UpdateToggleMetroWarriors ST.HomeScreenState
                    | GoToMetroWarriors ST.HomeScreenState
                    | UpdateStopsStatus ST.HomeScreenState

data Action = NoAction
            | BackPressed
            | ScreenClick
            | BookingOptions
            | Notification String ST.NotificationBody
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
            | TimeUpdate String String String String
            | RideActiveAction RidesInfo (Maybe RidesInfo)
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
            | FreeTrialRidesEndingAC PopUpModal.Action 
            | GetCurrentDuesAction GetCurrentPlanResp
            | GetCurrentDuesFailed
            | AutoPayBanner Banner.Action
            | AdvancedRideBannerAction Banner.Action
            | RCDeactivatedAC PopUpModal.Action
            | PopUpRentalInfoAction PopUpModal.Action
            | PopUpModalAccessibilityAction PopUpModal.Action
            | PopUpModalAdvancedRideAction PopUpModal.Action
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
            | AccountBlockedDueToCancellationsAC PopUpModal.Action
            | UpdateAndNotify Boolean
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
            | DriverStats DriverProfileStatsResp
            | AllChatsLoaded
            | OnMarkerClickCallBack String String String
            | UpdateSpecialLocation
            | SpecialZonePopup
            | SpecialZoneCardAC RequestInfoCard.Action
            | BgLocationAC
            | BgLocationPopupAC PopUpModal.Action
            | CoinEarnedPopupAC PopUpModal.Action
            | IsAcWorkingPopUpAction PopUpModal.Action
            | OnAudioCompleted String
            | ACExpController PopUpModal.Action
            | OpenLink String
            | RideStartRemainingTime Int String String
            | TollChargesPopUpAC PopUpModal.Action
            | TollChargesAmbigousPopUpAC PopUpModal.Action
            | RideRequestsList
            | SwitchBookingStage BookingTypes
            | AccessibilityHeaderAction
            | PopUpModalInterOperableAction PopUpModal.Action
            | UpdateSpecialZoneList
            | ReferralPopUpAction ST.HomeScreenPopUpTypes (Maybe KeyStore) PopUpModal.Action
            | AddAlternateNumberAction
            | GoToDriverProfile
            | SelectPlansModalAction SelectPlansModal.Action
            | PlanListResponse API.UiPlansResp
            | OpenHotspotScreen
            | RideSummary
            | UpComingRideDetails (Maybe RidesInfo)
            | RideDetail
            | ScheduledRideBannerClick
            | HomeScreenBannerCountDownTimer Int String String
            | OnRideBannerCountDownTimer Int String String
            | GetRideCount Int
            | FavouritePopUpAction DropDownCard.Action
            | CloseDeliveryCallPopup
            | DeliveryCall ST.DeliverCallType
            | NotifyReachedDestination
            | UpdateRetryRideList Boolean
            | OnRideAssignedAudioCompleted String
            | ProfileDataAPIResponseAction DriverProfileDataRes
            | DropDownCardAC DropDownCard.Action
            | UpdateLastExecutedTime
            | FavPopUpAction PopUpModal.Action
            | CompleteProfileAction PopUpModal.Action
            | ParcelIntroductionPopup PopUpModal.Action
            | ToggleMetroWarriors
            | ClickMetroWarriors
            | MetroWarriorPopupAC PopUpModal.Action
            | MetroWarriorSwitchAction SwitchButtonView.Action
            | UpdateState ST.HomeScreenState
            | HideBusOnline
            | BusNumber String
            | VOIPCallBack String String String Int Int String String String
            | RideEndWithStopsPopupAction PopUpModal.Action
            | UpdateRouteInState (Array Route)

uploadFileConfig :: Common.UploadFileConfig
uploadFileConfig = Common.UploadFileConfig {
  showAccordingToAspectRatio : false,
  imageAspectHeight : 0,
  imageAspectWidth : 0
}

eval :: Action -> ST.HomeScreenState -> Eval Action ScreenOutput ST.HomeScreenState

eval (CompleteProfileAction (PopUpModal.OnButton2Click) ) state = do
  let currentTime = HU.getCurrentUTC ""
  void $ pure $ setValueToLocalStore LAST_EXECUTED_TIME currentTime
  exit $ GoToCompleteProfile state

eval (CompleteProfileAction PopUpModal.DismissPopup) state = do
  let currentTime = HU.getCurrentUTC ""
  void $ pure $ setValueToLocalStore LAST_EXECUTED_TIME currentTime
  continue state

eval (FavPopUpAction PopUpModal.OnButton2Click) state = continueWithCmd state[pure $ FavPopUpAction PopUpModal.DismissPopup]

eval (FavPopUpAction PopUpModal.DismissPopup) state = continue state{data{favPopUp{visibility = false, title = "", message = ""}}}

eval (GoToButtonClickAC PrimaryButtonController.OnClick) state = do
  pure $ toggleBtnLoader "GotoClick" false
  if state.data.driverGotoState.isGotoEnabled then continue state { data { driverGotoState { confirmGotoCancel = true } }} 
  else if state.data.driverGotoState.gotoCount <=0 then continue state
  else do
    pure $ toggleBtnLoader "GotoClick" true
    updateAndExit state{ data { driverGotoState { savedLocationsArray = []}}} $ LoadGotoLocations state{ data { driverGotoState { savedLocationsArray = []}}}

eval (ProfileDataAPIResponseAction res) state = do 
  let DriverProfileDataRes resp = res 
  continue state{data{completingProfileRes{
    completed = getValueBtwRange ((boolToInt $ not Array.null resp.pledges) + (boolToInt $ not Array.null resp.aspirations) + (boolToInt $ not isNothing resp.drivingSince) + (boolToInt $ not isNothing resp.hometown) + (boolToInt $ not Array.null resp.vehicleTags) + (boolToInt $ not Array.null resp.otherImageIds)) 0 6 0 4}}}

eval ClickInfo state = continue state {data { driverGotoState {goToInfo = true}}}

eval (GotoKnowMoreAction PopUpModal.OnButton1Click) state = continue state { data { driverGotoState { goToInfo = false } }} 

eval BgLocationAC state = continue state { props { bgLocationPopup = true}}

eval (BgLocationPopupAC PopUpModal.OnButton1Click) state = 
  continueWithCmd state { props { bgLocationPopup = false}} [do
    void $ JB.requestBackgroundLocation unit
    pure NoAction
  ]

eval (ReferralPopUpAction popUpType storageKey PopUpModal.OnButton1Click) state = do 
  case storageKey of 
    Just value -> void $ pure $ setValueToLocalNativeStore value (getCurrentUTC "")
    Nothing -> pure unit
  case popUpType of
    ST.ReferralEarned -> exit $ CustomerReferralTrackerScreen state{props{showReferralEarnedPopUp = false}}
    ST.ReferNow -> exit $ BenefitsScreen state{props{showReferNowPopUp = false}}
    ST.AddUPI -> exit $ GotoAddUPIScreen state{props{showAddUPIPopUp = false}}
    ST.VerifyUPI -> exit $ VerifyManualUPI state{props{showVerifyUPIPopUp = false}}
    _ -> continue state

eval (ReferralPopUpAction popUpType storageKey PopUpModal.OnButton2Click) state = do 
  case storageKey of 
    Just value -> void $ pure $ setValueToLocalNativeStore value (getCurrentUTC "")
    Nothing -> pure unit
  case popUpType of
    ST.ReferralEarned -> continue state{props{showReferralEarnedPopUp = false}}
    ST.ReferNow -> continue state{props{showReferNowPopUp = false}}
    ST.AddUPI -> continue state{props{showAddUPIPopUp = false}}
    ST.VerifyUPI -> exit $ GotoAddUPIScreen state{props{showVerifyUPIPopUp = false}}
    _ -> continue state

eval (ReferralPopUpAction popUpType storageKey PopUpModal.DismissPopup) state = do 
  case storageKey of 
    Just value -> void $ pure $ setValueToLocalNativeStore value (getCurrentUTC "")
    Nothing -> pure unit
  case popUpType of
    ST.ReferralEarned -> continue state{props{showReferralEarnedPopUp = false}}
    ST.ReferNow -> continue state{props{showReferNowPopUp = false}}
    ST.AddUPI -> continue state{props{showAddUPIPopUp = false}}
    ST.VerifyUPI -> continue state{props{showVerifyUPIPopUp = false}}
    _ -> continue state

eval (ConfirmDisableGoto PopUpModal.OnButton2Click) state = continue state { data { driverGotoState { confirmGotoCancel = false } }} 

eval (ConfirmDisableGoto PopUpModal.OnButton1Click) state = updateAndExit state{ data { driverGotoState { confirmGotoCancel = false } }}  $ DisableGoto state{ data { driverGotoState { confirmGotoCancel = false } }} 

eval (AccountBlockedDueToCancellationsAC PopUpModal.OnButton2Click) state = continue state { props { accountBlockedPopupDueToCancellations = false } }

eval (AccountBlockedDueToCancellationsAC PopUpModal.OnButton1Click) state = do 
  void $ pure $ unsafePerformEffect $ contactSupportNumber ""
  continue state 

eval (AccountBlockedAC PopUpModal.OnButton2Click) state = continue state { props { accountBlockedPopup = false } }

eval (AccountBlockedAC PopUpModal.OnButton1Click) state = do 
  void $ pure $ unsafePerformEffect $ contactSupportNumber ""
  continue state 

eval BookingOptions state = exit $ GoToBookingPreferences state  

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

eval (EnableGotoTimerAC PrimaryButtonController.OnClick) state = do
  let metroWarriorsConfig = RC.metroWarriorsConfig (getValueToLocalStore DRIVER_LOCATION) (getValueToLocalStore VEHICLE_VARIANT)
  if state.data.isSpecialLocWarrior && metroWarriorsConfig.isMetroWarriorEnabled then do
    void $ pure $ toggleBtnLoader "EnableGoto" false
    continue state {props { showMetroWarriorWarningPopup = true }}
  else updateAndExit state $ EnableGoto state state.data.driverGotoState.selectedGoTo

eval AddGotoAC state = exit $ ExitGotoLocation state false

eval (UpdateGoHomeTimer seconds status timerID) state = do
  if status == "EXPIRED" then do 
    void $ pure $ TF.clearTimerWithId timerID
    void $ pure $ HU.setPopupType ST.VALIDITY_EXPIRED
    continue state { data { driverGotoState { goToPopUpType = ST.VALIDITY_EXPIRED, timerId = "" }}}
    else do
      let timeInMinutes = seconds/60 + 1
      continue state { data { driverGotoState { timerInMinutes = show timeInMinutes <> " " <>(getString LT.MIN_LEFT), timerId = timerID} } }

eval (CancelBackAC PrimaryButtonController.OnClick) state = continue state { data { driverGotoState { showGoto = false}}}

eval (AddLocation PrimaryButtonController.OnClick) state = exit $ ExitGotoLocation state true

eval AddNewLocation state = exit $ ExitGotoLocation state true

eval AfterRender state = continue state { props { mapRendered = true}}

eval SpecialZonePopup state = continue state{ props{ specialZoneProps{ specialZonePopup = true }}}

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
    void $ pure $ setValueToLocalStore PREVIOUS_LOCAL_STAGE $ show state.props.currentStage
    void $ pure $ setValueToLocalStore LOCAL_STAGE previousStage
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
  else if state.props.specialZoneProps.specialZonePopup then continue state { props { specialZoneProps{specialZonePopup = false} }}
  else if state.props.bgLocationPopup then continue state { props { bgLocationPopup = false}}
  else if state.props.acExplanationPopup then continue state { props { acExplanationPopup = false }}
  else if state.props.showReferralEarnedPopUp then continue state {props { showReferralEarnedPopUp = false}}
  else if state.props.showReferNowPopUp then do 
    void $ pure $ setValueToLocalNativeStore REFER_NOW_LAST_SHOWN (getCurrentUTC "")
    continue state {props { showReferNowPopUp = false}}
  else if state.props.showAddUPIPopUp then do 
    void $ pure $ setValueToLocalNativeStore ADD_UPI_LAST_SHOWN (getCurrentUTC "")
    continue state {props { showAddUPIPopUp = false}}
  else if state.props.showVerifyUPIPopUp then do 
    void $ pure $ setValueToLocalNativeStore VERIFY_UPI_LAST_SHOWN (getCurrentUTC "")
    continue state {props { showVerifyUPIPopUp = false}}
  else if state.data.plansState.showSwitchPlanModal then continue state { data { plansState { showSwitchPlanModal = false }}}
  else do
    _ <- pure $ minimizeApp ""
    continue state

eval (OnAudioCompleted status) state = do 
  let 
    needToTriggerMap = getValueToLocalStore TRIGGER_MAPS 
    _ = runFn2  EHC.updatePushInIdMap "PlayAudioAndLaunchMap" true

  if needToTriggerMap == "true" then
    continueWithCmd state [pure TriggerMaps]
  else
    continue state

eval TriggerMaps state = continueWithCmd state{props{triggerGMapsIntent = false}}[ do
  let _ = runFn2 EHC.updatePushInIdMap "PlayAudioAndLaunchMap" true
  if state.data.activeRide.tripType == ST.Rental then
      case state.data.activeRide.nextStopLat, state.data.activeRide.nextStopLon of
        Just nextStopLat,Just nextStopLon -> if state.props.currentStage == ST.RideAccepted && state.data.vehicleType == "AUTO_RICKSHAW" && state.data.cityConfig.cityName == "Chennai"
                                              then
                                                pure $ openNavigation nextStopLat nextStopLon "TWOWHEELER"
                                              else
                                                pure $ openNavigation nextStopLat nextStopLon "DRIVE"
        _,_ -> pure unit
  else do
    let upcomingStop = HU.getUpcomingStop state.data.activeRide.stops
        Tuple nextLat nextLon = 
          case upcomingStop of
            Just (API.Stop stop) -> do
              let (API.LocationInfo location) = stop.location
              Tuple location.lat location.lon
            _ -> Tuple state.data.activeRide.dest_lat state.data.activeRide.dest_lon
    if getDistanceBwCordinates state.data.currentDriverLat state.data.currentDriverLon nextLat nextLon  > 0.200 then do
      let driveMode =  if state.props.currentStage == ST.RideAccepted && ((state.data.vehicleType == "AUTO_RICKSHAW" && state.data.cityConfig.cityName == "Chennai") || (state.data.vehicleType == "BIKE") || (state.data.vehicleType == "DELIVERY_BIKE")) then  "TWOWHEELER" else "DRIVE"
      pure $ openNavigation nextLat nextLon driveMode
    else 
      void $ openUrlInApp $ "https://maps.google.com?saddr=&daddr="<> show nextLat <>","<> show nextLon <> "&dirflg=d"

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
  else
    continue state

eval (Notification notificationType notificationBody) state = do
  _ <- pure $ printLog "notificationType" notificationType
  if (checkNotificationType notificationType ST.DRIVER_REACHED && (state.props.currentStage == ST.RideAccepted || state.props.currentStage == ST.ChatWithCustomer) && (not state.data.activeRide.notifiedCustomer)) then do
    let newState = state{props{showAccessbilityPopup = isJust state.data.activeRide.disabilityTag, safetyAudioAutoPlay = false}}
    void $ pure $ setValueToLocalStore IS_DRIVER_AT_PICKUP "true"
    continueWithCmd newState [ pure if (not state.data.activeRide.notifiedCustomer) then NotifyAPI else AfterRender]
  else if (checkNotificationType notificationType ST.DRIVER_REACHED_DESTINATION && state.props.currentStage == ST.RideStarted && (not state.data.activeRide.notifiedReachedDestination)) then do
    continueWithCmd state [pure if (not state.data.activeRide.notifiedReachedDestination) then NotifyReachedDestination else AfterRender]
  else if (Array.any ( _ == notificationType) [show ST.CANCELLED_PRODUCT, show ST.DRIVER_ASSIGNMENT, show ST.RIDE_REQUESTED, show ST.DRIVER_REACHED, show ST.TRIP_STARTED, show ST.EDIT_LOCATION, show ST.USER_FAVOURITE_DRIVER]) then do
    exit $ FcmNotification notificationType notificationBody state{ props { specialZoneProps{ currentGeoHash = "" }} }
  else if (Array.any (checkNotificationType notificationType) [ST.FROM_METRO_COINS, ST.TO_METRO_COINS] && state.props.currentStage == ST.RideCompleted) then do
    let city = getValueToLocalStore DRIVER_LOCATION
        metroRideCoinConfig = RC.getMetroCoinsEvent city
        metroRideCoinData = case notificationType of
          "TO_METRO_COINS" ->
              { coinsEarned: metroRideCoinConfig.coinsToMetroRide, metroRideType: API.ToMetro }
          "FROM_METRO_COINS" ->
              { coinsEarned: metroRideCoinConfig.coinsFromMetroRide, metroRideType: API.FromMetro }
          _ ->
              { coinsEarned: 0, metroRideType: API.None }
    continue state { data = state.data { endRideData = state.data.endRideData { metroRideCoinData = Just metroRideCoinData } } }
  else continue state

eval CancelGoOffline state = do
  continue state { props = state.props { goOfflineModal = false } }

eval (GoOffline status) state = exit (DriverAvailabilityStatus state { props = state.props { goOfflineModal = false }} ST.Offline)

eval (ShowMap key lat lon) state = continueWithCmd state [ do
  id <- checkPermissionAndUpdateDriverMarker true
  pure AfterRender
  ]

eval UpdateSpecialZoneList state = continueWithCmd state { props { mapRendered = true}} [ do
  let specialLocationUpdatedAt = getValueToLocalStore SPECIAL_LOCATION_LIST_EXPIRY
      specialLocationListExpiry = 86400 - (getExpiryTime specialLocationUpdatedAt true)
  if (getValueToLocalStore SPECIAL_LOCATION_LIST == "__failed") || specialLocationListExpiry <= 0 || specialLocationUpdatedAt == "__failed" then do
    void $ pure $ setValueToLocalStore SPECIAL_LOCATION_LIST_EXPIRY (getCurrentUTC "LazyCheck")
    pure UpdateSpecialLocation
  else pure AfterRender
  ]

eval UpdateSpecialLocation state = exit $ UpdateSpecialLocationList state


eval (UploadImage) state = do
  let stage = getValueToLocalNativeStore LOCAL_STAGE
  if state.props.odometerImageUploading || ( stage == "RideStarted" && state.props.enterOdometerReadingModal) || ( stage == "RideCompleted" && state.props.endRideOdometerReadingModal) then
    continue state {props {odometerImageUploading = false}}
  else if state.props.odometerUploadAttempts < 3 then do
    continueWithCmd (state {props {odometerUploadAttempts = state.props.odometerUploadAttempts + 1,odometerImageUploading = true}}) [do
      let _ = unsafePerformEffect $ logEvent state.data.logField "UPLOAD odometer reading"
      _ <- liftEffect $ JB.uploadFile uploadFileConfig true
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
    case state.data.activeRide.tripType of
     ST.Rental -> do
      newState <- if state.props.endRideOdometerReadingModal then do
                    void $ pure $ setValueToLocalStore RIDE_END_ODOMETER image
                    pure state{props{endRideOdometerImage = Just image, odometerImageUploading = false}} 
                  else do 
                    void $ pure $ setValueToLocalStore RIDE_START_ODOMETER image 
                    pure state{props{startRideOdometerImage = Just image,currentStage= ST.RideStarted,odometerImageUploading = false}}
      
      continueWithCmd newState [do
          void $ runEffectFn5 JB.uploadMultiPartData imagePath (EP.uploadOdometerImage "") "Image" "fileId" "file"
          pure NoAction
        ]
     _ -> continue state

eval (UploadMultiPartDataCallback fileType fileId) state = do
  let newState = state{props{odometerFileId = Just fileId}}
      exitAction = if state.props.endRideOdometerReadingModal then
          EndRide newState
        else do
          StartRide newState
  updateAndExit newState exitAction

eval (CallBackNewStop stopStatus) state = do
  case stopStatus of
    "ADD_STOP"  -> exit $ GoToNewStop state
    "EDIT_STOP" -> exit $ GoToNewStop state
    _ -> continue state
  

eval (NewStopAdded currentLocationLatLong nextStopLocation lastStopLocation) state = do
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

eval (FreeTrialRidesEndingAC PopUpModal.OnButton1Click) state = do
  exit $ SubscriptionScreen state

eval (FreeTrialRidesEndingAC PopUpModal.OptionWithHtmlClick) state = do
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
    if state.props.isOdometerReadingsRequired then do
      void $ pure $ hideKeyboardOnNavigation true
      continue state {
        props = state.props { 
          rideOtp = otp,
          enterOtpModal = false,
          endRideOtpModal = false,
          otpIncorrect = false,
          enterOdometerReadingModal =  state.props.enterOtpModal, 
          endRideOdometerReadingModal = state.props.endRideOtpModal, 
          enterOdometerFocusIndex = 0,
          isInvalidOdometer = false
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

eval (InAppKeyboardModalAction (InAppKeyboardModal.RetakeParcelImage)) state = do
  void $ pure $ hideKeyboardOnNavigation true
  exit $ UploadParcelImage state

eval (InAppKeyboardModalOdometerAction (InAppKeyboardModal.BackPressed)) state = do
  continue $ state { props = state.props { enterOtpModal = state.props.enterOdometerReadingModal, enterOdometerReadingModal = false,endRideOtpModal = state.props.endRideOdometerReadingModal,endRideOdometerReadingModal = false} }

eval (InAppKeyboardModalOdometerAction (InAppKeyboardModal.OnclickTextBox index)) state = do
  continue state { props = state.props { enterOdometerFocusIndex = index } }

eval (InAppKeyboardModalOdometerAction (InAppKeyboardModal.OnClickBack text)) state = do
  let odometerReading  = if length text > 0 then (take (length ( text ) - 1 ) text) else "" 
  continue state { props = state.props { odometerValue = odometerReading } }

eval (InAppKeyboardModalOdometerAction (InAppKeyboardModal.OnClickDone odometerReading)) state = do
  let keyboardId = "OdometerKeyboard" <> show state.props.enterOdometerFocusIndex
  if length odometerReading < 4 then do
    continue state
  else do
    let newState = state{ props { odometerFileId = Nothing, enterOtpModal = false, endRideOtpModal = false, odometerValue = odometerReading, isInvalidOdometer = false } }
    if (state.props.currentStage == ST.RideStarted)
      then do
        let startOdometerValue = maybe "0000" show (maybe Nothing fromNumber state.data.activeRide.startOdometerReading)
        let startOdometerLength = length startOdometerValue
        let startOdometerReading = if startOdometerLength < 4 then (if startOdometerLength == 0 then "0000" else if startOdometerLength == 1 then "000" else if startOdometerLength == 2 then "00" else if startOdometerLength == 3 then "0" else "") <> (startOdometerValue) else startOdometerValue
        let endOdometerReading = if (take 1 startOdometerReading == "9") && (take 1 (odometerReading) == "0") then
            "1" <> odometerReading
          else odometerReading
        _ <- pure $ printLog "show startOdometer" (show state.data.activeRide.startOdometerReading)
        _ <- pure $ printLog "startOdometerValue" (startOdometerValue)
        _ <- pure $ printLog "startOdometerLength" startOdometerLength
        _ <- pure $ printLog "startodometerReading " startOdometerReading
        _ <- pure $ printLog "endOdometerReading " endOdometerReading
        _ <- pure $ printLog "number  startOdometer" (fromMaybe 0.0 $ Number.fromString startOdometerReading)
        _ <- pure $ printLog "number  endOdometer" (fromMaybe 0.0 $ Number.fromString endOdometerReading)


        if endOdometerReadingIsMoreThanStart endOdometerReading startOdometerReading && endOdometerReadingIsNotMoreThan500 endOdometerReading startOdometerReading then
          updateAndExit newState { props { odometerValue = endOdometerReading }} $ EndRide newState { props { odometerValue = endOdometerReading }}
        else do
          continue state { props { isInvalidOdometer = true}}
      else do
        updateAndExit newState $ StartRide newState
  where
    endOdometerReadingIsNotMoreThan500 endOdometerReading startOdometerReading = (fromMaybe 0.0 $ Number.fromString endOdometerReading) - (fromMaybe 0.0 $ Number.fromString startOdometerReading) <= 500.0
    endOdometerReadingIsMoreThanStart endOdometerReading startOdometerReading = (fromMaybe 0.0 $ Number.fromString startOdometerReading) <= (fromMaybe 0.0 $ Number.fromString endOdometerReading)
   
eval (InAppKeyboardModalOdometerAction (InAppKeyboardModal.OnSelection key index)) state = do 
  let odometerValue = if length state.props.odometerValue < 4 then state.props.odometerValue <> key else state.props.odometerValue
  continue state { props = state.props { odometerValue = odometerValue} }

eval (RideActionModalAction (RideActionModal.NoAction)) state = continue state {data{triggerPatchCounter = state.data.triggerPatchCounter + 1,peekHeight = getPeekHeight state}}
eval (RideActionModalAction (RideActionModal.StartRide)) state = do
  if state.data.activeRide.tripType == ST.Delivery && getValueToLocalStore PARCEL_IMAGE_UPLOADED /= "true" then do
    exit $ UploadParcelImage state 
  else
    continue state { props = state.props { enterOtpModal = true, rideOtp = "", enterOtpFocusIndex = 0, enterOdometerFocusIndex = 0, otpIncorrect = false, zoneRideBooking = false, arrivedAtStop = isNothing state.data.activeRide.nextStopAddress } }

eval (RideActionModalAction (RideActionModal.EndRide)) state = do
  if Array.any (_ == state.data.activeRide.tripType) [ST.Rental, ST.Intercity, ST.Delivery] then 
    continue state{props{endRideOtpModal = true,enterOtpFocusIndex = 0, enterOdometerFocusIndex = 0, otpIncorrect = false, rideOtp="",odometerValue=""}, data{route = []}}
  else continue state {props {endRidePopUp = true}, data {route = []}}

eval (RideActionModalAction (RideActionModal.ArrivedAtStop)) state = do
  exit $ ArrivedAtStop state

eval (RideActionModalAction (RideActionModal.OnNavigate)) state = do
  let isRideStartActive = (state.props.currentStage == ST.RideAccepted || state.props.currentStage == ST.ChatWithCustomer) && ((getHomeStageFromString $ getValueToLocalStore PREVIOUS_LOCAL_STAGE) /= ST.RideStarted)
      srcLat = state.data.activeRide.src_lat
      srcLon = state.data.activeRide.src_lon
      _ = runFn2  EHC.updatePushInIdMap "PlayAudioAndLaunchMap" true
      upcomingStop = HU.getUpcomingStop state.data.activeRide.stops
  void $ pure $ setValueToLocalStore TRIGGER_MAPS "false"
  void $ pure $ JB.clearAudioPlayer ""
  if isRideStartActive then
    action srcLat srcLon
  else if state.data.activeRide.tripType == ST.Rental then do
   case state.data.activeRide.nextStopLat, state.data.activeRide.nextStopLon of
    Just nextStopLat,Just nextStopLon -> action nextStopLat nextStopLon
    _,_ -> continue state
  else if isJust upcomingStop then do
    case upcomingStop of
      Just (API.Stop stop) -> do
        let (API.LocationInfo location) = stop.location
        action location.lat location.lon
      _ -> continue state
  else action state.data.activeRide.dest_lat state.data.activeRide.dest_lon
  where 
    action lat lon = 
      if getDistanceBwCordinates state.data.currentDriverLat state.data.currentDriverLon lat lon > 0.200 then do
        let driveMode = if state.props.currentStage == ST.RideAccepted && ((state.data.vehicleType == "AUTO_RICKSHAW" && state.data.cityConfig.cityName == "Chennai") || (state.data.vehicleType == "BIKE")) then "TWOWHEELER" else "DRIVE"
        void $ pure $ openNavigation lat lon driveMode
        continue state
      else 
        continueWithCmd state [do
          void $ openUrlInApp $ "https://maps.google.com?saddr=&daddr="<> show lat <>","<> show lon <> "&dirflg=d"
          pure NoAction
        ]
          
  
eval (RideActionModalAction (RideActionModal.CancelRide)) state = do
  continue state{ data {cancelRideConfirmationPopUp{delayInSeconds = 5,  continueEnabled=false}}, props{cancelConfirmationPopup = true}}
eval (RideActionModalAction (RideActionModal.CallCustomer)) state = do
  if state.data.activeRide.tripType == ST.Delivery && state.props.currentStage == ST.RideStarted then
    continue state{props{showDeliveryCallPopup = true}}
  else do
    let exoPhoneNo = if state.data.activeRide.tripType == ST.Delivery then maybe "0000" (\(API.PersonDetails det) -> det.primaryExophone) state.data.activeRide.senderPersonDetails else state.data.activeRide.exoPhone
    let exophoneNumber = if (take 1 exoPhoneNo) == "0" then exoPhoneNo else "0" <> exoPhoneNo
    let voipConfig = getDriverVoipConfig $ DS.toLower $ getValueToLocalStore DRIVER_LOCATION
    if (voipConfig.driver.enableVoipCalling) then do
      let customerCuid = state.data.activeRide.id
      continueWithCmd state [ do
        void $ launchAff $ EHC.flowRunner defaultGlobalState $ do
          if (not (DS.null customerCuid)) then do
            push <- liftFlow $ getPushFn Nothing "HomeScreen"
            void $ liftFlow $ runEffectFn6 JB.voipDialer customerCuid true exophoneNumber false push VOIPCallBack
            pure unit
          else pure unit
        pure NoAction
      ]

    else do
      updateWithCmdAndExit state [ do
        void $ pure $ showDialer exophoneNumber false -- TODO: FIX_DIALER
        _ <- logEventWithTwoParams state.data.logField "call_customer" "trip_id" (state.data.activeRide.id) "user_id" (getValueToLocalStore DRIVER_ID)
        pure NoAction
        ] $ CallCustomer state exophoneNumber

eval (RideActionModalAction (RideActionModal.SecondaryTextClick popUpType)) state = do
  let updatedState = if popUpType == RideActionModal.RentalInfo then state{props{rentalInfoPopUp = true, safetyAudioAutoPlay = false}} 
    else if popUpType == RideActionModal.IntercityInfo then state{props{intercityInfoPopUp = true, safetyAudioAutoPlay = false}} 
    else state{props{showAccessbilityPopup = true, safetyAudioAutoPlay = false}}
  continue updatedState
eval (RideActionModalAction (RideActionModal.MoreDetails)) state = do
  continue state { props { isSourceDetailsExpanded = not state.props.isSourceDetailsExpanded } }

eval (MakePaymentModalAC (MakePaymentModal.PrimaryButtonActionController PrimaryButtonController.OnClick)) state = updateAndExit state $ OpenPaymentPage state

eval (MakePaymentModalAC (MakePaymentModal.Cancel)) state = continue state{data { paymentState {makePaymentModal = false}}}

eval (MakePaymentModalAC (MakePaymentModal.Info)) state = continue state{data { paymentState {showRateCard = true}}}

eval (RateCardAC (RateCard.PrimaryButtonAC PrimaryButtonController.OnClick)) state = continue state{data { paymentState {showRateCard = false}} , props {showIntercityRateCard = false}}

eval  (RideActionModalAction (RideActionModal.GetFare)) state  = continue state {props {showIntercityRateCard  = true}}

eval (CloseDeliveryCallPopup) state = continue state{props{showDeliveryCallPopup = false}}

eval (DeliveryCall item) state =  do
  let exoPhoneNo = case item of
        ST.SENDER -> maybe "0000" (\(API.PersonDetails det) -> det.primaryExophone) state.data.activeRide.senderPersonDetails
        ST.RECEIVER -> maybe "0000" (\(API.PersonDetails det) -> det.primaryExophone) state.data.activeRide.receiverPersonDetails
  let exoPhoneNumber = if (take 1 exoPhoneNo) == "0" then exoPhoneNo else "0" <> exoPhoneNo
  let newState = state { props { showDeliveryCallPopup = false } }
  updateWithCmdAndExit newState [ do
    void $ pure $ showDialer exoPhoneNumber false 
    _ <- logEventWithTwoParams state.data.logField "call_customer" "trip_id" (state.data.activeRide.id) "user_id" (getValueToLocalStore DRIVER_ID)
    pure NoAction
    ] $ CallCustomer newState exoPhoneNumber
  
eval (GetRideCount (count)) state = continue state { data {scheduleRideCount =  Just (Tuple count (getCurrentUTC ""))}}
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

eval (InitializeChat ) state = continue state {props{chatcallbackInitiated = true, chatServiceKilled = false}}

eval RemoveChat state = do
  continueWithCmd state {props{chatcallbackInitiated = false, chatServiceKilled = false}} [ do
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
      toChatComponentConfig { message, sentBy, timeStamp, type: type_, delay } = 
        { message, messageTitle: Nothing, messageAction: Nothing, messageLabel: Nothing, sentBy, timeStamp, type: type_, delay}
  case (Array.last allMessages) of
      Just value -> if value.message == "" then continue state {data { messagesSize = show (fromMaybe 0 (fromString state.data.messagesSize) + 1)}, props {canSendSuggestion = true}} else
                      if value.sentBy == "Driver" then updateMessagesWithCmd state {data {messages = toChatComponentConfig <$> allMessages, chatSuggestionsList = []}, props {canSendSuggestion = true}}
                      else do
                        let readMessages = fromMaybe 0 (fromString (getValueToLocalNativeStore READ_MESSAGES))
                        let unReadMessages = (if (readMessages == 0 && state.props.currentStage /= ST.ChatWithCustomer) then true else (if (readMessages < (Array.length allMessages) && state.props.currentStage /= ST.ChatWithCustomer) then true else false))
                        let suggestions = getDriverSuggestions state $ getSuggestionsfromKey chatSuggestion value.message
                        updateMessagesWithCmd state {data {messages = toChatComponentConfig <$> allMessages, chatSuggestionsList = suggestions }, props {unReadMessages = unReadMessages, canSendSuggestion = true}}
      Nothing -> continue state {props {canSendSuggestion = true}, data{ messages = toChatComponentConfig <$> allMessages}}

eval ScrollToBottom state = do
  _ <- pure $ scrollToEnd (getNewIDWithTag "ChatScrollView") true
  continue state

eval (ChatViewActionController (ChatView.TextChanged value)) state = continue state{data{messageToBeSent = (trim value)},props{sendMessageActive = (length (trim value)) >= 1}}

eval (ChatViewActionController (ChatView.Call)) state = do
  let exophoneNumber = if (take 1 state.data.activeRide.exoPhone) == "0" then state.data.activeRide.exoPhone else "0" <> state.data.activeRide.exoPhone
  let voipConfig = getDriverVoipConfig $ DS.toLower $ getValueToLocalStore DRIVER_LOCATION
  if (voipConfig.driver.enableVoipCalling) then do
      let customerCuid = state.data.activeRide.id
      continueWithCmd state [ do
        when (not (DS.null customerCuid)) do
          push <-  getPushFn Nothing "HomeScreen"
          runEffectFn6 JB.voipDialer customerCuid true exophoneNumber false push VOIPCallBack
        pure NoAction
      ]
  else
    continueWithCmd state [ do
      _ <- pure $ showDialer exophoneNumber false -- TODO: FIX_DIALER
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

eval (RideActionModalAction (RideActionModal.WaitingInfo)) state = do
  continue state {data{activeRide {waitTimeInfo = true }}}

eval (RideActionModalAction (RideActionModal.TimerCallback timerID timeInMinutes seconds)) state = continueWithCmd state [do 

  pure $ (WaitTimerCallback timerID timeInMinutes seconds)
  ]

eval (RideActionModalAction (RideActionModal.StopActionButton (PrimaryButtonController.OnClick))) state = exit $ UpdateStopsStatus state

eval (RideActionModalAction RideActionModal.ShowEndRideWithStops) state = continue state{props{showEndRideWithStopPopup = true}}

eval (UpdateWaitTime status) state = do
  void $ pure $ setValueToLocalNativeStore WAITING_TIME_STATUS (show status)
  continue state { props { waitTimeStatus = status}, data {activeRide {notifiedCustomer = status /= ST.NoStatus}}}

eval (WaitTimerCallback timerID _ seconds) state = 
  if (Just state.data.activeRide.id) == (state.data.advancedRideData <#> _.id) 
    then update state
  else 
    continue state { data {activeRide {waitTimerId = timerID, waitTimeSeconds = seconds}}}

eval (RideStartRemainingTime seconds status timerId) state = do
  let id = "rideStartRemainingTimeId_" <> state.data.activeRide.id
  if status == "EXPIRED" || id /= timerId then do
    void $ pure $ TF.clearTimerWithId timerId
    updateAndExit state { props {rideStartRemainingTime = -1}} $ NotifyDriverArrived state { props {rideStartRemainingTime = -1}} 
  else continue state { props {rideStartRemainingTime = seconds}}
  
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

eval (PopUpModalCancelConfirmationAction (PopUpModal.OnButton1Click)) state = continue state {props {cancelRideModalShow = true, cancelConfirmationPopup = false},data {cancelRideConfirmationPopUp{enableTimer = false}, cancelRideModal {activeIndex=Nothing, selectedReasonCode="", selectionOptions = cancellationReasons state }}}

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
  let newLat = getLastKnownLocValue ST.LATITUDE lat
      newLon = getLastKnownLocValue ST.LONGITUDE lon
  let newState = state { data = state.data {currentDriverLat = newLat, currentDriverLon = newLon} }
  continueWithCmd newState [ do
    void $ launchAff $ EHC.flowRunner defaultGlobalState $ updateRouteOnMap newState newLat newLon
    pure NoAction
  ]

eval (IsMockLocation isMock) state = do
  let val = isMock == "true"
      _ = unsafePerformEffect $ if val then  logEvent (state.data.logField) "ny_fakeGPS_enabled" else pure unit -- we are using unsafePerformEffect becasue without it we are not getting logs in firebase, since we are passing a parameter from state i.e. logField then the output will be inline and it will not be able to precompute so it's safe to use it here.
  continue state{props{isMockLocation = val}}

eval RetryTimeUpdate state = do
  _ <-  pure $ setValueToLocalNativeStore REGISTERATION_TOKEN (getValueToLocalStore REGISTERATION_TOKEN)
  (updateAndExit state { data = state.data { locationLastUpdatedTime = "" }, props = state.props {refreshAnimation = true}} $ Refresh state { data = state.data { locationLastUpdatedTime = "" }, props = state.props {refreshAnimation = true}})

eval (TimeUpdate time lat lng errorCode) state = do
  let isDriverBlocked = (errorCode == "DRIVER_BLOCKED")
  if isDriverBlocked then do
    void $ pure $ setValueToLocalStore DRIVER_STATUS "false"
    void $ pure $ setValueToLocalStore DRIVER_STATUS_N "Offline" 
    continue state{ props{ statusOnline = not isDriverBlocked , driverStatusSet = if isDriverBlocked then ST.Offline else state.props.driverStatusSet } }
  else do
    let driverLat = getLastKnownLocValue ST.LATITUDE lat
        driverLong = getLastKnownLocValue ST.LONGITUDE lng
        geoHash = Uncurried.runFn3 encodeGeohash driverLat driverLong 7
        chargesOb = HU.getChargesOb state.data.activeRide.tripType state.data.cityConfig state.data.activeRide.driverVehicle
        waitTimeEnabledForCity = chargesOb.perMinCharges > 0.0
        nearestZone = case state.props.currentStage, state.data.config.feature.enableSpecialPickup of 
                        ST.HomeScreen, true -> findNearestSpecialZone driverLat driverLong
                        _, _ -> Nothing
        shouldUpdateGeoHash = isJust nearestZone && state.props.mapRendered
        newState = state { data{ currentDriverLat= driverLat,  currentDriverLon = driverLong, locationLastUpdatedTime = (convertUTCtoISC time "hh:mm a")}, props { specialZoneProps{ nearBySpecialZone = isJust nearestZone } }}
    void $ pure $ setValueToLocalStore IS_DRIVER_AT_PICKUP (show newState.data.activeRide.notifiedCustomer)
    void $ pure $ setValueToLocalStore LOCATION_UPDATE_TIME (convertUTCtoISC time "hh:mm a")
    continueWithCmd newState{ props{ specialZoneProps{ currentGeoHash = if shouldUpdateGeoHash then geoHash else "" }, mapRendered = true } } [ do
      if (getValueToLocalNativeStore IS_RIDE_ACTIVE == "false") then
        case nearestZone of
          Just zone -> do
            if state.props.specialZoneProps.currentGeoHash /= geoHash then do
              void $ launchAff $ flowRunner defaultGlobalState $ do
                push <- liftFlow $ getPushFn Nothing "HomeScreen"
                _ <- pure $ JB.exitLocateOnMap ""
                let _ = unsafePerformEffect $ runEffectFn1 JB.locateOnMap JB.locateOnMapConfig{ lat = driverLat, lon = driverLong, markerCallback = HU.onMarkerClickCallbackMapper push OnMarkerClickCallBack, markerCallbackForTags = ["selectedZoneGate"], geoJson = zone.geoJson, points = zone.gates, locationName = zone.locationName, navigateToNearestGate = false, specialZoneMarkerConfig{ showZoneLabel = true, labelActionImage = "ny_ic_navigation_blue_frame" }, enableMapClickListener = true }
                pure unit
              pure unit
            else pure unit
            checkPermissionAndUpdateDriverMarker false 
          Nothing -> do
            _ <- pure $ JB.exitLocateOnMap ""
            checkPermissionAndUpdateDriverMarker true
      else void $ launchAff $ flowRunner defaultGlobalState $ updateRouteOnMap newState driverLat driverLong

      case state.data.config.waitTimeConfig.enableWaitTime, state.props.currentStage, state.data.activeRide.notifiedCustomer, isJust state.data.advancedRideData, waitTimeEnabledForCity, state.data.activeRide.tripType of
        true, ST.RideAccepted, false, false, true, _ -> do
          let dist = getDistanceBwCordinates driverLat driverLong state.data.activeRide.src_lat state.data.activeRide.src_lon
              insideThreshold = dist <= state.data.config.waitTimeConfig.thresholdDist
          pure $ if insideThreshold && (getValueToLocalStore WAITING_TIME_STATUS == show ST.NoStatus) then UpdateAndNotify true else (UpdateLastLoc driverLat driverLong insideThreshold)
        _, ST.RideStarted, _, _, _, ST.Delivery -> do
          let dist = getDistanceBwCordinates driverLat driverLong state.data.activeRide.dest_lat state.data.activeRide.dest_lon
              insideThreshold = dist <= state.data.config.waitTimeConfig.thresholdDist
          pure $ if insideThreshold then UpdateAndNotify false else (UpdateLastLoc driverLat driverLong insideThreshold)
        _, _, _, _, _, _ -> pure $ UpdateLastLoc driverLat driverLong false
      ]

eval OpenHotspotScreen state = exit $ GotoHotspotScreen state

eval (OnMarkerClickCallBack tag lat lon) state = do
  case Number.fromString lat, Number.fromString lon of
    Just lat', Just lon' -> 
      if getDistanceBwCordinates state.data.currentDriverLat state.data.currentDriverLon lat' lon' > 0.200
        then do 
          if state.props.currentStage == ST.RideAccepted && state.data.vehicleType == "AUTO_RICKSHAW" && state.data.cityConfig.cityName == "Chennai"
            then
              void $ pure $ openNavigation lat' lon' "TWOWHEELER"
            else
              void $ pure $ openNavigation lat' lon' "DRIVE"
          continue state
      else 
        continueWithCmd state [do
          void $ openUrlInApp $ "https://www.google.com/maps/dir/?api=1&destination="<> lat <>","<> lon
          pure NoAction
        ]
    _, _ -> continue state

eval (UpdateLastLoc lat lon val) state = continue state {data { prevLatLon = Just {lat : lat, lon : lon, place : "", driverInsideThreshold : false}}}
  
eval (UpdateAndNotify atSource) state =
  continueWithCmd state
        [ do
            void $ launchAff $ EHC.flowRunner defaultGlobalState $ runExceptT $ runBackT
              $ do
                  push <- liftFlowBT $ getPushFn Nothing "HomeScreen"
                  GetRouteResp routeApiResponse <- case atSource of
                    true -> Remote.getRouteBT (Remote.makeGetRouteReq state.data.currentDriverLat state.data.currentDriverLon state.data.activeRide.src_lat state.data.activeRide.src_lon) "pickup"
                    false -> Remote.getRouteBT (Remote.makeGetRouteReq state.data.currentDriverLat state.data.currentDriverLon state.data.activeRide.dest_lat state.data.activeRide.dest_lon) "trip"
                  let shortRoute = (routeApiResponse Array.!! 0)
                  liftFlowBT $ push $ case shortRoute of
                      Just (Route route) -> do
                        case atSource of
                          true -> do
                            if route.distance <= state.data.config.waitTimeConfig.routeDistance then NotifyAPI 
                            else do
                              let dist = getDistanceBwCordinates state.data.currentDriverLat state.data.currentDriverLon state.data.activeRide.src_lat state.data.activeRide.src_lon
                              if dist <= state.data.config.waitTimeConfig.straightLineDist then NotifyAPI else UpdateLastLoc state.data.currentDriverLat state.data.currentDriverLon false
                          false -> do
                            if route.distance <= state.data.config.waitTimeConfig.routeDistance then NotifyReachedDestination
                            else do
                              let dist = getDistanceBwCordinates state.data.currentDriverLat state.data.currentDriverLon state.data.activeRide.dest_lat state.data.activeRide.dest_lon
                              if dist <= state.data.config.waitTimeConfig.straightLineDist then NotifyReachedDestination else UpdateLastLoc state.data.currentDriverLat state.data.currentDriverLon false
                      _ -> NoAction
                  pure unit
            pure NoAction
        ]

eval NotifyAPI state = updateAndExit state $ NotifyDriverArrived state 

eval NotifyReachedDestination state = updateAndExit state $ NotifyDriverReachedDestination state

eval (RideActiveAction activeRide mbAdvancedRide) state = do
  let currActiveRideDetails = activeRideDetail state activeRide
      advancedRideDetails = activeRideDetail state <$> mbAdvancedRide
      isOdoReadingsReq = checkIfOdometerReadingsRequired currActiveRideDetails.tripType activeRide
      updatedState = state { data {activeRide = currActiveRideDetails, advancedRideData = advancedRideDetails}, props{showAccessbilityPopup = (isJust currActiveRideDetails.disabilityTag), safetyAudioAutoPlay = false, isOdometerReadingsRequired = isOdoReadingsReq}}
      stage = (if currActiveRideDetails.status == NEW then (if (Array.any (\c -> c == ST.ChatWithCustomer) [state.props.currentStage, state.props.advancedRideStage]) then ST.ChatWithCustomer else ST.RideAccepted) else ST.RideStarted)
  updateAndExit updatedState $ UpdateStage stage updatedState
  where
    checkIfOdometerReadingsRequired tripType (RidesInfo ride) = (tripType == ST.Rental) && (maybe true (\val -> val) ride.isOdometerReadingsRequired)

eval RecenterButtonAction state = continue state

eval (SwitchDriverStatus status) state = do
  if not state.props.rcActive then do
    void $ pure $ toast $ getString LT.PLEASE_ADD_RC
    exit (DriverAvailabilityStatus state { props = state.props { goOfflineModal = false , rcDeactivePopup = true }} ST.Offline)
  else if state.data.paymentState.driverBlocked && not state.data.paymentState.subscribed then continue state { props{ subscriptionPopupType = ST.GO_ONLINE_BLOCKER }}
  else if state.data.paymentState.driverBlocked then continue state { data{paymentState{ showBlockingPopup = true}}}
  else if state.data.plansState.cityOrVehicleChanged then continue state {data { plansState { showSwitchPlanModal = true}}}
  else if ((getValueToLocalStore IS_DEMOMODE_ENABLED) == "true") then do
    continueWithCmd state [ do
          _ <- pure $ setValueToLocalStore IS_DEMOMODE_ENABLED "false"
          _ <- pure $ toast (getString LT.DEMO_MODE_DISABLED)
          _ <- pure $  deleteValueFromLocalStore DEMO_MODE_PASSWORD
          _ <- getCurrentPosition (showDriverMarker "ny_ic_auto" true) constructLatLong
          pure NoAction
          ]
  else if state.props.driverStatusSet == status then continue state
  else if not state.data.isVehicleSupported && status /= ST.Offline && state.props.rcActive then continue state { props{ vehicleNSPopup = true }}
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
eval AddAlternateNumberAction state = do
  let curr_time = getCurrentUTC ""
  let last_attempt_time = getValueToLocalStore SET_ALTERNATE_TIME
  let time_diff = runFn2 differenceBetweenTwoUTC curr_time last_attempt_time
  if(time_diff <= 600 && time_diff > 0) then do
    pure $ toast $ getString LT.TOO_MANY_ATTEMPTS_PLEASE_TRY_AGAIN_LATER
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
          BannerCarousel.AdvancedRide -> pure (AdvancedRideBannerAction (Banner.OnClick))
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

eval (AdvancedRideBannerAction (Banner.OnClick)) state = continue state{props{showAdvancedRidePopUp = true}}

eval (AccessibilityBannerAction (Banner.OnClick)) state = continue state{props{showGenericAccessibilityPopUp = true}}

eval (ToggleBonusPopup) state = continue state { data {activeRide {waitTimeInfo =false}}, props { showBonusInfo = not state.props.showBonusInfo } }

eval (RequestInfoCardAction RequestInfoCard.Close) state = continue state { data {activeRide {waitTimeInfo =false}}, props { showBonusInfo = false } }

eval (RequestInfoCardAction RequestInfoCard.BackPressed) state = continue state { data {activeRide {waitTimeInfo =false}}, props { showBonusInfo = false } }

eval (RequestInfoCardAction RequestInfoCard.NoAction) state = continue state

eval (SpecialZoneCardAC _ ) state = continue state { props { specialZoneProps{ specialZonePopup = false }} }

eval RemovePaymentBanner state = if state.data.paymentState.blockedDueToPayment then
                                                  continue state else continue state {data { paymentState {paymentStatusBanner = false}}}
eval (LinkAadhaarPopupAC PopUpModal.OnButton1Click) state = exit $ AadhaarVerificationFlow state

eval (LinkAadhaarPopupAC PopUpModal.DismissPopup) state = continue state {props{showAadharPopUp = false}}
eval (PopUpModalAccessibilityAction PopUpModal.OnButton1Click) state = continueWithCmd state{props{showAccessbilityPopup = false, safetyAudioAutoPlay = false}} [ do 
  _ <- pure $ pauseYoutubeVideo unit
  void $ runEffectFn1 removeMediaPlayer ""
  pure NoAction
  ] 
  
eval (PopUpModalAccessibilityAction PopUpModal.NoAction) state = continueWithCmd state [do
  pure $ PopUpModalAccessibilityAction PopUpModal.OnButton1Click
]

eval (PopUpRentalInfoAction PopUpModal.OnButton1Click) state = continue state{props{rentalInfoPopUp = false, intercityInfoPopUp = false}} 

eval (PopUpRentalInfoAction PopUpModal.OnSecondaryTextClick) state = do 
  let link = case state.data.linkedVehicleCategory of
              "AUTO_RICKSHAW" -> state.data.config.rentalRideVideoConfig.auto 
              _ -> state.data.config.rentalRideVideoConfig.cab 
  continueWithCmd state [ do 
    void $ JB.openUrlInApp link
    pure AfterRender
  ]

eval (PopUpModalAdvancedRideAction PopUpModal.OnButton1Click) state = continueWithCmd state{props{showAdvancedRidePopUp = true}} [ do 
  void $ openUrlInApp $ state.data.cityConfig.advancedRidePopUpYoutubeLink
  pure NoAction
  ] 

eval (PopUpModalAdvancedRideAction PopUpModal.OnButton2Click) state = continue state {props{showAdvancedRidePopUp = false}}

eval (PopUpModalAdvancedRideAction PopUpModal.DismissPopup) state = continue state {props{showAdvancedRidePopUp = false}}

eval (GenericAccessibilityPopUpAction PopUpModal.OnButton1Click) state = continueWithCmd state{props{showAccessbilityPopup = false, safetyAudioAutoPlay = false, showGenericAccessibilityPopUp = false}} [ do 
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
  _ <- pure $ showDialer state.data.cityConfig.supportNumber false
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
                  bannerTitle = getString LT.YOUR_PREVIOUS_PAYMENT_IS_PENDING,
                  bannerTitleColor = Color.dustyRed,
                  banneActionText = getString LT.CONTACT_SUPPORT,
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
eval (RatingCardAC (RatingCard.PrimaryButtonAC PrimaryButtonController.OnClick)) state = exit $ PostRideFeedback state {data {activeRide = fromMaybe state.data.activeRide state.data.advancedRideData, currentRideData = state.data.advancedRideData, advancedRideData = Nothing}, props {showRideRating = false, showRideCompleted = false}}

eval (RCDeactivatedAC PopUpModal.OnButton1Click) state = exit $ GoToVehicleDetailScreen state 

eval (RCDeactivatedAC PopUpModal.OnButton2Click) state = continue state {props {rcDeactivePopup = false}}

eval (CoinsPopupAC PopUpModal.OnButton1Click) state = do
  let _ = unsafePerformEffect $ logEvent state.data.logField "ny_driver_coins_intro_popup_check_now_click"
  exit $ EarningsScreen state true

eval (CoinsPopupAC PopUpModal.OptionWithHtmlClick) state = do
  void $ pure $ setValueToLocalNativeStore COINS_POPUP_SHOWN_DATE (getCurrentUTC "")
  void $ pure $ incrementValueOfLocalStoreKey INTRODUCING_YATRI_POINTS_POPUP_LIMIT
  continue state {props {showCoinsPopup = false}}

eval (CoinEarnedPopupAC PopUpModal.OnButton1Click) state = do
  void $ pure $ updateCoinPopupLocalStoreVal state
  let newState = state { props { coinPopupType = ST.NO_COIN_POPUP}}
  case state.props.coinPopupType of
    ST.REFER_AND_EARN_COIN -> updateAndExit newState $ GoToReferralScreen
    ST.CONVERT_COINS_TO_CASH -> updateAndExit newState $ EarningsScreen newState true 
    ST.EIGHT_RIDE_COMPLETED -> updateAndExit newState $ EarningsScreen newState true 
    ST.TWO_RIDE_COMPLETED -> updateAndExit newState $ EarningsScreen newState true 
    ST.FIVE_RIDE_COMPLETED -> updateAndExit newState $ EarningsScreen newState true 
    ST.SIX_RIDE_COMPLETED -> updateAndExit newState $ EarningsScreen newState true 
    _ -> continue newState

eval (CoinEarnedPopupAC PopUpModal.OptionWithHtmlClick) state = do
  void $ pure $ updateCoinPopupLocalStoreVal state
  let newState = state { props { coinPopupType = ST.NO_COIN_POPUP}}
  case state.props.coinPopupType of
    ST.EIGHT_RIDE_COMPLETED -> updateAndExit newState $ EarningsScreen newState true 
    ST.RIDE_MORE_EARN_COIN -> updateAndExit newState $ EarningsScreen newState true 
    _ -> continue newState

eval (CoinEarnedPopupAC PopUpModal.DismissPopup) state = do
  let newState = state { props { coinPopupType = ST.NO_COIN_POPUP}}
  continue newState

eval (AccessibilityBannerAction (Banner.OnClick)) state = continue state{props{showGenericAccessibilityPopUp = true}}

eval (PaymentBannerAC (Banner.OnClick)) state = do
  _ <- pure $ showDialer state.data.cityConfig.supportNumber false
  continue state

eval (SetBannerItem bannerItem) state = continue state{data{bannerData{bannerItem = Just bannerItem}}}

eval ToggleStatsModel state = continue state { props { isStatsModelExpanded = not state.props.isStatsModelExpanded } }

eval (GoToEarningsScreen showCoinsView) state = do
  let _ = unsafePerformEffect $ logEventWithMultipleParams state.data.logField  "ny_driver_coins_click_on_homescreen" $ [{key : "CoinBalance", value : unsafeToForeign state.data.coinBalance}]
  exit $ EarningsScreen state showCoinsView 

eval (DriverStats driverStats) state = do
  exit $ DriverStatsUpdate driverStats state{props{routeVisible = false, mapRendered = true}}

eval AccessibilityHeaderAction state = 
  if state.data.activeRide.bookingFromOtherPlatform then
    continue state{ props{ showInterOperablePopUp = true } }
  else update state

eval (PopUpModalInterOperableAction PopUpModal.OnButton2Click) state =
  continue state{ props{ showInterOperablePopUp = false } }

eval (IsAcWorkingPopUpAction PopUpModal.OnButton1Click) state = exit $ UpdateAirConditioned state true

eval (IsAcWorkingPopUpAction PopUpModal.OnButton2Click) state = exit $ UpdateAirConditioned state false

eval (IsAcWorkingPopUpAction PopUpModal.DismissPopup) state = continue state{props{showAcWorkingPopup = Just false}}

eval (ACExpController action) state = do
  let acVideoLink = "https://www.youtube.com/watch?v=MbgxZkqxPLQ"
      newState = state { props { acExplanationPopup = false } }
  case action of
    PopUpModal.DismissPopup -> continue newState
    PopUpModal.OnButton2Click -> continue newState
    PopUpModal.OnCoverImageClick -> continueWithCmd newState [pure $ OpenLink acVideoLink]
    PopUpModal.OnButton1Click -> continueWithCmd newState [pure $ OpenLink acVideoLink]
    _ -> continue state

eval (OpenLink link) state = continueWithCmd state [ do 
  void $ JB.openUrlInApp link
  pure AfterRender
  ]
  
eval (TollChargesPopUpAC PopUpModal.OnButton2Click) state = continue state {data { toll  {showTollChargePopup = false}}}

eval (TollChargesAmbigousPopUpAC PopUpModal.OnButton2Click) state = continue state {data {toll {showTollChargeAmbigousPopup = false}}}

eval (SwitchBookingStage stage) state = do
  if state.props.bookingStage == stage then continue state
  else do
    let currentRideData = if stage == CURRENT then fromMaybe state.data.activeRide state.data.currentRideData else state.data.activeRide
    let advancedRideData = if stage == ADVANCED then fromMaybe state.data.activeRide state.data.advancedRideData else state.data.activeRide
    let activeRideData = if stage == CURRENT then currentRideData else advancedRideData
    void $ pure $ setValueToLocalStore LOCAL_STAGE (show $ fetchStageFromRideStatus activeRideData)
    exit $ UpdateRouteOnStageSwitch state {
      data {activeRide = activeRideData, currentRideData = Just currentRideData},
      props {bookingStage = stage, currentStage = fetchStageFromRideStatus activeRideData}
    }

eval (PlanListResponse (API.UiPlansResp plansListResp)) state = do
  let isTamilSelected = (getLanguageLocale languageKey) == "TA_IN"
  let plans = map (\plan -> SubscriptionTransformer.transformPlan isTamilSelected $ SubscriptionTransformer.getPlanCardConfig plan false false state.data.config.subscriptionConfig) plansListResp.list
  continue state { data { plansState { plansList = plans, selectedPlan = Array.head plans, showSwitchPlanModal = true }}}

eval (SelectPlansModalAction SelectPlansModal.Dismiss) state = continue state {data {plansState {showSwitchPlanModal = false}}}

eval (SelectPlansModalAction SelectPlansModal.Support) state = do
  void $ pure $ showDialer state.data.cityConfig.supportNumber false
  continue state

eval (SelectPlansModalAction (SelectPlansModal.PrimaryButtonAC PrimaryButtonController.OnClick)) state = 
  case state.data.plansState.selectedPlan of
    Nothing -> continue state
    Just plan -> exit $ SwitchPlan plan state

eval (SelectPlansModalAction (SelectPlansModal.PlanCardAction item _)) state = continue state {data {plansState {selectedPlan = Just item}}}


eval RideRequestsList state = exit $ GoToRideReqScreen state 

eval (HomeScreenBannerCountDownTimer seconds status timerID) state = do
  let id = "bannerTimer_" <> state.data.activeRide.id
  if status == "EXPIRED" || timerID == id then do
    void $ pure $ TF.clearTimerWithId timerID
    continue state {props {homeScreenBannerVisibility  = false}}
  else if (Array.elem state.props.currentStage [ST.RideAccepted, ST.RideStarted, ST.ChatWithCustomer])  then do
    void $ pure  $ TF.clearTimerWithId timerID
    continue state
  else
    continue $ state { data { homeScreenBannerTimer = (seconds), homeScreenBannerTimerID = timerID } }

eval (OnRideBannerCountDownTimer seconds status onRideBannerTimerID) state = do
  let currentId = "onRideBannerTimer_" <> state.data.activeRide.id
  if (onRideBannerTimerID == currentId) || status == "EXPIRED" || (Array.notElem state.props.currentStage [ST.RideAccepted, ST.RideStarted, ST.ChatWithCustomer]) then do
    void $ pure $ TF.clearTimerWithId onRideBannerTimerID
    continue state 
  else
    continue $ state { data { onRideBannerTimer = (seconds), onRideBannerTimerID = onRideBannerTimerID } }

eval (UpComingRideDetails  resp) state = do
   let scheduledRide = activeRideDetail state <$> resp
   continue state {data {upcomingRide = scheduledRide} , props {checkUpcomingRide = false, homeScreenBannerVisibility = true , rideRequestPill{pillShimmerVisibility = false}}}

eval ScheduledRideBannerClick state  =  exit $ GoToRideSummaryScreen state
eval (UpdateRetryRideList retry) state = continue state {props {retryRideList = retry}}

eval HideBusOnline state = continue state { props { setBusOnline = false } }

eval (BusNumber val) state = do
  let newState = state {data = state.data { bus_number = DS.toUpper val }}
  continue newState

eval (VOIPCallBack callId status rideId errorCode driverFlag networkType networkStrength merchantId) state = do
  let req = {
      callId : callId,
      callStatus : status,
      rideId : rideId,
      errorCode : if (errorCode < 0 ) then Nothing else Just errorCode,
      userType : if (driverFlag == 1) then "DRIVER" else "RIDER",
      networkType : networkType,
      networkQuality : networkStrength,
      merchantId : merchantId,
      merchantOperatingCity : getValueToLocalStore DRIVER_LOCATION
    }
  continueWithCmd state [ do
    void $ launchAff $ EHC.flowRunner defaultGlobalState $ do
      resp :: (Either ErrorResponse API.ApiSuccessResult) <-  HelpersAPI.callApi $ API.VoipCallReq req
      pure unit
    pure NoAction
  ]

  
eval (RideEndWithStopsPopupAction PopUpModal.OnButton1Click) state = continueWithCmd state {props {showEndRideWithStopPopup = false}} [pure $ PopUpModalAction PopUpModal.OnButton2Click]

eval (RideEndWithStopsPopupAction PopUpModal.OnButton2Click) state = continue state {props {showEndRideWithStopPopup = false}}

eval (RideEndWithStopsPopupAction PopUpModal.DismissPopup) state = continue state {props {showEndRideWithStopPopup = false}}

eval (UpdateRouteInState route) state = continue state{data{route = route}}
 
eval (ParcelIntroductionPopup action) state = do
  let newState = state { props { showParcelIntroductionPopup = false } }
      city = getValueToLocalStore DRIVER_LOCATION
      parcelConfig = RC.getParcelConfig city
  case action of
    PopUpModal.DismissPopup -> continue newState
    PopUpModal.OnCoverImageClick -> onClick newState parcelConfig.introductionVideo
    PopUpModal.OnButton1Click -> onClick newState parcelConfig.introductionVideo
    _ -> continue state
  where
    onClick newState parcelIntroductionVideo = do
      let _ = unsafePerformEffect $ logEvent state.data.logField "ny_driver_parcel_introduction_clicked"
      void $ pure $ setValueToLocalStore SHOW_PARCEL_INTRODUCTION_POPUP "false"
      continueWithCmd newState [pure $ OpenLink parcelIntroductionVideo]

eval (MetroWarriorSwitchAction SwitchButtonView.OnClick) state = exit $ UpdateToggleMetroWarriors state

eval ClickMetroWarriors state = exit $ GoToMetroWarriors state

eval (MetroWarriorPopupAC PopUpModal.OnButton1Click) state = do
  let newState = state { props { showMetroWarriorWarningPopup = false }}
  updateAndExit newState $ EnableGoto newState state.data.driverGotoState.selectedGoTo

eval (MetroWarriorPopupAC PopUpModal.OnButton2Click) state = continue state { props { showMetroWarriorWarningPopup = false }}

eval (UpdateState newState) _ = continue newState 

eval _ state = update state 

checkPermissionAndUpdateDriverMarker :: Boolean -> Effect Unit
checkPermissionAndUpdateDriverMarker toAnimateCamera = do
  conditionA <- isLocationPermissionEnabled unit
  conditionB <- isLocationEnabled unit
  if conditionA && conditionB then do
    _ <- getCurrentPosition (showDriverMarker "ic_vehicle_side" toAnimateCamera) constructLatLong
    pure unit
    else do
      _ <- requestLocation unit
      pure unit

showDriverMarker :: String -> Boolean -> ST.Location -> Effect Unit
showDriverMarker marker toAnimateCamera location = do
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
      if toAnimateCamera then
        animateCamera location.lat location.lon zoomLevel "ZOOM"
      else pure unit

updateDemoLocationIcon :: Number -> Number -> Effect Unit
updateDemoLocationIcon lat lng = do
  void $ showMarker defaultMarkerConfig {markerId = "ny_ic_demo_location", pointerIcon = "ny_ic_demo_location"} lat lng 100 0.5 0.5 (getNewIDWithTag "CustomerHomeScreen")
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
      isSpecialPickupZone = checkSpecialPickupZone ride.specialLocationTag
      tripType = rideTypeConstructor ride.tripCategory
      (LocationInfo sourceAddress) = ride.fromLocation
      sourceCity =  fromMaybe "" sourceAddress.city
      (LocationInfo destinationAddress) =  fromMaybe dummyLocationInfo ride.toLocation
      destinationCity = fromMaybe "" destinationAddress.city
      roundTrip = ride.roundTrip
      returnTime  = fromMaybe ""  ride.returnTime

      -- _ = setValueToLocalStore HAS
  in 
  {
  id : ride.id,
  source : (decodeAddress ride.fromLocation true),
  sourceArea : ride.fromLocation ^. _area,
  destination : (\toLocation -> decodeAddress toLocation true) <$> ride.toLocation,
  destinationArea : ride.toLocation >>= (\toLocation -> toLocation ^. _area),
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
              "UPCOMING"  -> UPCOMING
              _ -> COMPLETED,
  distance : (toNumber ride.estimatedDistance),
  duration : state.data.activeRide.duration,
  tripDuration : ride.estimatedDuration,
  actualRideDuration : ride.actualDuration,
  riderName : fromMaybe "" ride.riderName,
  estimatedFare : ride.driverSelectedFare + ride.estimatedBaseFare,
  notifiedCustomer : Array.any (_ == getValueToLocalStore WAITING_TIME_STATUS) [(show ST.PostTriggered), (show ST.Triggered), (show ST.Scheduled), (show ST.NotTriggered)],
  exoPhone : ride.exoPhone,
  waitTimeSeconds :if ride.status == "INPROGRESS" && isTimerValid && ride.bookingType /= Just ADVANCED then waitTime else -1,
  rideCreatedAt : ride.createdAt,
  waitTimeInfo : if ride.bookingType /= Just ADVANCED then state.data.activeRide.waitTimeInfo else false,
  requestedVehicleVariant : ride.requestedVehicleVariant,
  waitTimerId : if ride.bookingType /= Just ADVANCED then state.data.activeRide.waitTimerId else "",
  enableFrequentLocationUpdates : fromMaybe false ride.enableFrequentLocationUpdates,
  specialLocationTag :  if isJust ride.disabilityTag then Just "Accessibility"
                        else if isSpecialPickupZone then Just "SpecialZonePickup"
                        else if isJust ride.driverGoHomeRequestId then Just "GOTO"
                        else if isSafetyRide then Just "Safety"
                        else ride.specialLocationTag, --  "None_SureMetro_PriorityDrop",--"GOTO",
  disabilityTag : case ride.disabilityTag of
              Just "BLIND_LOW_VISION" -> Just ST.BLIND_AND_LOW_VISION
              Just "HEAR_IMPAIRMENT" -> Just ST.HEAR_IMPAIRMENT
              Just "LOCOMOTOR_DISABILITY" -> Just ST.LOCOMOTOR_DISABILITY
              Just "OTHER" -> Just ST.OTHER_DISABILITY
              Just _ -> Just ST.OTHER_DISABILITY
              Nothing -> if isSpecialPickupZone then 
                            Just ST.SPECIAL_ZONE_PICKUP
                         else if isSafetyRide then
                            Just ST.SAFETY 
                         else Nothing,
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
  endOdometerReading : (\(API.OdometerReading {value}) -> value) <$> ride.endOdometerReading,
  driverVehicle : ride.vehicleVariant,
  serviceTier : ride.vehicleServiceTierName,
  capacity : ride.vehicleCapacity,
  estimatedTollCharges :  fromMaybe 0.0 ride.estimatedTollCharges,
  acRide : ride.isVehicleAirConditioned,
  bapName : transformBapName $ fromMaybe "" ride.bapName,
  bookingFromOtherPlatform : not ride.isValueAddNP,
  sourceCity : sourceCity,
  destinationCity : Just destinationCity,
  roundTrip : roundTrip,
  returnTime : returnTime,
  parkingCharge : fromMaybe 0.0 ride.parkingCharge,
  extraFromLocationInfo : (ride.fromLocation) ^. _extras,
  extraToLocationInfo : ride.toLocation >>= (\toLocation -> (toLocation ^. _extras)),
  senderInstructions : (ride.fromLocation) ^. _instructions,
  receiverInstructions : ride.toLocation >>= (\toLocation -> toLocation ^. _instructions),
  notifiedReachedDestination : Array.any (_ == getValueToLocalStore WAITING_TIME_STATUS) [(show ST.DestinationReachedTriggered)],
  senderPersonDetails : ride.senderDetails,
  receiverPersonDetails : ride.receiverDetails,
  stops : Array.sortBy (\(API.Stop s1) (API.Stop s2) -> 
                            case s1.stopInfo, s2.stopInfo of
                              Just (API.StopInformation s1stopInfo), Just (API.StopInformation s2stopInfo) -> compare s1stopInfo.stopOrder s2stopInfo.stopOrder
                              _,_ -> LT
                            ) $ fromMaybe [] ride.stops
}
  where 
    getAddressFromStopLocation :: Maybe API.StopLocation -> Maybe String
    getAddressFromStopLocation  stopLocation = (\(API.StopLocation {address,lat,lon}) -> decodeAddress (getLocationInfoFromStopLocation address lat lon) true) <$>  stopLocation


cancellationReasons :: ST.HomeScreenState -> Array Common.OptionButtonList
cancellationReasons state = [
        {
          reasonCode: "VEHICLE_ISSUE"
        , description: (getString LT.VEHICLE_ISSUE)
        , textBoxRequired : false
        , subtext: Nothing
        },
        {
          reasonCode: "PICKUP_TOO_FAR"
        , description: (getString LT.PICKUP_TOO_FAR)
        , textBoxRequired : false
        , subtext: Nothing
        },
        {
          reasonCode: "TRAFFIC_JAM"
        , description: (getString LT.TRAFFIC_JAM)
        , textBoxRequired : false
        , subtext: Nothing
        },
        {
          reasonCode: "CUSTOMER_WAS_RUDE"
        , description: (getString LT.CUSTOMER_WAS_RUDE)
        , textBoxRequired : false
        , subtext: Nothing
        }] <> 
        (if state.data.activeRide.tripType == ST.Delivery then deliveryCancellationReasons else customerNotPickingCall) 
        <> otherReasons
    where
      customerNotPickingCall = [{
          reasonCode: "CUSTOMER_NOT_PICKING_CALL"
        , description: (StringsV2.getStringV2 LT2.customer_not_picking_call)
        , textBoxRequired : false
        , subtext: Nothing
        }]
      otherReasons = [{
          reasonCode: "OTHER"
        , description: (getString LT.OTHER)
        , textBoxRequired : true
        , subtext: Nothing
        }]

deliveryCancellationReasons :: Array Common.OptionButtonList
deliveryCancellationReasons = 
  [{
    reasonCode: "CUSTOMER_NOT_PICKING_CALL"
  , description: (StringsV2.getStringV2 LT2.parcel_is_inappropriate)
  , textBoxRequired : false
  , subtext: Nothing
  },
  {
    reasonCode: "SENDER_UNAVAILABLE_UNREACHABLE"
  , description: (StringsV2.getStringV2 LT2.sender_unavailable_unreachable)
  , textBoxRequired : false
  , subtext: Nothing
  },
  {
    reasonCode: "SENDER_ASKING_DIFFERENT_LOCATION"
  , description: (StringsV2.getStringV2 LT2.sender_asking_different_location)
  , textBoxRequired : false
  , subtext: Nothing
  }]

dummyCancelReason :: Common.OptionButtonList
dummyCancelReason =  {
        reasonCode : ""
        , description :""
        , textBoxRequired : false
        , subtext : Nothing
        }


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
      currentPeekHeight = headerLayout.height  + contentLayout.height + (if RideActionModal.showTag (rideActionModalConfig state) then (labelLayout.height + 6) else 0)
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
  -- <> (if state.props.currentStage == ST.HomeScreen && HU.shouldShowPurpleVideos state then [accessbilityBannerConfig state BannerCarousal] else [])
  <> getRemoteBannerConfigs
  -- <> (if state.data.cityConfig.enableAdvancedBooking && (not DS.null state.data.cityConfig.advancedRidePopUpYoutubeLink) then [advancedRideBannerCarousel state BannerCarousal] else [])
  where 
    getRemoteBannerConfigs :: Array (BannerCarousel.Config (BannerCarousel.Action -> Action))
    getRemoteBannerConfigs = do 
      let driverLocation = toLower $ getValueToLocalStore DRIVER_LOCATION
          language = getLanguage $ getLanguageLocale languageKey
          configName = "driver_carousel_banner" <> language
          datas = RC.carouselConfigData driverLocation configName "driver_carousel_banner_en" (getValueFromWindow "DRIVER_ID") state.data.linkedVehicleCategory state.data.linkedVehicleVariant
      BannerCarousel.remoteConfigTransformer datas BannerCarousal Nothing
    getLanguage :: String -> String
    getLanguage lang = 
      let language = toLower $ take 2 lang
      in if not (null language) then "_" <> language else "_en"


isSafetyPeriod :: ST.HomeScreenState -> String -> Boolean
isSafetyPeriod state riseStartTime = 
  let timeStamp = EHC.convertUTCtoISC riseStartTime "HH:mm:ss"
  in JB.withinTimeRange state.data.config.safetyRide.startTime state.data.config.safetyRide.endTime timeStamp

updateCoinPopupLocalStoreVal :: ST.HomeScreenState -> Effect Unit
updateCoinPopupLocalStoreVal state = do
  let popupInfo = getValueFromCache "COIN_EARNED_POPUP_TYPE" getCoinPopupStatus
      newPopupInfo = case state.props.coinPopupType of
        ST.RIDE_MORE_EARN_COIN -> popupInfo { rideMoreEarnCoin = getCurrentUTC "" }
        ST.TWO_MORE_RIDES -> popupInfo { twoMoreRides = getCurrentUTC "" }
        ST.ONE_MORE_RIDE -> popupInfo { oneMoreRide = getCurrentUTC "" }
        ST.EIGHT_RIDE_COMPLETED -> popupInfo { eightRideCompleted = getCurrentUTC "" }
        ST.FIVE_RIDE_COMPLETED -> popupInfo { fiveRideCompleted = getCurrentUTC "" }
        ST.SIX_RIDE_COMPLETED -> popupInfo { sixRideCompleted = getCurrentUTC "" }
        ST.TEN_RIDE_COMPLETED -> popupInfo { tenRideCompleted = getCurrentUTC "" }
        ST.TWO_RIDE_COMPLETED -> popupInfo { twoRideCompleted = getCurrentUTC "" }
        ST.REFER_AND_EARN_COIN -> popupInfo { referAndEarnCoin = getCurrentUTC "" }
        ST.CONVERT_COINS_TO_CASH -> popupInfo { convertCoinsToCash = getCurrentUTC "" }
        _ -> popupInfo
  void $ pure $ setValueToCache "COIN_EARNED_POPUP_TYPE" newPopupInfo stringifyJSON

getCoinPopupStatus :: String -> ST.CoinEarnedPopupTypeShown
getCoinPopupStatus key = decodeForeignAny (parseJSON (getKeyInSharedPrefKeys key)) {rideMoreEarnCoin : "" , twoMoreRides : "", oneMoreRide : "", eightRideCompleted : "", fiveRideCompleted : "", sixRideCompleted : "", tenRideCompleted : "", twoRideCompleted : "", referAndEarnCoin : "", convertCoinsToCash : ""}

fetchStageFromRideStatus :: ST.ActiveRide -> ST.HomeScreenStage
fetchStageFromRideStatus activeRide = 
  case activeRide.status of
    NEW -> ST.RideAccepted
    INPROGRESS -> ST.RideStarted
    COMPLETED -> ST.RideCompleted
    CANCELLED -> ST.HomeScreen
    _ -> ST.HomeScreen

updateRouteOnMap :: ST.HomeScreenState -> Number -> Number-> Flow GlobalState Unit
updateRouteOnMap state lat lon= do
  let leftStops = Array.filter (\(API.Stop item) -> maybe true (\(API.StopInformation stopInfo) -> isNothing stopInfo.stopEndLatLng) item.stopInfo) state.data.activeRide.stops
      hasStops = not $ Array.null leftStops
      mbUpcomingStop = HU.getUpcomingStop state.data.activeRide.stops
  case mbUpcomingStop of
    Just (API.Stop upcomingStop) -> do
      let shortRoute = (state.data.route Array.!! 0)
          upcomingStopLocation = {
              lat : (unwrap upcomingStop.location).lat
            , lng : (unwrap upcomingStop.location).lon
          }
          srcDestConfig = HU.getSrcDestConfig state
          srcLat = srcDestConfig.srcLat
          srcLon = srcDestConfig.srcLon
          destLat = srcDestConfig.destLat
          destLon = srcDestConfig.destLon
          source = srcDestConfig.source
          destination = srcDestConfig.destination
          city = EHU.getCityFromString $ getValueToLocalStore DRIVER_LOCATION
          driverVehicle = getValueToLocalStore VEHICLE_VARIANT
          routeType = if hasStops then "DRIVER_LOCATION_UPDATE" else "NORMAL"
          sourcePointerIcon = if hasStops then EHU.getCitySpecificMarker city driverVehicle (Just $ show state.props.currentStage) else "ny_ic_src_marker"
          srcMarkerConfig = JB.defaultMarkerConfig{ markerId = sourcePointerIcon, pointerIcon = sourcePointerIcon, primaryText = source }
          destinationMarkericon = if state.props.currentStage == ST.RideAccepted && hasStops then "ny_ic_src_marker" else "ny_ic_dest_marker"
          destMarkerConfig =  JB.defaultMarkerConfig{ markerId = "ny_ic_dest_marker", pointerIcon = destinationMarkericon, primaryText = destination, anchorU = 0.5, anchorV = 1.0}
      case shortRoute of
        Just (Route route) -> do
          let extendedPath = JB.getExtendedPath $ Remote.walkCoordinates route.points
          locationResp <- liftFlow $ JB.isCoordOnPath extendedPath lat lon (route.distance / route.duration)
          checkUpcomingStop <- liftFlow $ JB.isCoordOnPath extendedPath upcomingStopLocation.lat upcomingStopLocation.lng (route.distance / route.duration)
          if locationResp.isInPath && checkUpcomingStop.isInPath then do
            let newPoints = { points : locationResp.points}
            liftFlow $ runEffectFn1 JB.updateRoute JB.updateRouteConfig { json = newPoints, destMarkerConfig = destMarkerConfig, pureScriptID = (getNewIDWithTag "DriverTrackingHomeScreenMap"),  polylineKey = "DEFAULT", srcMarker = sourcePointerIcon, locationName = destination}
          else updateRoute state{data{route = []}}
        _ -> updateRoute state
    Nothing -> updateRoute state 
  where
    drawRouteOnMap srcLat srcLon leftStops destLat destLon srcMarkerConfig destMarkerConfig routeType = do
      let points = (Array.singleton $ API.LatLong {lat : srcLat, lon : srcLon}) 
                          <> (if state.props.currentStage == ST.RideAccepted 
                                then [] 
                                else map (\(API.Stop item) -> getLatlon item.location ) leftStops)
                          <> (Array.singleton $ API.LatLong {lat : destLat, lon : destLon}) 
          getLatlon (API.LocationInfo location) = API.LatLong {lat : location.lat, lon : location.lon}
      resp <- Remote.getRoute (Remote.makeGetRouteReqArray points) $ if state.props.currentStage == ST.RideAccepted then "pickup" else "trip"
      case resp of
        Right (GetRouteResp routeApiResponse) -> do
          let shortRoute = (routeApiResponse Array.!! 0)
          case shortRoute of
            Just (Route route) -> do
              let coor = Remote.walkCoordinates route.points
              push <- liftFlow $ getPushFn Nothing "HomeScreen"
              liftFlow $ push $ UpdateRouteInState routeApiResponse
              void $ pure $ removeAllPolylines ""
              let normalRoute = JB.mkRouteConfig coor srcMarkerConfig destMarkerConfig Nothing routeType "LineString" true JB.DEFAULT (mapRouteConfig "" "" false getPolylineAnimationConfig) 
              liftFlow $ JB.drawRoute [normalRoute] (getNewIDWithTag "DriverTrackingHomeScreenMap")
              pure unit
            Nothing -> pure unit 
        Left err -> pure unit

updateRoute :: ST.HomeScreenState -> Flow GlobalState Unit
updateRoute state = do
  void $ pure $ JB.exitLocateOnMap ""   
  void $ pure $ JB.removeAllMarkers ""
  push <- liftFlow $ getPushFn Nothing "HomeScreen"
  let srcDestConfig = HU.getSrcDestConfig state
      hasStops = not $ Array.null state.data.activeRide.stops
      srcLat = srcDestConfig.srcLat
      srcLon = srcDestConfig.srcLon
      destLat = srcDestConfig.destLat
      destLon = srcDestConfig.destLon
      source = srcDestConfig.source
      destination = srcDestConfig.destination
      routeType = if state.props.currentStage == ST.RideAccepted then "pickup" else "trip"
      city = EHU.getCityFromString $ getValueToLocalStore DRIVER_LOCATION
      driverVehicle = getValueToLocalStore VEHICLE_VARIANT
      sourcePointerIcon = if hasStops then EHU.getCitySpecificMarker city driverVehicle (Just $ show state.props.currentStage) else "ny_ic_src_marker"
      destinationMarkericon = if state.props.currentStage == ST.RideAccepted && hasStops then "ny_ic_src_marker" else "ny_ic_dest_marker"
      srcMarkerConfig = JB.defaultMarkerConfig{ markerId = sourcePointerIcon, pointerIcon = sourcePointerIcon, primaryText = source }
      destMarkerConfig = JB.defaultMarkerConfig{ markerId = "ny_ic_dest_marker", pointerIcon = destinationMarkericon, primaryText = destination, anchorU = 0.5, anchorV = 1.0}
      drawRouteType = if hasStops then "DRIVER_LOCATION_UPDATE" else "NORMAL"

  if (state.data.activeRide.tripType == ST.Rental) && (state.props.currentStage == ST.RideStarted ) && isNothing state.data.activeRide.nextStopAddress then do
      liftFlow $ push $ UpdateState state{ props { routeVisible = true } }
      void $ pure $ removeAllPolylines ""
  else if state.data.config.feature.enableSpecialPickup && state.props.currentStage == ST.RideAccepted && state.data.activeRide.specialLocationTag == Just "SpecialZonePickup" then do
    liftFlow $ push $ UpdateState state{ props { routeVisible = true } }
    let specialPickupZone = HU.findSpecialPickupZone destLat destLon
    case specialPickupZone of
      Just pickupZone -> do
        void $ pure $ removeAllPolylines ""
        let _ = unsafePerformEffect $ runEffectFn1 JB.locateOnMap JB.locateOnMapConfig{ lat = destLat
                                                                                      , lon = destLon
                                                                                      , geoJson = pickupZone.geoJson
                                                                                      , points = pickupZone.gates
                                                                                      , locationName = pickupZone.locationName
                                                                                      , navigateToNearestGate = false
                                                                                      , specialZoneMarkerConfig { showZoneLabel = true
                                                                                                                , showLabelActionImage = true }
                                                                                      , locateOnMapPadding = { left : 2.0, top : 2.0, right : 2.0, bottom : 4.0 } }
        pure unit
      Nothing -> pure unit
  else if state.props.showDottedRoute then do
    let coors = (Remote.walkCoordinate srcLon srcLat destLon destLat)
    liftFlow $ push $ UpdateState state{ props { routeVisible = true } }
    let _ = removeAllPolylines ""
        normalRoute = JB.mkRouteConfig coors srcMarkerConfig destMarkerConfig Nothing "NORMAL" "DOT" false JB.DEFAULT (mapRouteConfig "" "" false getPolylineAnimationConfig) 
    liftFlow $ JB.drawRoute [normalRoute] (getNewIDWithTag "DriverTrackingHomeScreenMap")
  else if not Array.null state.data.route then do
    let shortRoute = (state.data.route Array.!! 0)
    case shortRoute of
      Just (Route route) -> do
        let coor = JB.getExtendedPath $ Remote.walkCoordinates route.points
        liftFlow $ push $ UpdateState state{ props { routeVisible = true } }
        let _ = JB.removeMarker "ic_vehicle_side"
            _ = removeAllPolylines ""
        let normalRoute = JB.mkRouteConfig coor srcMarkerConfig destMarkerConfig Nothing drawRouteType "LineString" true JB.DEFAULT (mapRouteConfig "" "" false getPolylineAnimationConfig) 
        liftFlow $ JB.drawRoute [normalRoute] (getNewIDWithTag "DriverTrackingHomeScreenMap")
        pure unit
      Nothing -> pure unit
  else do
    let leftStops = Array.filter (\(API.Stop item) -> maybe true (\(API.StopInformation stopInfo) -> isNothing stopInfo.stopEndLatLng) item.stopInfo) state.data.activeRide.stops
        points = (Array.singleton $ API.LatLong {lat : srcLat, lon : srcLon}) 
                  <> (if state.props.currentStage == ST.RideAccepted 
                        then [] 
                        else map (\(API.Stop item) -> getLatlon item.location ) leftStops)
                  <> (Array.singleton $ API.LatLong {lat : destLat, lon : destLon}) 
        getLatlon (API.LocationInfo location) = API.LatLong {lat : location.lat, lon : location.lon} 
    eRouteAPIResponse <- Remote.getRoute (Remote.makeGetRouteReq srcLat srcLon destLat destLon) routeType
    case eRouteAPIResponse of
      Right (GetRouteResp routeApiResponse) -> do
        let shortRoute = (routeApiResponse Array.!! 0)
        case shortRoute of
          Just (Route route) -> do
            let coor = JB.getExtendedPath $ Remote.walkCoordinates route.points
            liftFlow $ push $ UpdateState state { data { activeRide { actualRideDistance = if state.props.currentStage == ST.RideStarted then (toNumber route.distance) else state.data.activeRide.actualRideDistance , duration = route.duration } , route = routeApiResponse}, props { routeVisible = true } }
            pure $ JB.removeMarker "ny_ic_auto"
            void $ pure $ removeAllPolylines ""
            let normalRoute = JB.mkRouteConfig coor srcMarkerConfig destMarkerConfig Nothing drawRouteType "LineString" true JB.DEFAULT (mapRouteConfig "" "" false getPolylineAnimationConfig) 
            liftFlow $ JB.drawRoute [normalRoute] (getNewIDWithTag "DriverTrackingHomeScreenMap")
            pure unit
          Nothing -> pure unit   
      Left err -> pure unit        
    when (state.props.currentStage == ST.RideStarted) $ for_  state.data.activeRide.stops $ \(API.Stop stop) -> do
      let (API.LocationInfo stopLocation) = stop.location
      pure $ JB.removeMarker $ "stop" <> show stopLocation.lat <> show stopLocation.lon
      when (maybe true (\(API.StopInformation sInfo) -> isNothing sInfo.stopEndLatLng) stop.stopInfo) $ do
        let markerId = "stop" <> show stopLocation.lat <> show stopLocation.lon
            pt = {lat : stopLocation.lat, lng : stopLocation.lon}
            Tuple sourceArea _ = HU.getStopName (API.Stop stop)
        void $ liftFlow $ showMarker JB.defaultMarkerConfig{ markerId = markerId, pointerIcon = "ny_ic_stop_grey"} stopLocation.lat stopLocation.lon 40 0.5 0.9 (getNewIDWithTag "DriverTrackingHomeScreenMap")
        liftFlow $ runEffectFn1 EHR.upsertMarkerLabel  { id: markerId <> "label" , title: sourceArea, actionImage: "", actionCallBack: "", position: pt, markerImage : ""}
        pure unit
    pure unit