{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.HomeScreen.Controller where

import Accessor (_estimatedFare, _estimateId, _vehicleVariant, _status, _estimateFareBreakup, _title, _price, _totalFareRange, _maxFare, _minFare, _nightShiftRate, _nightShiftEnd, _nightShiftMultiplier, _nightShiftStart, _selectedQuotes, _specialLocationTag)
import Common.Types.App (EventPayload(..), GlobalPayload(..), LazyCheck(..), OptionButtonList, Payload(..), RateCardType(..), FeedbackAnswer(..))
import Components.Banner as Banner
import Components.ChatView as ChatView
import Components.ChatView.Controller as ChatView
import Components.ChooseVehicle as ChooseVehicleController
import Components.ChooseYourRide as ChooseYourRide
import Components.ChooseYourRide.Controller as ChooseYourRideController
import Components.DriverInfoCard.Controller as DriverInfoCardController
import Components.EmergencyHelp as EmergencyHelpController
import Components.ErrorModal.Controller as ErrorModalController
import Components.FavouriteLocationModel as FavouriteLocationModelController
import Components.GenericHeader.Controller as GenericHeaderController
import Components.LocationListItem.Controller as LocationListItemController
import Components.LocationTagBar as LocationTagBarController
import Components.MenuButton as MenuButton
import Components.MenuButton as MenuButton
import Components.RideCompletedCard.Controller as RideCompletedCard
import Components.MenuButton.Controller (Action(..)) as MenuButtonController
import Components.PopUpModal.Controller as PopUpModal
import Components.PricingTutorialModel.Controller as PricingTutorialModelController
import Components.PrimaryButton.Controller as PrimaryButtonController
import Components.PrimaryEditText.Controller as PrimaryEditTextController
import Components.QuoteListItem.Controller as QuoteListItemController
import Components.QuoteListModel.Controller as QuoteListModelController
import Components.QuoteListModel.View (dummyQuoteList)
import Components.RateCard as RateCard
import Components.RatingCard as RatingCard
import Components.RequestInfoCard as RequestInfoCard
import Components.SaveFavouriteCard as SaveFavouriteCardController
import Components.SavedLocationCard.Controller as SavedLocationCardController
import Components.SearchLocationModel.Controller as SearchLocationModelController
import Components.SelectListModal.Controller as CancelRidePopUp
import Components.SettingSideBar.Controller as SettingSideBarController
import Components.SourceToDestination.Controller as SourceToDestinationController
import Control.Monad.Except.Trans (runExceptT)
import Control.Monad.Except (runExcept)
import Control.Monad.Trans.Class (lift)
import Control.Transformers.Back.Trans (runBackT)
import Data.Array ((!!), filter, null, any, snoc, length, head, last, sortBy, union, elem, findIndex)
import Data.Function.Uncurried (runFn3)
import Data.Int (toNumber, round, fromString, fromNumber, ceil)
import Data.Lens ((^.))
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Number (fromString) as NUM
import Data.String as STR
import Debug (spy)
import Effect (Effect)
import Effect.Aff (launchAff)
import Effect.Unsafe (unsafePerformEffect)
import Effect.Uncurried (runEffectFn1)
import Engineering.Helpers.Commons (clearTimer, flowRunner, getNewIDWithTag, os, getExpiryTime, convertUTCtoISC, getCurrentUTC, isPreviousVersion, getDeviceHeight, screenHeight)
import Engineering.Helpers.LogEvent (logEvent, logEventWithTwoParams, logEventWithMultipleParams)
import Engineering.Helpers.Suggestions (getMessageFromKey, getSuggestionsfromKey)
import Foreign (unsafeToForeign)
import Foreign.Class (encode)
import JBridge (addMarker, animateCamera, currentPosition, exitLocateOnMap, firebaseLogEvent, firebaseLogEventWithParams, firebaseLogEventWithTwoParams, getCurrentPosition, hideKeyboardOnNavigation, isLocationEnabled, isLocationPermissionEnabled, locateOnMap, minimizeApp, openNavigation, openUrlInApp, removeAllPolylines, removeMarker, requestKeyboardShow, requestLocation, shareTextMessage, showDialer, toast, toggleBtnLoader, goBackPrevWebPage, stopChatListenerService, sendMessage, getCurrentLatLong, isInternetAvailable, emitJOSEvent, startLottieProcess, getSuggestionfromKey, scrollToEnd, lottieAnimationConfig, methodArgumentCount, getChatMessages, scrollViewFocus, updateInputString, checkAndAskNotificationPermission, locateOnMapConfig, addCarouselWithVideoExists, pauseYoutubeVideo, getLayoutBounds)
import Helpers.Utils (addToRecentSearches, getCurrentLocationMarker, clearCountDownTimer, getDistanceBwCordinates, getLocationName, getScreenFromStage, getSearchType, parseNewContacts, performHapticFeedback, setText, terminateApp, withinTimeRange, toStringJSON, secondsToHms, recentDistance, getDeviceDefaultDensity, getPixels, getDefaultPixels)
import Language.Strings (getString, getEN)
import Language.Types (STR(..))
import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, printLog, trackAppTextInput, trackAppScreenEvent)
import MerchantConfig.DefaultConfig as DC
import MerchantConfig.Utils (Merchant(..), getMerchant, getValueFromConfig)
import Prelude (class Applicative, class Show, Unit, Ordering, bind, compare, discard, map, negate, pure, show, unit, not, ($), (&&), (-), (/=), (<>), (==), (>), (||), (>=), void, (<), (*), (<=), (/), (+))
import Presto.Core.Types.API (ErrorResponse)
import PrestoDOM (Eval ,Visibility(..), BottomSheetState(..), continue, continueWithCmd, defaultPerformLog, exit, payload, updateAndExit)
import PrestoDOM.Types.Core (class Loggable)
import Resources.Constants (encodeAddress)
import Constants (defaultDensity)
import Screens (ScreenName(..), getScreen)
import Screens.AddNewAddressScreen.Controller (validTag, getSavedTagsFromHome)
import Screens.HomeScreen.ScreenData (dummyAddress, dummyQuoteAPIEntity, dummyZoneType)
import Screens.HomeScreen.ScreenData as HomeScreenData
import Screens.HomeScreen.Transformer (dummyRideAPIEntity, getDriverInfo, getEstimateList, getQuoteList, getSpecialZoneQuotes, transformContactList, getNearByDrivers, getEstimatesInfo, dummyEstimateEntity)
import Screens.RideBookingFlow.HomeScreen.Config (setTipViewData)
import Screens.SuccessScreen.Handler as UI
import Screens.Types (HomeScreenState, Location, SearchResultType(..), LocationListItemState, PopupType(..), SearchLocationModelType(..), Stage(..), CardType(..), RatingCard, CurrentLocationDetailsWithDistance(..), CurrentLocationDetails, LocationItemType(..), CallType(..), ZoneType(..), SpecialTags, TipViewStage(..), Trip)
import Services.API (EstimateAPIEntity(..), FareRange, GetDriverLocationResp, GetQuotesRes(..), GetRouteResp, LatLong(..), OfferRes, PlaceName(..), QuoteAPIEntity(..), RideBookingRes(..), SelectListRes(..), SelectedQuotes(..), RideBookingAPIDetails(..), GetPlaceNameResp(..))
import Services.Backend as Remote
import Services.Config (getDriverNumber, getSupportNumber)
import Storage (KeyStore(..), isLocalStageOn, updateLocalStage, getValueToLocalStore, setValueToLocalStore, getValueToLocalNativeStore, setValueToLocalNativeStore)
import Control.Monad.Trans.Class (lift)
import Presto.Core.Types.Language.Flow (doAff)
import Effect.Class (liftEffect)
import Screens.HomeScreen.ScreenData as HomeScreenData
import Types.App (defaultGlobalState)
import Screens.RideBookingFlow.HomeScreen.Config (setTipViewData, reportIssueOptions, metersToKm)
import Screens.Types (TipViewData(..) , TipViewProps(..), RateCardDetails, PermissionScreenStage(..), SuggestionsMap(..))
import Engineering.Helpers.Suggestions (getMessageFromKey, getSuggestionsfromKey)
import PrestoDOM.Properties (sheetState) as PP
import Screens.RideBookingFlow.HomeScreen.Config(reportIssueOptions)
import Data.Function (const)
import Data.List ((:))
import Common.Resources.Constants (zoomLevel, pickupZoomLevel)
import Screens.RideBookingFlow.HomeScreen.Config
import Data.Function.Uncurried (Fn3, runFn3, Fn1, runFn1)

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog = defaultPerformLog --case action of
    -- AfterRender -> trackAppScreenRender appId "screen" (getScreen HOME_SCREEN)
    -- BackPressed -> trackAppBackPress appId (getScreen HOME_SCREEN)
    -- CancelSearch -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "cancel_search"
    -- RecenterCurrentLocation -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "recenter_location"
    -- SidebarCloseAnimationCompleted -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "side_bar_close"
    -- OpenSettings -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "open_settings"
    -- OpenPricingTutorial -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "open_pricing_tutorial"
    -- OpenSearchLocation -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "open_search_modal"
    -- UpdateSource lat lon name -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "update_source_address"
    -- HideLiveDashboard val -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "hide_live_stats_dashboard"
    -- LiveDashboardAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "live_Dashboard_action"
    -- PrimaryButtonActionController act -> case act of
    --   PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "primary_button" "onclick"
    --   PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "primary_button" "no_action"
    -- SettingSideBarActionController act -> case act of
    --   SettingSideBarController.PastRides -> do
    --     trackAppActionClick appId (getScreen HOME_SCREEN) "setting_side_bar" "past_rides"
    --     trackAppEndScreen appId (getScreen HOME_SCREEN)
    --   SettingSideBarController.OnHelp -> do
    --     trackAppActionClick appId (getScreen HOME_SCREEN) "setting_side_bar" "on_help"
    --     trackAppEndScreen appId (getScreen HOME_SCREEN)
    --   SettingSideBarController.ChangeLanguage -> do
    --     trackAppActionClick appId (getScreen HOME_SCREEN) "setting_side_bar" "change_language"
    --     trackAppEndScreen appId (getScreen HOME_SCREEN)
    --   SettingSideBarController.GoToAbout -> do
    --     trackAppActionClick appId (getScreen HOME_SCREEN) "setting_side_bar" "go_to_about"
    --     trackAppEndScreen appId (getScreen HOME_SCREEN)
    --   SettingSideBarController.EditProfile -> do
    --     trackAppActionClick appId (getScreen HOME_SCREEN) "setting_side_bar" "edit_profile"
    --     trackAppEndScreen appId (getScreen HOME_SCREEN)
    --   SettingSideBarController.OnClosed -> do
    --     trackAppActionClick appId (getScreen HOME_SCREEN) "setting_side_bar" "on_closed"
    --     trackAppEndScreen appId (getScreen HOME_SCREEN)
    --   SettingSideBarController.OnClose -> do
    --     trackAppActionClick appId (getScreen HOME_SCREEN) "setting_side_bar" "on_close"
    --     trackAppEndScreen appId (getScreen HOME_SCREEN)
    --   SettingSideBarController.OnLogout -> do
    --     trackAppActionClick appId (getScreen HOME_SCREEN) "setting_side_bar" "on_logout"
    --     trackAppEndScreen appId (getScreen HOME_SCREEN)
    --   SettingSideBarController.ShareAppLink -> do
    --     trackAppActionClick appId (getScreen HOME_SCREEN) "setting_side_bar" "share_app_link"
    --     trackAppEndScreen appId (getScreen HOME_SCREEN)
    --   SettingSideBarController.GoToFavourites -> do
    --     trackAppActionClick appId (getScreen HOME_SCREEN) "setting_side_bar" "go_to_favourites"
    --     trackAppEndScreen appId (getScreen HOME_SCREEN)
    --   SettingSideBarController.GoToMyProfile -> do
    --     trackAppActionClick appId (getScreen HOME_SCREEN) "setting_side_bar" "go_to_my_profile"
    --     trackAppEndScreen appId (getScreen HOME_SCREEN)
    --   SettingSideBarController.GoToEmergencyContacts -> do
    --     trackAppActionClick appId (getScreen HOME_SCREEN) "setting_side_bar" "go_to_emergency_contacts_onclick"
    --     trackAppEndScreen appId (getScreen HOME_SCREEN)
    --     trackAppEndScreen appId (getScreen HOME_SCREEN)
    --   SettingSideBarController.LiveStatsDashboard -> do
    --     trackAppActionClick appId (getScreen HOME_SCREEN) "setting_side_bar" "go_to_live_stats_dashboard"
    --     trackAppEndScreen appId (getScreen HOME_SCREEN)
    --   SettingSideBarController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "setting_side_bar" "no_action"
    -- PricingTutorialModelActionController (PricingTutorialModelController.Close) -> trackAppActionClick appId (getScreen HOME_SCREEN) "pricing_tutorial" "close_icon"
    -- SearchLocationModelActionController act -> case act of
    --   SearchLocationModelController.LocationListItemActionController act -> case act of
    --     LocationListItemController.OnClick item -> trackAppActionClick appId (getScreen HOME_SCREEN) "search_location_modal" "location_list_item"
    --     LocationListItemController.SelectedCurrentLocation lat lng name -> trackAppActionClick appId (getScreen HOME_SCREEN) "search_location_modal" "location_list_item_onclick_current_location"
    --     LocationListItemController.FavClick item -> trackAppActionClick appId (getScreen HOME_SCREEN) "search_location_modal" "favourite"
    --   SearchLocationModelController.PrimaryButtonActionController act -> case act of
    --     PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "search_location_modal" "primary_button"
    --     PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "search_location_modal" "primary_button_no_action"
    --   SearchLocationModelController.SourceChanged input -> trackAppActionClick appId (getScreen HOME_SCREEN) "search_location_modal" "source_changed"
    --   SearchLocationModelController.DestinationChanged input -> trackAppActionClick appId (getScreen HOME_SCREEN) "search_location_modal" "destination_changed"
    --   SearchLocationModelController.EditTextFocusChanged textType -> trackAppActionClick appId (getScreen HOME_SCREEN) "search_location_modal" "edit_text_focus_changed"
    --   SearchLocationModelController.GoBack -> trackAppActionClick appId (getScreen HOME_SCREEN) "search_location_modal" "go_back"
    --   SearchLocationModelController.SetCurrentLocation -> trackAppActionClick appId (getScreen HOME_SCREEN) "search_location_modal" "set_current_location"
    --   SearchLocationModelController.SetLocationOnMap -> trackAppActionClick appId (getScreen HOME_SCREEN) "search_location_modal" "set_location_on_map"
    --   SearchLocationModelController.UpdateSource lat lng name -> trackAppActionClick appId (getScreen HOME_SCREEN) "search_location_modal" "set_current_location_update_source"
    --   SearchLocationModelController.SourceClear -> trackAppActionClick appId (getScreen HOME_SCREEN) "search_location_modal" "source_clear"
    --   SearchLocationModelController.DestinationClear -> trackAppActionClick appId (getScreen HOME_SCREEN) "search_location_modal" "destination_clear_options"
    --   SearchLocationModelController.DebounceCallBack searchString -> trackAppActionClick appId (getScreen HOME_SCREEN) "search_location_modal" "debounce_callback_search"
    --   SearchLocationModelController.UpdateCurrentLocation lat lng -> trackAppActionClick appId (getScreen HOME_SCREEN) "search_location_modal" "update_current_location"
    --   SearchLocationModelController.RecenterCurrentLocation -> trackAppActionClick appId (getScreen HOME_SCREEN) "search_location_modal" "recenter_location"
    --   SearchLocationModelController.SavedAddressClicked act -> case act of
    --     LocationTagBarController.TagClick savedAddressType arrItem -> trackAppActionClick appId (getScreen HOME_SCREEN) "location_tag_bar" "tag"
    --   SearchLocationModelController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "search_location_modal" "no_action"
    -- QuoteListModelActionController act -> case act of
    --   QuoteListModelController.QuoteListItemActionController act -> case act of
    --     QuoteListItemController.Click quote -> trackAppActionClick appId (getScreen HOME_SCREEN) "quote_list_modal" "quote_list_item_click"
    --     QuoteListItemController.CountDown seconds id status timerID -> trackAppActionClick appId (getScreen HOME_SCREEN) "quote_list_modal" "quote_list_item_count_down"
    --     QuoteListItemController.ConfirmRide -> trackAppActionClick appId (getScreen HOME_SCREEN) "quote_list_modal" "confirm_ride"
    --     QuoteListItemController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "quote_list_modal" "no_action"
    --     QuoteListItemController.CancelAutoAssigning -> trackAppActionClick appId (getScreen HOME_SCREEN) "quote_list_modal" "auto_assign_cancel"
    --   QuoteListModelController.PrimaryButtonActionController act -> case act of
    --     PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "quote_list_modal" "confirm_primary_button"
    --     PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "quote_list_modal" "primary_button_no_action"
    --   QuoteListModelController.GoBack -> trackAppActionClick appId (getScreen HOME_SCREEN) "quote_list_modal" "on_goback"
    --   QuoteListModelController.CancelAutoAssigning -> trackAppActionClick appId (getScreen HOME_SCREEN) "quote_list_modal" "auto_assign_cancel"
    --   QuoteListModelController.HomeButtonActionController act -> case act of
    --     PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "quote_list_modal" "home_primary_button"
    --     PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "quote_list_modal" "primary_button_no_action"
    --   QuoteListModelController.TryAgainButtonActionController act -> case act of
    --     PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "quote_list_modal" "try_again_primary_button"
    --     PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "quote_list_modal" "primary_button_no_action"
    --   QuoteListModelController.HidePopUp -> trackAppActionClick appId (getScreen HOME_SCREEN) "quote_list_modal" "hide_popup"
    --   QuoteListModelController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "quote_list_modal" "no_action"
    -- DriverInfoCardActionController act -> case act of
    --   DriverInfoCardController.PrimaryButtonAC act -> case act of
    --     PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "driver_info_card" "call_primary_button"
    --     PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "driver_info_card" "primary_button_no_action"
    --   DriverInfoCardController.Support -> trackAppActionClick appId (getScreen HOME_SCREEN) "driver_info_card" "support"
    --   DriverInfoCardController.CancelRide infoCard -> trackAppActionClick appId (getScreen HOME_SCREEN) "driver_info_card" "cancel_ride"
    --   DriverInfoCardController.LocationTracking -> trackAppActionClick appId (getScreen HOME_SCREEN) "driver_info_card" "location_tracking"
    --   DriverInfoCardController.OpenEmergencyHelp -> trackAppActionClick appId (getScreen HOME_SCREEN) "driver_info_card" "open_emergency_help"
    --   DriverInfoCardController.SourceToDestinationAC  act -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "driver_info_card" "source_to_destination"
    --   DriverInfoCardController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "driver_info_card" "no_action"
    --   DriverInfoCardController.MessageDriver -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "driver_info_card" "open_in_app_messaging"
    -- UpdateLocation key lat lon ->  trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "update_location"
    -- RateRideButtonActionController act -> case act of
    --   PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "rate_your_ride" "primary_button"
    --   PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "rate_your_ride" "primary_button_no_action"
    -- CancelRidePopUpAction act -> case act of
    --   CancelRidePopUp.Button1 act -> case act of
    --     PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "cancel_ride_popup" "cancel_ride_declined"
    --     PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "cancel_ride_popup" "primary_button_no_action"
    --   CancelRidePopUp.Button2 act -> case act of
    --     PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "cancel_ride_popup" "cancel_ride_accepted"
    --     PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "cancel_ride_popup" "primary_button_no_action"
    --   CancelRidePopUp.UpdateIndex index -> trackAppActionClick appId (getScreen HOME_SCREEN) "cancel_ride_popup" "update_index"
    --   CancelRidePopUp.OnGoBack -> trackAppActionClick appId (getScreen HOME_SCREEN) "cancel_ride_popup" "go_back"
    --   CancelRidePopUp.ClearOptions -> trackAppActionClick appId (getScreen HOME_SCREEN) "cancel_ride_popup" "clear_options"
    --   CancelRidePopUp.TextChanged valId newVal ->  trackAppTextInput appId (getScreen HOME_SCREEN) "cancelling_reason_text_changed" "cancel_ride_popup"
    --   CancelRidePopUp.NoAction ->  trackAppActionClick appId (getScreen HOME_SCREEN) "cancel_ride_popup" "no_action"
    -- PopUpModalAction act -> case act of
    --   PopUpModal.OnButton1Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_action" "on_goback"
    --   PopUpModal.OnButton2Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_action" "on_cancel"
    --   PopUpModal.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_action" "no_action"
    --   PopUpModal.OnImageClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_action" "image"
    --   PopUpModal.ETextController act -> trackAppTextInput appId (getScreen HOME_SCREEN) "popup_modal_action" "primary_edit_text"
    --   PopUpModal.CountDown arg1 arg2 arg3 arg4 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_action" "countdown_updated"
    --   PopUpModal.Tipbtnclick arg1 arg2 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_action" "tip_clicked"
    --   PopUpModal.DismissPopup -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_action" "popup_dismissed"
    -- RatingCardAC act -> case act of
    --   RatingCard.Rating index -> trackAppActionClick appId (getScreen HOME_SCREEN) "rating_card" "star"
    --   RatingCard.PrimaryButtonAC act -> case act of
    --     PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "rating_card" "primary_button"
    --     PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "rating_card" "primary_button_no_action"
    --   RatingCard.FareBreakUpAC act -> case act of
    --     FareBreakUp.ShowInvoice -> do
    --       trackAppActionClick appId (getScreen HOME_SCREEN) "rating_card" "show_invoice_click"
    --       trackAppEndScreen appId (getScreen HOME_SCREEN)
    --     FareBreakUp.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "fare_breakup" "no_action"
    --     FareBreakUp.SourceToDestinationActionController act -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "fare_breakup" "source_to_destination"
    --   RatingCard.SkipButtonAC act -> case act of
    --     PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "rating_card" "skip_primary_button"
    --     PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "rating_card" "primary_button_no_action"
    --   RatingCard.FeedbackChanged value -> trackAppActionClick appId (getScreen HOME_SCREEN) "rating_card" "feedback_changed"
    --   RatingCard.BackPressed -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "rating_card" "back_pressed"
    --   RatingCard.SourceToDestinationAC act -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "rating_card" "source_to_destination"
    --   RatingCard.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "rating_card" "no_action"
    -- CloseLocationTracking -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "close_location_tracking"
    -- StartLocationTracking item -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "start_location_tracking"
    -- DistanceOutsideLimitsActionController act -> case act of
    --   PopUpModal.OnButton2Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_distance_outside_limit" "change_drop_location"
    --   PopUpModal.OnButton1Click -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_distance_outside_limit" "change_drop_location_cancel"
    --   PopUpModal.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_distance_outside_limit" "no_action"
    --   PopUpModal.OnImageClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_distance_outside_limit" "image"
    --   PopUpModal.ETextController act -> trackAppTextInput appId (getScreen HOME_SCREEN) "popup_modal_distance_outside_limit" "primary_edit_text"
    --   PopUpModal.CountDown arg1 arg2 arg3 arg4 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_distance_outside_limit" "countdown_updated"
    --   PopUpModal.Tipbtnclick arg1 arg2 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_action" "tip_clicked"
    --   PopUpModal.DismissPopup -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_action" "popup_dismissed"
    -- ShortDistanceActionController act -> case act of
    --   PopUpModal.OnButton1Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_short_distance_action" "book_ride"
    --   PopUpModal.OnButton2Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_short_distance_action" "go_back"
    --   PopUpModal.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_short_distance_action" "no_action"
    --   PopUpModal.OnImageClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_short_distance_action" "image"
    --   PopUpModal.ETextController act -> trackAppTextInput appId (getScreen HOME_SCREEN) "popup_modal_short_distance_action" "primary_edit_text"
    --   PopUpModal.CountDown arg1 arg2 arg3 arg4 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_short_distance_action" "countdown_updated"
    --   PopUpModal.Tipbtnclick arg1 arg2 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_action" "tip_clicked"
    --   PopUpModal.DismissPopup -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_action" "popup_dismissed"
    -- SourceUnserviceableActionController act -> case act of
    --   ErrorModalController.PrimaryButtonActionController act -> case act of
    --     PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "source_unserviceable_error" "primary_button_change_location"
    --     PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "source_unservice_error_modal" "primary_button_no_action"
    -- GoBackToSearchLocationModal -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "go_back_search_location_modal"
    -- SkipButtonActionController act -> case act of
    --   PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "primary_button" "skip"
    --   PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "source_unservice_error_modal" "primary_button_no_action"
    -- EstimateChangedPopUpController act -> case act of
    --   PopUpModal.OnButton1Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_estimate_changed" "go_to_home"
    --   PopUpModal.OnButton2Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_estimate_changed" "continue"
    --   PopUpModal.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_estimate_changed" "no_action"
    --   PopUpModal.OnImageClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_estimate_changed" "image"
    --   PopUpModal.ETextController act -> trackAppTextInput appId (getScreen HOME_SCREEN) "popup_modal_estimate_changed" "primary_edit_text"
    --   PopUpModal.CountDown arg1 arg2 arg3 arg4 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_estimate_changed" "countdown_updated"
    --   PopUpModal.Tipbtnclick arg1 arg2 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_action" "tip_clicked"
    --   PopUpModal.DismissPopup -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_action" "popup_dismissed"
    -- RateCardAction act -> case act of
    --   RateCard.Close -> trackAppActionClick appId (getScreen HOME_SCREEN) "rate_card" "close_click"
    --   RateCard.BackPressed -> trackAppActionClick appId (getScreen HOME_SCREEN) "rate_card" "back_click"
    --   RateCard.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "rate_card_action" "no_action"
    --   RateCard.GoToDefaultStart -> trackAppActionClick appId (getScreen HOME_SCREEN) "rate_card_action" "back_click"
    --   RateCard.GoToDriverAddition -> trackAppActionClick appId (getScreen HOME_SCREEN) "rate_card_action" "view_change"
    --   RateCard.GoToFareUpdate -> trackAppActionClick appId (getScreen HOME_SCREEN) "rate_card_action" "view_change"
    -- ShowRateCard -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "show_rate_card"
    -- PredictionClickedAction act -> case act of
    --   LocationListItemController.OnClick item -> trackAppActionClick appId (getScreen HOME_SCREEN) "location_list_item" "prediction"
    --   LocationListItemController.FavClick item -> trackAppActionClick appId (getScreen HOME_SCREEN) "location_list_item" "prediction_fav_click"
    --   LocationListItemController.SelectedCurrentLocation lat lng name -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "location_list_item" "selected_current_location"
    -- SavedAddressClicked (LocationTagBarController.TagClick savedAddressType arrItem) -> trackAppActionClick appId (getScreen HOME_SCREEN) "location_tag_bar" "tag"
    -- FavouriteLocationModelAC act -> case act of
    --   FavouriteLocationModelController.GenericHeaderAC act -> case act of
    --     GenericHeaderController.PrefixImgOnClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "fav_location_modal" "generic_header_back_icon"
    --     GenericHeaderController.SuffixImgOnClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "fav_location_modal" "generic_header_forward_icon"
    --   FavouriteLocationModelController.FavouriteLocationAC act -> case act of
    --     SavedLocationCardController.CardClicked item -> trackAppActionClick appId (getScreen HOME_SCREEN) "fav_location_modal" "saved_loc_card"
    --     SavedLocationCardController.DeleteLocation act -> trackAppActionClick appId (getScreen HOME_SCREEN) "fav_location_modal" "delete_location"
    --     SavedLocationCardController.EditLocation act -> trackAppActionClick appId (getScreen HOME_SCREEN) "fav_location_modal" "edit_location_modal"
    --     SavedLocationCardController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "fav_location_modal" "no_action"
    --   FavouriteLocationModelController.ErrorModalAC act -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "fav_location_modal" "error_modal_action"
    -- SaveFavouriteCardAction act -> case act of
    --   SaveFavouriteCardController.OnClose -> trackAppActionClick appId (getScreen HOME_SCREEN) "save_fav_card" "on_close_click"
    --   SaveFavouriteCardController.SaveFavourite -> trackAppActionClick appId (getScreen HOME_SCREEN) "save_fav_card" "save_fav"
    --   SaveFavouriteCardController.PrimayEditTA (PrimaryEditTextController.TextChanged id val) -> trackAppTextInput appId (getScreen HOME_SCREEN) "save_fav_card_text_changed" "primary_edit_text"
    --   SaveFavouriteCardController.TagSelected act -> trackAppActionClick appId (getScreen HOME_SCREEN) "save_fav_card" "tag_selected"
    --   SaveFavouriteCardController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "save_fav_card" "no_action"
    -- UpdateCurrentLocation lat lng -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "update_current_location"
    -- EmergencyHelpModalAC act -> case act of
    --   EmergencyHelpController.GenericHeaderAC act -> case act of
    --     GenericHeaderController.PrefixImgOnClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "emergency_help_modal" "generic_header_back_icon"
    --     GenericHeaderController.SuffixImgOnClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "generic_header_action" "forward_icon"
    --   EmergencyHelpController.CallPolicePopup -> trackAppActionClick appId (getScreen HOME_SCREEN) "emergency_help_modal" "call_police_popup"
    --   EmergencyHelpController.ContactSupportPopup -> trackAppActionClick appId (getScreen HOME_SCREEN) "emergency_help_modal" "contact_support_popup"
    --   EmergencyHelpController.CallSuccessfulPopup -> trackAppActionClick appId (getScreen HOME_SCREEN) "emergency_help_modal" "call_successful_popup"
    --   EmergencyHelpController.CallContactPopUp contact -> trackAppActionClick appId (getScreen HOME_SCREEN) "emergency_help_modal" "call_contact_popup"
    --   EmergencyHelpController.StoreContacts -> trackAppActionClick appId (getScreen HOME_SCREEN) "emergency_help_modal" "store_contacts"
    --   EmergencyHelpController.AddedEmergencyContacts -> trackAppActionClick appId (getScreen HOME_SCREEN) "emergency_help_modal" "add_emergency_contacts"
    --   EmergencyHelpController.CallPolice act -> case act of
    --     PopUpModal.OnButton1Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_emergency_help" "call_police_cancel"
    --     PopUpModal.OnButton2Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_emergency_help" "call_police_accept"
    --     PopUpModal.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_emergency_help" "no_action"
    --     PopUpModal.OnImageClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_emergency_help" "image"
    --     PopUpModal.ETextController act -> trackAppTextInput appId (getScreen HOME_SCREEN) "popup_modal_emergency_help" "primary_edit_text"
    --     PopUpModal.CountDown arg1 arg2 arg3 arg4 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_emergency_help" "countdown_updated"
    --     PopUpModal.Tipbtnclick arg1 arg2 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_action" "tip_clicked"
    --     PopUpModal.DismissPopup -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_action" "popup_dismissed"
    --   EmergencyHelpController.CallEmergencyContact act -> case act of
    --     PopUpModal.OnButton1Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_emergency_help" "call_contact_cancel"
    --     PopUpModal.OnButton2Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_emergency_help" "call_contact_accept"
    --     PopUpModal.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_emergency_help" "no_action"
    --     PopUpModal.OnImageClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_emergency_help" "image"
    --     PopUpModal.ETextController act -> trackAppTextInput appId (getScreen HOME_SCREEN) "popup_modal_emergency_help" "primary_edit_text"
    --     PopUpModal.CountDown arg1 arg2 arg3 arg4 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_emergency_help" "countdown_updated"
    --     PopUpModal.Tipbtnclick arg1 arg2 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_action" "tip_clicked"
    --     PopUpModal.DismissPopup -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_action" "popup_dismissed"
    --   EmergencyHelpController.CallSuccessful act -> case act of
    --     PopUpModal.OnButton1Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_emergency_help" "call_feedback_cancel"
    --     PopUpModal.OnButton2Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_emergency_help" "call_feedback_accept"
    --     PopUpModal.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_emergency_help" "no_action"
    --     PopUpModal.OnImageClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_emergency_help" "image"
    --     PopUpModal.ETextController act -> trackAppTextInput appId (getScreen HOME_SCREEN) "popup_modal_emergency_help" "primary_edit_text"
    --     PopUpModal.CountDown arg1 arg2 arg3 arg4 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_emergency_help" "countdown_updated"
    --     PopUpModal.Tipbtnclick arg1 arg2 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_action" "tip_clicked"
    --     PopUpModal.DismissPopup -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_action" "popup_dismissed"
    --   EmergencyHelpController.ContactSupport act -> case act of
    --     PopUpModal.OnButton1Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_emergency_help" "contact_support_cancel"
    --     PopUpModal.OnButton2Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_emergency_help" "contact_support_accept"
    --     PopUpModal.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_emergency_help" "no_action"
    --     PopUpModal.OnImageClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_emergency_help" "image"
    --     PopUpModal.ETextController act -> trackAppTextInput appId (getScreen HOME_SCREEN) "popup_modal_emergency_help" "primary_edit_text"
    --     PopUpModal.CountDown arg1 arg2 arg3 arg4 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_emergency_help" "countdown_updated"
    --     PopUpModal.Tipbtnclick arg1 arg2 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_action" "tip_clicked"
    --     PopUpModal.DismissPopup -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_action" "popup_dismissed"
    --   EmergencyHelpController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_emergency_help" "no_action"
    -- PopUpModalShareAppAction act -> case act of
    --   PopUpModal.OnButton1Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_share_app" "cancel"
    --   PopUpModal.OnButton2Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_share_app" "accept"
    --   PopUpModal.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_share_app" "no_action"
    --   PopUpModal.OnImageClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_share_app" "image"
    --   PopUpModal.ETextController act -> trackAppTextInput appId (getScreen HOME_SCREEN) "popup_modal_share_app" "primary_edit_text"
    --   PopUpModal.CountDown arg1 arg2 arg3 arg4 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_share_app" "countdown_updated"
    --   PopUpModal.Tipbtnclick arg1 arg2 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_action" "tip_clicked"
    --   PopUpModal.DismissPopup -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_action" "popup_dismissed"
    -- CallSupportAction act -> case act of
    --   PopUpModal.OnButton1Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_contact_support" "cancel"
    --   PopUpModal.OnButton2Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_contact_support" "accept"
    --   PopUpModal.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_contact_support" "no_action"
    --   PopUpModal.OnImageClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_contact_support" "image"
    --   PopUpModal.ETextController act -> trackAppTextInput appId (getScreen HOME_SCREEN) "popup_modal_contact_support" "primary_edit_text"
    --   PopUpModal.CountDown arg1 arg2 arg3 arg4 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_contact_support" "countdown_updated"
    --   PopUpModal.Tipbtnclick arg1 arg2 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_action" "tip_clicked"
    --   PopUpModal.DismissPopup -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_action" "popup_dismissed"
    -- ContinueWithoutOffers resp -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "continue_without_offers"
    -- CheckBoxClick autoAssign -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "check_box_click"
    -- TagClick savedAddressType arrItem -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "tag_click"
    -- DriverArrivedAction driverArrivalTime -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "driver_arrived_action"
    -- WaitingTimeAction timerID timeInMinutes seconds -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "waiting_time_action"
    -- UpdateETA currentETA currentDistance -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "update_eta"
    -- EstimatesTryAgain quotesRes -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "estimates_try_again"
    -- SearchExpireCountDown seconds id status timerID -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "search_expiry_count_down"
    -- FareBreakUpActionController act -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "fare_break_up_action"
    -- UpdateCurrentStage stage -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "update_current_stage"
    -- ExitLocationSelected item addToRecents -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "exit_location_selected"
    -- NotificationListener notificationType -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "notification_listener"
    -- GetEstimates quotesRes -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "get_estimates"
    -- GetRideConfirmation resp -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "get_ride_confirmation"
    -- GetQuotesList (SelectListRes resp) -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" ("get_quotes_list -" <> fromMaybe "" resp.bookingId)
    -- MAPREADY key latitude longitude -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "map_ready"
    -- CurrentLocation lat lng -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "current_location"
    -- SourceToDestinationActionController act -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "source_to_destination"
    -- TrackDriver resp -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "track_driver"
    -- HandleCallback -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "handle_call_back"
    -- UpdatePickupLocation  key lat lon -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "update_pickup_location"
    -- ContinueCmd -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "continue_cmd"
    -- Restart err -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "restart"
    -- UpdateSourceName lat lon name -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "update_source_name"
    -- RequestInfoCardAction act -> case act of
    --   RequestInfoCard.Close -> trackAppActionClick appId (getScreen HOME_SCREEN) "request_info_card" "got_it"
    --   RequestInfoCard.BackPressed -> trackAppActionClick appId (getScreen HOME_SCREEN) "request_info_card" "backpressed_in_screen"
    --   RequestInfoCard.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "request_info_card" "no_action"
    -- PreferencesDropDown -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "preferences_drop_down"
    -- OnIconClick autoAssign -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "close_icon_auto_assign"
    -- PopUpModalAction act -> case act of
    --     PopUpModal.OnButton1Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_action" "on_goback"
    --     PopUpModal.OnButton2Click -> do
    --       trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_action" "register_on_different_number"
    --       trackAppEndScreen appId (getScreen HOME_SCREEN)
    --     PopUpModal.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_action" "no_action"
    --     PopUpModal.OnImageClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_action" "image"
    --     PopUpModal.ETextController act -> trackAppTextInput appId (getScreen HOME_SCREEN) "popup_modal_action" "primary_edit_text"
    --     PopUpModal.CountDown arg1 arg2 arg3 arg4 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_action" "countdown_updated"
    --     PopUpModal.Tipbtnclick arg1 arg2 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_action" "tip_clicked"
    --     PopUpModal.DismissPopup -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_action" "popup_dismissed"
    -- ReferralFlowAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "have_referral_code"
    -- ReferralFlowNoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "have_referral_code_no_action"
    -- NewUser -> do
    --   trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "have_referral_code_no_action"
    --   trackAppEndScreen appId (getScreen HOME_SCREEN)
    -- MapReadyAction -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "map_render"
    -- TrackLiveLocationAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "track_live_location_using"
    -- LottieLoaderAction -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "auto_rickshaw_processing"
    -- UpdateSourceFromPastLocations -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "update_source_from_past_saved_locations"
    -- UpdateLocAndLatLong lat lon-> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "update_current_loc_lat_and_lon"
    -- UpdateSavedLoc state -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "update_saved_loc"
    -- NoAction -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "no_action"
    -- UpdateMessages msg sender timeStamp -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "update_messages"
    -- InitializeChat -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "initialize_chat"
    -- RemoveChat -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "remove_chat"
    -- ChatViewActionController act -> case act of
    --   ChatView.SendMessage -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_app_messaging" "send_message"
    --   ChatView.SendSuggestion suggestion -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_app_messaging" "send_suggestion"
    --   ChatView.BackPressed -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_app_messaging" "back_pressed"
    --   ChatView.TextChanged input -> trackAppTextInput appId (getScreen HOME_SCREEN) "in_app_messaging" "text_changed"
    --   ChatView.Call -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_app_messaging" "call_driver"
    --   ChatView.Navigate -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_app_messaging" "navigate_to_google_maps"
    --   ChatView.NoAction -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_app_messaging" "no_action"
    -- OnResumeCallback -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "on_resume_callback"
    -- CheckFlowStatusAction -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "check_flow_status"
    -- GoToEditProfile -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "go_to_my_profile"

data ScreenOutput = LogoutUser
                  | Cancel HomeScreenState
                  | GoToHelp HomeScreenState
                  | ConfirmRide HomeScreenState
                  | GoToAbout HomeScreenState
                  | PastRides HomeScreenState
                  | GoToMyProfile HomeScreenState Boolean
                  | ChangeLanguage HomeScreenState
                  | GoToEmergencyContacts HomeScreenState
                  | Retry HomeScreenState
                  | GetQuotes HomeScreenState
                  | UpdatedState HomeScreenState Boolean
                  | CancelRide HomeScreenState
                  | NotificationHandler String HomeScreenState
                  | GetSelectList HomeScreenState
                  | RideConfirmed HomeScreenState
                  | SelectEstimate HomeScreenState
                  | LocationSelected LocationListItemState Boolean HomeScreenState
                  | SearchPlace String HomeScreenState
                  | UpdateLocationName HomeScreenState Number Number
                  | UpdatePickupName HomeScreenState Number Number
                  | GoToHome
                  | GoToFavourites HomeScreenState
                  | SubmitRating HomeScreenState
                  | UpdatedSource HomeScreenState
                  | OpenGoogleMaps HomeScreenState
                  | InAppTrackStatus HomeScreenState
                  | UpdateSavedLocation HomeScreenState
                  | CheckLocServiceability HomeScreenState Number Number
                  | GoToInvoice HomeScreenState
                  | CheckFavDistance HomeScreenState
                  | SaveFavourite HomeScreenState
                  | GoToReferral HomeScreenState
                  | CallDriver HomeScreenState CallType
                  | CallContact HomeScreenState
                  | CallSupport HomeScreenState
                  | CallPolice HomeScreenState
                  | UpdateSosStatus HomeScreenState
                  | FetchContacts HomeScreenState
                  | CheckCurrentStatus
                  | CheckFlowStatus HomeScreenState
                  | ExitToPermissionFlow PermissionScreenStage
                  | RetryFindingQuotes Boolean HomeScreenState
                  | ReportIssue HomeScreenState
                  | RideDetailsScreen HomeScreenState
                  | GoToTicketBookingFlow HomeScreenState
                  | GoToMyTickets HomeScreenState
                  | RepeatTrip HomeScreenState Trip

data Action = NoAction
            | BackPressed
            | CancelSearch
            | RecenterCurrentLocation
            | SidebarCloseAnimationCompleted
            | NotificationListener String
            | OpenSettings
            | ContinueCmd
            | OpenPricingTutorial
            | OpenSearchLocation
            | GetEstimates GetQuotesRes
            | GetRideConfirmation RideBookingRes
            | GetQuotesList SelectListRes
            | MAPREADY String String String
            | AfterRender
            | UpdateSource Number Number String
            | Restart ErrorResponse
            | CurrentLocation String String
            | PrimaryButtonActionController PrimaryButtonController.Action
            | SettingSideBarActionController SettingSideBarController.Action
            | PricingTutorialModelActionController PricingTutorialModelController.Action
            | SourceToDestinationActionController SourceToDestinationController.Action
            | SearchLocationModelActionController SearchLocationModelController.Action
            | QuoteListModelActionController QuoteListModelController.Action
            | DriverInfoCardActionController DriverInfoCardController.Action
            | RatingCardAC RatingCard.Action
            | UpdateLocation String String String
            | CancelRidePopUpAction CancelRidePopUp.Action
            | PopUpModalAction PopUpModal.Action
            | TrackDriver GetDriverLocationResp
            | HandleCallback
            | UpdatePickupLocation String String String
            | CloseLocationTracking
            | ShowCallDialer CallType
            | CloseShowCallDialer
            | StartLocationTracking String
            | ExitLocationSelected LocationListItemState Boolean
            | DistanceOutsideLimitsActionController PopUpModal.Action
            | ShortDistanceActionController PopUpModal.Action
            | PickUpFarFromCurrentLocAC PopUpModal.Action
            | SourceUnserviceableActionController ErrorModalController.Action
            | UpdateCurrentLocation String String
            | UpdateCurrentStage String
            | GoBackToSearchLocationModal
            | SkipButtonActionController PrimaryButtonController.Action
            | SearchExpireCountDown Int String String String
            | EstimatesTryAgain GetQuotesRes
            | EstimateChangedPopUpController PopUpModal.Action
            | RateCardAction RateCard.Action
            | ShowRateCard
            | UpdateETA Int Int
            | EmergencyHelpModalAC EmergencyHelpController.Action
            | WaitingTimeAction String String Int
            | DriverArrivedAction String
            | PredictionClickedAction LocationListItemController.Action
            | SavedAddressClicked LocationTagBarController.Action
            | FavouriteLocationModelAC FavouriteLocationModelController.Action
            | UpdateSourceName Number Number String
            | SaveFavouriteCardAction SaveFavouriteCardController.Action
            | TagClick CardType (Maybe LocationListItemState)
            | ContinueWithoutOffers SelectListRes
            | CheckBoxClick Boolean
            | PreferencesDropDown
            | PopUpModalShareAppAction PopUpModal.Action
            | CallSupportAction PopUpModal.Action
            | RequestInfoCardAction RequestInfoCard.Action
            | OnIconClick Boolean
            | ReferralFlowAction
            | NewUser
            | MapReadyAction
            | CheckAndAskNotificationPermission
            | TrackLiveLocationAction
            | LottieLoaderAction
            | ReferralFlowNoAction
            | UpdateSourceFromPastLocations
            | UpdateLocAndLatLong String String
            | UpdateSavedLoc (Array LocationListItemState)
            | UpdateMessages String String String String
            | InitializeChat
            | RemoveChat
            | OpenChatScreen
            | ChatViewActionController ChatView.Action
            | HideLiveDashboard String
            | LiveDashboardAction
            | OnResumeCallback
            | CheckFlowStatusAction
            | GoToEditProfile
            | IsMockLocation String
            | MenuButtonActionController MenuButtonController.Action
            | ChooseYourRideAction ChooseYourRideController.Action
            | SearchForSelectedLocation
            | GenderBannerModal Banner.Action
            | CancelSearchAction PopUpModal.Action
            | TriggerPermissionFlow PermissionScreenStage
            | PopUpModalCancelConfirmationAction PopUpModal.Action
            | ScrollToBottom
            | SelectButton Int
            | RateClick Int
            | Support
            | IssueReportPopUpAC CancelRidePopUp.Action
            | IssueReportIndex Int
            | RideDetails
            | TerminateApp
            | DirectSearch
            | ZoneTimerExpired PopUpModal.Action
            | DisabilityBannerAC Banner.Action
            | DisabilityPopUpAC PopUpModal.Action
            | RideCompletedAC RideCompletedCard.Action
            | LoadMessages
            | KeyboardCallback String
            | NotifyDriverStatusCountDown Int String String String
            | UpdateProfileButtonAC PrimaryButtonController.Action 
            | SkipAccessibilityUpdateAC PrimaryButtonController.Action
            | SpecialZoneOTPExpiryAction Int String String String
            | TicketBookingFlowBannerAC Banner.Action
            | RepeatRide Int Trip
            | Scroll Number
            | WhereToClick 
            | ShowMoreSuggestions 
            | SuggestedDestinationClicked LocationListItemState
            | RepeatRideCountDown Int String String String
            | StopRepeatRideTimer 
            | OpenLiveDashboard
            | UpdatePeekHeight 

eval :: Action -> HomeScreenState -> Eval Action ScreenOutput HomeScreenState

eval ShowMoreSuggestions state = continue state { props {suggestionsListExpanded = not state.props.suggestionsListExpanded} }

eval UpdatePeekHeight state = continue state{data{peekHeight = getPeekHeight state}}

eval (Scroll item) state = do
  let sheetState = if item == state.props.currSlideIndex then state.props.isHomescreenExpanded
                   else item > state.props.currSlideIndex
  let updatedState = state { props { isHomescreenExpanded = sheetState, currSlideIndex = item } }
  continue updatedState

eval SearchForSelectedLocation state = do
  let currentStage = if state.props.searchAfterEstimate then TryAgain else FindingEstimate
  updateAndExit state{props{isPopUp = NoPopUp}} $ LocationSelected (fromMaybe dummyListItem state.data.selectedLocationListItem) false state{props{currentStage = currentStage, sourceSelectedOnMap = true, isPopUp = NoPopUp}}

eval CheckFlowStatusAction state = exit $ CheckFlowStatus state

eval TerminateApp state = do
  pure $ terminateApp state.props.currentStage true
  continue state

eval (KeyboardCallback keyBoardState) state = do 
  let isOpen = case keyBoardState of
                    "onKeyboardOpen" -> true
                    "onKeyboardClose" -> false
                    _ -> false 
  if isLocalStageOn ChatWithDriver && isOpen then
    void $ pure $ scrollToEnd (getNewIDWithTag "ChatScrollView") true 
  else pure unit
  continue state

eval (NotifyDriverStatusCountDown seconds id status timerID) state = do 
  if status == "EXPIRED" then do
    _ <- pure $ clearCountDownTimer timerID
    _ <- pure $ setValueToLocalStore NOTIFIED_CUSTOMER "true"
    if isLocalStageOn RideAccepted && (secondsToHms state.data.driverInfoCardState.eta) /= "" then 
      continue state{data{lastMessage = state.data.lastMessage{message = state.data.config.notifyRideConfirmationConfig.autoGeneratedText <> (secondsToHms state.data.driverInfoCardState.eta), sentBy = "Driver"}},props{unReadMessages = true, showChatNotification = true}}
      else continue state
    else continue state

eval (RepeatRideCountDown seconds id status timerID) state = do
  if status == "EXPIRED" then do
    _ <- pure $ clearCountDownTimer timerID
    _ <- pure $ performHapticFeedback unit
    _ <- pure $ updateLocalStage FindingQuotes
    let updatedState = state{data{rideHistoryTrip = Nothing}, props{repeatRideTimerId = "", currentStage = FindingQuotes, searchExpire = (getSearchExpiryTime "LazyCheck")}}
    updateAndExit (updatedState) (GetQuotes updatedState)
  else continue state{props{repeatRideTimer = (show seconds), repeatRideTimerId = timerID}}

eval StopRepeatRideTimer state =  do
  _ <- pure $ clearCountDownTimer state.props.repeatRideTimerId
  continue state{props{repeatRideTimer = "", repeatRideTimerId = ""}}

eval (IsMockLocation isMock) state = do
  let val = isMock == "true"
      _ = unsafePerformEffect $ if val then  logEvent (state.data.logField) "ny_fakeGPS_enabled" else pure unit -- we are using unsafePerformEffect becasue without it we are not getting logs in firebase, since we are passing a parameter from state i.e. logField then the output will be inline and it will not be able to precompute so it's safe to use it here.
  continue state{props{isMockLocation = val}}

eval (UpdateCurrentStage stage) state = do
  _ <- pure $ spy "updateCurrentStage" stage
  if (stage == "INPROGRESS") && (not $ isLocalStageOn RideStarted) then
    exit $ NotificationHandler "TRIP_STARTED" state
  else if (stage == "COMPLETED") && (not $ isLocalStageOn HomeScreen) then
    exit $ NotificationHandler "TRIP_FINISHED" state
  else if (stage == "CANCELLED") && (not $ isLocalStageOn HomeScreen) then
    exit $ NotificationHandler "CANCELLED_PRODUCT" state
  else
    continue state

eval OnResumeCallback state =
  if(state.props.currentStage == RideAccepted || state.props.currentStage == RideStarted) && state.props.emergencyHelpModelState.waitingDialerCallback then 
    continue state {props {emergencyHelpModelState {showCallSuccessfulPopUp = true}}}
  else 
    case getValueToLocalNativeStore LOCAL_STAGE of
      "FindingQuotes" -> do
        let secondsLeft = findingQuotesSearchExpired false
        case (methodArgumentCount "startLottieProcess") == 1 of
          true  -> do
            let findingQuotesProgress = 1.0 - (toNumber secondsLeft)/(toNumber (getSearchExpiryTime "LazyCheck"))
            if secondsLeft > 0 then
              void $ pure $ startLottieProcess lottieAnimationConfig {rawJson = "progress_loader_line", lottieId = (getNewIDWithTag "lottieLoaderAnimProgress"), minProgress = findingQuotesProgress, scaleType="CENTER_CROP"}
            else pure unit
          false -> pure unit
        case flowWithoutOffers WithoutOffers of
          true | secondsLeft <= 0 -> do
              _ <- pure $ updateLocalStage QuoteList
              continueWithCmd state [do
                let response = SelectListRes { selectedQuotes: Nothing, bookingId : Nothing }
                pure $ GetQuotesList response
              ]
          _ -> continue state
      "QuoteList" -> do
        let findingQuotesProgress = 1.0 - 30.0/(toNumber (getSearchExpiryTime "LazyCheck"))
        void $ pure $ startLottieProcess lottieAnimationConfig {rawJson = "progress_loader_line", lottieId = (getNewIDWithTag "lottieLoaderAnimProgress"), minProgress = findingQuotesProgress, scaleType="CENTER_CROP"}
        continue state
      "RideAccepted" | state.data.currentSearchResultType == QUOTES -> exit $ Retry state
      _ -> continue state

eval (UpdateSavedLoc savedLoc) state = continue state{data{savedLocations = savedLoc}}

eval ( RideCompletedAC (RideCompletedCard.SelectButton index)) state = continue state { data { ratingViewState { selectedYesNoButton = index, doneButtonVisibility = index == 1}}}

eval ( RideCompletedAC (RideCompletedCard.RateClick index)) state = do
  _ <- pure $ setValueToLocalStore REFERRAL_STATUS "HAS_TAKEN_RIDE"
  continue
    state
      { props { currentStage = RideRating }
      , data
        { rideRatingState 
            { rating = index
            , feedbackList = state.data.rideRatingState.feedbackList
            }
          , ratingViewState { selectedRating = index }
        }
      }

eval ( RideCompletedAC (RideCompletedCard.IssueReportIndex index)) state =
  case index of
    0 -> continue state { data { ratingViewState { openReportIssue = true }}}
    1 -> exit $ ReportIssue state { data {  ratingViewState { issueReason = Nothing }}}
    _ -> continue state

eval (RideCompletedAC (RideCompletedCard.Support)) state = continue state {props {callSupportPopUp = true}}

eval (RideCompletedAC (RideCompletedCard.RideDetails)) state = exit $ RideDetailsScreen state -- TODO needs to fill the data

------------------------------- ChatService - Start --------------------------

eval (UpdateMessages message sender timeStamp size) state = do
  if not state.props.chatcallbackInitiated then continue state {props {canSendSuggestion = true}} else do
    continueWithCmd state{data{messagesSize = size}, props {canSendSuggestion = true}} [do
      pure $ (DriverInfoCardActionController (DriverInfoCardController.LoadMessages))
    ]

eval LoadMessages state = do
  continueWithCmd state [do
    pure $ (DriverInfoCardActionController (DriverInfoCardController.LoadMessages))
  ]

eval (DriverInfoCardActionController (DriverInfoCardController.LoadMessages)) state = do
  let allMessages = getChatMessages ""
  case (last allMessages) of
      Just value -> if value.message == "" then continue state {data { messagesSize = show (fromMaybe 0 (fromString state.data.messagesSize) + 1)}, props {canSendSuggestion = true, isChatNotificationDismissed = false}} else
                      if value.sentBy == "Customer" then updateMessagesWithCmd state {data {messages = allMessages, suggestionsList = []}, props {canSendSuggestion = true,  isChatNotificationDismissed = false}}
                      else do
                        let readMessages = fromMaybe 0 (fromString (getValueToLocalNativeStore READ_MESSAGES))
                            unReadMessages = (if readMessages == 0 && state.props.currentStage /= ChatWithDriver then true else (if (readMessages < (length allMessages) && state.props.currentStage /= ChatWithDriver) then true else false))
                            suggestions = getCustomerSuggestions state $ getSuggestionsfromKey value.message
                            isChatNotificationDismissed = not state.props.isChatNotificationDismissed || state.data.lastMessage.message /= value.message
                        updateMessagesWithCmd state {data {messages = allMessages, suggestionsList = suggestions, lastMessage = value }, props {unReadMessages = unReadMessages, showChatNotification = isChatNotificationDismissed && unReadMessages, canSendSuggestion = true,  isChatNotificationDismissed = false}}
      Nothing -> continue state {props {canSendSuggestion = true}}

eval (OpenChatScreen) state = do
  if not state.props.chatcallbackInitiated then continue state else do
    continueWithCmd state{props{openChatScreen = false}} [do
      pure $ (DriverInfoCardActionController (DriverInfoCardController.MessageDriver))
    ]

eval (ChatViewActionController (ChatView.TextChanged value)) state = continue state{data{messageToBeSent = (STR.trim value)},props{sendMessageActive = (STR.length (STR.trim value)) >= 1}}

eval(ChatViewActionController (ChatView.Call)) state = do
  _ <- pure $ performHapticFeedback unit
  _ <- pure $ hideKeyboardOnNavigation true
  if length state.data.config.callOptions > 1 then
    continue state { props { showCallPopUp = true } }
  else callDriver state $ fromMaybe "ANONYMOUS" $ state.data.config.callOptions !! 0

eval (ChatViewActionController (ChatView.SendMessage)) state = do
  if state.data.messageToBeSent /= ""
  then do
    pure $ sendMessage state.data.messageToBeSent
    pure $ setText (getNewIDWithTag "ChatInputEditText") ""
    continue state{data{messageToBeSent = ""},props {sendMessageActive = false}}
  else
    continue state

eval (ChatViewActionController (ChatView.BackPressed)) state = do
  _ <- pure $ performHapticFeedback unit
  _ <- pure $ hideKeyboardOnNavigation true
  continueWithCmd state [do
      pure $ BackPressed
    ]

eval ScrollToBottom state = do
  _ <- pure $ scrollToEnd (getNewIDWithTag "ChatScrollView") true
  continue state

eval InitializeChat state = do
  continue state {props { chatcallbackInitiated = true } }


eval RemoveChat state = do
  continueWithCmd state {props{chatcallbackInitiated = false}} [ do
    _ <- stopChatListenerService
    _ <- pure $ setValueToLocalNativeStore READ_MESSAGES "0"
    pure $ NoAction
  ]

eval (DriverInfoCardActionController (DriverInfoCardController.MessageDriver)) state = do
  if (getValueFromConfig "isChatEnabled") == "true" then do
    if not state.props.chatcallbackInitiated then continue state else do
      _ <- pure $ performHapticFeedback unit
      _ <- pure $ updateLocalStage ChatWithDriver
      _ <- pure $ setValueToLocalNativeStore READ_MESSAGES (show (length state.data.messages))
      let allMessages = getChatMessages ""
      continue state {data{messages = allMessages}, props {currentStage = ChatWithDriver, sendMessageActive = false, unReadMessages = false, showChatNotification = false, isChatOpened = true , isChatNotificationDismissed = false}}
  else continueWithCmd state[ do
        pure $ DriverInfoCardActionController (DriverInfoCardController.CallDriver)
      ]

eval (DriverInfoCardActionController (DriverInfoCardController.RemoveNotification)) state = do
  continue state {props { showChatNotification = false, isChatNotificationDismissed = true}}

eval (ChatViewActionController (ChatView.SendSuggestion chatSuggestion)) state = do
  if state.props.canSendSuggestion then do
    let message = getMessageFromKey chatSuggestion "EN_US"
    _ <- pure $ sendMessage message
    let _ = unsafePerformEffect $ logEvent state.data.logField $ "ny_" <> STR.toLower (STR.replaceAll (STR.Pattern "'") (STR.Replacement "") (STR.replaceAll (STR.Pattern ",") (STR.Replacement "") (STR.replaceAll (STR.Pattern " ") (STR.Replacement "_") chatSuggestion)))
    continue state {data {suggestionsList = []}, props {canSendSuggestion = false}}
  else continue state

------------------------------- ChatService - End --------------------------

eval (DriverInfoCardActionController (DriverInfoCardController.CallDriver)) state = do
  if length state.data.config.callOptions > 1 then
    continue state { props { showCallPopUp = true } }
  else callDriver state $ fromMaybe "ANONYMOUS" $ state.data.config.callOptions !! 0

eval DirectSearch state =continue state{props{currentStage = SearchLocationModel}}

eval BackPressed state = do
  _ <- pure $ toggleBtnLoader "" false
  case state.props.currentStage of
    SearchLocationModel -> do
                            if state.props.isSaveFavourite then continueWithCmd state [pure $ (SaveFavouriteCardAction (SaveFavouriteCardController.OnClose))]
                              else do
                                if state.props.isSearchLocation == LocateOnMap then do
                                    _ <- pure $ exitLocateOnMap ""
                                    _ <- pure $ hideKeyboardOnNavigation true
                                    continue state{props{isSearchLocation = SearchLocation, locateOnMap = false}}
                                  else do
                                    if (getSearchType unit) == "direct_search" then
                                      pure $ terminateApp state.props.currentStage false
                                      else pure unit
                                    updateAndExit state{props{currentStage = HomeScreen}} $ GoToHome
    SettingPrice    -> do
                      _ <- pure $ performHapticFeedback unit
                      _ <- pure $ clearCountDownTimer state.props.repeatRideTimerId
                      let updatedState = state{props{repeatRideTimer = "", repeatRideTimerId = ""}}
                      if updatedState.props.showRateCard then do
                        if (updatedState.data.rateCard.currentRateCardType /= DefaultRateCard) then
                         continue updatedState{data{rateCard {currentRateCardType = DefaultRateCard}}}
                        else continue updatedState{props{showRateCard = false}}
                      else if updatedState.props.showMultipleRideInfo then continue updatedState{props{showMultipleRideInfo=false}}
                        else do
                        _ <- pure $ updateLocalStage SearchLocationModel
                        continue updatedState{data{rideHistoryTrip = Nothing},props{rideRequestFlow = false, currentStage = SearchLocationModel, searchId = "", isSource = Just false,isSearchLocation = SearchLocation, isRepeatRide = false}}
    ConfirmingLocation -> do
                      _ <- pure $ performHapticFeedback unit
                      _ <- pure $ exitLocateOnMap ""
                      _ <- pure $ removeAllPolylines ""
                      _ <- pure $ updateLocalStage SearchLocationModel
                      continue state{props{defaultPickUpPoint = "", rideRequestFlow = false, currentStage = SearchLocationModel, searchId = "", isSource = Just false,isSearchLocation = SearchLocation},data{polygonCoordinates = "", nearByPickUpPoints = []}}
    FindingEstimate -> do
                      _ <- pure $ performHapticFeedback unit
                      _ <- pure $ updateLocalStage SearchLocationModel
                      continue state{props{rideRequestFlow = false, currentStage = SearchLocationModel, searchId = "", isSource = Just false,isSearchLocation = SearchLocation}}
    QuoteList       -> do
                      _ <- pure $ performHapticFeedback unit
                      if state.props.isPopUp == NoPopUp then continue $ state { props{isPopUp = ConfirmBack}} else continue state
    PricingTutorial -> do
                      _ <- pure $ performHapticFeedback unit
                      continue state { props { currentStage = SettingPrice}}
    DistanceOutsideLimits -> do
                      _ <- pure $ performHapticFeedback unit
                      _ <- pure $ updateLocalStage SearchLocationModel
                      continue state{props{rideRequestFlow = false, currentStage = SearchLocationModel, searchId = "", isSource = Just false,isSearchLocation = SearchLocation }}
    ShortDistance -> do
                      _ <- pure $ performHapticFeedback unit
                      _ <- pure $ updateLocalStage SearchLocationModel
                      continue state{props{isSource = Just false,isPopUp = NoPopUp, rideRequestFlow = false, currentStage = SearchLocationModel, searchId = "", isSearchLocation = SearchLocation}}
    FindingQuotes ->  do
                      _ <- pure $ performHapticFeedback unit
                      continue $ state { props{isPopUp = ConfirmBack}}
    FavouriteLocationModel -> do
                      _ <- pure $ performHapticFeedback unit
                      _ <- pure $ updateLocalStage (if state.props.isSearchLocation == NoView then HomeScreen else SearchLocationModel)
                      continue state { props { currentStage = if state.props.isSearchLocation == NoView then HomeScreen else SearchLocationModel}}
    ChatWithDriver -> do
                        if state.props.showCallPopUp then continue state {props{showCallPopUp = false}}
                         else do
                            _ <- pure $ updateLocalStage RideAccepted
                            continue state {props {currentStage = RideAccepted}}
    RideRating ->     do
                      _ <- pure $ updateLocalStage RideCompleted
                      continue state {props {currentStage = RideCompleted}}
    _               -> do
                        if state.props.isLocationTracking then continue state{props{isLocationTracking = false}}
                          else if state.props.cancelSearchCallDriver then continue state{props{cancelSearchCallDriver = false}}
                          else if state.props.showCallPopUp then continue state{props{showCallPopUp = false}}
                          else if state.props.isCancelRide then continue state{props{isCancelRide = false}}
                          else if state.props.isSaveFavourite then continueWithCmd state [pure $ SaveFavouriteCardAction SaveFavouriteCardController.OnClose]
                          else if state.props.showShareAppPopUp then continue state{props{showShareAppPopUp=false}}
                          else if state.props.showMultipleRideInfo then continue state{props{showMultipleRideInfo=false}}
                          else if state.props.emergencyHelpModelState.showContactSupportPopUp then continue state {props {emergencyHelpModelState{showContactSupportPopUp = false}}}
                          else if state.props.emergencyHelpModelState.showCallPolicePopUp then continue state {props{emergencyHelpModelState{showCallPolicePopUp = false}}}
                          else if state.props.emergencyHelpModelState.showCallSuccessfulPopUp then continue state {props{emergencyHelpModelState{showCallSuccessfulPopUp = false}}}
                          else if state.props.showLiveDashboard then do
                            continueWithCmd state [do
                              _ <- pure $ goBackPrevWebPage (getNewIDWithTag "webview")
                              pure NoAction
                            ]
                          else if state.props.emergencyHelpModal then continue state {props {emergencyHelpModal = false}}
                          else if state.props.callSupportPopUp then continue state {props {callSupportPopUp = false}}
                          else if state.data.ratingViewState.openReportIssue then continue state {data {ratingViewState {openReportIssue = false}}}
                          else if state.props.showEducationalCarousel then do 
                            _ <- pure $ pauseYoutubeVideo unit
                            continue state{props{showEducationalCarousel = false}}
                          else do
                              pure $ terminateApp state.props.currentStage true
                              continue state

eval GoBackToSearchLocationModal state = do
  _ <- pure $ updateLocalStage SearchLocationModel
  continue state { props { rideRequestFlow = false, currentStage = SearchLocationModel, searchId = "", isSearchLocation = SearchLocation, isSource = Just true, isSrcServiceable = true, isRideServiceable = true } }

eval HandleCallback state = do
  continue state { props { callbackInitiated = true } }

eval (UpdateSource lat lng name) state = do
  _ <- pure $ printLog "Name::" name
  exit $ UpdatedState state { data { source = name, sourceAddress = encodeAddress name [] state.props.sourcePlaceId}, props { sourceLat = lat, sourceLong = lng, searchLocationModelProps{crossBtnSrcVisibility = (STR.length name) > 2}} } true

eval (HideLiveDashboard val) state = continue state {props {showLiveDashboard =false}}

eval LiveDashboardAction state = do
  _ <- pure $ firebaseLogEvent "ny_user_on_ride_live_stats"
  if os == "IOS" then do
      continueWithCmd state [do
        _ <- openUrlInApp "https://nammayatri.in/open?source=in-app"
        pure NoAction
      ]
  else continue state {props {showLiveDashboard = true}}


eval (UpdateSourceName lat lon name) state = continue state {data{source = name, sourceAddress = encodeAddress name [] state.props.sourcePlaceId}, props{searchLocationModelProps{crossBtnSrcVisibility = (STR.length name) > 2}} }

eval (MAPREADY key latitude longitude) state =
  case key of
    _ -> continueWithCmd state [ do
      _ <- checkPermissionAndUpdatePersonMarker state
      pure AfterRender
    ]

eval OpenSearchLocation state = do
  _ <- pure $ performHapticFeedback unit
  let srcValue = if state.data.source == "" then (getString CURRENT_LOCATION) else state.data.source
  exit $ UpdateSavedLocation state { props { isSource = Just true, currentStage = SearchLocationModel, isSearchLocation = SearchLocation, searchLocationModelProps{crossBtnSrcVisibility = (STR.length srcValue) > 2}}, data {source=srcValue} }

eval (SourceUnserviceableActionController (ErrorModalController.PrimaryButtonActionController PrimaryButtonController.OnClick)) state = continueWithCmd state [ do pure $ OpenSearchLocation ]

eval (UpdateLocation key lat lon) state = case key of
  "LatLon" -> do
    exit $ UpdateLocationName state{props{defaultPickUpPoint = ""}} (fromMaybe 0.0 (NUM.fromString lat)) (fromMaybe 0.0 (NUM.fromString lon))
  _ ->  if length (filter( \ (item) -> (item.place == key)) state.data.nearByPickUpPoints) > 0 then do
          exit $ UpdateLocationName state{props{defaultPickUpPoint = key}} (fromMaybe 0.0 (NUM.fromString lat)) (fromMaybe 0.0 (NUM.fromString lon))
        else continue state

eval (UpdatePickupLocation  key lat lon) state =
  case key of
    "LatLon" -> do
      exit $ UpdatePickupName state{props{defaultPickUpPoint = ""}} (fromMaybe 0.0 (NUM.fromString lat)) (fromMaybe 0.0 (NUM.fromString lon))
    _ -> do
      let focusedIndex = findIndex (\item -> item.place == key) state.data.nearByPickUpPoints
      case focusedIndex of
        Just index -> do
          _ <- pure $ scrollViewFocus (getNewIDWithTag "scrollViewParent") index
          exit $ UpdatePickupName state{props{defaultPickUpPoint = key}} (fromMaybe 0.0 (NUM.fromString lat)) (fromMaybe 0.0 (NUM.fromString lon))
        Nothing -> continue state

eval (CheckBoxClick autoAssign) state = do
  _ <- pure $ performHapticFeedback unit
  let event = if autoAssign then "ny_user_pref_autoassigned" else "ny_user_pref_driveroffers"
  let _ = unsafePerformEffect $ logEvent state.data.logField event
  _ <- pure $ setValueToLocalStore FLOW_WITHOUT_OFFERS (show autoAssign)
  _ <- pure $ setValueToLocalStore TEST_MINIMUM_POLLING_COUNT $ if autoAssign then "4" else "17"
  _ <- pure $ setValueToLocalStore TEST_POLLING_INTERVAL $ if autoAssign then "8000.0" else "1500.0"
  _ <- pure $ setValueToLocalStore TEST_POLLING_COUNT $ if autoAssign then "22" else "117"
  continue state{props{flowWithoutOffers = (show autoAssign) == "true"}}

eval (OnIconClick autoAssign) state = do
  continue state { props {showMultipleRideInfo = not autoAssign}}

eval PreferencesDropDown state = do
  continue state { data { showPreferences = not state.data.showPreferences}}

eval (RatingCardAC (RatingCard.Rating index)) state = do
  let feedbackListArr = if index == state.data.rideRatingState.rating then state.data.rideRatingState.feedbackList else []
  continue state { data { rideRatingState { rating = index , feedbackList = feedbackListArr}, ratingViewState { selectedRating = index} } }

eval (RatingCardAC (RatingCard.SelectPill feedbackItem id)) state = do
  let newFeedbackList = updateFeedback id feedbackItem state.data.rideRatingState.feedbackList
      filterFeedbackList = filter (\item -> length item.answer > 0) newFeedbackList
  continue state { data { rideRatingState {  feedbackList = filterFeedbackList} } }

eval (RatingCardAC (RatingCard.PrimaryButtonAC PrimaryButtonController.OnClick)) state = updateAndExit state $ SubmitRating state

eval (RatingCardAC (RatingCard.FeedbackChanged value)) state = continue state { data { rideRatingState { feedback = value } } }

eval (RatingCardAC (RatingCard.BackPressed)) state = do
  _ <- pure $ updateLocalStage RideCompleted
  continue state {props {currentStage = RideCompleted}}

eval (SettingSideBarActionController (SettingSideBarController.PastRides)) state = do
  let _ = unsafePerformEffect $ logEvent state.data.logField "ny_user_myrides_click"
  exit $ PastRides state { data { settingSideBar { opened = SettingSideBarController.OPEN } } }

eval (SettingSideBarActionController (SettingSideBarController.OnHelp)) state = exit $ GoToHelp state { data { settingSideBar { opened = SettingSideBarController.OPEN } } }

eval (SettingSideBarActionController (SettingSideBarController.ChangeLanguage)) state = do
  let _ = unsafePerformEffect $ logEvent state.data.logField "ny_user_language"
  exit $ ChangeLanguage state { data { settingSideBar { opened = SettingSideBarController.OPEN } } }

eval (SettingSideBarActionController (SettingSideBarController.GoToAbout)) state = do
  let _ = unsafePerformEffect $ logEvent state.data.logField "ny_user_about"
  exit $ GoToAbout state { data { settingSideBar { opened = SettingSideBarController.OPEN } } }

eval (SettingSideBarActionController (SettingSideBarController.GoToEmergencyContacts)) state = do
  let _ = unsafePerformEffect $ logEvent state.data.logField "ny_user_emergency_contacts"
  exit $ GoToEmergencyContacts state { data{settingSideBar{opened = SettingSideBarController.OPEN}}}

eval (SettingSideBarActionController (SettingSideBarController.GoToMyTickets)) state = do
  let _ = unsafePerformEffect $ logEvent state.data.logField "ny_user_zoo_tickets"
  exit $ GoToMyTickets state { data{settingSideBar{opened = SettingSideBarController.OPEN}}}

eval (SettingSideBarActionController (SettingSideBarController.ShareAppLink)) state =
  do
    let _ = unsafePerformEffect $ logEvent state.data.logField "ny_user_share_app_menu"
    _ <- pure $ shareTextMessage (getValueFromConfig "shareAppTitle") (getValueFromConfig "shareAppContent")
    continue state

eval (SettingSideBarActionController (SettingSideBarController.EditProfile)) state = exit $ GoToMyProfile state { data { settingSideBar { opened = SettingSideBarController.OPEN } } } false

eval (SettingSideBarActionController (SettingSideBarController.OnClosed)) state = continue state{ data{settingSideBar {opened = SettingSideBarController.CLOSED}}}

eval (SettingSideBarActionController (SettingSideBarController.OnClose)) state =
  if state.props.showLiveDashboard then do
    continueWithCmd state [do
      _ <- pure $ goBackPrevWebPage (getNewIDWithTag "webview")
      pure NoAction
    ]
    else if state.props.isPopUp == Logout then
      continue state {props{isPopUp = NoPopUp}}
      else case state.data.settingSideBar.opened of
                SettingSideBarController.CLOSED -> do
                                                    if state.props.currentStage == HomeScreen then do
                                                      pure $ terminateApp state.props.currentStage false
                                                      continue state
                                                      else continueWithCmd state [pure $ BackPressed]
                _                               -> continue state {data{settingSideBar{opened = SettingSideBarController.CLOSING}}}

eval (SettingSideBarActionController (SettingSideBarController.OnLogout)) state = continue state { props { isPopUp = Logout } }

eval (SettingSideBarActionController (SettingSideBarController.GoToFavourites)) state = exit $ GoToFavourites state {data{settingSideBar{opened = SettingSideBarController.OPEN}}}

eval (SettingSideBarActionController (SettingSideBarController.GoToMyProfile)) state = exit $ GoToMyProfile state { data { settingSideBar { opened = SettingSideBarController.OPEN } } } false

eval (SettingSideBarActionController (SettingSideBarController.LiveStatsDashboard)) state = do
  _ <- pure $ setValueToLocalStore LIVE_DASHBOARD "LIVE_DASHBOARD_SELECTED"
  let _ = unsafePerformEffect $ logEvent state.data.logField "ny_user_live_stats_dashboard"
  if os == "IOS" then do
    continueWithCmd state [do
      _ <- openUrlInApp state.data.config.dashboardUrl
      pure NoAction
    ]
  else continue state {props {showLiveDashboard = true}}

eval OpenLiveDashboard state = do
  _ <- pure $ setValueToLocalStore LIVE_DASHBOARD "LIVE_DASHBOARD_SELECTED"
  let _ = unsafePerformEffect $ logEvent state.data.logField "ny_user_live_stats_dashboard"
  if os == "IOS" then do
    continueWithCmd state [do
      _ <- openUrlInApp state.data.config.dashboardUrl
      pure NoAction
    ]
  else continue state {props {showLiveDashboard = true}}

eval (SearchLocationModelActionController (SearchLocationModelController.PrimaryButtonActionController PrimaryButtonController.OnClick)) state = do
  _ <- pure $ performHapticFeedback unit
  _ <- pure $ exitLocateOnMap ""
  let newState = state{props{isSource = Just false, isSearchLocation = SearchLocation, currentStage = SearchLocationModel, locateOnMap = false, defaultPickUpPoint = ""}}
  updateAndExit newState $ LocationSelected (fromMaybe dummyListItem (if state.props.isSource == Just false then state.data.selectedLocationListItem else Nothing)) (if state.props.isSource == Just false then true else false) newState

eval (PrimaryButtonActionController (PrimaryButtonController.OnClick)) state = do
    _ <- pure $ spy "state homeScreen" state
    case state.props.currentStage of
      HomeScreen   -> do
        _ <- pure $ performHapticFeedback unit
        let _ = unsafePerformEffect $ logEvent state.data.logField "ny_user_where_to_btn"
        exit $ UpdateSavedLocation state{props{isSource = Just false, isSearchLocation = SearchLocation, currentStage = SearchLocationModel, searchLocationModelProps{crossBtnSrcVisibility = false}}, data{source=(getString CURRENT_LOCATION)}}
      ConfirmingLocation -> do
        _ <- pure $ performHapticFeedback unit
        _ <- pure $ exitLocateOnMap ""
        _ <- pure $ updateLocalStage FindingEstimate
        let _ = unsafePerformEffect $ logEvent state.data.logField "ny_user_confirm_pickup"
        let updatedState = state{props{currentStage = FindingEstimate, locateOnMap = false}}
        updateAndExit updatedState $  (UpdatedSource updatedState)
      SettingPrice -> do
                        _ <- pure $ performHapticFeedback unit
                        _ <- pure $ updateLocalStage FindingQuotes
                        let updatedState = state{data{rideHistoryTrip = Nothing}, props{currentStage = FindingQuotes, searchExpire = (getSearchExpiryTime "LazyCheck")}}
                        updateAndExit (updatedState) (GetQuotes updatedState)
      _            -> continue state

eval WhereToClick state = do
  _ <- pure $ performHapticFeedback unit
  let _ = unsafePerformEffect $ logEvent state.data.logField "ny_user_where_to_btn"
      updateState = state{props{isSource = Just false, isSearchLocation = SearchLocation, currentStage = SearchLocationModel, searchLocationModelProps{crossBtnSrcVisibility = false}}, data{source=(getString CURRENT_LOCATION)}}
  exit $ UpdateSavedLocation updateState 

eval (RideCompletedAC (RideCompletedCard.SkipButtonActionController (PrimaryButtonController.OnClick))) state =
  case state.data.ratingViewState.issueFacedView of
    true -> do
            _ <- pure $ setValueToLocalStore REFERRAL_STATUS "HAS_TAKEN_RIDE"
            continue
              state
                { props { currentStage = RideRating }
                , data
                  { rideRatingState =
                    dummyRideRatingState
                      { driverName = state.data.driverInfoCardState.driverName
                      , rideId = state.data.driverInfoCardState.rideId
                      }
                  }
                }
    _ ->  if state.data.ratingViewState.selectedRating > 0 then updateAndExit state $ SubmitRating state{ data {rideRatingState {rating = state.data.ratingViewState.selectedRating }}}
          else do
            _ <- pure $ firebaseLogEvent "ny_user_ride_skip_feedback"
            _ <- pure $ setValueToLocalStore RATING_SKIPPED "true"
            _ <- pure $ runFn3 emitJOSEvent "java" "onEvent" $ encode $ EventPayload {
                                        event : "process_result"
                                      , payload : Just {
                                        action : "feedback_skipped"
                                      , trip_amount : Just state.data.finalAmount
                                      , trip_id : Just state.props.bookingId
                                      , ride_status : Nothing
                                      , screen : Just $ getScreenFromStage state.props.currentStage
                                      , exit_app : false
                                      }
                                      }
            updateAndExit state GoToHome

eval OpenSettings state = do
  _ <- pure $ hideKeyboardOnNavigation true
  let _ = unsafePerformEffect $ logEvent state.data.logField "ny_user_burger_menu"
  continue state { data { settingSideBar { opened = SettingSideBarController.OPEN } } }

eval (SearchExpireCountDown seconds id status timerID) state = do
  if status == "EXPIRED" then do
    _ <- pure $ clearTimer timerID
    let tipViewData = HomeScreenData.initData.props.tipViewProps
    _ <- pure $ setTipViewData (TipViewData { stage : tipViewData.stage , activeIndex : tipViewData.activeIndex , isVisible : tipViewData.isVisible })
    continue state { props { searchExpire = seconds } }
  else do
    let enableTips = isTipEnabled state
    if any ( _ == state.props.currentStage) [FindingQuotes , QuoteList] then continue state { props { searchExpire = seconds ,timerId = timerID , tipViewProps {isVisible = enableTips && (seconds <= (getSearchExpiryTime "LazyCheck")-30 || state.props.tipViewProps.isVisible) && (state.props.customerTip.tipActiveIndex >0) }, customerTip{enableTips = enableTips}} }
      else do
        _ <- pure $ clearTimer timerID
        let tipViewData = HomeScreenData.initData.props.tipViewProps
        _ <- pure $ setTipViewData (TipViewData { stage : tipViewData.stage , activeIndex : tipViewData.activeIndex , isVisible : tipViewData.isVisible })
        continue state { props { searchExpire = (getSearchExpiryTime "LazyCheck") ,timerId = timerID , tipViewProps {isVisible = false}} }

eval CancelSearch state = case state.props.currentStage of
  FindingEstimate -> do
    _ <- pure $ performHapticFeedback unit
    _ <- pure $ updateLocalStage SearchLocationModel
    let _ = unsafePerformEffect $ logEvent state.data.logField  "ny_user_estimate_cancel_search"
    continue state { props { currentStage = SearchLocationModel, rideRequestFlow = false, isSearchLocation = SearchLocation } }
  ConfirmingRide -> do
    _ <- pure $ performHapticFeedback unit
    continue state { props { currentStage = SettingPrice, isSearchLocation = NoView } }
  _ -> continue state

eval SidebarCloseAnimationCompleted state = continue state --{props{sideBarStatus = SettingSideBarController.CLOSED}}

eval OpenPricingTutorial state = continue state { props { currentStage = PricingTutorial } }

eval (PricingTutorialModelActionController (PricingTutorialModelController.Close)) state = continue state { props { currentStage = SettingPrice } }

eval (DriverInfoCardActionController (DriverInfoCardController.PrimaryButtonAC PrimaryButtonController.OnClick)) state = do
  _ <- pure $ performHapticFeedback unit
  continueWithCmd state
    [ do
        _ <- pure $ showDialer (getDriverNumber "") false -- TODO: FIX_DIALER
        _ <- (logEventWithTwoParams state.data.logField "ny_user_call_click" "trip_id" (state.props.bookingId) "user_id" (getValueToLocalStore CUSTOMER_ID))
        pure NoAction
    ]
eval (DriverArrivedAction driverArrivalTime) state = do
  _ <- pure $ setValueToLocalStore DRIVER_ARRIVAL_ACTION "TRIGGER_WAITING_ACTION"
  exit $ Cancel state { data { driverInfoCardState { driverArrived = true, driverArrivalTime = getExpiryTime driverArrivalTime true } } }

eval (WaitingTimeAction timerID timeInMinutes seconds) state = do
  _ <- pure $ if getValueToLocalStore DRIVER_ARRIVAL_ACTION == "TRIGGER_WAITING_ACTION"
                then setValueToLocalStore DRIVER_ARRIVAL_ACTION "WAITING_ACTION_TRIGGERED"
                else pure unit
  continue state { data { driverInfoCardState { waitingTime = timeInMinutes} }, props { waitingTimeTimerIds = union state.props.waitingTimeTimerIds [timerID] } }

eval (SpecialZoneOTPExpiryAction seconds id status timerID) state = do
  if status == "EXPIRED" then do
    _ <- pure $ toast $ getString OTP_FOR_THE_JATRI_SATHI_ZONE_HAS_BEEN_EXPIRED_PLEASE_TRY_LOOKING_AGAIN
    _ <- pure $ clearTimer timerID
    exit $ NotificationHandler "CANCELLED_PRODUCT" state
  else do
    let timeInMinutes = formatDigits $ seconds/60
        timeInSeconds = formatDigits $ seconds - (seconds/60) * 60
    continue state { data { driverInfoCardState { waitingTime = timeInMinutes <> " : " <> timeInSeconds } }, props { waitingTimeTimerIds = union state.props.waitingTimeTimerIds [timerID] } }
  where
    formatDigits :: Int -> String
    formatDigits time = (if time >= 10 then "" else "0") <> show time

eval (DriverInfoCardActionController (DriverInfoCardController.OnNavigate)) state = do
  void $ pure $ openNavigation 0.0 0.0 state.data.driverInfoCardState.destinationLat state.data.driverInfoCardState.destinationLng "DRIVE"
  continue state

eval (DriverInfoCardActionController (DriverInfoCardController.OnNavigateToZone)) state = do
  void $ pure $ openNavigation 0.0 0.0 state.data.driverInfoCardState.sourceLat state.data.driverInfoCardState.sourceLng "WALK"
  continue state

eval (DriverInfoCardActionController (DriverInfoCardController.Support)) state = do
  _ <- pure $ performHapticFeedback unit
  continue state{props{callSupportPopUp = true}}

eval (CancelSearchAction PopUpModal.DismissPopup) state = do continue state {props { cancelSearchCallDriver = false }}

eval (CancelSearchAction PopUpModal.OnButton1Click) state = do
  if length state.data.config.callOptions > 1 then
    continue state { props { showCallPopUp = true, cancelSearchCallDriver = false } }
  else callDriver state $ fromMaybe "ANONYMOUS" $ state.data.config.callOptions !! 0

eval (CancelSearchAction PopUpModal.OnButton2Click) state = do
  continue state { props { isCancelRide = true, cancellationReasons = cancelReasons "", cancelRideActiveIndex = Nothing, cancelReasonCode = "", cancelDescription = "", cancelSearchCallDriver = false } }

eval (DriverInfoCardActionController (DriverInfoCardController.CancelRide infoCard)) state =
  if state.data.config.driverInfoConfig.showCancelPrevention && not state.props.isSpecialZone then
    continue state { props { cancelSearchCallDriver = true } }
      else continueWithCmd state [ pure $ CancelSearchAction PopUpModal.OnButton2Click]

eval (DriverInfoCardActionController (DriverInfoCardController.LocationTracking)) state = do
  _ <- pure $ performHapticFeedback unit
  continue state { props { isLocationTracking = true } }

eval (DriverInfoCardActionController (DriverInfoCardController.OpenEmergencyHelp)) state = do
  _ <- pure $ performHapticFeedback unit
  continue state{props{emergencyHelpModal = true}}

eval (DriverInfoCardActionController (DriverInfoCardController.ExpandBottomSheet)) state = continue state{props{sheetState = if state.props.sheetState == EXPANDED then COLLAPSED else EXPANDED}}
eval (DriverInfoCardActionController (DriverInfoCardController.ShareRide)) state = do
  continueWithCmd state
        [ do
            _ <- pure $ shareTextMessage (getValueToLocalStore USER_NAME <> "is on a Namma Yatri Ride") $ "👋 Hey,\n\nI am riding with Namma Driver " <> (state.data.driverInfoCardState.driverName) <> "! Track this ride on: " <> ("https://nammayatri.in/track/?id="<>state.data.driverInfoCardState.rideId) <> "\n\nVehicle number: " <> (state.data.driverInfoCardState.registrationNumber)
            pure NoAction
         ]

eval (EmergencyHelpModalAC (EmergencyHelpController.GenericHeaderAC  GenericHeaderController.PrefixImgOnClick)) state = continue state{props{emergencyHelpModal = false}}

eval (EmergencyHelpModalAC (EmergencyHelpController.CallPolicePopup)) state = continue state{props{emergencyHelpModelState{showCallPolicePopUp = true}}}

eval (EmergencyHelpModalAC (EmergencyHelpController.ContactSupportPopup)) state = continue state{props{emergencyHelpModelState{showContactSupportPopUp = true}}}

eval (EmergencyHelpModalAC (EmergencyHelpController.CallContactPopUp item)) state= continue state{props{emergencyHelpModelState{showCallContactPopUp = true, currentlySelectedContact = item}}}

eval (EmergencyHelpModalAC (EmergencyHelpController.CallEmergencyContact PopUpModal.OnButton1Click)) state = continue state{props{emergencyHelpModelState{showCallContactPopUp = false}}}

eval (EmergencyHelpModalAC (EmergencyHelpController.StoreContacts)) state  = do
  if ((getValueToLocalStore CONTACTS == "__failed") || (getValueToLocalStore CONTACTS == "(null)")) then do
        exit $ FetchContacts state
  else do
    contacts <- pure $ getValueToLocalStore CONTACTS
    contactsInJson <- pure $ parseNewContacts contacts
    let newContacts = transformContactList contactsInJson
        newState = state{props{emergencyHelpModelState{emergencyContactData = newContacts}}}
    continue newState

eval (EmergencyHelpModalAC (EmergencyHelpController.AddedEmergencyContacts)) state  =  updateAndExit state{props{emergencyHelpModelState{isSelectEmergencyContact = true}}} $ GoToEmergencyContacts state {props {emergencyHelpModelState{isSelectEmergencyContact = true}}}

eval (EmergencyHelpModalAC (EmergencyHelpController.CallEmergencyContact PopUpModal.OnButton2Click)) state = do
    void <- pure $ showDialer state.props.emergencyHelpModelState.currentlySelectedContact.phoneNo false -- TODO: FIX_DIALER
    let newState = state{props{emergencyHelpModelState{showCallContactPopUp = false, waitingDialerCallback = true}}}
    updateAndExit newState $ CallContact newState

eval (EmergencyHelpModalAC (EmergencyHelpController.CallSuccessful PopUpModal.OnButton1Click)) state = do
    let newState = state{props{emergencyHelpModelState{waitingDialerCallback = false, showCallSuccessfulPopUp = false, sosStatus = "NotResolved"}}}
    updateAndExit newState $ UpdateSosStatus newState
eval (EmergencyHelpModalAC (EmergencyHelpController.CallSuccessful PopUpModal.OnButton2Click)) state = do
    let newState = state{props{emergencyHelpModelState{waitingDialerCallback = false, showCallSuccessfulPopUp = false, sosStatus = "Resolved"}}}
    updateAndExit newState $ UpdateSosStatus newState

eval (EmergencyHelpModalAC (EmergencyHelpController.CallPolice PopUpModal.OnButton1Click)) state = continue state{props{emergencyHelpModelState{showCallPolicePopUp = false}}}
eval (EmergencyHelpModalAC (EmergencyHelpController.CallPolice PopUpModal.OnButton2Click)) state = do
    void $ pure $  showDialer "112" false -- TODO: FIX_DIALER
    let newState = state{props{emergencyHelpModelState{showCallPolicePopUp = false, waitingDialerCallback = true}}}
    updateAndExit newState $ CallPolice newState

eval (EmergencyHelpModalAC (EmergencyHelpController.ContactSupport PopUpModal.OnButton1Click)) state = continue state{props{emergencyHelpModelState{showContactSupportPopUp = false}}}
eval (EmergencyHelpModalAC (EmergencyHelpController.ContactSupport PopUpModal.OnButton2Click)) state = do
    void $ pure $  showDialer (getSupportNumber "") false -- TODO: FIX_DIALER
    let newState = state{props{emergencyHelpModelState{showContactSupportPopUp = false, waitingDialerCallback = true}}}
    updateAndExit newState $ CallSupport newState

eval (CancelRidePopUpAction (CancelRidePopUp.Button1 PrimaryButtonController.OnClick)) state = do
      _ <- pure $ performHapticFeedback unit
      continue state { props { isCancelRide = false } }

eval (CancelRidePopUpAction (CancelRidePopUp.OnGoBack)) state = continue state { props { isCancelRide = false } }

eval (CancelRidePopUpAction (CancelRidePopUp.UpdateIndex index)) state = continue state { props { cancelRideActiveIndex = Just index, cancelReasonCode = (fromMaybe dummyCancelReason (state.props.cancellationReasons !! index)).reasonCode } }

eval (CancelRidePopUpAction (CancelRidePopUp.TextChanged valId newVal)) state = continue state { props { cancelDescription = newVal } }

eval (CancelRidePopUpAction (CancelRidePopUp.ClearOptions)) state = do
  _ <- pure $ hideKeyboardOnNavigation true
  continue state { props { cancelDescription = "", cancelReasonCode = "", cancelRideActiveIndex = Nothing } }

eval (CancelRidePopUpAction (CancelRidePopUp.Button2 PrimaryButtonController.OnClick)) state = do
    _ <- pure $ performHapticFeedback unit
    let newState = state{props{isCancelRide = false,currentStage = HomeScreen, rideRequestFlow = false, isSearchLocation = NoView }}
    case state.props.cancelRideActiveIndex of
      Just index -> if ( (fromMaybe dummyCancelReason (state.props.cancellationReasons !! index)).reasonCode == "OTHER" || (fromMaybe dummyCancelReason (state.props.cancellationReasons !! index)).reasonCode == "TECHNICAL_GLITCH" ) then exit $ CancelRide newState{props{cancelDescription = if (newState.props.cancelDescription == "") then (fromMaybe dummyCancelReason (state.props.cancellationReasons !!index)).description else newState.props.cancelDescription }}
                      else exit $ CancelRide newState{props{cancelDescription = (fromMaybe dummyCancelReason (state.props.cancellationReasons !!index)).description , cancelReasonCode = (fromMaybe dummyCancelReason (state.props.cancellationReasons !! index)).reasonCode }}
      Nothing    -> continue state

eval ( RideCompletedAC (RideCompletedCard.IssueReportPopUpAC (CancelRidePopUp.Button1 PrimaryButtonController.OnClick))) state = continue state { data { ratingViewState { openReportIssue = false } } }

eval ( RideCompletedAC (RideCompletedCard.IssueReportPopUpAC (CancelRidePopUp.OnGoBack))) state = continue state { data { ratingViewState { openReportIssue = false } } }

eval ( RideCompletedAC (RideCompletedCard.IssueReportPopUpAC (CancelRidePopUp.UpdateIndex index))) state = continue state { data { ratingViewState { issueReportActiveIndex = Just index} } }

eval ( RideCompletedAC (RideCompletedCard.IssueReportPopUpAC (CancelRidePopUp.Button2 PrimaryButtonController.OnClick))) state = do
  let issue = (reportIssueOptions state)!!(fromMaybe 1 state.data.ratingViewState.issueReportActiveIndex)
  let reason = (fromMaybe dummyCancelReason issue).description
  exit $ ReportIssue state { data {
    ratingViewState { issueReason = Just reason, issueDescription = reason},
    rideRatingState {rideId = state.data.driverInfoCardState.rideId, feedback = ""}
    }}

eval (PredictionClickedAction (LocationListItemController.OnClick item)) state = do
  let _ = unsafePerformEffect $ logEvent state.data.logField "ny_user_prediction_list_item"
  locationSelected item false state{data{source = (getString CURRENT_LOCATION)}, props{isSource = Just false}}

eval (SuggestedDestinationClicked item) state = do
  let _ = unsafePerformEffect $ logEvent state.data.logField "ny_user_sd_list_item"
  locationSelected item false state{data{source = (getString CURRENT_LOCATION)}, props{isSource = Just false}}

eval (PredictionClickedAction (LocationListItemController.FavClick item)) state = do
  if (length state.data.savedLocations >= 20) then do
    void $ pure $ toast (getString SORRY_LIMIT_EXCEEDED_YOU_CANT_ADD_ANY_MORE_FAVOURITES)
    continue state
    else exit $ CheckFavDistance state{data{saveFavouriteCard{ address = item.description, selectedItem = item, tag = "", tagExists = false, tagData = [], isBtnActive = false }, selectedLocationListItem = Just item}}

eval (SaveFavouriteCardAction (SaveFavouriteCardController.OnClose)) state = continue state{props{isSaveFavourite = false},data{selectedLocationListItem = Nothing, saveFavouriteCard {address = "" , tag = "", isBtnActive = false}}}

eval (SaveFavouriteCardAction (SaveFavouriteCardController.SaveFavourite)) state = do
  _ <- pure $ hideKeyboardOnNavigation true
  exit $ SaveFavourite state{props{isSaveFavourite = false},data{selectedLocationListItem = Nothing}}

eval (SaveFavouriteCardAction (SaveFavouriteCardController.PrimayEditTA (PrimaryEditTextController.TextChanged id val))) state = do
  let input = STR.trim val
  let updatedState = state{data{saveFavouriteCard{isBtnActive = ((STR.length input) >=3),tagExists = not (validTag (getSavedTagsFromHome state.data.savedLocations) input ""),tag = input}}}
  continue updatedState

eval (SearchLocationModelActionController (SearchLocationModelController.LocationListItemActionController (LocationListItemController.FavClick item))) state = continueWithCmd state [pure $ (PredictionClickedAction (LocationListItemController.FavClick item))]

eval (FavouriteLocationModelAC (FavouriteLocationModelController.GenericHeaderAC (GenericHeaderController.PrefixImgOnClick))) state = continue state { props { currentStage = if state.props.isSearchLocation == NoView then HomeScreen else SearchLocationModel} }

eval (FavouriteLocationModelAC (FavouriteLocationModelController.FavouriteLocationAC (SavedLocationCardController.CardClicked item))) state = do
  if state.props.isSource == Just true then do
    let newState = state {data{ source = item.savedLocation, sourceAddress = item.fullAddress},props{sourcePlaceId = item.placeId,sourceLat = fromMaybe 0.0 item.lat,sourceLong =fromMaybe 0.0  item.lon, sourceSelectedOnMap = true }}
    pure $ setText (getNewIDWithTag "SourceEditText") item.savedLocation
    exit $ LocationSelected item  false newState
    else do
      let newState = state {data{ destination = item.savedLocation,destinationAddress = item.fullAddress},props{destinationPlaceId = item.placeId, destinationLat = fromMaybe 0.0 item.lat, destinationLong = fromMaybe 0.0 item.lon}}
      pure $ setText (getNewIDWithTag "DestinationEditText") item.savedLocation
      exit $ LocationSelected item  false newState

eval (SavedAddressClicked (LocationTagBarController.TagClick savedAddressType arrItem)) state =  tagClickEvent savedAddressType arrItem state{data{source = (getString CURRENT_LOCATION)}, props{isSource = Just false}}

eval (SearchLocationModelActionController (SearchLocationModelController.SavedAddressClicked (LocationTagBarController.TagClick savedAddressType arrItem))) state = tagClickEvent savedAddressType arrItem state

eval (TagClick savedAddressType arrItem) state = tagClickEvent savedAddressType arrItem state

eval (SearchLocationModelActionController (SearchLocationModelController.LocationListItemActionController (LocationListItemController.OnClick item))) state = do
  let _ = unsafePerformEffect $ logEvent state.data.logField "ny_user_location_list_item"
  let condition = state.props.isSource == Just true && (item.locationItemType == Just RECENTS || item.locationItemType == Just SUGGESTED_DESTINATIONS) 

  locationSelected item {tag = if condition then "" else item.tag, showDistance = Just false} true state{ props { sourceSelectedOnMap = if condition then false else state.props.sourceSelectedOnMap }, data { nearByDrivers = Nothing } }

eval (ExitLocationSelected item addToRecents)state = exit $ LocationSelected item  addToRecents state

eval (SearchLocationModelActionController (SearchLocationModelController.DebounceCallBack searchString isSource)) state = do
  if (STR.length searchString > 2) && (isSource == fromMaybe true state.props.isSource) then do 
    validateSearchInput state searchString
  else continue state

eval (SearchLocationModelActionController (SearchLocationModelController.SourceChanged input)) state = do
  let srcValue = if (state.data.source == "" || state.data.source == "Current Location") then true else false
  let
    newState = state {props{sourceSelectedOnMap = if (state.props.locateOnMap) then true else state.props.sourceSelectedOnMap}}
  if (input /= state.data.source) then do 
    continueWithCmd newState { props { isRideServiceable = true, searchLocationModelProps{crossBtnSrcVisibility = (STR.length input) > 2, isAutoComplete = if (STR.length input) > 2 then state.props.searchLocationModelProps.isAutoComplete else false}}} 
      [ do
          _ <- pure $ updateInputString input
          pure NoAction
      ]
  else
    continueWithCmd newState{props {searchLocationModelProps{crossBtnSrcVisibility = (STR.length input) > 2, isAutoComplete = false}}}
      [ do
          _ <- pure $ updateInputString input
          pure NoAction
      ]

eval (SearchLocationModelActionController (SearchLocationModelController.DestinationChanged input)) state = do
  if (input /= state.data.destination) then do
    continueWithCmd state { props { isRideServiceable = true, searchLocationModelProps{crossBtnDestVisibility = (STR.length input) > 2, isAutoComplete = if (STR.length input)>2 then state.props.searchLocationModelProps.isAutoComplete else false}} }
      [ do
          _ <- pure $ updateInputString input
          pure NoAction
      ]
  else
    continueWithCmd state{props {searchLocationModelProps{crossBtnDestVisibility = (STR.length input) > 2, isAutoComplete = false}}}
      [ do
          _ <- pure $ updateInputString input
          pure NoAction
      ]

eval (SearchLocationModelActionController (SearchLocationModelController.EditTextFocusChanged textType)) state = do
  _ <- pure $ spy "searchLocationModal" textType
  if textType == "D" then
    continue state { props { isSource = Just false, searchLocationModelProps{crossBtnDestVisibility = (STR.length state.data.destination) > 2}}, data {source = if state.data.source == "" then state.data.searchLocationModelData.prevLocation else state.data.source, locationList = if state.props.isSource == Just false then state.data.locationList else state.data.destinationSuggestions } }
  else
    continue state { props { isSource = Just true, searchLocationModelProps{crossBtnSrcVisibility = (STR.length state.data.source) > 2}} , data{ locationList = if state.props.isSource == Just true then state.data.locationList else state.data.recentSearchs.predictionArray } }

eval (SearchLocationModelActionController (SearchLocationModelController.NoAction)) state = continue state

eval (SearchLocationModelActionController (SearchLocationModelController.SourceClear)) state = do
  _ <- pure $ performHapticFeedback unit
  if (state.props.isSearchLocation /= LocateOnMap) then do
    _ <- pure $ requestKeyboardShow (getNewIDWithTag "SourceEditText")
    pure unit
  else
    pure unit
  let predicArray = (recentDistance state.data.recentSearchs.predictionArray state.props.sourceLat state.props.sourceLong)
  continue state { data { source = "", recentSearchs {predictionArray = predicArray}, locationList = predicArray, searchLocationModelData{prevLocation = state.data.source}}, props { isSource = Just true, isSrcServiceable = true, isRideServiceable = true, searchLocationModelProps{crossBtnSrcVisibility = false} } }

eval (SearchLocationModelActionController (SearchLocationModelController.DestinationClear)) state = do
  _ <- pure $ performHapticFeedback unit
  if (state.props.isSearchLocation /= LocateOnMap) then do
    _ <- pure $ requestKeyboardShow (getNewIDWithTag "DestinationEditText")
    pure unit
  else
    pure unit
  let predicArray = (recentDistance state.data.recentSearchs.predictionArray state.props.sourceLat state.props.sourceLong)
  continue state { data { destination = "", locationList = predicArray }, props {isSource = Just false, isDestServiceable = true, isRideServiceable = true, searchLocationModelProps{crossBtnDestVisibility = false}} }

eval (SearchLocationModelActionController (SearchLocationModelController.GoBack)) state = do
  _ <- pure $ performHapticFeedback unit
  continueWithCmd state
    [ do
        _ <- pure $ hideKeyboardOnNavigation true
        pure $ BackPressed
    ]

eval (SearchLocationModelActionController (SearchLocationModelController.SetCurrentLocation)) state = do
  _ <- pure $ currentPosition ""
  let _ = unsafePerformEffect $ logEvent state.data.logField "ny_user_currentlocation_click"
  pure $ setText (getNewIDWithTag "SourceEditText") (getString CURRENT_LOCATION)
  continue state{ data{source = (getString CURRENT_LOCATION)} ,props{ sourceSelectedOnMap = if (state.props.isSource == Just true) then false else state.props.sourceSelectedOnMap, searchLocationModelProps{isAutoComplete = false}}}

eval (SearchLocationModelActionController (SearchLocationModelController.SetLocationOnMap)) state = do
  _ <- pure $ performHapticFeedback unit
  let isSource = case state.props.isSource of
                    Just true -> true
                    _         -> false
      isDestinationNotEmpty = (not isSource && state.props.destinationLat /= 0.0 && state.props.destinationLong /= 0.0)
      lat = if isDestinationNotEmpty then state.props.destinationLat else state.props.sourceLat
      lon = if isDestinationNotEmpty then state.props.destinationLong else state.props.sourceLong
  _ <- pure $ hideKeyboardOnNavigation true
  _ <- pure $ removeAllPolylines ""
  _ <- pure $ unsafePerformEffect $ runEffectFn1 locateOnMap locateOnMapConfig { goToCurrentLocation = false, lat = lat, lon = lon, geoJson = state.data.polygonCoordinates, points = state.data.nearByPickUpPoints, zoomLevel = pickupZoomLevel, labelId = getNewIDWithTag "LocateOnMapPin"}
  pure $ unsafePerformEffect $ logEvent state.data.logField if state.props.isSource == Just true  then "ny_user_src_set_location_on_map" else "ny_user_dest_set_location_on_map"
  let srcValue = if state.data.source == "" then getString CURRENT_LOCATION else state.data.source
  let newState = state
                  { data {source = srcValue}
                  , props { isSearchLocation = LocateOnMap
                          , currentStage = SearchLocationModel
                          , locateOnMap = true,
                           isRideServiceable = true
                           , showlocUnserviceablePopUp = false
                           , searchLocationModelProps{isAutoComplete = false}
                           , locateOnMapLocation
                              { sourceLat = state.props.sourceLat
                              , sourceLng = state.props.sourceLong
                              , source = state.data.source
                              , sourceAddress = state.data.sourceAddress
                              , destinationLat = if state.props.destinationLat /= 0.0 then state.props.destinationLat else state.props.sourceLat
                              , destinationLng = if state.props.destinationLong /= 0.0 then state.props.destinationLong else state.props.sourceLong
                              , destination = state.data.destination
                              , destinationAddress = state.data.destinationAddress 
                              }
                           }
                    }
  (updateAndExit newState) $ UpdatedState newState false

eval (SearchLocationModelActionController (SearchLocationModelController.UpdateSource lat lng name)) state = do
  _ <- pure $ hideKeyboardOnNavigation true
  if state.props.isSource == Just true then do
    let newState = state{data{source = (getString CURRENT_LOCATION),sourceAddress = encodeAddress name [] Nothing},props{ sourceLat= lat,  sourceLong = lng, sourcePlaceId = Nothing, searchLocationModelProps{isAutoComplete = false}}}
    updateAndExit newState $ LocationSelected (fromMaybe dummyListItem newState.data.selectedLocationListItem) false newState
    else do
      let newState = state{data{destination = name,destinationAddress = encodeAddress name [] Nothing},props{ destinationLat = lat,  destinationLong = lng, destinationPlaceId = Nothing}}
      updateAndExit newState $ LocationSelected (fromMaybe dummyListItem newState.data.selectedLocationListItem) false newState

eval (QuoteListModelActionController (QuoteListModelController.QuoteListItemActionController (QuoteListItemController.Click quote))) state = do
  _ <- pure $ performHapticFeedback unit
  continueWithCmd (state { data { quoteListModelState = map (\x -> x { selectedQuote = (Just quote.id) }) state.data.quoteListModelState }, props { selectedQuote = Just quote.id } })
    [ do
        if (getValueToLocalStore AUTO_SELECTING) == "CANCELLED_AUTO_ASSIGN" then
          pure NoAction
        else do
          void $ pure $ setValueToLocalStore AUTO_SELECTING quote.id
          pure NoAction
    ]


eval (QuoteListModelActionController (QuoteListModelController.CancelAutoAssigning)) state = do
  _ <- pure $ performHapticFeedback unit
  _ <- pure $ setValueToLocalStore AUTO_SELECTING "CANCELLED_AUTO_ASSIGN"
  continue state


eval (QuoteListModelActionController (QuoteListModelController.TipViewPrimaryButtonClick PrimaryButtonController.OnClick)) state = do
  _ <- pure $ clearTimer state.props.timerId
  void $ pure $ startLottieProcess lottieAnimationConfig {rawJson = "progress_loader_line", lottieId = (getNewIDWithTag "lottieLoaderAnimProgress"), scaleType="CENTER_CROP"}
  let tipViewData = state.props.tipViewProps{stage = TIP_ADDED_TO_SEARCH }
  let newState = state{ props{findingRidesAgain = true ,searchExpire = (getSearchExpiryTime "LazyCheck"), currentStage = TryAgain, sourceSelectedOnMap = true, isPopUp = NoPopUp ,tipViewProps = tipViewData ,customerTip {tipForDriver = (fromMaybe 10 (state.props.tipViewProps.customerTipArrayWithValues !! state.props.tipViewProps.activeIndex)) , tipActiveIndex = state.props.tipViewProps.activeIndex+1 , isTipSelected = true } }, data{nearByDrivers = Nothing}}
  _ <- pure $ setTipViewData (TipViewData { stage : tipViewData.stage , activeIndex : tipViewData.activeIndex , isVisible : tipViewData.isVisible })
  updateAndExit newState $ RetryFindingQuotes false newState

eval (QuoteListModelActionController (QuoteListModelController.TipBtnClick index value)) state = do
  let check = index == state.props.tipViewProps.activeIndex
  continue state { props {tipViewProps { stage = (if check then DEFAULT else TIP_AMOUNT_SELECTED) , isprimaryButtonVisible = not check , activeIndex = (if check then -1 else index)}}}

eval (QuoteListModelActionController (QuoteListModelController.QuoteListItemActionController QuoteListItemController.ConfirmRide)) state = do
  _ <- pure $ performHapticFeedback unit
  let _ = unsafePerformEffect $ logEvent state.data.logField "ny_user_quote_confirm"
  exit $ ConfirmRide state

eval (QuoteListModelActionController (QuoteListModelController.QuoteListItemActionController (QuoteListItemController.CountDown seconds id status timerID))) state = do
  if status == "EXPIRED" then do
    _ <- pure $ clearTimer timerID
    let
      autoSelecting = (getValueToLocalStore AUTO_SELECTING) == id
    if (id == fromMaybe "" state.props.selectedQuote && autoSelecting && state.props.currentStage == QuoteList) then do
      let _ = unsafePerformEffect $ logEvent state.data.logField "ny_user_auto_assign"
      continueWithCmd state [ pure $ (QuoteListModelActionController (QuoteListModelController.PrimaryButtonActionController PrimaryButtonController.OnClick)) ]
    else do
      let
        newState = state { data { quoteListModelState = filter (\x -> x.id /= id) state.data.quoteListModelState } }
      continue newState { props { selectedQuote = if newState.data.quoteListModelState == [] then Nothing else newState.props.selectedQuote, expiredQuotes = (snoc state.props.expiredQuotes id) } }
  else do
    let
      newState = state { data = state.data { quoteListModelState = map (\x -> if x.id == id then x { timer = (show seconds) } else x) state.data.quoteListModelState } }
    continue newState { props { selectedQuote = if newState.data.quoteListModelState == [] then Nothing else newState.props.selectedQuote } }

eval (QuoteListModelActionController (QuoteListModelController.PrimaryButtonActionController PrimaryButtonController.OnClick)) state = do
  _ <- pure $ performHapticFeedback unit
  case state.props.selectedQuote, (null state.data.quoteListModelState) of
    Just _, false -> do
      _ <- pure $ updateLocalStage ConfirmingRide
      let
        newState = state { props { currentStage = ConfirmingRide } }
      updateAndExit newState $ ConfirmRide newState
    _, _ -> continue state

eval (QuoteListModelActionController (QuoteListModelController.GoBack)) state = do
  _ <- pure $ performHapticFeedback unit
  continueWithCmd state [ do pure $ BackPressed ]

eval (QuoteListModelActionController (QuoteListModelController.TryAgainButtonActionController  PrimaryButtonController.OnClick)) state = updateAndExit state $ LocationSelected (fromMaybe dummyListItem state.data.selectedLocationListItem) false state{props{currentStage = TryAgain, sourceSelectedOnMap = true}}

eval (QuoteListModelActionController (QuoteListModelController.HomeButtonActionController PrimaryButtonController.OnClick)) state = do
  _ <- pure $ performHapticFeedback unit
  updateAndExit state CheckCurrentStatus

eval (Restart err) state = exit $ LocationSelected (fromMaybe dummyListItem state.data.selectedLocationListItem) false state

eval (PopUpModalAction (PopUpModal.OnButton1Click)) state =   case state.props.isPopUp of
  TipsPopUp -> do
    _ <- pure $ performHapticFeedback unit
    let _ = unsafePerformEffect $ logEvent state.data.logField if state.props.customerTip.isTipSelected then ("ny_added_tip_for_" <> (show state.props.currentStage)) else "ny_no_tip_added"
    _ <- pure $ clearTimer state.props.timerId
    let tipViewData = state.props.tipViewProps{stage = RETRY_SEARCH_WITH_TIP , isVisible = not (state.props.customerTip.tipActiveIndex == 0) , activeIndex = state.props.customerTip.tipActiveIndex-1 }
    let newState = state{ props{findingRidesAgain = true ,searchExpire = (getSearchExpiryTime "LazyCheck"), currentStage = RetryFindingQuote, sourceSelectedOnMap = true, isPopUp = NoPopUp ,tipViewProps = tipViewData }}
    _ <- pure $ setTipViewData (TipViewData { stage : tipViewData.stage , activeIndex : tipViewData.activeIndex , isVisible : tipViewData.isVisible })
    updateAndExit newState $ RetryFindingQuotes true newState
  Logout -> continue state{props{isPopUp = NoPopUp}}
  _ -> do
    _ <- pure $ performHapticFeedback unit
    _ <- pure $ firebaseLogEvent "ny_tip_not_applicable"
    if (isLocalStageOn FindingQuotes ) then do
        _ <- pure $ clearTimer state.props.timerId
        let tipViewData = HomeScreenData.initData.props.tipViewProps
        _ <- pure $ setTipViewData (TipViewData { stage : tipViewData.stage , activeIndex : tipViewData.activeIndex , isVisible : tipViewData.isVisible })
        exit $ CheckCurrentStatus
      else do
      _ <- pure $ clearTimer state.props.timerId
      let newState = state{props{findingRidesAgain = true , searchExpire = (getSearchExpiryTime "LazyCheck"), currentStage = RetryFindingQuote, sourceSelectedOnMap = true, isPopUp = NoPopUp}}
      updateAndExit newState $ RetryFindingQuotes true newState

eval (PopUpModalAction (PopUpModal.OnButton2Click)) state = case state.props.isPopUp of
    TipsPopUp -> case state.props.currentStage of
      QuoteList -> do
        _ <- pure $ performHapticFeedback unit
        updateAndExit state CheckCurrentStatus
      FindingQuotes -> do
        _ <- pure $ performHapticFeedback unit
        exit $ CheckCurrentStatus
      _ -> continue state
    Logout -> exit LogoutUser
    ConfirmBack -> do
      let _ = unsafePerformEffect $ logEvent state.data.logField "ny_no_retry"
      case (getValueToLocalStore LOCAL_STAGE) of
        "QuoteList" -> do
          _ <- pure $ performHapticFeedback unit
          exit $ CheckCurrentStatus
        "FindingQuotes" -> do
          _ <- pure $ performHapticFeedback unit
          continue state{props{isPopUp = NoPopUp}}
        _ -> continue state
    NoPopUp -> continue state
    ActiveQuotePopUp -> do
      _ <- pure $ performHapticFeedback unit
      exit $ CheckCurrentStatus

eval (PopUpModalAction (PopUpModal.Tipbtnclick index value)) state = do
  _ <- pure $ performHapticFeedback unit
  case state.props.isPopUp of
    TipsPopUp -> continue state{props{customerTip{tipActiveIndex = index, tipForDriver= value, isTipSelected = not (index == 0)}}}
    _ -> continue state

eval (PopUpModalAction (PopUpModal.DismissPopup)) state = do
  let newState = if (isLocalStageOn QuoteList) then state else state{props{isPopUp = NoPopUp, customerTip{tipActiveIndex = 1,tipForDriver = 10, isTipSelected = false} }}
  continue newState

eval (DistanceOutsideLimitsActionController (PopUpModal.OnButton2Click)) state = do
  _ <- pure $ performHapticFeedback unit
  _ <- pure $ updateLocalStage SearchLocationModel
  continue state { props { isPopUp = NoPopUp, rideRequestFlow = false, currentStage = SearchLocationModel, searchId = "", isSearchLocation = SearchLocation, isSource = Just false, isSrcServiceable = true, isDestServiceable = true, isRideServiceable = true } }

eval (ShortDistanceActionController (PopUpModal.OnButton2Click)) state = do
  _ <- pure $ performHapticFeedback unit
  _ <- pure $ exitLocateOnMap ""
  exit $ UpdatedSource state

eval (ShortDistanceActionController (PopUpModal.OnButton1Click)) state = do
  _ <- pure $ performHapticFeedback unit
  _ <- pure $ updateLocalStage SearchLocationModel
  continue state{props{isSource = Just false, isPopUp = NoPopUp, rideRequestFlow = false, currentStage = SearchLocationModel, searchId = "", isSearchLocation = SearchLocation}}

eval (PickUpFarFromCurrentLocAC (PopUpModal.OnButton2Click)) state = do 
  if (state.props.isShorterTrip)  then do 
    void $ pure $ updateLocalStage ShortDistance
    continue state {props{currentStage = ShortDistance}}
    else continueWithCmd state [ do pure $ ShortDistanceActionController (PopUpModal.OnButton2Click) ]

eval (PickUpFarFromCurrentLocAC (PopUpModal.OnButton1Click)) state = do 
  continueWithCmd state [ do pure $ ShortDistanceActionController (PopUpModal.OnButton1Click) ]

eval (EstimateChangedPopUpController (PopUpModal.OnButton1Click)) state = exit GoToHome

eval (EstimateChangedPopUpController (PopUpModal.OnButton2Click)) state = do
  _ <- pure $ updateLocalStage FindingQuotes
  let
    updatedState = state { props { currentStage = FindingQuotes, isEstimateChanged = false, searchExpire = (getSearchExpiryTime "LazyCheck") } }
  updateAndExit updatedState $ GetQuotes updatedState

eval CloseLocationTracking state = continue state { props { isLocationTracking = false } }

eval CloseShowCallDialer state = continue state { props { showCallPopUp = false } }

eval (ShowCallDialer item) state = do
  case item of
    ANONYMOUS_CALLER -> callDriver state "ANONYMOUS"
    DIRECT_CALLER -> callDriver state "DIRECT"

eval (StartLocationTracking item) state = do
  _ <- pure $ performHapticFeedback unit
  case item of
    "GOOGLE_MAP" -> do
      let
        newState = state { props { isLocationTracking = false } }
      updateAndExit (newState) (OpenGoogleMaps newState)
    "IN_APP" -> exit $ InAppTrackStatus state { props { isInApp = not state.props.isInApp, isLocationTracking = false, forFirst = true } }
    _ -> continue state

eval (GetEstimates (GetQuotesRes quotesRes)) state = do
  case null quotesRes.quotes of
    false -> specialZoneFlow quotesRes.quotes state
    true -> case (getMerchant FunctionCall) of
      YATRI -> estimatesListFlow quotesRes.estimates state
      YATRISATHI -> estimatesListFlow quotesRes.estimates state
      _ -> estimatesFlow quotesRes.estimates state


eval (EstimatesTryAgain (GetQuotesRes quotesRes)) state = do
  case (getMerchant FunctionCall) of
    YATRI -> estimatesListTryAgainFlow (GetQuotesRes quotesRes) state
    YATRISATHI -> estimatesListTryAgainFlow (GetQuotesRes quotesRes) state
    _ -> do
      let _ = unsafePerformEffect $ logEvent state.data.logField "ny_user_estimate_try_again"
      let
        estimatedQuotes = quotesRes.estimates

        estimatedVarient = filter (\x -> x ^. _vehicleVariant == "AUTO_RICKSHAW") estimatedQuotes

        estimatedPrice = if (isJust (estimatedVarient !! 0)) then (fromMaybe dummyEstimateEntity (estimatedVarient !! 0)) ^. _estimatedFare else 0

        estimateId = if isJust (estimatedVarient !! 0) then (fromMaybe dummyEstimateEntity (estimatedVarient !! 0)) ^. _estimateId else ""
      case (null estimatedVarient) of
        true -> do
          _ <- pure $ hideKeyboardOnNavigation true
          _ <- pure $ toast (getString NO_DRIVER_AVAILABLE_AT_THE_MOMENT_PLEASE_TRY_AGAIN)
          continue state { props { currentStage = SearchLocationModel, rideRequestFlow = false, isSearchLocation = SearchLocation, isSrcServiceable = true, isDestServiceable = true, isRideServiceable = true } }
        false -> do
          if (estimatedPrice > state.data.suggestedAmount) then
            continue state { data { suggestedAmount = estimatedPrice }, props { estimateId = estimateId, isEstimateChanged = true } }
          else do
            _ <- pure $ updateLocalStage FindingQuotes
            let
              updatedState = state { data { suggestedAmount = estimatedPrice }, props { estimateId = estimateId, currentStage = FindingQuotes, searchExpire = (getSearchExpiryTime "LazyCheck") } }
            updateAndExit updatedState $ GetQuotes updatedState


eval (GetQuotesList (SelectListRes resp)) state = do
  case flowWithoutOffers WithoutOffers of
    true  -> do
      continueWithCmd state [pure $ ContinueWithoutOffers (SelectListRes resp)]
    false -> do
              let selectedQuotes = getQuoteList ((fromMaybe dummySelectedQuotes resp.selectedQuotes)^._selectedQuotes)
              _ <- pure $ printLog "vehicle Varient " selectedQuotes
              let filteredQuoteList = filter (\a -> length (filter (\b -> a.id == b.id )state.data.quoteListModelState) == 0 ) selectedQuotes
              let removeExpired = filter (\a -> a.seconds > 0) filteredQuoteList
              _ <- pure $ spy "quotes" filteredQuoteList
              let quoteListModelState = state.data.quoteListModelState <> removeExpired
              if (getValueToLocalStore GOT_ONE_QUOTE == "FALSE") && (length quoteListModelState > 0) then do
                _ <- pure $ firebaseLogEvent "ny_user_received_quotes"
                _ <- pure $ setValueToLocalStore GOT_ONE_QUOTE "TRUE"
                pure unit
              else pure unit
              let newState = state{data{quoteListModelState = quoteListModelState },props{isSearchLocation = NoView, isSource = Nothing,currentStage = QuoteList}}
              if isLocalStageOn QuoteList then do
                let updatedState = if isTipEnabled state then tipEnabledState newState{props{isPopUp = TipsPopUp, findingQuotesProgress = 0.0}} else newState{props{isPopUp = ConfirmBack, findingQuotesProgress = 0.0}}
                exit $ GetSelectList updatedState
              else if(state.props.selectedQuote == Nothing && (getValueToLocalStore AUTO_SELECTING) /= "CANCELLED_AUTO_ASSIGN") then do
                let id = (fromMaybe dummyQuoteList (newState.data.quoteListModelState!!0)).id
                    nextState = newState{data{quoteListModelState = map (\x -> x{selectedQuote = (Just id)}) newState.data.quoteListModelState}, props{selectedQuote = if (id /= "") then Just id else Nothing}}
                _ <- pure $ setValueToLocalStore AUTO_SELECTING id
                continue nextState
              else do
                let value = null newState.data.quoteListModelState
                _ <- pure $ setValueToLocalStore AUTO_SELECTING (if value then "false" else getValueToLocalStore AUTO_SELECTING)
                continue newState{props{selectedQuote = if value then Nothing else newState.props.selectedQuote}}

eval (ContinueWithoutOffers (SelectListRes resp)) state = do
  case resp.bookingId of
    Just bookingId -> do
      case STR.trim bookingId of
        "" -> continue state
        _  -> do
          _ <- pure $ updateLocalStage ConfirmingRide
          exit $ ConfirmRide state{props{currentStage = ConfirmingRide, bookingId = bookingId, isPopUp = NoPopUp, selectedQuote = Nothing}}
    Nothing -> do
      if isLocalStageOn QuoteList then do
        let updatedState = if isTipEnabled state then tipEnabledState state{props{isPopUp = TipsPopUp, customerTip{enableTips = true}}} else state{props{isPopUp = ConfirmBack}}
        continue updatedState
        else continue state

eval (GetRideConfirmation resp) state = do
  case state.props.isSpecialZone of
    false -> normalRideFlow resp state
    true -> specialZoneRideFlow resp state

eval (NotificationListener notificationType) state = do
  _ <- pure $ printLog "storeCallBackCustomer notificationType" notificationType
  case notificationType of
    "DRIVER_QUOTE_INCOMING" -> continue state
    _ -> exit $ NotificationHandler notificationType state { props { callbackInitiated = false}}

eval RecenterCurrentLocation state = recenterCurrentLocation state

eval (SearchLocationModelActionController (SearchLocationModelController.RecenterCurrentLocation)) state = recenterCurrentLocation state

eval (SearchLocationModelActionController (SearchLocationModelController.UpdateCurrentLocation lat lng)) state = do
  if state.props.isSource == Just true then
    updateCurrentLocation state lat lng
  else
    continue state

eval (UpdateCurrentLocation lat lng) state = updateCurrentLocation state lat lng

eval (CurrentLocation lat lng) state = exit $ UpdatedState state { props { sourceLat = fromMaybe 0.0 (NUM.fromString lat), sourceLong = fromMaybe 0.0 (NUM.fromString lng) } } false

eval (RateCardAction RateCard.Close) state = continue state { props { showRateCard = false } , data{rateCard{onFirstPage = false,currentRateCardType = DefaultRateCard}}}

eval (RateCardAction RateCard.BackPressed) state = continue state { props { showRateCard = false } ,data{rateCard{onFirstPage = false,currentRateCardType = DefaultRateCard}}}

eval (RateCardAction RateCard.NoAction) state = continue state

eval (RateCardAction RateCard.GoToDefaultStart) state = continue state { data{rateCard{currentRateCardType = DefaultRateCard}}}

eval (RateCardAction RateCard.GoToDriverAddition) state = continue state { data{rateCard{currentRateCardType = DriverAddition,onFirstPage = true}}}

eval (RateCardAction RateCard.GoToFareUpdate) state = continue state { data{rateCard{currentRateCardType = FareUpdate,onFirstPage = true}}}

eval (RequestInfoCardAction RequestInfoCard.Close) state = continue state { props { showMultipleRideInfo = false } }

eval (RequestInfoCardAction RequestInfoCard.BackPressed) state = continue state { props { showMultipleRideInfo = false } }

eval (RequestInfoCardAction RequestInfoCard.NoAction) state = continue state

eval (GenderBannerModal Banner.OnClick) state = exit $ GoToMyProfile state true

eval (UpdateProfileButtonAC PrimaryButtonController.OnClick) state = do 
  _ <- pure $ pauseYoutubeVideo unit
  let newState = state{props{showEducationalCarousel = false}} 
  updateAndExit newState $ GoToMyProfile newState true

eval (DisabilityBannerAC Banner.OnClick) state = if (addCarouselWithVideoExists unit ) then continue state{props{showEducationalCarousel = true}} else exit $ GoToMyProfile state true

eval (TicketBookingFlowBannerAC Banner.OnClick) state = exit $ GoToTicketBookingFlow state

eval (SkipAccessibilityUpdateAC PrimaryButtonController.OnClick) state = do 
  _ <- pure $ pauseYoutubeVideo unit
  continue state{props{showEducationalCarousel = false}}

eval (DisabilityPopUpAC PopUpModal.OnButton1Click) state = do 
  _ <- pure $ pauseYoutubeVideo unit
  continue state{props{showDisabilityPopUp = false}}

eval ShowRateCard state = do
  continue state { props { showRateCard = true } }

eval (PopUpModalShareAppAction PopUpModal.OnButton1Click) state= continue state{props{showShareAppPopUp=false}}

eval (PopUpModalShareAppAction PopUpModal.OnButton2Click) state= do
  _ <- pure $ setValueToLocalStore SHARE_APP_COUNT "-1"
  _ <- pure $ shareTextMessage (getValueFromConfig "shareAppTitle") (getValueFromConfig "shareAppContent")
  continue state{props{showShareAppPopUp=false}}

eval (CallSupportAction PopUpModal.OnButton1Click) state= do
  _ <- pure $ performHapticFeedback unit
  continue state{props{callSupportPopUp=false}}

eval (CallSupportAction PopUpModal.OnButton2Click) state= do
  _ <- pure $ performHapticFeedback unit
  _ <- pure $ showDialer (getSupportNumber "") false -- TODO: FIX_DIALER
  let _ = unsafePerformEffect $ logEvent state.data.logField "ny_user_ride_support_click"
  continue state{props{callSupportPopUp=false}}

eval (UpdateETA currentETA currentDistance) state = do
  let initDistance = state.data.driverInfoCardState.initDistance
  distance <- case initDistance of
                      Just initDistance -> pure initDistance
                      Nothing -> do
                                    let storedDistance = getValueToLocalStore PICKUP_DISTANCE
                                    if storedDistance == "0" || storedDistance == "__failed" || storedDistance == "(null)" then do
                                      _ <- pure $ setValueToLocalStore PICKUP_DISTANCE (show currentDistance)
                                      pure currentDistance
                                      else pure $ fromMaybe 0 (fromString storedDistance)
  let
    newState = state { data { driverInfoCardState { eta = currentETA, distance = currentDistance, initDistance = Just distance } } }
  continue newState

eval (RepeatRide index item) state = do 
  let _ = unsafePerformEffect $ logEvent state.data.logField "ny_user_repeat_trip"
  _ <- pure $ setValueToLocalStore FLOW_WITHOUT_OFFERS (show true)
  _ <- pure $ setValueToLocalStore TEST_MINIMUM_POLLING_COUNT $ "4" 
  _ <- pure $ setValueToLocalStore TEST_POLLING_INTERVAL $ "8000.0" 
  _ <- pure $ setValueToLocalStore TEST_POLLING_COUNT $ "22" 
  updateAndExit state{props{currentStage = LoadMap}, data{settingSideBar { opened = SettingSideBarController.CLOSED }}} $ RepeatTrip state{props{isRepeatRide = true}} item

eval (ReferralFlowAction) state = exit $ GoToReferral state
eval NewUser state = continueWithCmd state [ do
  if (getValueToLocalNativeStore REGISTRATION_APPROVED) == "true" then do
    _ <- pure $ setValueToLocalStore REGISTRATION_APPROVED "false"
    _ <- launchAff $ flowRunner defaultGlobalState $ runExceptT $ runBackT $ do
      _ <- UI.successScreen ((getString HEY) <> " " <> (getValueToLocalStore USER_NAME)) (getString SUCCESSFUL_ONBOARD)
      pure unit
    pure unit
    else
      pure unit
  pure NoAction
]

eval UpdateSourceFromPastLocations state = do
  let currLocationArr = (getNearestCurrentLocation state.props.sourceLat state.props.sourceLong state.data.previousCurrentLocations.pastCurrentLocations)
      currLocation = (fromMaybe {locationDetails: {lat:0.0, lon : 0.0, placeName : ""}, distance: 0.0} ((currLocationArr)!!0))
      savedLocationArr = (getNearestSavedLocation state.props.sourceLat state.props.sourceLong state.data.savedLocations)
      arr = (sortBy compareByDistance (currLocationArr <> savedLocationArr))
      nearestLocation = (fromMaybe {locationDetails: {lat:0.0, lon : 0.0, placeName : ""}, distance: 0.0} ((arr)!!0))
  continue state{data{source = nearestLocation.locationDetails.placeName, sourceAddress = encodeAddress nearestLocation.locationDetails.placeName [] Nothing}}

eval (UpdateLocAndLatLong lat lng) state = do
  let slat = fromMaybe 0.0 (NUM.fromString lat)
      slng = fromMaybe 0.0 (NUM.fromString lng)
  continueWithCmd state{props{currentLocation { lat = slat, lng = slng } , sourceLat = slat, sourceLong = slng , locateOnMapLocation {sourceLat = slat, sourceLng = slng, source = state.data.source, sourceAddress = state.data.sourceAddress}}} [do
    if os == "IOS" then do
      _ <- addMarker (getCurrentLocationMarker (getValueToLocalStore VERSION_NAME)) 9.9 9.9 160 (0.5) (0.9)
      pure unit
      else pure unit
    pure NoAction
  ]

eval GoToEditProfile state = do
  exit $ GoToMyProfile state true
eval (MenuButtonActionController (MenuButtonController.OnClick config)) state = do
  continueWithCmd state{props{defaultPickUpPoint = config.id}} [do
      let focusedIndex = findIndex (\item -> item.place == config.id) state.data.nearByPickUpPoints
      case focusedIndex of
        Just index -> do
          _ <- pure $ scrollViewFocus (getNewIDWithTag "scrollViewParent") index
          pure unit
        Nothing -> pure unit
      _ <- animateCamera config.lat config.lng 25.0 "NO_ZOOM"
      pure NoAction
    ]
eval (ChooseYourRideAction (ChooseYourRideController.ChooseVehicleAC ChooseVehicleController.NoAction)) state =
  continue state{ props{ defaultPickUpPoint = "" } }

eval (ChooseYourRideAction (ChooseYourRideController.ChooseVehicleAC (ChooseVehicleController.OnSelect config))) state = do
  let updatedQuotes = map (\item -> item{activeIndex = config.index}) state.data.specialZoneQuoteList
      newState = state{data{specialZoneQuoteList = updatedQuotes}}
  if state.data.currentSearchResultType == QUOTES then do
              _ <- pure $ setValueToLocalNativeStore SELECTED_VARIANT (config.vehicleVariant)
              continue newState{data{specialZoneSelectedQuote = Just config.id ,specialZoneSelectedVariant = Just config.vehicleVariant }}
              else continue newState{props{estimateId = config.id }, data {selectedEstimatesObject = config}}

eval (ChooseYourRideAction (ChooseYourRideController.ChooseVehicleAC (ChooseVehicleController.ShowRateCard vehicleVariant))) state =
  continue state{ props { showRateCard = true }
                , data {  rateCard {  onFirstPage = false
                                    , vehicleVariant = vehicleVariant
                                    , currentRateCardType = DefaultRateCard
                                    }}}

eval (ChooseYourRideAction (ChooseYourRideController.PrimaryButtonActionController (PrimaryButtonController.OnClick))) state = do
  _ <- pure $ setValueToLocalStore FARE_ESTIMATE_DATA state.data.selectedEstimatesObject.price
  if state.data.currentSearchResultType == QUOTES then  do
    _ <- pure $ updateLocalStage ConfirmingRide
    exit $ ConfirmRide state{props{currentStage = ConfirmingRide}}
  else do
    _ <- pure $ updateLocalStage FindingQuotes
    let updatedState = state{props{currentStage = FindingQuotes, searchExpire = (getSearchExpiryTime "LazyCheck")}}
    updateAndExit (updatedState) (GetQuotes updatedState)

eval (ChooseYourRideAction ChooseYourRideController.NoAction) state =
  continue state{ props{ defaultPickUpPoint = "" } }

eval MapReadyAction state = continueWithCmd state [ do
      permissionConditionA <- isLocationPermissionEnabled unit
      permissionConditionB <- isLocationEnabled unit
      internetCondition <- isInternetAvailable unit
      let action =  if( not internetCondition) then TriggerPermissionFlow INTERNET_ACTION
                    else if ( not (permissionConditionA && permissionConditionB)) then TriggerPermissionFlow LOCATION_DISABLED
                    else CheckAndAskNotificationPermission
      pure action
    ]

eval CheckAndAskNotificationPermission state = do 
  _ <- pure $ checkAndAskNotificationPermission unit
  continue state

eval (TriggerPermissionFlow flowType) state = exit $ ExitToPermissionFlow flowType

eval (GenderBannerModal (Banner.OnClick)) state = exit $ GoToMyProfile state true

eval _ state = continue state

validateSearchInput :: HomeScreenState -> String -> Eval Action ScreenOutput HomeScreenState
validateSearchInput state searchString =
  if STR.length (STR.trim searchString) > 2 && searchString /= state.data.source && (searchString /= state.data.destination || ((getSearchType unit) == "direct_search") && (state.props.isSearchLocation == SearchLocation)) then
    callSearchLocationAPI
  else
    continue state
  where
  callSearchLocationAPI = updateAndExit state{props{ searchLocationModelProps{showLoader = true}}} $ SearchPlace searchString state

constructLatLong :: Number -> Number -> String -> Location
constructLatLong lat lng _ =
  { lat: lat
  , lng: lng
  , place: ""
  , address: Nothing
  }

addItemToFeedbackList :: Array String -> String -> Array String
addItemToFeedbackList feedbackList feedbackItem = if (any (_ == feedbackItem) feedbackList ) then (filter (\item -> feedbackItem /= item) feedbackList) else snoc feedbackList feedbackItem

checkPermissionAndUpdatePersonMarker :: HomeScreenState -> Effect Unit
checkPermissionAndUpdatePersonMarker state = do
  conditionA <- isLocationPermissionEnabled unit
  conditionB <- isLocationEnabled unit
  let
    conditionC = (state.props.currentStage == HomeScreen)
  if (conditionA && conditionB && conditionC) then do
    _ <- getLocationName (showPersonMarker state (getCurrentLocationMarker (getValueToLocalStore VERSION_NAME))) 9.9 9.9 "Current Location" constructLatLong
    pure unit
  else do
    if (os == "IOS" && conditionC) then do
      _ <- getLocationName (showPersonMarker state (getCurrentLocationMarker (getValueToLocalStore VERSION_NAME))) 9.9 9.9 "Current Location" constructLatLong
      pure unit
    else pure unit

updateFeedback :: String -> String -> Array FeedbackAnswer -> Array FeedbackAnswer
updateFeedback feedbackId feedbackItem feedbackList =
  if hasFeedbackId feedbackId feedbackList
    then updateFeedbackAnswer feedbackId feedbackItem feedbackList
    else addFeedbackAnswer feedbackId feedbackItem feedbackList
  where
    hasFeedbackId :: String -> Array FeedbackAnswer -> Boolean
    hasFeedbackId fid list = any (\feedback -> feedback.questionId == fid) list

    updateFeedbackAnswer :: String -> String -> Array FeedbackAnswer -> Array FeedbackAnswer
    updateFeedbackAnswer fid newItem list =
      map (\feedback ->
        if feedback.questionId == fid
                then feedback { answer = if newItem `elem` (feedback.answer) then filter (\x -> x /= newItem) feedback.answer else feedback.answer <> [newItem] }
          else feedback
        ) list

    addFeedbackAnswer :: String -> String -> Array FeedbackAnswer -> Array FeedbackAnswer
    addFeedbackAnswer fid newItem list = do
      let config = {questionId : fid, answer : [newItem]}
      list <> [config]

showPersonMarker :: HomeScreenState -> String -> Location -> Effect Unit
showPersonMarker state marker location = do
  _ <- addMarker (getCurrentLocationMarker (getValueToLocalStore VERSION_NAME)) location.lat location.lng 160 0.5 0.9
  _ <- pure $ printLog "Location :: " location
  animateCamera location.lat location.lng zoomLevel "ZOOM"

getCurrentCustomerLocation :: forall t44 t51. Applicative t51 => (Action -> Effect Unit) -> t44 -> Effect (t51 Unit)
getCurrentCustomerLocation push state = do
  location <- getCurrentLatLong
  push $ UpdateSource location.lat location.lng (getString CURRENT_LOCATION)
  pure (pure unit)

cancelReasons :: String -> Array OptionButtonList
cancelReasons dummy =
  [ { reasonCode: "CHANGE_OF_PLANS"
    , description: getString CHANGE_OF_PLANS
    , subtext: Just $ getString NO_LONGER_REQUIRE_A_RIDE_DUE_TO_CHANGE_IN_PLANS
    , textBoxRequired : false
    }
  , { reasonCode: "GOT_ANOTHER_RIDE"
    , description: getString GOT_ANOTHER_RIDE_ELSE_WHERE
    , subtext: Just $ getString CANCELLING_AS_I_GOT_A_RIDE_ON_ANOTHER_APP
    , textBoxRequired : false
    }
  , { reasonCode: "DRIVER_NOT_MOVING"
    , description: getString DRIVER_IS_NOT_MOVING
    , subtext: Just $ getString DRIVER_LOCATION_WASNT_CHANGING_ON_THE_MAP
    , textBoxRequired : false
    }
  , { reasonCode: "WAIT_TIME_TOO_LONG"
    , description: getString WAIT_TIME_TOO_LONG
    , subtext: Just $ getString DRIVER_WAS_TAKING_TOO_LONG_TO_REACH_THE_PICKUP_LOCATION
    , textBoxRequired : false
    }
  , { reasonCode: "WRONG_PICKUP_LOCATION"
    , description: getString WRONG_PICKUP_LOCATION
    , subtext: Just $ getString THE_PICKUP_LOCATION_ENTERED_WAS_WRONG
    , textBoxRequired : false
    }
  , { reasonCode: "OTHER"
    , description: getString OTHER
    , subtext: Just $ getString SOME_OTHER_REASON
    , textBoxRequired : true
    }
  ]

dummyCancelReason :: OptionButtonList
dummyCancelReason =
  { reasonCode: ""
  , description: ""
  , textBoxRequired: false
  , subtext: Nothing
  }

dummyRideRatingState :: RatingCard
dummyRideRatingState = {
  rating              : 0,
  driverName          : "",
  rideId              :  "",
  finalAmount         : 0,
  rideStartTime       : "",
  rideStartDate       : "",
  rideEndTime         : "",
  source              : "",
  destination         : "",
  vehicleNumber       : "",
  status              : "",
  shortRideId         : "",
  bookingId           : "",
  rideEndTimeUTC      : "",
  dateDDMMYY          : "",
  offeredFare         : 0,
  distanceDifference  : 0,
  feedback            : "",
  feedbackList        : []
}
dummyListItem :: LocationListItemState
dummyListItem = {
    prefixImageUrl : ""
  , postfixImageUrl : ""
  , postfixImageVisibility : false
  , lat : Nothing
  , lon : Nothing
  , placeId : Nothing
  , subTitle : ""
  , title : ""
  , description : ""
  , tag : ""
  , tagType : Nothing
  , cardType : Nothing
  , address : ""
  , tagName : ""
  , isEditEnabled : true
  , savedLocation : ""
  , placeName : ""
  , isClickable : true
  , alpha : 1.0
  , fullAddress : HomeScreenData.dummyAddress
  , locationItemType : Nothing
  , distance : Nothing
  , showDistance : Just false
  , actualDistance : Nothing
  , frequencyCount : Nothing
  , recencyDate : Nothing
  , locationScore : Nothing
}

tagClickEvent :: CardType -> (Maybe LocationListItemState) -> HomeScreenState -> Eval Action ScreenOutput HomeScreenState
tagClickEvent savedAddressType arrItem state =
    case savedAddressType, arrItem of
        OTHER_TAG,_  -> do
          _ <- pure $ updateLocalStage FavouriteLocationModel
          continue state{props{currentStage = FavouriteLocationModel}}
        _,Nothing    -> do
          if (length state.data.savedLocations >= 20) then do
            _ <- pure $ toast (getString SORRY_LIMIT_EXCEEDED_YOU_CANT_ADD_ANY_MORE_FAVOURITES)
            continue state
            else updateAndExit state{props{tagType = Just savedAddressType}}  $ CheckFavDistance state{props{tagType = Just savedAddressType}}
        _,Just item  -> do
          if state.props.isSource == Just true then do
              let newState = state {data{ source = item.description, sourceAddress = item.fullAddress},props{sourcePlaceId = item.placeId,sourceLat = fromMaybe 0.0 item.lat,sourceLong =fromMaybe 0.0  item.lon, sourceSelectedOnMap = true }}
              pure $ setText (getNewIDWithTag "SourceEditText") item.description
              pure $ removeMarker (getCurrentLocationMarker (getValueToLocalStore VERSION_NAME))
              updateAndExit state{props{currentStage = LoadMap}} $ LocationSelected item false newState
            else do
              let newState = state {data{ destination = item.description,destinationAddress = item.fullAddress},props{destinationPlaceId = item.placeId, destinationLat = fromMaybe 0.0 item.lat, destinationLong = fromMaybe 0.0 item.lon}}
              pure $ setText (getNewIDWithTag "DestinationEditText") item.description
              pure $ removeMarker $ getCurrentLocationMarker (getValueToLocalStore VERSION_NAME)
              updateAndExit state{props{currentStage = LoadMap}} $ LocationSelected item false newState

flowWithoutOffers :: LazyCheck -> Boolean
flowWithoutOffers dummy = not $ (getValueToLocalStore FLOW_WITHOUT_OFFERS) == "false"

recenterCurrentLocation :: HomeScreenState -> Eval Action ScreenOutput HomeScreenState
recenterCurrentLocation state = continueWithCmd state [ do
    if state.props.locateOnMap || (not state.props.locateOnMap && state.props.currentStage == ConfirmingLocation) then do
      _ <- pure $ currentPosition "NO_ZOOM"
      pure unit
    else do
      _ <- pure $ currentPosition ""
      _ <- addMarker (getCurrentLocationMarker (getValueToLocalStore VERSION_NAME)) 9.9 9.9 160 (0.5) (0.9)
      pure unit
    pure NoAction
  ]

updateCurrentLocation :: HomeScreenState -> String -> String -> Eval Action  ScreenOutput HomeScreenState
updateCurrentLocation state lat lng = exit $ (CheckLocServiceability state (fromMaybe 0.0 (NUM.fromString lat )) (fromMaybe 0.0 (NUM.fromString lng)))

locationSelected :: LocationListItemState -> Boolean -> HomeScreenState -> Eval Action ScreenOutput HomeScreenState
locationSelected item addToRecents state = do
  _ <- pure $ hideKeyboardOnNavigation true
  let favClick = if item.postfixImageUrl == "ny_ic_fav_red,https://assets.juspay.in/beckn/nammayatri/user/images/ny_ic_fav_red.png" then "true" else "false"
  if state.props.isSource == Just true then do
    let _ = unsafePerformEffect $ logEventWithMultipleParams state.data.logField  "ny_user_pickup_select" $ [ {key : "Source", value : unsafeToForeign item.title},
                                                                                                              {key : "Favourite", value : unsafeToForeign favClick}]
    let newState = state {data{ source = item.title, sourceAddress = encodeAddress (item.title <> ", " <>item.subTitle) [] item.placeId},props{sourcePlaceId = item.placeId,sourceLat = fromMaybe 0.0 item.lat,sourceLong =fromMaybe 0.0  item.lon, sourceSelectedOnMap = (item.tag /= "") }}
    pure $ setText (getNewIDWithTag "SourceEditText") item.title
    updateAndExit state{props{currentStage = LoadMap}} $ LocationSelected item addToRecents newState
    else do
      let _ = unsafePerformEffect $ logEventWithMultipleParams state.data.logField  "ny_user_destination_select" $ [{key : "Destination", value : unsafeToForeign item.title},
                                                                                                                    {key : "Favourite", value : unsafeToForeign favClick}]
      let newState = state {data{ destination = item.title,destinationAddress = encodeAddress (item.title <> ", " <>item.subTitle) [] item.placeId},props{destinationPlaceId = item.placeId, destinationLat = fromMaybe 0.0 item.lat, destinationLong = fromMaybe 0.0 item.lon}}
      pure $ setText (getNewIDWithTag "DestinationEditText") item.title
      updateAndExit state{props{currentStage = LoadMap}} $ LocationSelected item addToRecents newState

checkCurrentLocation :: Number -> Number -> Array CurrentLocationDetails -> Boolean
checkCurrentLocation lat lon previousCurrentLocations =  (length (filter (\ (item) -> (filterFunction lat lon item))(previousCurrentLocations)) > 0)

checkSavedLocations :: Number -> Number -> Array LocationListItemState -> Boolean
checkSavedLocations lat lon savedLocations = (length (filter(\item -> (filterSavedLocations lat lon item)) (savedLocations)) > 0 )
filterSavedLocations :: Number -> Number -> LocationListItemState -> Boolean
filterSavedLocations lat lon savedLocation = not ((getDistanceBwCordinates lat lon (fromMaybe 0.0 savedLocation.lat) (fromMaybe 0.0 savedLocation.lon)) > 0.05)

filterFunction :: Number -> Number -> CurrentLocationDetails -> Boolean
filterFunction lat lon   currLocation = not ((getDistanceBwCordinates lat lon (currLocation.lat) (currLocation.lon)) > 0.05)

getNearestCurrentLocation :: Number -> Number -> Array CurrentLocationDetails -> Array CurrentLocationDetailsWithDistance
getNearestCurrentLocation lat lon previousCurrentLocations =  (sortBy compareByDistance (map (\ (item) ->
  { distance : (getDistanceBwCordinates lat lon (item.lat) (item.lon)),
    locationDetails : item
  })
  (previousCurrentLocations)))

getNearestSavedLocation :: Number -> Number -> Array LocationListItemState -> Array CurrentLocationDetailsWithDistance
getNearestSavedLocation lat lon savedLocations = (sortBy compareByDistance (map(\item ->
  { distance : (getDistanceBwCordinates lat lon (fromMaybe 0.0 item.lat) (fromMaybe 0.0 item.lon)),
    locationDetails :  {lat : (fromMaybe 0.0 item.lat), lon : (fromMaybe 0.0 item.lon), placeName : (item.description)}
  }) (savedLocations)))

compareByDistance :: CurrentLocationDetailsWithDistance -> CurrentLocationDetailsWithDistance -> Ordering
compareByDistance ( a) ( b) = compare (a.distance ) (b.distance)

updateMessagesWithCmd :: HomeScreenState -> Eval Action ScreenOutput HomeScreenState
updateMessagesWithCmd state =
  continueWithCmd state [ do
    if(state.props.currentStage == ChatWithDriver) then do
      _ <- pure $ setValueToLocalNativeStore READ_MESSAGES (show (length state.data.messages))
      pure unit
    else
      pure unit
    pure NoAction
    ]

dummySelectedQuotes :: SelectedQuotes
dummySelectedQuotes = SelectedQuotes {
  selectedQuotes : []
}

getSearchExpiryTime :: String -> Int
getSearchExpiryTime dummy =
  let count = fromMaybe 0 (fromString (getValueToLocalStore TEST_POLLING_COUNT))
      interval = (fromMaybe 0.0 (NUM.fromString (getValueToLocalStore TEST_POLLING_INTERVAL)) / 1000.0)
      searchExpiryTime = round $ (toNumber count) * interval
  in searchExpiryTime

tipEnabledState :: HomeScreenState -> HomeScreenState
tipEnabledState state = state { props{customerTip {isTipSelected= true, tipForDriver= (fromMaybe 10 (state.props.tipViewProps.customerTipArrayWithValues !! (state.props.customerTip.tipActiveIndex-1)))}}}

isTipEnabled :: HomeScreenState -> Boolean
isTipEnabled state = do
    let tipConfig = state.data.config.customerTip
        selectedEstimatesObject = getSelectedEstimatesObject "Lazy"
    case selectedEstimatesObject of
      Just obj -> 
        case obj.vehicleVariant of 
            "AUTO_RICKSHAW" -> tipConfig.auto
            _ -> tipConfig.cabs
      Nothing -> case state.data.selectedEstimatesObject.vehicleVariant of 
                    "AUTO_RICKSHAW" -> tipConfig.auto
                    _ -> tipConfig.cabs

estimatesFlow :: Array EstimateAPIEntity -> HomeScreenState -> Eval Action ScreenOutput HomeScreenState
estimatesFlow estimatedQuotes state = do
  let estimatesInfo = getEstimatesInfo estimatedQuotes "AUTO_RICKSHAW" state
      _ = unsafePerformEffect $ logEvent state.data.logField "ny_user_estimate"
  void $ pure $ setSelectedEstimatesObject estimatesInfo.defaultQuote
  if not (null estimatesInfo.estimatedVarient) && isLocalStageOn FindingEstimate then do
    let lang = getValueToLocalStore LANGUAGE_KEY
    exit
      $ SelectEstimate
          state
            { data  { suggestedAmount = estimatesInfo.estimatedPrice,
                      currentSearchResultType = ESTIMATES,
                      rateCard =  { additionalFare: estimatesInfo.additionalFare
                                  , nightShiftMultiplier: estimatesInfo.nightShiftMultiplier
                                  , nightCharges: estimatesInfo.nightCharges
                                  , currentRateCardType: DefaultRateCard
                                  , onFirstPage: false
                                  , baseFare: estimatesInfo.baseFare, extraFare: estimatesInfo.extraFare, pickUpCharges: estimatesInfo.pickUpCharges
                                  , vehicleVariant : estimatesInfo.defaultQuote.vehicleVariant
                                  }
                                  ,
                      selectedEstimatesObject = estimatesInfo.defaultQuote
                      }
            , props { estimateId = estimatesInfo.estimateId, currentStage = SettingPrice, showRateCardIcon = estimatesInfo.showRateCardIcon, zoneType = estimatesInfo.zoneType, specialZoneType = ""}
            }
  else do
    _ <- pure $ hideKeyboardOnNavigation true
    _ <- pure $ if (null estimatesInfo.estimatedVarient) then toast (getString NO_DRIVER_AVAILABLE_AT_THE_MOMENT_PLEASE_TRY_AGAIN) else unit
    let lang = getValueToLocalStore LANGUAGE_KEY
    continue
      state
        { props { currentStage = SearchLocationModel, rideRequestFlow = false, isSearchLocation = SearchLocation, isSrcServiceable = true, isDestServiceable = true, isRideServiceable = true, showRateCardIcon = estimatesInfo.showRateCardIcon }
        , data { currentSearchResultType = ESTIMATES, rateCard = { additionalFare: estimatesInfo.additionalFare
                            , nightShiftMultiplier: estimatesInfo.nightShiftMultiplier
                            , nightCharges: estimatesInfo.nightCharges
                            , currentRateCardType: DefaultRateCard
                            , onFirstPage: false
                            , baseFare: estimatesInfo.baseFare, extraFare: estimatesInfo.extraFare, pickUpCharges: estimatesInfo.pickUpCharges
                            , vehicleVariant : ""
                            }
                , selectedEstimatesObject = estimatesInfo.defaultQuote
                }
        }

specialZoneFlow :: Array OfferRes -> HomeScreenState -> Eval Action ScreenOutput HomeScreenState
specialZoneFlow estimatedQuotes state = do
  let quoteList = getSpecialZoneQuotes estimatedQuotes state.data.config.estimateAndQuoteConfig
      defaultQuote = fromMaybe ChooseVehicleController.config (quoteList !! 0)
  void $ pure $ setSelectedEstimatesObject defaultQuote
  if ((not (null quoteList)) && (isLocalStageOn FindingEstimate)) then do
    let _ = unsafePerformEffect $ logEvent state.data.logField "ny_user_quote"
    _ <- pure $ updateLocalStage SettingPrice
    _ <- pure $ setValueToLocalStore SELECTED_VARIANT (defaultQuote.vehicleVariant)
    continue state { data {specialZoneQuoteList = quoteList, currentSearchResultType = QUOTES, specialZoneSelectedQuote = Just defaultQuote.id, specialZoneSelectedVariant = Just defaultQuote.vehicleVariant}, props {currentStage = SettingPrice, specialZoneType = "OneWaySpecialZoneAPIDetails"}}
  else do
    _ <- pure $ hideKeyboardOnNavigation true
    _ <- pure $ updateLocalStage SearchLocationModel
    _ <- pure $ toast (getString NO_DRIVER_AVAILABLE_AT_THE_MOMENT_PLEASE_TRY_AGAIN)
    continue state { props {currentStage = SearchLocationModel}, data{currentSearchResultType = QUOTES}}

estimatesListFlow :: Array EstimateAPIEntity -> HomeScreenState -> Eval Action ScreenOutput HomeScreenState
estimatesListFlow estimates state = do
  let estimatesInfo = getEstimatesInfo estimates "" state
  void $ pure $ setSelectedEstimatesObject estimatesInfo.defaultQuote
  if ((not (null estimatesInfo.quoteList)) && (isLocalStageOn FindingEstimate)) then do
    let _ = unsafePerformEffect $ logEvent state.data.logField "ny_user_quote"
        nearByDrivers = getNearByDrivers estimates
        nearByDriversLength = length nearByDrivers
    _ <- pure $ updateLocalStage SettingPrice
    continue state { data {specialZoneQuoteList = estimatesInfo.quoteList, currentSearchResultType = ESTIMATES, rateCard {vehicleVariant = estimatesInfo.defaultQuote.vehicleVariant, nightShiftMultiplier= estimatesInfo.nightShiftMultiplier, nightCharges= estimatesInfo.nightCharges}, selectedEstimatesObject = estimatesInfo.defaultQuote, pickUpCharges = estimatesInfo.pickUpCharges, nearByDrivers = if nearByDriversLength > 0 then Just nearByDriversLength else Nothing}, props {currentStage = SettingPrice, estimateId = estimatesInfo.defaultQuote.id}}
  else do
    _ <- pure $ hideKeyboardOnNavigation true
    _ <- pure $ updateLocalStage SearchLocationModel
    _ <- pure $ toast (getString NO_DRIVER_AVAILABLE_AT_THE_MOMENT_PLEASE_TRY_AGAIN)
    continue state { props {currentStage = SearchLocationModel}, data{currentSearchResultType = ESTIMATES}}


estimatesListTryAgainFlow :: GetQuotesRes -> HomeScreenState -> Eval Action ScreenOutput HomeScreenState
estimatesListTryAgainFlow (GetQuotesRes quotesRes) state = do
  let
    estimates = quotesRes.estimates
    estimatedVarient = filter (\x -> x ^. _vehicleVariant == state.data.selectedEstimatesObject.vehicleVariant) estimates
    estimatedPrice = if (isJust (estimatedVarient !! 0)) then (fromMaybe dummyEstimateEntity (estimatedVarient !! 0)) ^. _estimatedFare else 0
    quoteList = getEstimateList estimatedVarient state.data.config.estimateAndQuoteConfig
    defaultQuote = fromMaybe ChooseVehicleController.config (quoteList !! 0)
  case (null estimatedVarient) of
    true -> do
      _ <- pure $ hideKeyboardOnNavigation true
      _ <- pure $ toast $ getString NO_DRIVER_AVAILABLE_AT_THE_MOMENT_PLEASE_TRY_AGAIN
      continue state { props { currentStage = SearchLocationModel, rideRequestFlow = false, isSearchLocation = SearchLocation, isSrcServiceable = true, isDestServiceable = true, isRideServiceable = true } }
    false -> do
      if (estimatedPrice >  state.data.selectedEstimatesObject.basePrice) then
            continue state { data { suggestedAmount = estimatedPrice }, props { estimateId = defaultQuote.id, isEstimateChanged = true } }
      else do
        _ <- pure $ updateLocalStage FindingQuotes
        let updatedState = state { data { suggestedAmount = estimatedPrice }, props { estimateId = defaultQuote.id, currentStage = FindingQuotes, searchExpire = (getSearchExpiryTime "LazyCheck") } }
        updateAndExit updatedState $ GetQuotes updatedState


normalRideFlow :: RideBookingRes -> HomeScreenState -> Eval Action ScreenOutput HomeScreenState
normalRideFlow  (RideBookingRes response) state = do
  let rideStatus = (fromMaybe dummyRideAPIEntity ((response.rideList) !! 0)) ^. _status
      newState = state{ props { currentStage =
            case rideStatus of
              "NEW" -> RideAccepted
              "INPROGRESS" -> RideStarted
              "COMPLETED" -> RideCompleted
              "CANCELLED" -> HomeScreen
              _ -> RideAccepted
          , isSearchLocation = NoView
          }
        , data
          { driverInfoCardState = getDriverInfo state.data.specialZoneSelectedVariant (RideBookingRes response) (state.data.currentSearchResultType == QUOTES)
          }}
  exit $ RideConfirmed newState { props { isInApp = true } }

specialZoneRideFlow :: RideBookingRes -> HomeScreenState -> Eval Action ScreenOutput HomeScreenState
specialZoneRideFlow  (RideBookingRes response) state = do
  let
    newState =
      state
        { props
          { currentStage = RideAccepted
          , isSearchLocation = NoView
          }
        , data
          { driverInfoCardState = getDriverInfo state.data.specialZoneSelectedVariant (RideBookingRes response) (state.data.currentSearchResultType == QUOTES)
          }
        }
  exit $ RideConfirmed newState { props { isInApp = true } }

getRateCardArray :: Boolean -> String -> Int -> Int -> Int -> Array {title :: String , description :: String}
getRateCardArray nightCharges lang baseFare extraFare additionalFare = ([ { title :( if (lang == "EN_US") then (getString MIN_FARE_UPTO) <> " 2 km" else "2 km " <> (getString MIN_FARE_UPTO) ) <> if nightCharges then " 🌙" else "" , description : "₹" <> toStringJSON (baseFare) }
                      , { title : (getString RATE_ABOVE_MIN_FARE) <> if nightCharges then " 🌙" else "", description : "₹" <> toStringJSON (extraFare) <> " / km"} ]
                      <> if (getMerchant FunctionCall) == NAMMAYATRI && additionalFare > 0 then [ {title : (getString DRIVER_ADDITIONS) , description : (getString PERCENTAGE_OF_NOMINAL_FARE)}] else [])

findingQuotesSearchExpired :: Boolean -> Int
findingQuotesSearchExpired gotQuotes =
  let secondsPassed = getExpiryTime (getValueToLocalStore FINDING_QUOTES_START_TIME) true
      searchExpiryTime = getSearchExpiryTime "LazyCheck"
      secondsLeft = case gotQuotes of
                      true  -> if (searchExpiryTime - secondsPassed) < 30 then (searchExpiryTime - secondsPassed) else 30
                      false -> (searchExpiryTime - secondsPassed)
  in secondsLeft

callDriver :: HomeScreenState -> String -> Eval Action ScreenOutput HomeScreenState
callDriver state callType = do
  continueWithCmd state{props{ showCallPopUp = false }}
    [ do
        let driverNumber = case callType of
                            "DIRECT" ->(fromMaybe state.data.driverInfoCardState.merchantExoPhone state.data.driverInfoCardState.driverNumber)
                            _ -> if (STR.take 1 state.data.driverInfoCardState.merchantExoPhone) == "0" then state.data.driverInfoCardState.merchantExoPhone else "0" <> state.data.driverInfoCardState.merchantExoPhone
        _ <- pure $ showDialer driverNumber false
        let _ = unsafePerformEffect $ logEventWithTwoParams state.data.logField ("ny_user_"<> callType <>"_call_click") "trip_id" (state.props.bookingId) "user_id" (getValueToLocalStore CUSTOMER_ID)
        pure NoAction
    ]

getCustomerSuggestions :: HomeScreenState -> Array String -> Array String
getCustomerSuggestions state suggestions = case (length suggestions == 0) of
                                  true -> if (metersToKm state.data.driverInfoCardState.distance state) == (getString AT_PICKUP) then getSuggestionsfromKey "customerDefaultAP" else getSuggestionsfromKey "customerDefaultBP"
                                  false -> suggestions

getPeekHeight :: HomeScreenState -> Int
getPeekHeight state = 
      let androidPixels = runFn1 getPixels ""
          androidDensity = (runFn1 getDeviceDefaultDensity "")/  defaultDensity
          homescreenHeader =  (runFn1 getLayoutBounds (getNewIDWithTag "homescreenHeader")).height
          scrHeight = (getDeviceHeight unit)
          iosScale = runFn1 getPixels ""
          iosNativeScale = runFn1 getDefaultPixels ""
          displayZoomFactor = iosNativeScale / iosScale

          requiredPeekHeight = if os == "IOS" then ceil((((toNumber scrHeight) / displayZoomFactor)/ iosScale) - (toNumber homescreenHeader) )
                                else  (screenHeight unit) - ( ceil(((toNumber homescreenHeader)/androidPixels) *androidDensity))
      in if homescreenHeader == 0 then 700 else requiredPeekHeight
