{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.HomeScreen.Controller where

import Accessor (_estimatedFare, _estimateId, _vehicleVariant, _status, _estimateFareBreakup, _title, _price, _totalFareRange, _maxFare, _minFare, _nightShiftRate, _nightShiftEnd, _nightShiftMultiplier, _nightShiftStart)
import Common.Types.App (CancellationReasons, LazyCheck(..))
import Components.CancelRide.Controller as CancelRidePopUp
import Components.DriverInfoCard.Controller as DriverInfoCardController
import Components.EmergencyHelp as EmergencyHelpController
import Components.ErrorModal.Controller as ErrorModalController
import Components.FareBreakUp as FareBreakUp
import Components.FavouriteLocationModel as FavouriteLocationModelController
import Components.GenericHeader.Controller as GenericHeaderController
import Components.LocationListItem.Controller as LocationListItemController
import Components.LocationTagBar as LocationTagBarController
import Components.MenuButton.Controller (Action(..)) as MenuButtonController
import Components.PopUpModal.Controller as PopUpModal
import Components.PricingTutorialModel.Controller as PricingTutorialModelController
import Components.ChatView as ChatView
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
import Components.SettingSideBar.Controller as SettingSideBarController
import Components.SourceToDestination.Controller as SourceToDestinationController
import Data.Array ((!!), filter, null, snoc, length, take, drop, head, last, sortBy)
import Data.Lens ((^.))
import EN (getEN)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Number (fromString)
import Data.String as STR
import Debug.Trace (spy)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Engineering.Helpers.Commons (clearTimer, flowRunner, getNewIDWithTag, os)
import Global (readFloat)
import Helpers.Utils (addToRecentSearches, getLocationName, saveRecents, setText', updateInputString, withinTimeRange, getExpiryTime, getDistanceBwCordinates, getCurrentLocationMarker, parseNewContacts)
import JBridge (addMarker, animateCamera, currentPosition, exitLocateOnMap, firebaseLogEvent, firebaseLogEventWithParams, hideKeyboardOnNavigation, isLocationEnabled, isLocationPermissionEnabled, locateOnMap, minimizeApp, removeAllPolylines, requestKeyboardShow, requestLocation, showDialer, toast, toggleBtnLoader, shareTextMessage, firebaseLogEventWithTwoParams, removeMarker,  sendMessage, scrollToBottom, stopChatListenerService)
import Language.Strings (getString)
import Language.Types (STR(..))
import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, printLog, trackAppTextInput, trackAppScreenEvent)
import Prelude (class Applicative, class Show, Unit, Ordering, bind, compare, discard, map, negate, pure, show, unit, not, void, ($), (&&), (-), (/=), (<>), (==), (>), (>=), (||), (<), void, (<), (*))
import Presto.Core.Types.API (ErrorResponse)
import PrestoDOM (Eval, Visibility(..), continue, continueWithCmd, exit, updateAndExit)
import PrestoDOM.Types.Core (class Loggable)
import Resources.Constants (encodeAddress)
import Screens (ScreenName(..), getScreen)
import Screens.AddNewAddressScreen.Controller (validTag, getSavedTagsFromHome)
import Screens.HomeScreen.ScreenData (dummyAddress, dummyQuoteAPIEntity)
import Screens.HomeScreen.Transformer (dummyRideAPIEntity, getDriverInfo, getQuoteList, transformContactList)
import Screens.SuccessScreen.Handler as UI
import Screens.Types (HomeScreenState, Location, LocationListItemState, PopupType(..), SearchLocationModelType(..), Stage(..), CardType(..), RatingCard, CurrentLocationDetailsWithDistance(..), CurrentLocationDetails, LocationItemType(..))
import Services.API (EstimateAPIEntity(..), GetDriverLocationResp, GetQuotesRes(..), GetRouteResp, LatLong(..), PlaceName(..), RideBookingRes(..), SelectListRes(..), FareRange, QuoteAPIEntity(..))
import Services.Backend as Remote
import Services.Config (getDriverNumber, getSupportNumber)
import Storage (KeyStore(..), isLocalStageOn, updateLocalStage, getValueToLocalStore, getValueToLocalStoreEff, setValueToLocalStore, getValueToLocalNativeStore)
import Control.Monad.Except.Trans (runExceptT)
import Control.Transformers.Back.Trans (runBackT)
import Data.Int (toNumber, round)

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog action appId  = case action of
    AfterRender -> trackAppScreenRender appId "screen" (getScreen HOME_SCREEN)
    BackPressed -> trackAppBackPress appId (getScreen HOME_SCREEN)
    CancelSearch -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "cancel_search"
    RecenterCurrentLocation -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "recenter_location"
    SidebarCloseAnimationCompleted -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "side_bar_close"
    OpenSettings -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "open_settings"
    OpenPricingTutorial -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "open_pricing_tutorial"
    OpenSearchLocation -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "open_search_modal"
    UpdateSource lat lon name -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "update_source_address"
    PrimaryButtonActionController act -> case act of
      PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "primary_button" "onclick"
      PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "primary_button" "no_action"
    SettingSideBarActionController act -> case act of
      SettingSideBarController.PastRides -> do
        trackAppActionClick appId (getScreen HOME_SCREEN) "setting_side_bar" "past_rides"
        trackAppEndScreen appId (getScreen HOME_SCREEN)
      SettingSideBarController.OnHelp -> do
        trackAppActionClick appId (getScreen HOME_SCREEN) "setting_side_bar" "on_help"
        trackAppEndScreen appId (getScreen HOME_SCREEN)
      SettingSideBarController.ChangeLanguage -> do
        trackAppActionClick appId (getScreen HOME_SCREEN) "setting_side_bar" "change_language"
        trackAppEndScreen appId (getScreen HOME_SCREEN)
      SettingSideBarController.GoToAbout -> do
        trackAppActionClick appId (getScreen HOME_SCREEN) "setting_side_bar" "go_to_about"
        trackAppEndScreen appId (getScreen HOME_SCREEN)
      SettingSideBarController.EditProfile -> do
        trackAppActionClick appId (getScreen HOME_SCREEN) "setting_side_bar" "edit_profile"
        trackAppEndScreen appId (getScreen HOME_SCREEN)
      SettingSideBarController.OnClosed -> do
        trackAppActionClick appId (getScreen HOME_SCREEN) "setting_side_bar" "on_closed"
        trackAppEndScreen appId (getScreen HOME_SCREEN)
      SettingSideBarController.OnClose -> do
        trackAppActionClick appId (getScreen HOME_SCREEN) "setting_side_bar" "on_close"
        trackAppEndScreen appId (getScreen HOME_SCREEN)
      SettingSideBarController.OnLogout -> do
        trackAppActionClick appId (getScreen HOME_SCREEN) "setting_side_bar" "on_logout"
        trackAppEndScreen appId (getScreen HOME_SCREEN)
      SettingSideBarController.ShareAppLink -> do
        trackAppActionClick appId (getScreen HOME_SCREEN) "setting_side_bar" "share_app_link"
        trackAppEndScreen appId (getScreen HOME_SCREEN)
      SettingSideBarController.GoToFavourites -> do
        trackAppActionClick appId (getScreen HOME_SCREEN) "setting_side_bar" "go_to_favourites"
        trackAppEndScreen appId (getScreen HOME_SCREEN)
      SettingSideBarController.GoToMyProfile -> do
        trackAppActionClick appId (getScreen HOME_SCREEN) "setting_side_bar" "go_to_my_profile"
        trackAppEndScreen appId (getScreen HOME_SCREEN)     
      SettingSideBarController.GoToEmergencyContacts -> do
        trackAppActionClick appId (getScreen HOME_SCREEN) "setting_side_bar" "go_to_emergency_contacts_onclick"
        trackAppEndScreen appId (getScreen HOME_SCREEN) 
      SettingSideBarController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "setting_side_bar" "no_action"
    PricingTutorialModelActionController (PricingTutorialModelController.Close) -> trackAppActionClick appId (getScreen HOME_SCREEN) "pricing_tutorial" "close_icon"
    SearchLocationModelActionController act -> case act of
      SearchLocationModelController.LocationListItemActionController act -> case act of
        LocationListItemController.OnClick item -> trackAppActionClick appId (getScreen HOME_SCREEN) "search_location_modal" "location_list_item"
        LocationListItemController.SelectedCurrentLocation lat lng name -> trackAppActionClick appId (getScreen HOME_SCREEN) "search_location_modal" "location_list_item_onclick_current_location"
        LocationListItemController.FavClick item -> trackAppActionClick appId (getScreen HOME_SCREEN) "search_location_modal" "favourite"
      SearchLocationModelController.PrimaryButtonActionController act -> case act of
        PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "search_location_modal" "primary_button"
        PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "search_location_modal" "primary_button_no_action"
      SearchLocationModelController.SourceChanged input -> trackAppActionClick appId (getScreen HOME_SCREEN) "search_location_modal" "source_changed"
      SearchLocationModelController.DestinationChanged input -> trackAppActionClick appId (getScreen HOME_SCREEN) "search_location_modal" "destination_changed"
      SearchLocationModelController.EditTextFocusChanged textType -> trackAppActionClick appId (getScreen HOME_SCREEN) "search_location_modal" "edit_text_focus_changed"
      SearchLocationModelController.GoBack -> trackAppActionClick appId (getScreen HOME_SCREEN) "search_location_modal" "go_back"
      SearchLocationModelController.SetCurrentLocation -> trackAppActionClick appId (getScreen HOME_SCREEN) "search_location_modal" "set_current_location"
      SearchLocationModelController.SetLocationOnMap -> trackAppActionClick appId (getScreen HOME_SCREEN) "search_location_modal" "set_location_on_map"
      SearchLocationModelController.UpdateSource lat lng name -> trackAppActionClick appId (getScreen HOME_SCREEN) "search_location_modal" "set_current_location_update_source"
      SearchLocationModelController.SourceClear -> trackAppActionClick appId (getScreen HOME_SCREEN) "search_location_modal" "source_clear"
      SearchLocationModelController.DestinationClear -> trackAppActionClick appId (getScreen HOME_SCREEN) "search_location_modal" "destination_clear_options"
      SearchLocationModelController.DebounceCallBack searchString -> trackAppActionClick appId (getScreen HOME_SCREEN) "search_location_modal" "debounce_callback_search"
      SearchLocationModelController.UpdateCurrentLocation lat lng -> trackAppActionClick appId (getScreen HOME_SCREEN) "search_location_modal" "update_current_location"
      SearchLocationModelController.RecenterCurrentLocation -> trackAppActionClick appId (getScreen HOME_SCREEN) "search_location_modal" "recenter_location"
      SearchLocationModelController.SavedAddressClicked act -> case act of
        LocationTagBarController.TagClick savedAddressType arrItem -> trackAppActionClick appId (getScreen HOME_SCREEN) "location_tag_bar" "tag"
      SearchLocationModelController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "search_location_modal" "no_action"
    QuoteListModelActionController act -> case act of
      QuoteListModelController.QuoteListItemActionController act -> case act of
        QuoteListItemController.Click quote -> trackAppActionClick appId (getScreen HOME_SCREEN) "quote_list_modal" "quote_list_item_click"
        QuoteListItemController.CountDown seconds id status timerID -> trackAppActionClick appId (getScreen HOME_SCREEN) "quote_list_modal" "quote_list_item_count_down"
        QuoteListItemController.ConfirmRide -> trackAppActionClick appId (getScreen HOME_SCREEN) "quote_list_modal" "confirm_ride"
        QuoteListItemController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "quote_list_modal" "no_action" 
        QuoteListItemController.CancelAutoAssigning -> trackAppActionClick appId (getScreen HOME_SCREEN) "quote_list_modal" "auto_assign_cancel"
      QuoteListModelController.PrimaryButtonActionController act -> case act of
        PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "quote_list_modal" "confirm_primary_button"
        PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "quote_list_modal" "primary_button_no_action"
      QuoteListModelController.GoBack -> trackAppActionClick appId (getScreen HOME_SCREEN) "quote_list_modal" "on_goback"
      QuoteListModelController.CancelAutoAssigning -> trackAppActionClick appId (getScreen HOME_SCREEN) "quote_list_modal" "auto_assign_cancel"
      QuoteListModelController.HomeButtonActionController act -> case act of
        PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "quote_list_modal" "home_primary_button"
        PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "quote_list_modal" "primary_button_no_action"
      QuoteListModelController.TryAgainButtonActionController act -> case act of
        PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "quote_list_modal" "try_again_primary_button"
        PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "quote_list_modal" "primary_button_no_action"
      QuoteListModelController.HidePopUp -> trackAppActionClick appId (getScreen HOME_SCREEN) "quote_list_modal" "hide_popup"
      QuoteListModelController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "quote_list_modal" "no_action"
    DriverInfoCardActionController act -> case act of
      DriverInfoCardController.PrimaryButtonAC act -> case act of
        PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "driver_info_card" "call_primary_button"
        PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "driver_info_card" "primary_button_no_action" 
      DriverInfoCardController.Support -> trackAppActionClick appId (getScreen HOME_SCREEN) "driver_info_card" "support"
      DriverInfoCardController.CancelRide infoCard -> trackAppActionClick appId (getScreen HOME_SCREEN) "driver_info_card" "cancel_ride"
      DriverInfoCardController.LocationTracking -> trackAppActionClick appId (getScreen HOME_SCREEN) "driver_info_card" "location_tracking"
      DriverInfoCardController.OpenEmergencyHelp -> trackAppActionClick appId (getScreen HOME_SCREEN) "driver_info_card" "open_emergency_help"
      DriverInfoCardController.SourceToDestinationAC  act -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "driver_info_card" "source_to_destination"
      DriverInfoCardController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "driver_info_card" "no_action"
      DriverInfoCardController.MessageDriver -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "driver_info_card" "open_in_app_messaging"
    UpdateLocation key lat lon ->  trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "update_location"
    RateRideButtonActionController act -> case act of
      PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "rate_your_ride" "primary_button"
      PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "rate_your_ride" "primary_button_no_action"
    CancelRidePopUpAction act -> case act of
      CancelRidePopUp.Button1 act -> case act of 
        PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "cancel_ride_popup" "cancel_ride_declined"
        PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "cancel_ride_popup" "primary_button_no_action"
      CancelRidePopUp.Button2 act -> case act of 
        PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "cancel_ride_popup" "cancel_ride_accepted"
        PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "cancel_ride_popup" "primary_button_no_action"
      CancelRidePopUp.UpdateIndex index -> trackAppActionClick appId (getScreen HOME_SCREEN) "cancel_ride_popup" "update_index"
      CancelRidePopUp.OnGoBack -> trackAppActionClick appId (getScreen HOME_SCREEN) "cancel_ride_popup" "go_back"
      CancelRidePopUp.ClearOptions -> trackAppActionClick appId (getScreen HOME_SCREEN) "cancel_ride_popup" "clear_options"
      CancelRidePopUp.TextChanged valId newVal ->  trackAppTextInput appId (getScreen HOME_SCREEN) "cancelling_reason_text_changed" "cancel_ride_popup"
      CancelRidePopUp.NoAction ->  trackAppActionClick appId (getScreen HOME_SCREEN) "cancel_ride_popup" "no_action"
    PopUpModalAction act -> case act of
      PopUpModal.OnButton1Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_action" "on_goback"
      PopUpModal.OnButton2Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_action" "on_cancel"
      PopUpModal.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_action" "no_action"
      PopUpModal.OnImageClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_action" "image"
      PopUpModal.ETextController act -> trackAppTextInput appId (getScreen HOME_SCREEN) "popup_modal_action" "primary_edit_text"
      PopUpModal.CountDown arg1 arg2 arg3 arg4 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_action" "countdown_updated"
    RatingCardAC act -> case act of
      RatingCard.Rating index -> trackAppActionClick appId (getScreen HOME_SCREEN) "rating_card" "star"
      RatingCard.PrimaryButtonAC act -> case act of 
        PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "rating_card" "primary_button"
        PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "rating_card" "primary_button_no_action"
      RatingCard.FareBreakUpAC act -> case act of
        FareBreakUp.ShowInvoice -> do
          trackAppActionClick appId (getScreen HOME_SCREEN) "rating_card" "show_invoice_click"
          trackAppEndScreen appId (getScreen HOME_SCREEN)
        FareBreakUp.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "fare_breakup" "no_action"
        FareBreakUp.SourceToDestinationActionController act -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "fare_breakup" "source_to_destination"
      RatingCard.SkipButtonAC act -> case act of
        PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "rating_card" "skip_primary_button"
        PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "rating_card" "primary_button_no_action"
      RatingCard.FeedbackChanged value -> trackAppActionClick appId (getScreen HOME_SCREEN) "rating_card" "feedback_changed" 
      RatingCard.BackPressed -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "rating_card" "back_pressed" 
      RatingCard.SourceToDestinationAC act -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "rating_card" "source_to_destination"
      RatingCard.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "rating_card" "no_action"
    CloseLocationTracking -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "close_location_tracking"
    StartLocationTracking item -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "start_location_tracking"
    DistanceOutsideLimitsActionController act -> case act of
      PopUpModal.OnButton2Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_distance_outside_limit" "change_drop_location"
      PopUpModal.OnButton1Click -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_distance_outside_limit" "change_drop_location_cancel"
      PopUpModal.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_distance_outside_limit" "no_action"
      PopUpModal.OnImageClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_distance_outside_limit" "image"
      PopUpModal.ETextController act -> trackAppTextInput appId (getScreen HOME_SCREEN) "popup_modal_distance_outside_limit" "primary_edit_text"
      PopUpModal.CountDown arg1 arg2 arg3 arg4 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_distance_outside_limit" "countdown_updated"
    ShortDistanceActionController act -> case act of
      PopUpModal.OnButton1Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_short_distance_action" "book_ride"
      PopUpModal.OnButton2Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_short_distance_action" "go_back"
      PopUpModal.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_short_distance_action" "no_action"
      PopUpModal.OnImageClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_short_distance_action" "image"
      PopUpModal.ETextController act -> trackAppTextInput appId (getScreen HOME_SCREEN) "popup_modal_short_distance_action" "primary_edit_text"
      PopUpModal.CountDown arg1 arg2 arg3 arg4 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_short_distance_action" "countdown_updated"
    SourceUnserviceableActionController act -> case act of
      ErrorModalController.PrimaryButtonActionController act -> case act of 
        PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "source_unserviceable_error" "primary_button_change_location"
        PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "source_unservice_error_modal" "primary_button_no_action"
    GoBackToSearchLocationModal -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "go_back_search_location_modal"
    SkipButtonActionController act -> case act of
      PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "primary_button" "skip"
      PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "source_unservice_error_modal" "primary_button_no_action"
    EstimateChangedPopUpController act -> case act of
      PopUpModal.OnButton1Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_estimate_changed" "go_to_home"
      PopUpModal.OnButton2Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_estimate_changed" "continue"
      PopUpModal.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_estimate_changed" "no_action"
      PopUpModal.OnImageClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_estimate_changed" "image"
      PopUpModal.ETextController act -> trackAppTextInput appId (getScreen HOME_SCREEN) "popup_modal_estimate_changed" "primary_edit_text"
      PopUpModal.CountDown arg1 arg2 arg3 arg4 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_estimate_changed" "countdown_updated"
    RateCardAction act -> case act of
      RateCard.Close -> trackAppActionClick appId (getScreen HOME_SCREEN) "rate_card" "close_click"
      RateCard.BackPressed -> trackAppActionClick appId (getScreen HOME_SCREEN) "rate_card" "back_click"
      RateCard.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "rate_card_action" "no_action"
    ShowRateCard -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "show_rate_card"
    PredictionClickedAction act -> case act of
      LocationListItemController.OnClick item -> trackAppActionClick appId (getScreen HOME_SCREEN) "location_list_item" "prediction"
      LocationListItemController.FavClick item -> trackAppActionClick appId (getScreen HOME_SCREEN) "location_list_item" "prediction_fav_click"
      LocationListItemController.SelectedCurrentLocation lat lng name -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "location_list_item" "selected_current_location"
    SavedAddressClicked (LocationTagBarController.TagClick savedAddressType arrItem) -> trackAppActionClick appId (getScreen HOME_SCREEN) "location_tag_bar" "tag"
    FavouriteLocationModelAC act -> case act of
      FavouriteLocationModelController.GenericHeaderAC act -> case act of
        GenericHeaderController.PrefixImgOnClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "fav_location_modal" "generic_header_back_icon"
        GenericHeaderController.SuffixImgOnClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "fav_location_modal" "generic_header_forward_icon"
      FavouriteLocationModelController.FavouriteLocationAC act -> case act of 
        SavedLocationCardController.CardClicked item -> trackAppActionClick appId (getScreen HOME_SCREEN) "fav_location_modal" "saved_loc_card"
        SavedLocationCardController.DeleteLocation act -> trackAppActionClick appId (getScreen HOME_SCREEN) "fav_location_modal" "delete_location"
        SavedLocationCardController.EditLocation act -> trackAppActionClick appId (getScreen HOME_SCREEN) "fav_location_modal" "edit_location_modal"
        SavedLocationCardController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "fav_location_modal" "no_action"
      FavouriteLocationModelController.ErrorModalAC act -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "fav_location_modal" "error_modal_action"
    SaveFavouriteCardAction act -> case act of
      SaveFavouriteCardController.OnClose -> trackAppActionClick appId (getScreen HOME_SCREEN) "save_fav_card" "on_close_click"
      SaveFavouriteCardController.SaveFavourite -> trackAppActionClick appId (getScreen HOME_SCREEN) "save_fav_card" "save_fav"
      SaveFavouriteCardController.PrimayEditTA (PrimaryEditTextController.TextChanged id val) -> trackAppTextInput appId (getScreen HOME_SCREEN) "save_fav_card_text_changed" "primary_edit_text"
      SaveFavouriteCardController.TagSelected act -> trackAppActionClick appId (getScreen HOME_SCREEN) "save_fav_card" "tag_selected"
      SaveFavouriteCardController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "save_fav_card" "no_action"
    UpdateCurrentLocation lat lng -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "update_current_location"
    EmergencyHelpModalAC act -> case act of
      EmergencyHelpController.GenericHeaderAC act -> case act of
        GenericHeaderController.PrefixImgOnClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "emergency_help_modal" "generic_header_back_icon"
        GenericHeaderController.SuffixImgOnClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "generic_header_action" "forward_icon"
      EmergencyHelpController.CallPolicePopup -> trackAppActionClick appId (getScreen HOME_SCREEN) "emergency_help_modal" "call_police_popup"
      EmergencyHelpController.ContactSupportPopup -> trackAppActionClick appId (getScreen HOME_SCREEN) "emergency_help_modal" "contact_support_popup"
      EmergencyHelpController.CallSuccessfulPopup -> trackAppActionClick appId (getScreen HOME_SCREEN) "emergency_help_modal" "call_successful_popup"
      EmergencyHelpController.CallContactPopUp contact -> trackAppActionClick appId (getScreen HOME_SCREEN) "emergency_help_modal" "call_contact_popup"
      EmergencyHelpController.StoreContacts -> trackAppActionClick appId (getScreen HOME_SCREEN) "emergency_help_modal" "store_contacts"
      EmergencyHelpController.AddedEmergencyContacts -> trackAppActionClick appId (getScreen HOME_SCREEN) "emergency_help_modal" "add_emergency_contacts"
      EmergencyHelpController.CallPolice act -> case act of
        PopUpModal.OnButton1Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_emergency_help" "call_police_cancel"
        PopUpModal.OnButton2Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_emergency_help" "call_police_accept"
        PopUpModal.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_emergency_help" "no_action"
        PopUpModal.OnImageClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_emergency_help" "image"
        PopUpModal.ETextController act -> trackAppTextInput appId (getScreen HOME_SCREEN) "popup_modal_emergency_help" "primary_edit_text"
        PopUpModal.CountDown arg1 arg2 arg3 arg4 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_emergency_help" "countdown_updated"
      EmergencyHelpController.CallEmergencyContact act -> case act of
        PopUpModal.OnButton1Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_emergency_help" "call_contact_cancel"
        PopUpModal.OnButton2Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_emergency_help" "call_contact_accept"
        PopUpModal.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_emergency_help" "no_action"
        PopUpModal.OnImageClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_emergency_help" "image"
        PopUpModal.ETextController act -> trackAppTextInput appId (getScreen HOME_SCREEN) "popup_modal_emergency_help" "primary_edit_text"
        PopUpModal.CountDown arg1 arg2 arg3 arg4 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_emergency_help" "countdown_updated"
      EmergencyHelpController.CallSuccessful act -> case act of
        PopUpModal.OnButton1Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_emergency_help" "call_feedback_cancel"
        PopUpModal.OnButton2Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_emergency_help" "call_feedback_accept"
        PopUpModal.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_emergency_help" "no_action"
        PopUpModal.OnImageClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_emergency_help" "image"
        PopUpModal.ETextController act -> trackAppTextInput appId (getScreen HOME_SCREEN) "popup_modal_emergency_help" "primary_edit_text"
        PopUpModal.CountDown arg1 arg2 arg3 arg4 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_emergency_help" "countdown_updated"
      EmergencyHelpController.ContactSupport act -> case act of
        PopUpModal.OnButton1Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_emergency_help" "contact_support_cancel"
        PopUpModal.OnButton2Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_emergency_help" "contact_support_accept"
        PopUpModal.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_emergency_help" "no_action"
        PopUpModal.OnImageClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_emergency_help" "image"
        PopUpModal.ETextController act -> trackAppTextInput appId (getScreen HOME_SCREEN) "popup_modal_emergency_help" "primary_edit_text"
        PopUpModal.CountDown arg1 arg2 arg3 arg4 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_emergency_help" "countdown_updated"
      EmergencyHelpController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_emergency_help" "no_action"
    PopUpModalShareAppAction act -> case act of
      PopUpModal.OnButton1Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_share_app" "cancel"
      PopUpModal.OnButton2Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_share_app" "accept"
      PopUpModal.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_share_app" "no_action"
      PopUpModal.OnImageClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_share_app" "image"
      PopUpModal.ETextController act -> trackAppTextInput appId (getScreen HOME_SCREEN) "popup_modal_share_app" "primary_edit_text"
      PopUpModal.CountDown arg1 arg2 arg3 arg4 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_share_app" "countdown_updated"
    ContinueWithoutOffers resp -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "continue_without_offers"
    CheckBoxClick autoAssign -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "check_box_click"
    TagClick savedAddressType arrItem -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "tag_click"
    DriverArrivedAction driverArrivalTime -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "driver_arrived_action"
    WaitingTimeAction timerID timeInMinutes seconds -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "waiting_time_action"
    UpdateETA currentETA currentDistance -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "update_eta"
    EstimatesTryAgain quotesRes -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "estimates_try_again"
    SearchExpireCountDown seconds id status timerID -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "search_expiry_count_down"
    FareBreakUpActionController act -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "fare_break_up_action"
    UpdateCurrentStage stage -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "update_current_stage"
    ExitLocationSelected item addToRecents -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "exit_location_selected"
    NotificationListener notificationType -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "notification_listener"
    GetEstimates quotesRes -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "get_estimates"
    GetRideConfirmation resp -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "get_ride_confirmation"
    GetQuotesList resp -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "get_quotes_list"
    MAPREADY key latitude longitude -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "map_ready"
    CurrentLocation lat lng -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "current_location"
    SourceToDestinationActionController act -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "source_to_destination"
    TrackDriver resp -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "track_driver"
    TrackDriverTemp resp str -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "track_driver_temp"
    HandleCallback -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "handle_call_back"
    UpdatePickupLocation  key lat lon -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "update_pickup_location"
    ContinueCmd -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "continue_cmd"
    Restart err -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "restart"
    UpdateSourceName lat lon name -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "update_source_name"
    RequestInfoCardAction act -> case act of 
      RequestInfoCard.Close -> trackAppActionClick appId (getScreen HOME_SCREEN) "request_info_card" "got_it"
      RequestInfoCard.BackPressed -> trackAppActionClick appId (getScreen HOME_SCREEN) "request_info_card" "backpressed_in_screen"
      RequestInfoCard.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "request_info_card" "no_action"
    PreferencesDropDown -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "preferences_drop_down"
    OnIconClick autoAssign -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "close_icon_auto_assign"
    PopUpModalAction act -> case act of
        PopUpModal.OnButton1Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_action" "on_goback"
        PopUpModal.OnButton2Click -> do
          trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_action" "register_on_different_number"
          trackAppEndScreen appId (getScreen HOME_SCREEN)
        PopUpModal.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_action" "no_action"
        PopUpModal.OnImageClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_action" "image"
        PopUpModal.ETextController act -> trackAppTextInput appId (getScreen HOME_SCREEN) "popup_modal_action" "primary_edit_text"
        PopUpModal.CountDown arg1 arg2 arg3 arg4 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_action" "countdown_updated"
    ReferralFlowAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "have_referral_code"
    ReferralFlowNoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "have_referral_code_no_action"
    NewUser -> do
      trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "have_referral_code_no_action"
      trackAppEndScreen appId (getScreen HOME_SCREEN)
    MapReadyAction -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "map_render"
    TrackLiveLocationAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "track_live_location_using"
    LottieLoaderAction -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "auto_rickshaw_processing"
    UpdateSourceFromPastLocations -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "update_source_from_past_saved_locations"
    UpdateLocAndLatLong lat lon-> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "update_current_loc_lat_and_lon"
    UpdateSavedLoc state -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "update_saved_loc"
    NoAction -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "no_action"
    UpdateMessages msg sender timeStamp -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "update_messages"
    InitializeChat -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "initialize_chat"
    RemoveChat -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "remove_chat"
    ChatViewActionController act -> case act of 
      ChatView.SendMessage -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_app_messaging" "send_message"
      ChatView.SendSuggestion suggestion -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_app_messaging" "send_suggestion"
      ChatView.BackPressed -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_app_messaging" "back_pressed"
      ChatView.TextChanged input -> trackAppTextInput appId (getScreen HOME_SCREEN) "in_app_messaging" "text_changed" 
      ChatView.Call -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_app_messaging" "call_driver" 
      ChatView.Navigate -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_app_messaging" "navigate_to_google_maps"
      ChatView.NoAction -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_app_messaging" "no_action"

data ScreenOutput = LogoutUser
                  | Cancel HomeScreenState
                  | GoToHelp HomeScreenState
                  | ConfirmRide HomeScreenState
                  | GoToAbout HomeScreenState
                  | PastRides HomeScreenState
                  | GoToMyProfile HomeScreenState
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
                  | CallContact HomeScreenState
                  | CallSupport HomeScreenState
                  | CallPolice HomeScreenState
                  | UpdateSosStatus HomeScreenState
                  | FetchContacts HomeScreenState

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
            | UpdateSource String String String
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
            | RateRideButtonActionController PrimaryButtonController.Action 
            | CancelRidePopUpAction CancelRidePopUp.Action
            | PopUpModalAction PopUpModal.Action
            | TrackDriver GetDriverLocationResp
            | TrackDriverTemp GetRouteResp RideBookingRes
            | HandleCallback
            | UpdatePickupLocation String String String
            | CloseLocationTracking
            | StartLocationTracking String
            | ExitLocationSelected LocationListItemState Boolean
            | DistanceOutsideLimitsActionController PopUpModal.Action
            | ShortDistanceActionController PopUpModal.Action
            | SourceUnserviceableActionController ErrorModalController.Action
            | UpdateCurrentLocation String String
            | UpdateCurrentStage String
            | GoBackToSearchLocationModal
            | FareBreakUpActionController FareBreakUp.Action
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
            | UpdateSourceName String String String
            | SaveFavouriteCardAction SaveFavouriteCardController.Action
            | TagClick CardType (Maybe LocationListItemState)
            | ContinueWithoutOffers SelectListRes
            | CheckBoxClick Boolean
            | PreferencesDropDown
            | PopUpModalShareAppAction PopUpModal.Action
            | RequestInfoCardAction RequestInfoCard.Action
            | OnIconClick Boolean
            | ReferralFlowAction
            | NewUser
            | MapReadyAction
            | TrackLiveLocationAction
            | LottieLoaderAction
            | ReferralFlowNoAction
            | UpdateSourceFromPastLocations
            | UpdateLocAndLatLong String String
            | UpdateSavedLoc (Array LocationListItemState)
            | UpdateMessages String String String
            | InitializeChat
            | RemoveChat
            | ChatViewActionController ChatView.Action

eval :: Action -> HomeScreenState -> Eval Action ScreenOutput HomeScreenState
eval (UpdateCurrentStage stage) state = do
  _ <- pure $ spy "updateCurrentStage" stage
  if (stage == "INPROGRESS") && (not $ isLocalStageOn RideStarted) then
    exit $ NotificationHandler "TRIP_STARTED" state { props { isInApp = false } }
  else if (stage == "COMPLETED") && (not $ isLocalStageOn HomeScreen) then
    exit $ NotificationHandler "TRIP_FINISHED" state
  else if (stage == "CANCELLED") && (not $ isLocalStageOn HomeScreen) then
    exit $ NotificationHandler "CANCELLED_PRODUCT" state
  else
    continue state

eval (UpdateSavedLoc savedLoc) state = continue state{data{savedLocations = savedLoc}}
eval (UpdateMessages message sender timeStamp) state = do 
  _ <- pure $ spy "Got message in purs" message
  let newMessage = [(ChatView.makeChatComponent message sender timeStamp)]
  let messages = state.data.messages <> [((ChatView.makeChatComponent (getMessage message) sender timeStamp))]
  case (last newMessage) of
    Just value -> if value.sentBy == "Customer" 
                    then updateMessagesWithCmd state {data {suggestionsList = [] , messages = messages}} 
                  else do 
                    let readMessages = fromMaybe 0.0 (fromString (getValueToLocalStore READ_MESSAGES))
                    let unReadMessages = (if readMessages == 0.0 then true else (if (readMessages < (toNumber (length messages)) && state.props.currentStage /= ChatWithDriver) then true else false))
                    let suggestionsList = case value.message of
                                            "I've Arrived" -> replySuggestions ""
                                            _ -> []
                    updateMessagesWithCmd state {data {messages = messages, suggestionsList = suggestionsList}, props {unReadMessages = unReadMessages}}   
    Nothing -> continue state

eval (ChatViewActionController (ChatView.TextChanged value)) state = do
  let sendMessageActive = if (STR.length (STR.trim value)) >= 1 then
                          true
                        else
                          false
  continue state{data{messageToBeSent = (STR.trim value)},props{sendMessageActive = sendMessageActive}}

eval(ChatViewActionController (ChatView.Call)) state = 
  continueWithCmd state
    [ do
        _ <- pure $ showDialer (getDriverNumber "")
        customerId <- getValueToLocalStoreEff CUSTOMER_ID
        _ <- (firebaseLogEventWithTwoParams "ny_user_call_click" "trip_id" (state.props.bookingId) "user_id" customerId)
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
  let suggestions = filter (\item -> (getString item) == chatSuggestion) (chatSuggestionsList "")
  let message = map (\item -> (getEN item)) suggestions
  _ <- pure $ firebaseLogEvent (fromMaybe "" (message !! 0))
  _ <- pure $ sendMessage (fromMaybe "" (message !! 0))
  continue state

eval (ChatViewActionController (ChatView.BackPressed)) state = do
  _ <- pure $ hideKeyboardOnNavigation true
  continueWithCmd state [do 
      pure $ BackPressed 
    ]

eval InitializeChat state = do
  continue state {props { chatcallbackInitiated = true } }

eval RemoveChat state = do 
  continueWithCmd state {props{chatcallbackInitiated = false}} [ do
    _ <- stopChatListenerService
    _ <- pure $ setValueToLocalStore READ_MESSAGES "0.0"
    pure $ NoAction
  ]

eval (DriverInfoCardActionController (DriverInfoCardController.MessageDriver)) state = do
  _ <- pure $ updateLocalStage ChatWithDriver
  _ <- pure $ setValueToLocalStore READ_MESSAGES (show (length state.data.messages))
  continue state {props {currentStage = ChatWithDriver, sendMessageActive = false, unReadMessages = false }}

eval BackPressed state = do
  _ <- pure $ toggleBtnLoader "" false  
  case state.props.currentStage of 
    SearchLocationModel -> do 
                            if state.props.isSaveFavourite then continueWithCmd state [pure $ (SaveFavouriteCardAction (SaveFavouriteCardController.OnClose))]
                              else do
                                _ <- pure $ exitLocateOnMap ""
                                _ <- pure $ hideKeyboardOnNavigation true
                                exit $ GoToHome
    SettingPrice    -> do 
                      if state.props.showRateCard then continue state{props{showRateCard = false}}
                      else if state.props.showMultipleRideInfo then continue state{props{showMultipleRideInfo=false}}
                        else do 
                        _ <- pure $ updateLocalStage SearchLocationModel
                        continue state{props{rideRequestFlow = false, currentStage = SearchLocationModel, searchId = "", isSource = Just false,isSearchLocation = SearchLocation}}
    ConfirmingLocation -> do
                      _ <- pure $ exitLocateOnMap ""
                      _ <- pure $ updateLocalStage SearchLocationModel
                      continue state{props{rideRequestFlow = false, currentStage = SearchLocationModel, searchId = "", isSource = Just false,isSearchLocation = SearchLocation}}
    FindingEstimate -> do
                      _ <- pure $ updateLocalStage SearchLocationModel
                      continue state{props{rideRequestFlow = false, currentStage = SearchLocationModel, searchId = "", isSource = Just false,isSearchLocation = SearchLocation}}
    QuoteList       -> continue state{ props{isPopUp = if ( not null (filter (\a -> a.seconds > 0) state.data.quoteListModelState)) then ActiveQuotePopUp else ConfirmBack}}
    PricingTutorial -> continue state { props { currentStage = SettingPrice}}
    DistanceOutsideLimits -> do 
                      _ <- pure $ updateLocalStage SearchLocationModel
                      continue state{props{rideRequestFlow = false, currentStage = SearchLocationModel, searchId = "", isSource = Just false,isSearchLocation = SearchLocation }}
    ShortDistance -> do 
                      _ <- pure $ updateLocalStage SearchLocationModel
                      continue state{props{isSource = Just false,isPopUp = NoPopUp, rideRequestFlow = false, currentStage = SearchLocationModel, searchId = "", isSearchLocation = SearchLocation}}
    FindingQuotes -> continue state{ props{isPopUp = ConfirmBack}}
    FavouriteLocationModel -> do
                      _ <- pure $ updateLocalStage (if state.props.isSearchLocation == NoView then HomeScreen else SearchLocationModel)
                      continue state { props { currentStage = if state.props.isSearchLocation == NoView then HomeScreen else SearchLocationModel}}
    ChatWithDriver -> do 
                      _ <- pure $ updateLocalStage RideAccepted
                      continue state {props {currentStage = RideAccepted}}
    _               -> do 
                        if state.props.isLocationTracking then continue state{props{isLocationTracking = false}}
                          else if state.props.isSaveFavourite then continueWithCmd state [pure $ (SaveFavouriteCardAction (SaveFavouriteCardController.OnClose))]
                          else if state.props.showShareAppPopUp then continue state{props{showShareAppPopUp=false}} 
                          else if state.props.showMultipleRideInfo then continue state{props{showMultipleRideInfo=false}}
                          else if state.props.emergencyHelpModelState.showContactSupportPopUp then continue state {props {emergencyHelpModelState{showContactSupportPopUp = false}}}
                          else if state.props.emergencyHelpModelState.showCallPolicePopUp then continue state {props{emergencyHelpModelState{showCallPolicePopUp = false}}}
                          else if state.props.emergencyHelpModal then continue state {props {emergencyHelpModal = false}}
                          else do
                            _ <- pure $ minimizeApp ""
                            continue state



eval GoBackToSearchLocationModal state = do
  _ <- pure $ updateLocalStage SearchLocationModel
  continue state { props { rideRequestFlow = false, currentStage = SearchLocationModel, searchId = "", isSearchLocation = SearchLocation, isSource = Just true, isSrcServiceable = true, isRideServiceable = true } }

eval HandleCallback state = do
  _ <- pure $ printLog "storeCallBackCustomer inside HandleCallback" "."
  continue state { props { callbackInitiated = true } }

eval (UpdateSource lat lng name) state = do
  _ <- pure $ printLog "Name::" name
  exit $ UpdatedState state { data { source = name, sourceAddress = encodeAddress name [] state.props.sourcePlaceId}, props { sourceLat = fromMaybe 0.0 (fromString lat), sourceLong = fromMaybe 0.0 (fromString lng) } } true


eval (UpdateSourceName lat lon name) state = continue state {data{source = name, sourceAddress = encodeAddress name [] state.props.sourcePlaceId}}

eval (MAPREADY key latitude longitude) state = 
  case key of 
    _ -> continueWithCmd state [ do
      _ <- checkPermissionAndUpdatePersonMarker state
      pure AfterRender
    ]

eval OpenSearchLocation state = exit $ UpdateSavedLocation state { props { isSource = Just true, currentStage = SearchLocationModel, isSearchLocation = SearchLocation } }

eval (SourceUnserviceableActionController (ErrorModalController.PrimaryButtonActionController PrimaryButtonController.OnClick)) state = continueWithCmd state [ do pure $ OpenSearchLocation ]

eval (UpdateLocation key lat lon) state = case key of
  "LatLon" -> do
    exit $ UpdateLocationName state (fromMaybe 0.0 (fromString lat)) (fromMaybe 0.0 (fromString lon))
  _ -> continue state

eval (UpdatePickupLocation  key lat lon) state = 
  case key of
    "LatLon" -> do 
      exit $ UpdatePickupName state (fromMaybe 0.0 (fromString lat)) (fromMaybe 0.0 (fromString lon))
    _ -> continue state

eval (CheckBoxClick autoAssign) state = do
  let event = if autoAssign then "ny_user_pref_autoassigned" else "ny_user_pref_driveroffers"
  _ <- pure $ firebaseLogEvent event
  _ <- pure $ setValueToLocalStore FLOW_WITHOUT_OFFERS (show autoAssign)
  continue state

eval (OnIconClick autoAssign) state = do
  continue state { props {showMultipleRideInfo = not autoAssign}}

eval PreferencesDropDown state = do
  continue state { data { showPreferences = not state.data.showPreferences}}

eval (RatingCardAC (RatingCard.Rating index)) state = continue state { data { previousRideRatingState { rating = index } } }

eval (RatingCardAC (RatingCard.PrimaryButtonAC PrimaryButtonController.OnClick)) state = updateAndExit state $ SubmitRating state

eval (RatingCardAC (RatingCard.FareBreakUpAC FareBreakUp.ShowInvoice)) state = exit $ GoToInvoice state

eval (RatingCardAC (RatingCard.SkipButtonAC PrimaryButtonController.OnClick)) state = updateAndExit state GoToHome

eval (RatingCardAC (RatingCard.FeedbackChanged value)) state = continue state { data { previousRideRatingState { feedback = value } } }

eval (SettingSideBarActionController (SettingSideBarController.PastRides)) state = exit $ PastRides state { data { settingSideBar { opened = SettingSideBarController.OPEN } } }

eval (SettingSideBarActionController (SettingSideBarController.OnHelp)) state = exit $ GoToHelp state { data { settingSideBar { opened = SettingSideBarController.OPEN } } }

eval (SettingSideBarActionController (SettingSideBarController.ChangeLanguage)) state = exit $ ChangeLanguage state { data { settingSideBar { opened = SettingSideBarController.OPEN } } }

eval (SettingSideBarActionController (SettingSideBarController.GoToAbout)) state = exit $ GoToAbout state { data { settingSideBar { opened = SettingSideBarController.OPEN } } }

eval (SettingSideBarActionController (SettingSideBarController.GoToEmergencyContacts)) state = exit $ GoToEmergencyContacts state { data{settingSideBar{opened = SettingSideBarController.OPEN}}}

eval (SettingSideBarActionController (SettingSideBarController.GoToAbout)) state = exit $ GoToAbout state { data{settingSideBar{opened = SettingSideBarController.OPEN}}}

eval (SettingSideBarActionController (SettingSideBarController.ShareAppLink)) state = 
  do
    _ <- pure $ shareTextMessage "Share Namma Yatri!" "Hey there!\n\nCheck India's first Zero Commission auto booking app.\n100% Open source | 100% Open Data\n\nDownload Namma Yatri now! \nhttps://nammayatri.in/link/rider/SJ8D \n\n #beOpen #chooseOpen"
    continue state

eval (SettingSideBarActionController (SettingSideBarController.EditProfile)) state = exit $ GoToMyProfile state { data { settingSideBar { opened = SettingSideBarController.OPEN } } }

eval (SettingSideBarActionController (SettingSideBarController.OnClosed)) state = continue state{ data{settingSideBar {opened = SettingSideBarController.CLOSED}}}

eval (SettingSideBarActionController (SettingSideBarController.OnClose)) state = 
  if state.props.isPopUp == Logout then 
    continue state {props{isPopUp = NoPopUp}}
    else case state.data.settingSideBar.opened of
              SettingSideBarController.CLOSED -> do 
                                                  if state.props.currentStage == HomeScreen then do
                                                    _ <- pure $ minimizeApp ""
                                                    continue state
                                                    else continueWithCmd state [pure $ BackPressed]
              _                               -> continue state {data{settingSideBar{opened = SettingSideBarController.CLOSING}}}

eval (SettingSideBarActionController (SettingSideBarController.OnLogout)) state = continue state { props { isPopUp = Logout } }

eval (SettingSideBarActionController (SettingSideBarController.GoToFavourites)) state = exit $ GoToFavourites state {data{settingSideBar{opened = SettingSideBarController.OPEN}}}

eval (SettingSideBarActionController (SettingSideBarController.GoToMyProfile)) state = exit $ GoToMyProfile state { data { settingSideBar { opened = SettingSideBarController.OPEN } } }

eval (SearchLocationModelActionController (SearchLocationModelController.PrimaryButtonActionController PrimaryButtonController.OnClick)) state = do 
  _ <- pure $ exitLocateOnMap ""
  let newState = state{props{isSource = Just false, sourceSelectedOnMap = if (state.props.isSource == Just true) then true else state.props.sourceSelectedOnMap, isSearchLocation = SearchLocation, currentStage = SearchLocationModel, locateOnMap = false}} 
  updateAndExit newState $ LocationSelected (fromMaybe dummyListItem state.data.selectedLocationListItem) false newState

eval (PrimaryButtonActionController (PrimaryButtonController.OnClick)) state = do
    _ <- pure $ spy "state homeScreen" state
    case state.props.currentStage of 
      HomeScreen   -> do
        _ <- pure $ firebaseLogEvent "ny_user_where_to_btn"
        exit $ UpdateSavedLocation state{props{isSource = Just false, isSearchLocation = SearchLocation, currentStage = SearchLocationModel}} 
      ConfirmingLocation -> do
        _ <- pure $ exitLocateOnMap ""
        _ <- pure $ updateLocalStage FindingEstimate
        _ <- pure $ firebaseLogEvent "ny_user_confirm_pickup"
        let updatedState = state{props{currentStage = FindingEstimate, locateOnMap = false}}
        -- updateAndExit (updatedState) (UpdatedSource updatedState)
        updateAndExit updatedState $  (UpdatedSource updatedState)
      SettingPrice -> do 
                        _ <- pure $ updateLocalStage FindingQuotes
                        let updatedState = state{props{currentStage = FindingQuotes, searchExpire = 90}}
                        updateAndExit (updatedState) (GetQuotes updatedState) 
      _            -> continue state

eval (RateRideButtonActionController (PrimaryButtonController.OnClick)) state = do
  _ <- pure $ setValueToLocalStore REFERRAL_STATUS "HAS_TAKEN_RIDE"
  continue
    state
      { props { currentStage = RideRating }
      , data
        { previousRideRatingState =
          dummyRideRatingState
            { driverName = state.data.driverInfoCardState.driverName
            , rideId = state.data.driverInfoCardState.rideId
            }
        }
      }

eval (SkipButtonActionController (PrimaryButtonController.OnClick)) state = do
  _ <- pure $ setValueToLocalStore REFERRAL_STATUS "HAS_TAKEN_RIDE"
  updateAndExit state GoToHome

eval OpenSettings state = do 
  _ <- pure $ hideKeyboardOnNavigation true
  continue state { data { settingSideBar { opened = SettingSideBarController.OPEN } } }

eval (SearchExpireCountDown seconds id status timerID) state = do
  if status == "EXPIRED" then do
    _ <- pure $ clearTimer timerID
    continue state { props { searchExpire = seconds } }
  else
    continue state { props { searchExpire = seconds } }

eval CancelSearch state = case state.props.currentStage of
  FindingEstimate -> do
    _ <- pure $ updateLocalStage SearchLocationModel
    continue state { props { currentStage = SearchLocationModel, rideRequestFlow = false, isSearchLocation = SearchLocation } }
  ConfirmingRide -> continue state { props { currentStage = SettingPrice, isSearchLocation = NoView } }
  _ -> continue state

eval SidebarCloseAnimationCompleted state = continue state --{props{sideBarStatus = SettingSideBarController.CLOSED}}

eval OpenPricingTutorial state = continue state { props { currentStage = PricingTutorial } }

eval (PricingTutorialModelActionController (PricingTutorialModelController.Close)) state = continue state { props { currentStage = SettingPrice } }

eval (DriverInfoCardActionController (DriverInfoCardController.PrimaryButtonAC PrimaryButtonController.OnClick)) state =
  continueWithCmd state
    [ do
        _ <- pure $ showDialer (getDriverNumber "")
        _ <- (firebaseLogEventWithTwoParams "ny_user_call_click" "trip_id" (state.props.bookingId) "user_id" (getValueToLocalStore CUSTOMER_ID))
        pure NoAction
    ]

eval (DriverArrivedAction driverArrivalTime) state = do
  _ <- pure $ setValueToLocalStore DRIVER_ARRIVAL_ACTION "TRIGGER_WAITING_ACTION"
  exit $ Cancel state { data { driverInfoCardState { driverArrived = true, driverArrivalTime = getExpiryTime driverArrivalTime "" true } } }

eval (WaitingTimeAction timerID timeInMinutes seconds) state = do
  _ <- pure $ setValueToLocalStore DRIVER_ARRIVAL_ACTION "WAITING_ACTION_TRIGGERED"
  continue state { data { driverInfoCardState { waitingTime = timeInMinutes, driverArrived = false } }, props { waitingTimeTimerId = timerID } }

eval (DriverInfoCardActionController (DriverInfoCardController.Support)) state = do
  _ <- pure $ showDialer (getSupportNumber "")
  _ <- pure $ firebaseLogEvent "ny_user_ride_support_click"
  continue state

eval (DriverInfoCardActionController (DriverInfoCardController.CancelRide infoCard)) state = do
  continue state { props { isCancelRide = true, cancellationReasons = cancelReasons "", cancelRideActiveIndex = Nothing, cancelReasonCode = "", cancelDescription = "" } }

eval (DriverInfoCardActionController (DriverInfoCardController.LocationTracking)) state = continue state { props { isLocationTracking = true } }

eval (DriverInfoCardActionController (DriverInfoCardController.OpenEmergencyHelp)) state = continue state{props{emergencyHelpModal = true}}

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
    void <- pure $ showDialer state.props.emergencyHelpModelState.currentlySelectedContact.phoneNo
    updateAndExit state{props{emergencyHelpModelState{showCallContactPopUp = false}}} $ CallContact state {props {emergencyHelpModelState{showCallContactPopUp = false}}}

eval (EmergencyHelpModalAC (EmergencyHelpController.CallSuccessful PopUpModal.OnButton1Click)) state = do 
    updateAndExit state{props{emergencyHelpModelState{showCallSuccessfulPopUp = false, sosStatus = "NotResolved"}}} $ UpdateSosStatus state {props{emergencyHelpModelState {showCallSuccessfulPopUp = false, sosStatus = "NotResolved"}}}
eval (EmergencyHelpModalAC (EmergencyHelpController.CallSuccessful PopUpModal.OnButton2Click)) state = do
    updateAndExit state{props{emergencyHelpModelState{showCallSuccessfulPopUp = false, sosStatus = "Resolved"}}} $ UpdateSosStatus state {props{emergencyHelpModelState {showCallSuccessfulPopUp = false, sosStatus = "Resolved"}}}

eval (EmergencyHelpModalAC (EmergencyHelpController.CallPolice PopUpModal.OnButton1Click)) state = continue state{props{emergencyHelpModelState{showCallPolicePopUp = false}}}
eval (EmergencyHelpModalAC (EmergencyHelpController.CallPolice PopUpModal.OnButton2Click)) state = do
    void $ pure $  showDialer "100"
    updateAndExit state{props{emergencyHelpModelState{showCallPolicePopUp = false}}} $ CallPolice state {props {emergencyHelpModelState{showCallPolicePopUp = false}}}

eval (EmergencyHelpModalAC (EmergencyHelpController.ContactSupport PopUpModal.OnButton1Click)) state = continue state{props{emergencyHelpModelState{showContactSupportPopUp = false}}}
eval (EmergencyHelpModalAC (EmergencyHelpController.ContactSupport PopUpModal.OnButton2Click)) state = do 
    void $ pure $  showDialer $ getSupportNumber ""
    updateAndExit state{props{emergencyHelpModelState{showContactSupportPopUp = false}}} $ CallSupport state {props {emergencyHelpModelState{showContactSupportPopUp = false}}}

eval (CancelRidePopUpAction (CancelRidePopUp.Button1 PrimaryButtonController.OnClick)) state = continue state { props { isCancelRide = false } }

eval (CancelRidePopUpAction (CancelRidePopUp.OnGoBack)) state = continue state { props { isCancelRide = false } }

eval (CancelRidePopUpAction (CancelRidePopUp.UpdateIndex index)) state = continue state { props { cancelRideActiveIndex = Just index, cancelReasonCode = (fromMaybe dummyCancelReason (state.props.cancellationReasons !! index)).reasonCode } }

eval (CancelRidePopUpAction (CancelRidePopUp.TextChanged valId newVal)) state = continue state { props { cancelDescription = newVal } }

eval (CancelRidePopUpAction (CancelRidePopUp.ClearOptions)) state = do
  _ <- pure $ hideKeyboardOnNavigation true
  continue state { props { cancelDescription = "", cancelReasonCode = "", cancelRideActiveIndex = Nothing } }

eval (CancelRidePopUpAction (CancelRidePopUp.Button2 PrimaryButtonController.OnClick)) state = do
    let newState = state{props{isCancelRide = false,currentStage = HomeScreen, rideRequestFlow = false, isSearchLocation = NoView }}
    case state.props.cancelRideActiveIndex of 
      Just index -> if ( (fromMaybe dummyCancelReason (state.props.cancellationReasons !! index)).reasonCode == "OTHER" || (fromMaybe dummyCancelReason (state.props.cancellationReasons !! index)).reasonCode == "TECHNICAL_GLITCH" ) then exit $ CancelRide newState{props{cancelDescription = if (newState.props.cancelDescription == "") then (fromMaybe dummyCancelReason (state.props.cancellationReasons !!index)).description else newState.props.cancelDescription }}
                      else exit $ CancelRide newState{props{cancelDescription = (fromMaybe dummyCancelReason (state.props.cancellationReasons !!index)).description , cancelReasonCode = (fromMaybe dummyCancelReason (state.props.cancellationReasons !! index)).reasonCode }}
      Nothing    -> continue state

eval (PredictionClickedAction (LocationListItemController.OnClick item)) state = do
  _ <- pure $ firebaseLogEvent "ny_user_prediction_list_item"
  locationSelected item false state{data{source = "Current Location"}, props{isSource = Just false}}

eval (PredictionClickedAction (LocationListItemController.FavClick item)) state = do 
  if (length state.data.savedLocations >= 20) then do 
    void $ pure $ toast (getString FAVOURITE_LIMIT_REACHED)
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
    continueWithCmd newState [do 
        _ <- (setText' (getNewIDWithTag "SourceEditText") item.savedLocation )
        pure $ ExitLocationSelected item false
      ]
    else do 
      let newState = state {data{ destination = item.savedLocation,destinationAddress = item.fullAddress},props{destinationPlaceId = item.placeId, destinationLat = fromMaybe 0.0 item.lat, destinationLong = fromMaybe 0.0 item.lon}}
      continueWithCmd newState [do 
        _ <- (setText' (getNewIDWithTag "DestinationEditText") item.savedLocation )
        pure $ ExitLocationSelected item false
      ]

eval (SavedAddressClicked (LocationTagBarController.TagClick savedAddressType arrItem)) state =  tagClickEvent savedAddressType arrItem state{data{source = "Current Location"}, props{isSource = Just false}} 

eval (SearchLocationModelActionController (SearchLocationModelController.SavedAddressClicked (LocationTagBarController.TagClick savedAddressType arrItem))) state = tagClickEvent savedAddressType arrItem state

eval (TagClick savedAddressType arrItem) state = tagClickEvent savedAddressType arrItem state

eval (SearchLocationModelActionController (SearchLocationModelController.LocationListItemActionController (LocationListItemController.OnClick item))) state = do
  _ <- pure $ firebaseLogEvent "ny_user_location_list_item"
  let condition = state.props.isSource == Just true && item.locationItemType == Just RECENTS
  locationSelected item{tag=if condition then "RECENT" else item.tag} true state{ props { sourceSelectedOnMap = if condition then true else state.props.sourceSelectedOnMap }}

eval (ExitLocationSelected item addToRecents)state = exit $ LocationSelected item  addToRecents state

eval (SearchLocationModelActionController (SearchLocationModelController.PrimaryButtonActionController  PrimaryButtonController.OnClick)) state =
      case state.props.selectedQuote,(null state.data.quoteListModelState),(getValueToLocalStore LOCAL_STAGE) of
                    Nothing, true, "SearchLocationModel"  -> exit $ LocationSelected (fromMaybe dummyListItem state.data.selectedLocationListItem) false state
                    Just _ , false, _                     -> exit $ ConfirmRide state
                    Nothing, true, "QuoteList"            -> exit $ GoToHome
                    _,_,_                                 -> continue state

eval (SearchLocationModelActionController (SearchLocationModelController.DebounceCallBack searchString)) state = do
  if (STR.length searchString > 3) then
    validateSearchInput state searchString
  else
    continue state

eval (SearchLocationModelActionController (SearchLocationModelController.SourceChanged input)) state = do
  _ <- pure $ (setText' (getNewIDWithTag "SourceEditText") input)
  let 
    newState = state {props{sourceSelectedOnMap = if (state.props.locateOnMap) then true else state.props.sourceSelectedOnMap}}
  if (input /= state.data.source) then do
    continueWithCmd newState { props { isRideServiceable = true } }
      [ do
          _ <- pure $ updateInputString input
          pure NoAction
      ]
  else
    continueWithCmd newState
      [ do
          _ <- pure $ updateInputString input
          pure NoAction
      ]

eval (SearchLocationModelActionController (SearchLocationModelController.DestinationChanged input)) state = do
  _ <- pure $ (setText' (getNewIDWithTag "DestinationEditText") input)
  if (input /= state.data.destination) then do
    continueWithCmd state { props { isRideServiceable = true } }
      [ do
          _ <- pure $ updateInputString input
          pure NoAction
      ]
  else
    continueWithCmd state
      [ do
          _ <- pure $ updateInputString input
          pure NoAction
      ]

eval (SearchLocationModelActionController (SearchLocationModelController.EditTextFocusChanged textType)) state = do
  _ <- pure $ spy "searchLocationModal" textType
  if textType == "D" then
    continueWithCmd state { props { isSource = Just false } }
      [ do
          if state.props.isSearchLocation /= LocateOnMap then do
            _ <- (setText' (getNewIDWithTag "DestinationEditText") state.data.destination)
            pure $ NoAction
          else
            pure $ NoAction
      ]
  else
    continueWithCmd state { props { isSource = Just true} }
      [ do
          if state.props.isSearchLocation /= LocateOnMap then do
            _ <- (setText' (getNewIDWithTag "SourceEditText") state.data.source)
            pure $ NoAction
          else
            pure $ NoAction
      ]

eval (SearchLocationModelActionController (SearchLocationModelController.NoAction)) state = continue state

eval (SearchLocationModelActionController (SearchLocationModelController.SourceClear)) state = do
  if (state.props.isSearchLocation /= LocateOnMap) then do
    _ <- pure $ requestKeyboardShow (getNewIDWithTag "SourceEditText")
    pure unit
  else
    pure unit
  continue state { data { source = "" }, props { sourceLat = -0.1, sourceLong = -0.1, sourcePlaceId = Nothing, isSource = Just true, isSrcServiceable = true, isRideServiceable = true } } 

eval (SearchLocationModelActionController (SearchLocationModelController.DestinationClear)) state = do
  if (state.props.isSearchLocation /= LocateOnMap) then do
    _ <- pure $ requestKeyboardShow (getNewIDWithTag "DestinationEditText")
    pure unit
  else
    pure unit
  continue state { data { destination = "" }, props { destinationLat = -0.1, destinationLong = -0.1, destinationPlaceId = Nothing, isSource = Just false, isDestServiceable = true, isRideServiceable = true } } 

eval (SearchLocationModelActionController (SearchLocationModelController.GoBack)) state =
  continueWithCmd state
    [ do
        _ <- pure $ hideKeyboardOnNavigation true
        pure $ BackPressed
    ]

eval (SearchLocationModelActionController (SearchLocationModelController.SetCurrentLocation)) state = do
  _ <- pure $ currentPosition ""
  _ <- pure $ firebaseLogEvent "ny_user_currentlocation_click"
  continue state{props{ sourceSelectedOnMap = if (state.props.isSource == Just true) then false else state.props.sourceSelectedOnMap}}

eval (SearchLocationModelActionController (SearchLocationModelController.SetLocationOnMap)) state = do
  _ <- pure $ locateOnMap true 0.0 0.0
  _ <- pure $ removeAllPolylines ""
  _ <- pure $ hideKeyboardOnNavigation true
  _ <- pure $ firebaseLogEvent "ny_user_click_set_location_on_map"
  let newState = state{props{isSearchLocation = LocateOnMap, currentStage = SearchLocationModel, locateOnMap = true, isRideServiceable = true, showlocUnserviceablePopUp = false}}
  (updateAndExit newState) $ UpdatedState newState false

eval (SearchLocationModelActionController (SearchLocationModelController.UpdateSource lat lng name)) state = do 
  _ <- pure $ hideKeyboardOnNavigation true
  if state.props.isSource == Just true then do 
    let newState = state{data{source = name,sourceAddress = encodeAddress name [] Nothing},props{ sourceLat= fromMaybe 0.0 (fromString lat),  sourceLong = fromMaybe 0.0 (fromString lng), sourcePlaceId = Nothing}}
    updateAndExit newState $ LocationSelected (fromMaybe dummyListItem newState.data.selectedLocationListItem) false newState
    else do
      let newState = state{data{destination = name,destinationAddress = encodeAddress name [] Nothing},props{ destinationLat = fromMaybe 0.0 (fromString lat),  destinationLong = fromMaybe 0.0 (fromString lng), destinationPlaceId = Nothing}}
      updateAndExit newState $ LocationSelected (fromMaybe dummyListItem newState.data.selectedLocationListItem) false newState
   
eval (QuoteListModelActionController (QuoteListModelController.QuoteListItemActionController (QuoteListItemController.Click quote))) state = do
  continueWithCmd (state { data { quoteListModelState = map (\x -> x { selectedQuote = (Just quote.id) }) state.data.quoteListModelState }, props { selectedQuote = Just quote.id } })
    [ do
        if (getValueToLocalStore AUTO_SELECTING) == "CANCELLED_AUTO_ASSIGN" then
          pure NoAction
        else do
          void $ pure $ setValueToLocalStore AUTO_SELECTING quote.id
          pure NoAction
    ]


eval (QuoteListModelActionController (QuoteListModelController.CancelAutoAssigning)) state = do
  _ <- pure $ setValueToLocalStore AUTO_SELECTING "CANCELLED_AUTO_ASSIGN"
  continue state

eval (QuoteListModelActionController (QuoteListModelController.QuoteListItemActionController QuoteListItemController.ConfirmRide)) state = do
  _ <- pure $ firebaseLogEvent "ny_user_quote_confirm"
  exit $ ConfirmRide state

eval (QuoteListModelActionController (QuoteListModelController.QuoteListItemActionController (QuoteListItemController.CountDown seconds id status timerID))) state = do
  if status == "EXPIRED" then do
    _ <- pure $ clearTimer timerID
    let
      autoSelecting = (getValueToLocalStore AUTO_SELECTING) == id
    if (id == fromMaybe "" state.props.selectedQuote && autoSelecting && state.props.currentStage == QuoteList) then do
      _ <- pure $ firebaseLogEvent "ny_user_auto_assign"
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
  case state.props.selectedQuote, (null state.data.quoteListModelState) of
    Just _, false -> do
      _ <- pure $ updateLocalStage ConfirmingRide
      let
        newState = state { props { currentStage = ConfirmingRide } }
      updateAndExit newState $ ConfirmRide newState
    _, _ -> continue state

eval (QuoteListModelActionController (QuoteListModelController.GoBack)) state = continueWithCmd state [ do pure $ BackPressed ]

eval (QuoteListModelActionController (QuoteListModelController.TryAgainButtonActionController  PrimaryButtonController.OnClick)) state = updateAndExit state $ LocationSelected (fromMaybe dummyListItem state.data.selectedLocationListItem) false state{props{currentStage = TryAgain, sourceSelectedOnMap = true}}

eval (QuoteListModelActionController (QuoteListModelController.HomeButtonActionController PrimaryButtonController.OnClick)) state = updateAndExit state GoToHome

eval (Restart err) state = exit $ (LocationSelected (fromMaybe dummyListItem state.data.selectedLocationListItem) false state)

eval (PopUpModalAction (PopUpModal.OnButton1Click)) state = continue state { props { isPopUp = NoPopUp } }

eval (PopUpModalAction (PopUpModal.OnButton2Click)) state = case state.props.isPopUp of
  Logout -> exit LogoutUser
  ConfirmBack -> case state.props.currentStage of
    QuoteList -> exit $ GoToHome
    FindingQuotes -> exit $ GoToHome
    _ -> continue state
  NoPopUp -> continue state
  ActiveQuotePopUp -> exit $ GoToHome

eval (DistanceOutsideLimitsActionController (PopUpModal.OnButton2Click)) state = do
  _ <- pure $ updateLocalStage SearchLocationModel
  continue state { props { isPopUp = NoPopUp, rideRequestFlow = false, currentStage = SearchLocationModel, searchId = "", isSearchLocation = SearchLocation, isSource = Just false, isSrcServiceable = true, isDestServiceable = true, isRideServiceable = true } }

eval (ShortDistanceActionController (PopUpModal.OnButton2Click)) state = do
  _ <- pure $ exitLocateOnMap ""
  exit $ UpdatedSource state

eval (ShortDistanceActionController (PopUpModal.OnButton1Click)) state = do
  _ <- pure $ updateLocalStage SearchLocationModel
  continue state{props{isSource = Just false, isPopUp = NoPopUp, rideRequestFlow = false, currentStage = SearchLocationModel, searchId = "", isSearchLocation = SearchLocation}}


eval (EstimateChangedPopUpController (PopUpModal.OnButton1Click)) state = exit GoToHome

eval (EstimateChangedPopUpController (PopUpModal.OnButton2Click)) state = do
  _ <- pure $ updateLocalStage FindingQuotes
  let
    updatedState = state { props { currentStage = FindingQuotes, isEstimateChanged = false, searchExpire = 90 } }
  updateAndExit updatedState $ GetQuotes updatedState

eval CloseLocationTracking state = continue state { props { isLocationTracking = false } }

eval (StartLocationTracking item) state = do
  case item of
    "GOOGLE_MAP" -> do
      let
        newState = state { props { isLocationTracking = false } }
      updateAndExit (newState) (OpenGoogleMaps newState)
    "IN_APP" -> exit $ InAppTrackStatus state { props { isInApp = not state.props.isInApp, isLocationTracking = false, forFirst = true } }
    _ -> continue state

eval (GetEstimates (GetQuotesRes quotesRes)) state = do
  let
    estimatedQuotes = quotesRes.estimates

    estimatedVarient = filter (\x -> x ^. _vehicleVariant == "AUTO_RICKSHAW") estimatedQuotes

    estimatedPrice = if (isJust (estimatedVarient !! 0)) then (fromMaybe dummyEstimateEntity (estimatedVarient !! 0)) ^. _estimatedFare else 0

    estimateId = if isJust (estimatedVarient !! 0) then (fromMaybe dummyEstimateEntity (estimatedVarient !! 0)) ^. _estimateId else ""

    estimateFareBreakup =
      if isJust (estimatedVarient !! 0) then case (fromMaybe dummyEstimateEntity (estimatedVarient !! 0)) ^. _estimateFareBreakup of
        Just a -> a
        Nothing -> []
      else
        []


    pickUpCharges = case (head (filter (\a -> a ^. _title == "DEAD_KILOMETER_FARE") estimateFareBreakup)) of
      Just a -> a ^. _price
      Nothing -> 0

    additionalFare =
      if isJust (estimatedVarient !! 0) then case (fromMaybe dummyEstimateEntity (estimatedVarient !! 0)) ^. _totalFareRange of
        Just a -> (a ^. _maxFare - a ^. _minFare)
        Nothing -> 20
      else
        20

    nightShiftRate = if isJust (estimatedVarient !! 0) then (fromMaybe dummyEstimateEntity (estimatedVarient !! 0)) ^. _nightShiftRate else Nothing

    nightShiftStart = case nightShiftRate of
      Just a -> fromMaybe "" (a ^. _nightShiftStart)
      Nothing -> ""

    nightShiftEnd = case nightShiftRate of
      Just a -> fromMaybe "" (a ^. _nightShiftEnd)
      Nothing -> ""

    nightShiftMultiplier = case nightShiftRate of
      Just a -> fromMaybe 0.0 (a ^. _nightShiftMultiplier)
      Nothing -> 0.0

    nightCharges = withinTimeRange nightShiftStart nightShiftEnd

    baseFare = case (head (filter (\a -> a ^. _title == "BASE_DISTANCE_FARE") estimateFareBreakup)) of
      Just a -> round $ (toNumber $ a ^. _price) * (if nightCharges then nightShiftMultiplier else 1.0)
      Nothing -> 0

    extraFare = case (head (filter (\a -> a ^. _title == "EXTRA_PER_KM_FARE") estimateFareBreakup)) of
      Just a -> round $ (toNumber $ a ^. _price) * (if nightCharges then nightShiftMultiplier else 1.0)
      Nothing -> 0

    showRateCardIcon = if (null estimateFareBreakup) then false else true
  if ((not (null estimatedVarient)) && (isLocalStageOn FindingEstimate)) then do
    _ <- pure $ firebaseLogEvent "ny_user_estimate"
    exit
      $ SelectEstimate
          state
            { data { suggestedAmount = estimatedPrice, rateCard = { baseFare: baseFare, extraFare: extraFare, pickUpCharges: pickUpCharges, additionalFare: additionalFare, nightShiftMultiplier: nightShiftMultiplier, nightCharges: nightCharges}, showPreferences = false  }
            , props { estimateId = estimateId, currentStage = SettingPrice, showRateCardIcon = showRateCardIcon}
            }
  else do
    _ <- pure $ hideKeyboardOnNavigation true
    _ <- pure $ toast (getString NO_DRIVERS_AVAILABLE)
    continue
      state
        { props { currentStage = SearchLocationModel, rideRequestFlow = false, isSearchLocation = SearchLocation, isSrcServiceable = true, isDestServiceable = true, isRideServiceable = true, showRateCardIcon = showRateCardIcon }
        , data { rateCard = { baseFare: baseFare, extraFare: extraFare, pickUpCharges: pickUpCharges, additionalFare: additionalFare, nightShiftMultiplier: nightShiftMultiplier, nightCharges: nightCharges } }
        }

eval (EstimatesTryAgain (GetQuotesRes quotesRes)) state = do
  _ <- pure $ firebaseLogEvent "ny_user_estimate_try_again"
  let
    estimatedQuotes = quotesRes.estimates

    estimatedVarient = filter (\x -> x ^. _vehicleVariant == "AUTO_RICKSHAW") estimatedQuotes

    estimatedPrice = if (isJust (estimatedVarient !! 0)) then (fromMaybe dummyEstimateEntity (estimatedVarient !! 0)) ^. _estimatedFare else 0

    estimateId = if isJust (estimatedVarient !! 0) then (fromMaybe dummyEstimateEntity (estimatedVarient !! 0)) ^. _estimateId else ""
  case (null estimatedVarient) of
    true -> do
      _ <- pure $ hideKeyboardOnNavigation true
      _ <- pure $ toast (getString NO_DRIVERS_AVAILABLE)
      continue state { props { currentStage = SearchLocationModel, rideRequestFlow = false, isSearchLocation = SearchLocation, isSrcServiceable = true, isDestServiceable = true, isRideServiceable = true } }
    false -> do
      if (estimatedPrice > state.data.suggestedAmount) then
        continue state { data { suggestedAmount = estimatedPrice }, props { estimateId = estimateId, isEstimateChanged = true } }
      else do
        _ <- pure $ updateLocalStage FindingQuotes
        let
          updatedState = state { data { suggestedAmount = estimatedPrice }, props { estimateId = estimateId, currentStage = FindingQuotes, searchExpire = 90 } }
        updateAndExit updatedState $ GetQuotes updatedState

eval (GetQuotesList (SelectListRes resp)) state = do 
  case flowWithoutOffers WithoutOffers of
    true  -> continueWithCmd state [pure $ ContinueWithoutOffers (SelectListRes resp)]
    false -> do
              let selectedQuotes = getQuoteList resp.selectedQuotes
              _ <- pure $ printLog "vehicle Varient " selectedQuotes
              let filteredQuoteList = filter (\a -> length (filter (\b -> a.id == b.id )state.data.quoteListModelState) == 0 ) selectedQuotes
              let removeExpired = filter (\a -> length (filter (\b -> a.id == b ) state.props.expiredQuotes) == 0) filteredQuoteList
              _ <- pure $ spy "quotes" filteredQuoteList
              let newState = state{data{quoteListModelState = state.data.quoteListModelState <> removeExpired },props{isSearchLocation = NoView, isSource = Nothing,currentStage = QuoteList,
                  isPopUp = if state.props.isPopUp == NoPopUp then NoPopUp
                            else if not null (filter (\a -> a.seconds > 0) state.data.quoteListModelState) then ActiveQuotePopUp 
                            else ConfirmBack}}
              if isLocalStageOn QuoteList then do 
                exit $ GetSelectList newState{props{isPopUp = NoPopUp}}
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
  let (QuoteAPIEntity firstOffer) = fromMaybe dummyQuoteAPIEntity (resp.selectedQuotes!!0)
  let quoteId = firstOffer.id
  if quoteId /= "" then exit $ ConfirmRide state{props{selectedQuote = Just quoteId, isSearchLocation = NoView, isSource = Nothing,currentStage = QuoteList}} 
    else continue state


eval (GetRideConfirmation resp) state = do
  let
    (RideBookingRes response) = resp

    rideStatus = (fromMaybe dummyRideAPIEntity ((response.rideList) !! 0)) ^. _status

    newState =
      state
        { props
          { currentStage =
            case rideStatus of
              "NEW" -> RideAccepted
              "INPROGRESS" -> RideStarted
              "COMPLETED" -> RideCompleted
              "CANCELLED" -> HomeScreen
              _ -> RideAccepted
          , isSearchLocation = NoView
          }
        , data
          { driverInfoCardState = getDriverInfo resp
          }
        }
  exit $ RideConfirmed newState { props { isInApp = true } }

eval (NotificationListener notificationType) state = do
  _ <- pure $ printLog "storeCallBackCustomer notificationType" notificationType
  exit $ NotificationHandler notificationType state { props { callbackInitiated = false, isInApp = if notificationType == "DRIVER_ASSIGNMENT" || state.props.currentStage == RideAccepted then true else false } }

eval RecenterCurrentLocation state = recenterCurrentLocation state

eval (SearchLocationModelActionController (SearchLocationModelController.RecenterCurrentLocation)) state = recenterCurrentLocation state 

eval (SearchLocationModelActionController (SearchLocationModelController.UpdateCurrentLocation lat lng)) state = updateCurrentLocation state lat lng

eval (UpdateCurrentLocation lat lng) state = updateCurrentLocation state lat lng

eval (CurrentLocation lat lng) state = exit $ UpdatedState state { props { sourceLat = fromMaybe 0.0 (fromString lat), sourceLong = fromMaybe 0.0 (fromString lng) } } false

eval (RateCardAction RateCard.Close) state = continue state { props { showRateCard = false } }

eval (RateCardAction RateCard.BackPressed) state = continue state { props { showRateCard = false } }

eval (RateCardAction RateCard.NoAction) state = continue state

eval (RequestInfoCardAction RequestInfoCard.Close) state = continue state { props { showMultipleRideInfo = false } }

eval (RequestInfoCardAction RequestInfoCard.BackPressed) state = continue state { props { showMultipleRideInfo = false } }

eval (RequestInfoCardAction RequestInfoCard.NoAction) state = continue state

eval ShowRateCard state = do
  continue state { props { showRateCard = true } }

eval (PopUpModalShareAppAction PopUpModal.OnButton1Click) state= continue state{props{showShareAppPopUp=false}}

eval (PopUpModalShareAppAction PopUpModal.OnButton2Click) state= do
  _ <- pure $ setValueToLocalStore SHARE_APP_COUNT "-1"
  _ <- pure $ shareTextMessage "Share Namma Yatri!" "Hey there!\n\nCheck India's first Zero Commission auto booking app.\n100% Open source | 100% Open Data\n\nDownload Namma Yatri now! \nhttps://nammayatri.in/link/rider/SJ8D \n\n #beOpen #chooseOpen"
  continue state{props{showShareAppPopUp=false}}

eval (UpdateETA currentETA currentDistance) state = do
  let
    newState = state { data { driverInfoCardState { eta = currentETA, distance = currentDistance } } }
  continue newState

eval (ReferralFlowAction) state = exit $ GoToReferral state
eval NewUser state = continueWithCmd state [ do
  if (getValueToLocalNativeStore REGISTRATION_APPROVED) == "true" then do 
    _ <- pure $ setValueToLocalStore REGISTRATION_APPROVED "false"
    _ <- launchAff_ $ flowRunner $ runExceptT $ runBackT $ do 
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
  continueWithCmd state{props{sourceLat = (fromMaybe 0.0 (fromString lat)), sourceLong = (fromMaybe 0.0 (fromString lng))}} [do 
    if os == "IOS" then do
      _ <- addMarker (getCurrentLocationMarker (getValueToLocalStore VERSION_NAME)) 9.9 9.9 160 (0.5) (0.9) 
      pure unit 
      else pure unit
    pure NoAction
  ]

eval _ state = continue state

validateSearchInput :: HomeScreenState -> String -> Eval Action ScreenOutput HomeScreenState
validateSearchInput state searchString =
  if STR.length (STR.trim searchString) > 2 && searchString /= state.data.source && searchString /= state.data.destination then
    callSearchLocationAPI
  else
    continue state
  where
  callSearchLocationAPI = exit $ SearchPlace searchString state

constructLatLong :: String -> String -> String -> Location
constructLatLong lat lng _ =
  { lat: readFloat lat
  , lng: readFloat lng
  , place: ""
  }

checkPermissionAndUpdatePersonMarker :: HomeScreenState -> Effect Unit
checkPermissionAndUpdatePersonMarker state = do
  conditionA <- isLocationPermissionEnabled unit
  conditionB <- isLocationEnabled unit
  let
    conditionC = (state.props.currentStage == HomeScreen)
  if (conditionA && conditionB && conditionC) then do
    _ <- getLocationName (showPersonMarker state (getCurrentLocationMarker (getValueToLocalStore VERSION_NAME))) "9.9" "9.9" "Current Location" constructLatLong
    pure unit
  else do
    if (os == "IOS" && conditionC) then do
      _ <- getLocationName (showPersonMarker state (getCurrentLocationMarker (getValueToLocalStore VERSION_NAME))) "9.9" "9.9" "Current Location" constructLatLong
      pure unit
    else do
      _ <- requestLocation unit
      _ <- checkPermissionAndUpdatePersonMarker state
      pure unit

showPersonMarker :: HomeScreenState -> String -> Location -> Effect Unit
showPersonMarker state marker location = do
  _ <- addMarker (getCurrentLocationMarker (getValueToLocalStore VERSION_NAME)) location.lat location.lng 160 0.5 0.9
  _ <- pure $ printLog "Location :: " location
  animateCamera location.lat location.lng 17

getCurrentCustomerLocation :: forall t44 t51. Applicative t51 => (Action -> Effect Unit) -> t44 -> Effect (t51 Unit)
getCurrentCustomerLocation push state = do
  _ <- getLocationName push "9.9" "9.9" "Current Location" UpdateSource
  pure (pure unit)

dummyEstimateEntity :: EstimateAPIEntity
dummyEstimateEntity =
  EstimateAPIEntity
    { agencyNumber: ""
    , createdAt: ""
    , discount: Nothing
    , estimatedTotalFare: 0
    , agencyName: ""
    , vehicleVariant: ""
    , estimatedFare: 0
    , tripTerms: []
    , id: ""
    , agencyCompletedRidesCount: 0
    , estimateFareBreakup: Nothing
    , totalFareRange: Nothing
    , nightShiftRate: Nothing
    }

cancelReasons :: String -> Array CancellationReasons
cancelReasons dummy =
  [ { reasonCode: "GOT_ANOTHER_RIDE"
    , description: getString GOT_ANOTHER_RIDE_ELSE_WHERE
    }
  , { reasonCode: "HIGH_FARE"
    , description: getString FARE_WAS_HIGH
    }
  , { reasonCode: "WAIT_TIME_TOO_LONG"
    , description: getString WAIT_TIME_TOO_LONG
    } 
  , { reasonCode: "DRIVER_UNREACHABLE"
    , description: getString DRIVER_WAS_NOT_REACHABLE
    }
  , { reasonCode: "DRIVER_WAS_RUDE"
    , description: getString DRIVER_WAS_RUDE
    }
  , { reasonCode: "FORCED_BY_DRIVER"
    , description: getString DRIVER_REQUESTED_TO_CANCEL
    }
  , { reasonCode: "OTHER"
    , description: getString OTHER
    }
  ]

dummyCancelReason :: CancellationReasons
dummyCancelReason =
  { reasonCode: ""
  , description: ""
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
  feedback            : ""
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
  , fullAddress : dummyAddress
  , locationItemType : Nothing
}

tagClickEvent :: CardType -> (Maybe LocationListItemState) -> HomeScreenState -> Eval Action ScreenOutput HomeScreenState
tagClickEvent savedAddressType arrItem state = 
    case savedAddressType, arrItem of
        OTHER_TAG,_  -> do 
          _ <- pure $ updateLocalStage FavouriteLocationModel
          continue state{props{currentStage = FavouriteLocationModel}}
        _,Nothing    -> do 
          if (length state.data.savedLocations >= 20) then do 
            _ <- pure $ toast (getString FAVOURITE_LIMIT_REACHED)
            continue state 
            else updateAndExit state{props{tagType = Just savedAddressType}}  $ CheckFavDistance state{props{tagType = Just savedAddressType}}
        _,Just item  -> do
          if state.props.isSource == Just true then do
            let newState = state {data{ source = item.description, sourceAddress = item.fullAddress},props{sourcePlaceId = item.placeId,sourceLat = fromMaybe 0.0 item.lat,sourceLong =fromMaybe 0.0  item.lon, sourceSelectedOnMap = true }}
            continueWithCmd newState [do 
              _ <- (setText' (getNewIDWithTag "SourceEditText") item.description )
              _ <- removeMarker (getCurrentLocationMarker (getValueToLocalStore VERSION_NAME))
              pure $ ExitLocationSelected item false
            ]
            else do 
              let newState = state {data{ destination = item.description,destinationAddress = item.fullAddress},props{destinationPlaceId = item.placeId, destinationLat = fromMaybe 0.0 item.lat, destinationLong = fromMaybe 0.0 item.lon}}
              continueWithCmd newState [do 
                _ <- (setText' (getNewIDWithTag "DestinationEditText") item.description )
                _ <- removeMarker (getCurrentLocationMarker (getValueToLocalStore VERSION_NAME))
                pure $ ExitLocationSelected item false
                ]
flowWithoutOffers :: LazyCheck -> Boolean
flowWithoutOffers dummy = not $ (getValueToLocalStore FLOW_WITHOUT_OFFERS) == "false" 

recenterCurrentLocation :: HomeScreenState -> Eval Action ScreenOutput HomeScreenState
recenterCurrentLocation state = continueWithCmd state [ do
    _ <- pure $ currentPosition ""
    if state.props.locateOnMap || (not state.props.locateOnMap && state.props.currentStage == ConfirmingLocation) then do 
      pure unit 
    else do
      _ <- addMarker (getCurrentLocationMarker (getValueToLocalStore VERSION_NAME)) 9.9 9.9 160 (0.5) (0.9) 
      pure unit 
    pure NoAction
  ]

updateCurrentLocation :: HomeScreenState -> String -> String -> Eval Action  ScreenOutput HomeScreenState
updateCurrentLocation state lat lng = exit $ (CheckLocServiceability state (fromMaybe 0.0 (fromString lat )) (fromMaybe 0.0 (fromString lng)))

locationSelected :: LocationListItemState -> Boolean -> HomeScreenState -> Eval Action ScreenOutput HomeScreenState
locationSelected item addToRecents state = do 
  _ <- pure $ hideKeyboardOnNavigation true
  if state.props.isSource == Just true then do
    let newState = state {data{ source = item.title, sourceAddress = encodeAddress (item.title <> ", " <>item.subTitle) [] item.placeId},props{sourcePlaceId = item.placeId,sourceLat = fromMaybe 0.0 item.lat,sourceLong =fromMaybe 0.0  item.lon, sourceSelectedOnMap = (item.tag /= "") }}
    continueWithCmd newState [do 
        _ <- (setText' (getNewIDWithTag "SourceEditText") item.title )
        pure $ ExitLocationSelected item addToRecents
      ]
    else do 
      let newState = state {data{ destination = item.title,destinationAddress = encodeAddress (item.title <> ", " <>item.subTitle) [] item.placeId},props{destinationPlaceId = item.placeId, destinationLat = fromMaybe 0.0 item.lat, destinationLong = fromMaybe 0.0 item.lon}}
      continueWithCmd newState [do 
        _ <- (setText' (getNewIDWithTag "DestinationEditText") item.title )
        pure $ ExitLocationSelected item addToRecents
      ]

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

replySuggestions :: String -> Array String
replySuggestions _ = 
  [
    (getString I_WILL_BE_THERE_IN_2_MINS),
    (getString I_AM_AT_THE_PICKUP_LOCATION)
  ]

chatSuggestionsList :: String -> Array STR
chatSuggestionsList _ =
  [
    ARE_YOU_COMING,
    I_WILL_BE_THERE_IN_2_MINS,
    I_AM_AT_THE_PICKUP_LOCATION
  ]
  