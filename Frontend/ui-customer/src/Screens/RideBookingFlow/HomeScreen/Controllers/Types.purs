
module Screens.HomeScreen.Controllers.Types where

import Components.Banner as Banner
import Components.MessagingView as MessagingView
import Components.ChooseVehicle as ChooseVehicleController
import Components.ChooseYourRide as ChooseYourRide
import Components.DriverInfoCard.Controller as DriverInfoCardController
import Components.ErrorModal.Controller as ErrorModalController
import Components.FavouriteLocationModel as FavouriteLocationModelController
import Components.LocationListItem.Controller as LocationListItemController
import Components.LocationTagBar as LocationTagBarController
import Components.LocationTagBarV2 as LocationTagBarV2Controller
import Components.RideCompletedCard.Controller as RideCompletedCard
import Screens.RideBookingFlow.RiderRideCompletedCard.Controller as RiderRideCompletedCard
import Components.MenuButton.Controller (Action) as MenuButtonController
import Components.PopUpModal.Controller as PopUpModal
import Components.PricingTutorialModel.Controller as PricingTutorialModelController
import Components.PrimaryButton.Controller as PrimaryButtonController
import Components.QuoteListModel.Controller as QuoteListModelController
import Components.RateCard as RateCard
import Components.DeliveryParcelImageAndOtp as DeliveryParcelImageAndOtp
import Components.RatingCard as RatingCard
import Components.RequestInfoCard as RequestInfoCard
import Components.SaveFavouriteCard as SaveFavouriteCardController
import Components.SearchLocationModel.Controller as SearchLocationModelController
import Components.SelectListModal.Controller as CancelRidePopUp
import Components.SettingSideBar.Controller as SettingSideBarController
import Components.SourceToDestination.Controller as SourceToDestinationController
import Components.Referral as ReferralComponent
import Components.PopupWithCheckbox.Controller as PopupWithCheckboxController
import Components.BannerCarousel as BannerCarousel

import PrestoDOM.Types.Core (class Loggable)
import Presto.Core.Types.API (ErrorResponse)
import PrestoDOM (BottomSheetState, defaultPerformLog)
import PrestoDOM.List (ListItem)

import Prelude (class Show)
import Data.Maybe (Maybe)

import Screens.Types (BottomNavBarIcon, CallType, CancelSearchType, CardType, HomeScreenState, LocationListItemState, NewContacts, PermissionScreenStage, ReferralType, Trip, NotificationBody, LocationType, LocationType(..), LocationActionId)
import Screens.NammaSafetyFlow.Components.ContactCircle as ContactCircle

import Services.API (FollowRideRes, GetDriverLocationResp, GetEditLocResultResp, GetQuotesRes, RideBookingListRes, RideBookingRes,RideBookingStatusRes, SelectListRes, GetEmergencySettingsRes)
import Common.Types.App as CTP

import RemoteConfig as RemoteConfig


data ScreenOutput = LogoutUser
  | RefreshHomeScreen HomeScreenState
  | GoToHelp HomeScreenState
  | ConfirmRide HomeScreenState
  | GoToAbout HomeScreenState
  | GoToNammaSafety HomeScreenState Boolean Boolean
  | PastRides HomeScreenState Boolean
  | GoToMyProfile HomeScreenState Boolean
  | ChangeLanguage HomeScreenState
  | Retry HomeScreenState
  | GetQuotes HomeScreenState
  | ConfirmFare HomeScreenState
  | UpdatedState HomeScreenState Boolean
  | CancelRide HomeScreenState CancelSearchType
  | NotificationHandler String NotificationBody HomeScreenState
  | GetSelectList HomeScreenState
  | RideConfirmed HomeScreenState
  | SelectEstimate HomeScreenState
  | LocationSelected LocationListItemState Boolean HomeScreenState
  | EditDestLocationSelected LocationListItemState Boolean HomeScreenState
  | EditDestinationSoft HomeScreenState
  | SearchPlace String HomeScreenState Boolean
  | UpdateLocationName HomeScreenState Number Number
  | UpdatePickupName HomeScreenState Number Number
  | GoToHome HomeScreenState
  | GoToFavourites HomeScreenState
  | UpdatedSource HomeScreenState
  | OpenGoogleMaps HomeScreenState
  | InAppTrackStatus HomeScreenState
  | UpdateSavedLocation HomeScreenState
  | CheckLocServiceability HomeScreenState Number Number
  | GoToInvoice HomeScreenState
  | CheckFavDistance HomeScreenState
  | SaveFavourite HomeScreenState
  | GoToReferral ReferralType HomeScreenState
  | CallDriver HomeScreenState CallType String
  | CallContact HomeScreenState
  | CallSupport HomeScreenState
  | CallPolice HomeScreenState
  | UpdateSosStatus HomeScreenState
  | FetchContacts HomeScreenState
  | CheckCurrentStatus
  | CheckFlowStatus HomeScreenState
  | ExitToPermissionFlow PermissionScreenStage
  | RetryFindingQuotes Boolean String HomeScreenState
  | GoToTicketBookingFlow HomeScreenState
  | GoToMyTickets HomeScreenState
  | RepeatTrip HomeScreenState Trip
  | ExitToTicketing HomeScreenState
  | EditLocationScreenOutput HomeScreenState
  | ConfirmEditedPickup HomeScreenState
  | ReAllocateRide HomeScreenState
  | GoToRentalsFlow HomeScreenState
  | GoToScheduledRides HomeScreenState (Maybe String)
  | Add_Stop HomeScreenState
  | SafetySupport HomeScreenState Boolean
  | GoToShareRide HomeScreenState
  | GoToNotifyRideShare HomeScreenState
  | ExitToFollowRide HomeScreenState
  | GoToMyMetroTickets HomeScreenState
  | GoToMetroTicketBookingFlow HomeScreenState
  | GoToSafetyEducation HomeScreenState
  | RepeatSearch HomeScreenState
  | ChangeVehicleVarient HomeScreenState
  | ExitToConfirmingLocationStage HomeScreenState
  | UpdateReferralCode HomeScreenState String
  | GoToSafetySettingScreen 
  | GoToDriverProfiles HomeScreenState
  | GoToRideRelatedIssues HomeScreenState
  | Go_To_Search_Location_Flow HomeScreenState Boolean
  | RideSearchSO
  | ConfirmRentalRideSO HomeScreenState
  | StayInHomeScreenSO HomeScreenState
  | ReloadFlowStatus HomeScreenState
  | ExitToPickupInstructions HomeScreenState Number Number String String
  | EditDestLocSelected HomeScreenState
  | EditDestBackPressed HomeScreenState
  | ExitAndEnterHomeScreen HomeScreenState
  | SelectEstimateAndQuotes HomeScreenState
  | UpdateChatScreen HomeScreenState
  | GoToTripSelectionScreen HomeScreenState
  | RideSummary HomeScreenState
  | GoToParcelInstructions HomeScreenState
  | GetDeliveryImage HomeScreenState
  | GoToDeliveryDetails HomeScreenState
  | GoToSearchLocationScreenForRoutes HomeScreenState LocationActionId
  | GoToBusTicketBookingFlow HomeScreenState

data Action = NoAction
  | BackPressed
  | CancelSearch
  | RideSearchAction
  | RecenterCurrentLocation
  | ConfirmRentalRideAction
  | ChangeToRideAcceptedAction
  | ChangeToRideStartedAction
  | SidebarCloseAnimationCompleted
  | RideDurationTimer String String Int
  | NotificationListener String NotificationBody
  | OpenSettings
  | ContinueCmd
  | OpenPricingTutorial
  | OpenSearchLocation
  | GetEstimates GetQuotesRes Int
  | GetRideConfirmation RideBookingRes
  | GetQuotesList SelectListRes
  | GetEditLocResult GetEditLocResultResp
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
  | EditDestSearchLocationModelActionController SearchLocationModelController.Action
  | QuoteListModelActionController QuoteListModelController.Action
  | DriverInfoCardActionController DriverInfoCardController.Action
  | RatingCardAC RatingCard.Action
  | CancelRidePopUpAction CancelRidePopUp.Action
  | PopUpModalAction PopUpModal.Action
  | TrackDriver GetDriverLocationResp
  | HandleCallback
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
  | UpdateCurrentStage String RideBookingRes
  | UpdateCurrentStageStatus String RideBookingStatusRes
  | GoBackToSearchLocationModal
  | SkipButtonActionController PrimaryButtonController.Action
  | SearchExpireCountDown Int String String
  | EstimatesTryAgain GetQuotesRes Int
  | EstimateChangedPopUpController PopUpModal.Action
  | RateCardAction RateCard.Action
  | ShowRateCard
  | ShowRevisedFareDetails
  | UpdateETA Int Int
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
  | MessagingViewActionController MessagingView.Action
  | HideLiveDashboard String
  | LiveDashboardAction
  | OnResumeCallback
  | CheckFlowStatusAction
  | GoToEditProfile
  | IsMockLocation String
  | MenuButtonActionController MenuButtonController.Action
  | ChooseYourRideAction ChooseYourRide.Action
  | SearchForSelectedLocation
  | GenderBannerModal Banner.Action
  | CancelSearchAction PopUpModal.Action
  | RequestEditAction PopUpModal.Action
  | TriggerPermissionFlow PermissionScreenStage
  | PopUpModalCancelConfirmationAction PopUpModal.Action
  | ScrollToBottom
  | RateClick Int
  | Support
  | RideDetails
  | TerminateApp
  | DirectSearch
  | ZoneTimerExpired PopUpModal.Action
  | ScheduledRideExistsAction PopUpModal.Action
  | DisabilityBannerAC Banner.Action
  | DisabilityPopUpAC PopUpModal.Action
  | RideCompletedAC RideCompletedCard.Action
  | RiderRideCompletedAC RiderRideCompletedCard.Action
  | LoadMessages
  | KeyboardCallback String
  | NotifyDriverStatusCountDown Int String String
  | UpdateProfileButtonAC PrimaryButtonController.Action 
  | SkipAccessibilityUpdateAC PrimaryButtonController.Action
  | SpecialZoneOTPExpiryAction Int String String
  | TicketBookingFlowBannerAC Banner.Action
  | MetroTicketBookingBannerAC Banner.Action
  | ScrollStateChanged String
  | RemoveNotification
  | MessageDriver
  | ToggleMapLottieView Int String String
  | SendQuickMessage String
  | MessageExpiryTimer Int String String
  | NotificationAnimationEnd
  | ShareRide
  | OpenEmergencyHelp
  | OpenOffUsSOS
  | MessageViewAnimationEnd
  | RepeatRide Int Trip
  | Scroll Number
  | WhereToClick 
  | ShowMoreSuggestions 
  | SuggestedDestinationClicked LocationListItemState Boolean
  | RepeatRideCountDown Int String String
  | StopRepeatRideTimer 
  | OpenLiveDashboard
  | UpdatePeekHeight 
  | ReAllocate
  | AutoScrollCountDown Int String String 
  | StopAutoScrollTimer 
  | UpdateRepeatTrips RideBookingListRes 
  | RemoveShimmer 
  | ReportIssueClick
  | EditLocation LocationType
  | DateTimePickerAction String Int Int Int String Int Int
  | ChooseSingleVehicleAction ChooseVehicleController.Action
  | LocationTagBarAC LocationTagBarV2Controller.Action
  | UpdateSheetState BottomSheetState
  | BottomNavBarAction BottomNavBarIcon
  | BannerCarousel BannerCarousel.Action
  | SetBannerItem ListItem
  | UpdateBanner
  | BannerChanged String
  | BannerStateChanged String
  | MetroTicketBannerClickAC Banner.Action
  | SafetyBannerAction Banner.Action
  | SafetyAlertAction PopUpModal.Action
  | ContactAction ContactCircle.Action
  | NotifyRideShare PrimaryButtonController.Action
  | ToggleShare Int
  | UpdateFollowers FollowRideRes
  | GoToFollowRide
  | ShowBookingPreference
  | UpdateBookingDetails RideBookingRes
  | UpdateContacts (Array NewContacts)
  | UpdateChatWithEM Boolean NewContacts
  | ShareRideAction PopupWithCheckboxController.Action
  | AllChatsLoaded
  | GoToSafetyEducationScreen
  | SpecialZoneInfoTag
  | GoToConfirmingLocationStage
  | ReferralComponentAction ReferralComponent.Action
  | GoToHomeScreen
  | ShowMultipleProvider Boolean
  | ShowPref
  | ProviderAutoSelected Int String String
  | ShowProviderInfo Boolean
  | AcWorkingPopupAction PopUpModal.Action
  | NoRender
  | UpdateRateCardCache
  | BannerCarousal BannerCarousel.Action
  | ShowEndOTP
  | RentalInfoAction PopUpModal.Action
  | IntercitySpecialZone PopUpModal.Action
  | StartScheduledRidePolling 
  | RentalBannerClick 
  | SetIssueReportBannerItems ListItem
  | UpdateNextIssueBannerPage Int
  | UpdateNextIssueBanneerSwipe Int 
  | TollChargeAmbigousPopUpAction PopUpModal.Action
  | UpdateNoInternet
  | InternetCallBackCustomer String
  | MarkerLabelOnClick String 
  | ShimmerTimer Int String String
  | ContactSupportAction PopUpModal.Action
  | TollChargeIncludedPopUpAction PopUpModal.Action
  | LocateOnMapCallBack String String String
  | UpdatePickupLocation String String String
  | AmbulanceAgreeClick 
  | AgreePopUp PopUpModal.Action
  | ShakeActionCallback Int
  | UpdateSafetySettings GetEmergencySettingsRes
  | ServicesOnClick RemoteConfig.Service
  | EnableShareRideForContact String 
  | EditPickupPopupOnCancelAC PopUpModal.Action
  | DateSelectAction String String Int Int Int String Int Int
  | IntercityBusPermissionAction PopUpModal.Action
  | IntercityBusAC
  | HideIntercityBusView String
  | OpenDeliverySearchLocation 
  | ToggleCurrentPickupDropCurrentLocation Boolean
  | UpdateSearchActionType
  | DeliveryParcelImageOtpAction DeliveryParcelImageAndOtp.Action
  | ConfirmDeliveryRide
  | RefreshDelveryParcelImage
  | DriverReachedDestinationAction String
  | VOIPCallBack String String String Int Int String String String

instance showAction :: Show Action where show _ = ""
instance loggableAction :: Loggable Action where
  performLog = defaultPerformLog
  -- performLog action appId = case action of
  --   AfterRender -> trackAppScreenRender appId "screen" (getScreen HOME_SCREEN)
  --   BackPressed -> trackAppBackPress appId (getScreen HOME_SCREEN)
  --   CancelSearch -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "cancel_search"
  --   RecenterCurrentLocation -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "recenter_location"
  --   SidebarCloseAnimationCompleted -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "side_bar_close"
  --   OpenSettings -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "open_settings"
  --   OpenPricingTutorial -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "open_pricing_tutorial"
  --   OpenSearchLocation -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "open_search_modal"
  --   UpdateSource lat lon name -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "update_source_address"
  --   HideLiveDashboard val -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "hide_live_stats_dashboard"
  --   LiveDashboardAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "live_Dashboard_action"
  --   PrimaryButtonActionController act -> case act of
  --     PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "primary_button" "onclick"
  --     PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "primary_button" "no_action"
  --   SettingSideBarActionController act -> case act of
  --     SettingSideBarController.PastRides -> do
  --       trackAppActionClick appId (getScreen HOME_SCREEN) "setting_side_bar" "past_rides"
  --       trackAppEndScreen appId (getScreen HOME_SCREEN)
  --     SettingSideBarController.OnHelp -> do
  --       trackAppActionClick appId (getScreen HOME_SCREEN) "setting_side_bar" "on_help"
  --       trackAppEndScreen appId (getScreen HOME_SCREEN)
  --     SettingSideBarController.ChangeLanguage -> do
  --       trackAppActionClick appId (getScreen HOME_SCREEN) "setting_side_bar" "change_language"
  --       trackAppEndScreen appId (getScreen HOME_SCREEN)
  --     SettingSideBarController.GoToAbout -> do
  --       trackAppActionClick appId (getScreen HOME_SCREEN) "setting_side_bar" "go_to_about"
  --       trackAppEndScreen appId (getScreen HOME_SCREEN)
  --     SettingSideBarController.EditProfile -> do
  --       trackAppActionClick appId (getScreen HOME_SCREEN) "setting_side_bar" "edit_profile"
  --       trackAppEndScreen appId (getScreen HOME_SCREEN)
  --     SettingSideBarController.OnClosed -> do
  --       trackAppActionClick appId (getScreen HOME_SCREEN) "setting_side_bar" "on_closed"
  --       trackAppEndScreen appId (getScreen HOME_SCREEN)
  --     SettingSideBarController.OnClose -> do
  --       trackAppActionClick appId (getScreen HOME_SCREEN) "setting_side_bar" "on_close"
  --       trackAppEndScreen appId (getScreen HOME_SCREEN)
  --     SettingSideBarController.OnLogout -> do
  --       trackAppActionClick appId (getScreen HOME_SCREEN) "setting_side_bar" "on_logout"
  --       trackAppEndScreen appId (getScreen HOME_SCREEN)
  --     SettingSideBarController.ShareAppLink -> do
  --       trackAppActionClick appId (getScreen HOME_SCREEN) "setting_side_bar" "share_app_link"
  --       trackAppEndScreen appId (getScreen HOME_SCREEN)
  --     SettingSideBarController.GoToFavourites -> do
  --       trackAppActionClick appId (getScreen HOME_SCREEN) "setting_side_bar" "go_to_favourites"
  --       trackAppEndScreen appId (getScreen HOME_SCREEN)
  --     SettingSideBarController.GoToMyProfile -> do
  --       trackAppActionClick appId (getScreen HOME_SCREEN) "setting_side_bar" "go_to_my_profile"
  --       trackAppEndScreen appId (getScreen HOME_SCREEN)
  --     SettingSideBarController.GoToEmergencyContacts -> do
  --       trackAppActionClick appId (getScreen HOME_SCREEN) "setting_side_bar" "go_to_emergency_contacts_onclick"
  --       trackAppEndScreen appId (getScreen HOME_SCREEN)
  --       trackAppEndScreen appId (getScreen HOME_SCREEN)
  --     SettingSideBarController.LiveStatsDashboard -> do
  --       trackAppActionClick appId (getScreen HOME_SCREEN) "setting_side_bar" "go_to_live_stats_dashboard"
  --       trackAppEndScreen appId (getScreen HOME_SCREEN)
  --     SettingSideBarController.GoToMyTickets -> do
  --       trackAppActionClick appId (getScreen HOME_SCREEN) "setting_side_bar" "go_to_my_tickets"
  --       trackAppEndScreen appId (getScreen HOME_SCREEN)
  --     SettingSideBarController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "setting_side_bar" "no_action"
  --   PricingTutorialModelActionController (PricingTutorialModelController.Close) -> trackAppActionClick appId (getScreen HOME_SCREEN) "pricing_tutorial" "close_icon"
  --   SearchLocationModelActionController act -> case act of
  --     SearchLocationModelController.LocationListItemActionController act -> case act of
  --       LocationListItemController.OnClick item -> trackAppActionClick appId (getScreen HOME_SCREEN) "search_location_modal" "location_list_item"
  --       LocationListItemController.SelectedCurrentLocation lat lng name -> trackAppActionClick appId (getScreen HOME_SCREEN) "search_location_modal" "location_list_item_onclick_current_location"
  --       LocationListItemController.FavClick item -> trackAppActionClick appId (getScreen HOME_SCREEN) "search_location_modal" "favourite"
  --     SearchLocationModelController.PrimaryButtonActionController act -> case act of
  --       PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "search_location_modal" "primary_button"
  --       PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "search_location_modal" "primary_button_no_action"
  --     SearchLocationModelController.SourceChanged input -> trackAppActionClick appId (getScreen HOME_SCREEN) "search_location_modal" "source_changed"
  --     SearchLocationModelController.DestinationChanged input -> trackAppActionClick appId (getScreen HOME_SCREEN) "search_location_modal" "destination_changed"
  --     SearchLocationModelController.EditTextFocusChanged textType -> trackAppActionClick appId (getScreen HOME_SCREEN) "search_location_modal" "edit_text_focus_changed"
  --     SearchLocationModelController.GoBack -> trackAppActionClick appId (getScreen HOME_SCREEN) "search_location_modal" "go_back"
  --     SearchLocationModelController.SetCurrentLocation -> trackAppActionClick appId (getScreen HOME_SCREEN) "search_location_modal" "set_current_location"
  --     SearchLocationModelController.SetLocationOnMap -> trackAppActionClick appId (getScreen HOME_SCREEN) "search_location_modal" "set_location_on_map"
  --     SearchLocationModelController.UpdateSource lat lng name -> trackAppActionClick appId (getScreen HOME_SCREEN) "search_location_modal" "set_current_location_update_source"
  --     SearchLocationModelController.SourceClear -> trackAppActionClick appId (getScreen HOME_SCREEN) "search_location_modal" "source_clear"
  --     SearchLocationModelController.DestinationClear -> trackAppActionClick appId (getScreen HOME_SCREEN) "search_location_modal" "destination_clear_options"
  --     SearchLocationModelController.DebounceCallBack searchString arg -> trackAppActionClick appId (getScreen HOME_SCREEN) "search_location_modal" "debounce_callback_search"
  --     SearchLocationModelController.UpdateCurrentLocation lat lng -> trackAppActionClick appId (getScreen HOME_SCREEN) "search_location_modal" "update_current_location"
  --     SearchLocationModelController.RecenterCurrentLocation -> trackAppActionClick appId (getScreen HOME_SCREEN) "search_location_modal" "recenter_location"
  --     SearchLocationModelController.SavedAddressClicked act -> case act of
  --       LocationTagBarController.TagClick savedAddressType arrItem -> trackAppActionClick appId (getScreen HOME_SCREEN) "location_tag_bar" "tag"
  --     SearchLocationModelController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "search_location_modal" "no_action"
  --   QuoteListModelActionController act -> case act of
  --     QuoteListModelController.QuoteListItemActionController act -> case act of
  --       QuoteListItemController.Click quote -> trackAppActionClick appId (getScreen HOME_SCREEN) "quote_list_modal" "quote_list_item_click"
  --       QuoteListItemController.CountDown seconds status timerID -> trackAppActionClick appId (getScreen HOME_SCREEN) "quote_list_modal" "quote_list_item_count_down"
  --       QuoteListItemController.ConfirmRide -> trackAppActionClick appId (getScreen HOME_SCREEN) "quote_list_modal" "confirm_ride"
  --       QuoteListItemController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "quote_list_modal" "no_action"
  --       QuoteListItemController.CancelAutoAssigning -> trackAppActionClick appId (getScreen HOME_SCREEN) "quote_list_modal" "auto_assign_cancel"
  --     QuoteListModelController.PrimaryButtonActionController act -> case act of
  --       PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "quote_list_modal" "confirm_primary_button"
  --       PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "quote_list_modal" "primary_button_no_action"
  --     QuoteListModelController.GoBack -> trackAppActionClick appId (getScreen HOME_SCREEN) "quote_list_modal" "on_goback"
  --     QuoteListModelController.CancelAutoAssigning -> trackAppActionClick appId (getScreen HOME_SCREEN) "quote_list_modal" "auto_assign_cancel"
  --     QuoteListModelController.HomeButtonActionController act -> case act of
  --       PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "quote_list_modal" "home_primary_button"
  --       PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "quote_list_modal" "primary_button_no_action"
  --     QuoteListModelController.TryAgainButtonActionController act -> case act of
  --       PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "quote_list_modal" "try_again_primary_button"
  --       PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "quote_list_modal" "primary_button_no_action"
  --     QuoteListModelController.HidePopUp -> trackAppActionClick appId (getScreen HOME_SCREEN) "quote_list_modal" "hide_popup"
  --     QuoteListModelController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "quote_list_modal" "no_action"
  --     QuoteListModelController.TipBtnClick arg1 arg2-> trackAppActionClick appId (getScreen HOME_SCREEN) "quote_list_modal" "tip_button_click"
  --     QuoteListModelController.TipViewPrimaryButtonClick act -> case act of
  --       PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "quote_list_modal" "primary_button_on_click"
  --       PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "quote_list_modal" "primary_button_no_action"
  --   DriverInfoCardActionController act -> case act of
  --     DriverInfoCardController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "driver_info_card" "no_action"
  --     DriverInfoCardController.PrimaryButtonAC act -> case act of
  --       PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "driver_info_card" "call_primary_button"
  --       PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "driver_info_card" "primary_button_no_action"
  --     DriverInfoCardController.SourceToDestinationAC  act -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "driver_info_card" "source_to_destination"
  --     DriverInfoCardController.CancelRide infoCard -> trackAppActionClick appId (getScreen HOME_SCREEN) "driver_info_card" "cancel_ride"
  --     DriverInfoCardController.LocationTracking -> trackAppActionClick appId (getScreen HOME_SCREEN) "driver_info_card" "location_tracking"
  --     DriverInfoCardController.MessageDriver -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "driver_info_card" "open_in_app_messaging"
  --     DriverInfoCardController.OnNavigate -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "driver_info_card" "on_navigate"
  --     DriverInfoCardController.CallDriver -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "driver_info_card" "call_driver"
  --     DriverInfoCardController.OnNavigateToZone -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "driver_info_card" "on_navigate_to_zone"
  --     DriverInfoCardController.ToggleBottomSheet -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "driver_info_card" "toggle_bottom_sheet"
  --     DriverInfoCardController.CollapseBottomSheet -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "driver_info_card" "collapse_bottom_sheet"
  --   UpdateLocation key lat lon ->  trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "update_location"
  --   CancelRidePopUpAction act -> case act of
  --     CancelRidePopUp.Button1 act -> case act of
  --       PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "cancel_ride_popup" "cancel_ride_declined"
  --       PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "cancel_ride_popup" "primary_button_no_action"
  --     CancelRidePopUp.Button2 act -> case act of
  --       PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "cancel_ride_popup" "cancel_ride_accepted"
  --       PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "cancel_ride_popup" "primary_button_no_action"
  --     CancelRidePopUp.UpdateIndex index -> trackAppActionClick appId (getScreen HOME_SCREEN) "cancel_ride_popup" "update_index"
  --     CancelRidePopUp.OnGoBack -> trackAppActionClick appId (getScreen HOME_SCREEN) "cancel_ride_popup" "go_back"
  --     CancelRidePopUp.ClearOptions -> trackAppActionClick appId (getScreen HOME_SCREEN) "cancel_ride_popup" "clear_options"
  --     CancelRidePopUp.TextChanged valId newVal ->  trackAppTextInput appId (getScreen HOME_SCREEN) "cancelling_reason_text_changed" "cancel_ride_popup"
  --     CancelRidePopUp.NoAction ->  trackAppActionClick appId (getScreen HOME_SCREEN) "cancel_ride_popup" "no_action"
  --   PopUpModalAction act -> case act of
  --     PopUpModal.OnButton1Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_action" "on_goback"
  --     PopUpModal.OnButton2Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_action" "on_cancel"
  --     PopUpModal.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_action" "no_action"
  --     PopUpModal.OnImageClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_action" "image"
  --     PopUpModal.ETextController act -> trackAppTextInput appId (getScreen HOME_SCREEN) "popup_modal_action" "primary_edit_text"
  --     PopUpModal.CountDown arg1 arg2 arg3 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_action" "countdown_updated"
  --     PopUpModal.Tipbtnclick arg1 arg2 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_action" "tip_clicked"
  --     PopUpModal.DismissPopup -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_action" "popup_dismissed"
  --     PopUpModal.OptionWithHtmlClick -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_action" "options_with_html_click"
  --     PopUpModal.OnSecondaryTextClick -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_action" "secondary_text_click"
  --   RatingCardAC act -> case act of
  --     RatingCard.Rating index -> trackAppActionClick appId (getScreen HOME_SCREEN) "rating_card" "star"
  --     RatingCard.PrimaryButtonAC act -> case act of
  --       PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "rating_card" "primary_button"
  --       PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "rating_card" "primary_button_no_action"
  --     RatingCard.FeedbackChanged value -> trackAppActionClick appId (getScreen HOME_SCREEN) "rating_card" "feedback_changed"
  --     RatingCard.BackPressed -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "rating_card" "back_pressed"
  --     RatingCard.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "rating_card" "no_action"
  --     RatingCard.SelectPill arg1 arg2 -> trackAppActionClick appId (getScreen HOME_SCREEN) "rating_card" "select_pill"
  --   CloseLocationTracking -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "close_location_tracking"
  --   StartLocationTracking item -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "start_location_tracking"
  --   DistanceOutsideLimitsActionController act -> case act of
  --     PopUpModal.OnButton2Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_distance_outside_limit" "change_drop_location"
  --     PopUpModal.OnButton1Click -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_distance_outside_limit" "change_drop_location_cancel"
  --     PopUpModal.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_distance_outside_limit" "no_action"
  --     PopUpModal.OnImageClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_distance_outside_limit" "image"
  --     PopUpModal.ETextController act -> trackAppTextInput appId (getScreen HOME_SCREEN) "popup_modal_distance_outside_limit" "primary_edit_text"
  --     PopUpModal.CountDown arg1 arg2 arg3 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_distance_outside_limit" "countdown_updated"
  --     PopUpModal.Tipbtnclick arg1 arg2 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_distance_outside_limit" "tip_clicked"
  --     PopUpModal.DismissPopup -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_distance_outside_limit" "popup_dismissed"
  --     PopUpModal.OptionWithHtmlClick -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_distance_outside_limit" "options_with_html_click"
  --     PopUpModal.OnSecondaryTextClick -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_distance_outside_limit" "secondary_text_click"
  --   ShortDistanceActionController act -> case act of
  --     PopUpModal.OnButton1Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_short_distance_action" "book_ride"
  --     PopUpModal.OnButton2Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_short_distance_action" "go_back"
  --     PopUpModal.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_short_distance_action" "no_action"
  --     PopUpModal.OnImageClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_short_distance_action" "image"
  --     PopUpModal.ETextController act -> trackAppTextInput appId (getScreen HOME_SCREEN) "popup_modal_short_distance_action" "primary_edit_text"
  --     PopUpModal.CountDown arg1 arg2 arg3 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_short_distance_action" "countdown_updated"
  --     PopUpModal.Tipbtnclick arg1 arg2 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_short_distance_action" "tip_clicked"
  --     PopUpModal.DismissPopup -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_short_distance_action" "popup_dismissed"
  --     PopUpModal.OptionWithHtmlClick -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_short_distance_action" "options_with_html_click"
  --     PopUpModal.OnSecondaryTextClick -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_short_distance_action" "secondary_text_click"
  --   SourceUnserviceableActionController act -> case act of
  --     ErrorModalController.PrimaryButtonActionController act -> case act of
  --       PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "source_unserviceable_error" "primary_button_change_location"
  --       PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "source_unservice_error_modal" "primary_button_no_action"
  --   GoBackToSearchLocationModal -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "go_back_search_location_modal"
  --   SkipButtonActionController act -> case act of
  --     PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "primary_button" "skip"
  --     PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "source_unservice_error_modal" "primary_button_no_action"
  --   EstimateChangedPopUpController act -> case act of
  --     PopUpModal.OnButton1Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_estimate_changed" "go_to_home"
  --     PopUpModal.OnButton2Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_estimate_changed" "continue"
  --     PopUpModal.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_estimate_changed" "no_action"
  --     PopUpModal.OnImageClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_estimate_changed" "image"
  --     PopUpModal.ETextController act -> trackAppTextInput appId (getScreen HOME_SCREEN) "popup_modal_estimate_changed" "primary_edit_text"
  --     PopUpModal.CountDown arg1 arg2 arg3 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_estimate_changed" "countdown_updated"
  --     PopUpModal.Tipbtnclick arg1 arg2 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_estimate_changed" "tip_clicked"
  --     PopUpModal.DismissPopup -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_estimate_changed" "popup_dismissed"
  --     PopUpModal.OptionWithHtmlClick -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_estimate_changed" "options_with_html_click"
  --     PopUpModal.OnSecondaryTextClick -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_estimate_changed" "secondary_text_click"
  --   RateCardAction act -> case act of
  --     RateCard.Close -> trackAppActionClick appId (getScreen HOME_SCREEN) "rate_card" "close_click"
  --     RateCard.BackPressed -> trackAppActionClick appId (getScreen HOME_SCREEN) "rate_card" "back_click"
  --     RateCard.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "rate_card_action" "no_action"
  --     RateCard.GoToDefaultStart -> trackAppActionClick appId (getScreen HOME_SCREEN) "rate_card_action" "back_click"
  --     RateCard.GoToDriverAddition -> trackAppActionClick appId (getScreen HOME_SCREEN) "rate_card_action" "go_to_driver_addition"
  --     RateCard.GoToFareUpdate -> trackAppActionClick appId (getScreen HOME_SCREEN) "rate_card_action" "go_to_fare_update"
  --     RateCard.PrimaryButtonAC act -> case act of
  --       PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "rate_card_primary_button" "on_click"
  --       PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "rate_card_primary_button" "no_action"
  --     RateCard.GoToWaitingCharges -> trackAppActionClick appId (getScreen HOME_SCREEN) "rate_card_action" "go_to_waiting_charges"
  --   ShowRateCard -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "show_rate_card"
  --   PredictionClickedAction act -> case act of
  --     LocationListItemController.OnClick item -> trackAppActionClick appId (getScreen HOME_SCREEN) "location_list_item" "prediction"
  --     LocationListItemController.FavClick item -> trackAppActionClick appId (getScreen HOME_SCREEN) "location_list_item" "prediction_fav_click"
  --     LocationListItemController.SelectedCurrentLocation lat lng name -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "location_list_item" "selected_current_location"
  --   SavedAddressClicked (LocationTagBarController.TagClick savedAddressType arrItem) -> trackAppActionClick appId (getScreen HOME_SCREEN) "location_tag_bar" "tag"
  --   FavouriteLocationModelAC act -> case act of
  --     FavouriteLocationModelController.GenericHeaderAC act -> case act of
  --       GenericHeaderController.PrefixImgOnClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "fav_location_modal" "generic_header_back_icon"
  --       GenericHeaderController.SuffixImgOnClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "fav_location_modal" "generic_header_forward_icon"
  --     FavouriteLocationModelController.FavouriteLocationAC act -> case act of
  --       SavedLocationCardController.CardClicked item -> trackAppActionClick appId (getScreen HOME_SCREEN) "fav_location_modal" "saved_loc_card"
  --       SavedLocationCardController.DeleteLocation act -> trackAppActionClick appId (getScreen HOME_SCREEN) "fav_location_modal" "delete_location"
  --       SavedLocationCardController.EditLocation act -> trackAppActionClick appId (getScreen HOME_SCREEN) "fav_location_modal" "edit_location_modal"
  --       SavedLocationCardController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "fav_location_modal" "no_action"
  --     FavouriteLocationModelController.ErrorModalAC act -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "fav_location_modal" "error_modal_action"
  --   SaveFavouriteCardAction act -> case act of
  --     SaveFavouriteCardController.OnClose -> trackAppActionClick appId (getScreen HOME_SCREEN) "save_fav_card" "on_close_click"
  --     SaveFavouriteCardController.SaveFavourite -> trackAppActionClick appId (getScreen HOME_SCREEN) "save_fav_card" "save_fav"
  --     SaveFavouriteCardController.PrimayEditTA (PrimaryEditTextController.TextChanged id val) -> trackAppTextInput appId (getScreen HOME_SCREEN) "save_fav_card_text_changed" "primary_edit_text"
  --     SaveFavouriteCardController.PrimayEditTA (PrimaryEditTextController.FocusChanged id) -> trackAppTextInput appId (getScreen HOME_SCREEN) "save_fav_card_text_changed" "focus_changed"
  --     SaveFavouriteCardController.TagSelected act -> trackAppActionClick appId (getScreen HOME_SCREEN) "save_fav_card" "tag_selected"
  --     SaveFavouriteCardController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "save_fav_card" "no_action"
  --   UpdateCurrentLocation lat lng -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "update_current_location"
  --   PopUpModalShareAppAction act -> case act of
  --     PopUpModal.OnButton1Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_share_app" "cancel"
  --     PopUpModal.OnButton2Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_share_app" "accept"
  --     PopUpModal.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_share_app" "no_action"
  --     PopUpModal.OnImageClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_share_app" "image"
  --     PopUpModal.ETextController act -> trackAppTextInput appId (getScreen HOME_SCREEN) "popup_modal_share_app" "primary_edit_text"
  --     PopUpModal.CountDown arg1 arg2 arg3 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_share_app" "countdown_updated"
  --     PopUpModal.Tipbtnclick arg1 arg2 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_share_app" "tip_clicked"
  --     PopUpModal.DismissPopup -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_share_app" "popup_dismissed"
  --     PopUpModal.OptionWithHtmlClick -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_share_app" "options_with_html_click"
  --     PopUpModal.OnSecondaryTextClick -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_share_app" "secondary_text_click"
  --   CallSupportAction act -> case act of
  --     PopUpModal.OnButton1Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_contact_support" "cancel"
  --     PopUpModal.OnButton2Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_contact_support" "accept"
  --     PopUpModal.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_contact_support" "no_action"
  --     PopUpModal.OnImageClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_contact_support" "image"
  --     PopUpModal.ETextController act -> trackAppTextInput appId (getScreen HOME_SCREEN) "popup_modal_contact_support" "primary_edit_text"
  --     PopUpModal.CountDown arg1 arg2 arg3 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_contact_support" "countdown_updated"
  --     PopUpModal.Tipbtnclick arg1 arg2 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_contact_support" "tip_clicked"
  --     PopUpModal.DismissPopup -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_contact_support" "popup_dismissed"
  --     PopUpModal.OptionWithHtmlClick -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_contact_support" "options_with_html_click"
  --     PopUpModal.OnSecondaryTextClick -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_contact_support" "secondary_text_click"
  --   ContinueWithoutOffers resp -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "continue_without_offers"
  --   CheckBoxClick autoAssign -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "check_box_click"
  --   TagClick savedAddressType arrItem -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "tag_click"
  --   DriverArrivedAction driverArrivalTime -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "driver_arrived_action"
  --   WaitingTimeAction timerID timeInMinutes seconds -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "waiting_time_action"
  --   UpdateETA currentETA currentDistance -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "update_eta"
  --   EstimatesTryAgain quotesRes -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "estimates_try_again"
  --   SearchExpireCountDown seconds status timerID -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "search_expiry_count_down"
  --   UpdateCurrentStage stage -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "update_current_stage"
  --   ExitLocationSelected item addToRecents -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "exit_location_selected"
  --   NotificationListener notificationType -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "notification_listener"
  --   GetEstimates quotesRes -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "get_estimates"
  --   GetRideConfirmation resp -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "get_ride_confirmation"
  --   GetQuotesList (SelectListRes resp) -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "get_quotes_list"
  --   MAPREADY key latitude longitude -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "map_ready"
  --   CurrentLocation lat lng -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "current_location"
  --   SourceToDestinationActionController act -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "source_to_destination"
  --   TrackDriver resp -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "track_driver"
  --   HandleCallback -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "handle_call_back"
  --   ContinueCmd -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "continue_cmd"
  --   Restart err -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "restart"
  --   UpdateSourceName lat lon name -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "update_source_name"
  --   RequestInfoCardAction act -> case act of
  --     RequestInfoCard.Close -> trackAppActionClick appId (getScreen HOME_SCREEN) "request_info_card" "got_it"
  --     RequestInfoCard.BackPressed -> trackAppActionClick appId (getScreen HOME_SCREEN) "request_info_card" "backpressed_in_screen"
  --     RequestInfoCard.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "request_info_card" "no_action"
  --   PreferencesDropDown -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "preferences_drop_down"
  --   OnIconClick autoAssign -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "close_icon_auto_assign"
  --   PopUpModalAction act -> case act of
  --       PopUpModal.OnButton1Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_action" "on_goback"
  --       PopUpModal.OnButton2Click -> do
  --         trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_action" "register_on_different_number"
  --         trackAppEndScreen appId (getScreen HOME_SCREEN)
  --       PopUpModal.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_action" "no_action"
  --       PopUpModal.OnImageClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_action" "image"
  --       PopUpModal.ETextController act -> trackAppTextInput appId (getScreen HOME_SCREEN) "popup_modal_action" "primary_edit_text"
  --       PopUpModal.CountDown arg1 arg2 arg3 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_action" "countdown_updated"
  --       PopUpModal.Tipbtnclick arg1 arg2 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_action" "tip_clicked"
  --       PopUpModal.DismissPopup -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_action" "popup_dismissed"
  --       PopUpModal.OptionWithHtmlClick -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_action" "options_with_html_click"
  --       PopUpModal.OnSecondaryTextClick -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_action" "secondary_text_click"
  --   ReferralFlowAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "have_referral_code"
  --   ReferralFlowNoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "have_referral_code_no_action"
  --   NewUser -> do
  --     trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "have_referral_code_no_action"
  --     trackAppEndScreen appId (getScreen HOME_SCREEN)
  --   MapReadyAction -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "map_render"
  --   TrackLiveLocationAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "track_live_location_using"
  --   LottieLoaderAction -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "auto_rickshaw_processing"
  --   UpdateSourceFromPastLocations -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "update_source_from_past_saved_locations"
  --   UpdateLocAndLatLong lat lon-> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "update_current_loc_lat_and_lon"
  --   UpdateSavedLoc state -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "update_saved_loc"
  --   NoAction -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "no_action"
  --   UpdateMessages msg sender timeStamp size -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "update_messages"
  --   InitializeChat -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "initialize_chat"
  --   RemoveChat -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "remove_chat"
  --   MessagingViewActionController act -> case act of
  --     MessagingView.SendMessage -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_app_messaging" "send_message"
  --     MessagingView.SendSuggestion suggestion -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_app_messaging" "send_suggestion"
  --     MessagingView.BackPressed -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_app_messaging" "back_pressed"
  --     MessagingView.TextChanged input -> trackAppTextInput appId (getScreen HOME_SCREEN) "in_app_messaging" "text_changed"
  --     MessagingView.Call -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_app_messaging" "call_driver"
  --     MessagingView.NoAction -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_app_messaging" "no_action"
  --   OnResumeCallback -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "on_resume_callback"
  --   CheckFlowStatusAction -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "check_flow_status"
  --   GoToEditProfile -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "GoToEditProfile"
  --   HideLiveDashboard val -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "hide_live_dashboard"
  --   LiveDashboardAction -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "LiveDashboardAction"
  --   OnResumeCallback -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "OnResumeCallback"
  --   CheckFlowStatusAction -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "CheckFlowStatusAction"
  --   IsMockLocation val -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "IsMockLocation"
  --   MenuButtonActionController act -> case act of 
  --     MenuButtonController.OnClick arg -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "menu_button_action" "on_click"
  --   ChooseYourRideAction act -> case act of 
  --     ChooseYourRideController.NoAction -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "choose_your_ride_action" "no_action"
  --     ChooseYourRideController.ChooseVehicleAC arg -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "choose_your_ride_action" "choose_vehicle"
  --     ChooseYourRideController.RadioButtonClick arg -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "choose_your_ride_action" "CheckBoxClick"
  --     ChooseYourRideController.OnIconClick arg -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "choose_your_ride_action" "OnIconClick"
  --     ChooseYourRideController.PreferencesDropDown -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "choose_your_ride_action" "preferences_drop_down"
  --     ChooseYourRideController.PrimaryButtonActionController act -> case act of
  --       PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "choose_your_ride_action" "primary_button"
  --       PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "choose_your_ride_action" "primary_button_no_action"
  --   SearchForSelectedLocation -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "search_for_saved_location"
  --   GenderBannerModal act -> case act of 
  --     Banner.OnClick -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "gender_banner_modal" "banner_on_click"
  --     Banner.NoAction -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "gender_banner_modal" "banner_no_action"
  --   CancelSearchAction act -> case act of 
  --     PopUpModal.OnButton1Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_cancel_search" "cancel"
  --     PopUpModal.OnButton2Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_cancel_search" "accept"
  --     PopUpModal.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_cancel_search" "no_action"
  --     PopUpModal.OnImageClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_cancel_search" "image"
  --     PopUpModal.ETextController act -> trackAppTextInput appId (getScreen HOME_SCREEN) "popup_modal_cancel_search" "primary_edit_text"
  --     PopUpModal.CountDown arg1 arg2 arg3 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_cancel_search" "countdown_updated"
  --     PopUpModal.Tipbtnclick arg1 arg2 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_cancel_search" "tip_clicked"
  --     PopUpModal.DismissPopup -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_cancel_search" "popup_dismissed"
  --     PopUpModal.OptionWithHtmlClick -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_cancel_search" "options_with_html_click"
  --     PopUpModal.OnSecondaryTextClick -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_cancel_search" "secondary_text_click"
  --   TriggerPermissionFlow val -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "trigger_persmission_screen_flow"
  --   PopUpModalCancelConfirmationAction act -> case act of 
  --     PopUpModal.OnButton1Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_cancel_confirmation" "button1_click"
  --     PopUpModal.OnButton2Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_cancel_confirmation" "button2_click"
  --     PopUpModal.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_cancel_confirmation" "no_action"
  --     PopUpModal.OnImageClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_cancel_confirmation" "image_click"
  --     PopUpModal.ETextController act -> trackAppTextInput appId (getScreen HOME_SCREEN) "popup_modal_cancel_confirmation" "primary_edit_text"
  --     PopUpModal.CountDown arg1 arg2 arg3 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_cancel_confirmation" "countdown_updated"
  --     PopUpModal.Tipbtnclick arg1 arg2 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_cancel_confirmation" "tip_clicked"
  --     PopUpModal.DismissPopup -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_cancel_confirmation" "popup_dismissed"
  --     PopUpModal.OptionWithHtmlClick -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_cancel_confirmation" "options_with_html_click"
  --     PopUpModal.OnSecondaryTextClick -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_cancel_confirmation" "secondary_text_click"
  --   ScrollToBottom -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "scroll_to_bottom"
  --   SelectButton val -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "select_button"
  --   RateClick val -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "rate_click"
  --   Support -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "support"
  --   IssueReportIndex val -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "issue_report_index"
  --   RideDetails -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "ride_details"
  --   TerminateApp -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "terminate_app"
  --   DirectSearch -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "direct_search"
  --   ZoneTimerExpired act -> case act of 
  --     PopUpModal.OnButton1Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_zone_timer_expired" "button1_click"
  --     PopUpModal.OnButton2Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_zone_timer_expired" "button2_click"
  --     PopUpModal.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_zone_timer_expired" "no_action"
  --     PopUpModal.OnImageClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_zone_timer_expired" "image_click"
  --     PopUpModal.ETextController act -> trackAppTextInput appId (getScreen HOME_SCREEN) "popup_modal_zone_timer_expired" "primary_edit_text"
  --     PopUpModal.CountDown arg1 arg2 arg3 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_zone_timer_expired" "countdown_updated"
  --     PopUpModal.Tipbtnclick arg1 arg2 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_zone_timer_expired" "tip_clicked"
  --     PopUpModal.DismissPopup -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_zone_timer_expired" "popup_dismissed"
  --     PopUpModal.OptionWithHtmlClick -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_zone_timer_expired" "options_with_html_click"
  --     PopUpModal.OnSecondaryTextClick -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_zone_timer_expired" "secondary_text_click"
  --   DisabilityBannerAC act -> case act of 
  --     Banner.OnClick -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "disability_banner" "banner_on_click"
  --     Banner.NoAction -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "disability_banner" "banner_no_action"
  --   DisabilityPopUpAC act -> case act of 
  --     PopUpModal.OnButton1Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "disability_pop_up" "button1_click"
  --     PopUpModal.OnButton2Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "disability_pop_up" "button2_click"
  --     PopUpModal.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "disability_pop_up" "no_action"
  --     PopUpModal.OnImageClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "disability_pop_up" "image_click"
  --     PopUpModal.ETextController act -> trackAppTextInput appId (getScreen HOME_SCREEN) "disability_pop_up" "primary_edit_text"
  --     PopUpModal.CountDown arg1 arg2 arg3 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "disability_pop_up" "countdown_updated"
  --     PopUpModal.Tipbtnclick arg1 arg2 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "disability_pop_up" "tip_clicked"
  --     PopUpModal.DismissPopup -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "disability_pop_up" "popup_dismissed"
  --     PopUpModal.OptionWithHtmlClick -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "disability_pop_up" "options_with_html_click"
  --     PopUpModal.OnSecondaryTextClick -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "disability_pop_up" "secondary_text_click"
  --   RideCompletedAC act -> case act of 
  --     RideCompletedCard.Support -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "ride_completed_screen" "support_clicked"
  --     RideCompletedCard.RideDetails -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "ride_completed_screen" "ride_details_clicked"
  --     RideCompletedCard.SelectButton arg1 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "ride_completed_screen" "select_button_click"
  --     RideCompletedCard.IssueReportIndex arg1 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "ride_completed_screen" "issue_report_index"
  --     RideCompletedCard.RateClick arg1 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "ride_completed_screen" "rate_click"
  --     RideCompletedCard.IssueReportPopUpAC act -> case act of 
  --       CancelRidePopUp.Button1 act -> case act of
  --         PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "ride_completed_screen_issue_report_popup_1" "primary_button_on_click"
  --         PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "ride_completed_screen_issue_report_popup_1" "primary_button_no_action"
  --       CancelRidePopUp.Button2 act -> case act of
  --         PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "ride_completed_screen_issue_report_popup_2" "primary_button_on_click"
  --         PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "ride_completed_screen_issue_report_popup_2" "primary_button_no_action"
  --       CancelRidePopUp.UpdateIndex index -> trackAppActionClick appId (getScreen HOME_SCREEN) "ride_completed_screen_issue_report_popup" "update_index"
  --       CancelRidePopUp.OnGoBack -> trackAppActionClick appId (getScreen HOME_SCREEN) "ride_completed_screen_issue_report_popup" "go_back"
  --       CancelRidePopUp.ClearOptions -> trackAppActionClick appId (getScreen HOME_SCREEN) "ride_completed_screen_issue_report_popup" "clear_options"
  --       CancelRidePopUp.TextChanged valId newVal ->  trackAppTextInput appId (getScreen HOME_SCREEN) "ride_completed_screen_issue_report_popup" "text_changed"
  --       CancelRidePopUp.NoAction ->  trackAppActionClick appId (getScreen HOME_SCREEN) "ride_completed_screen_issue_report_popup" "no_action"
  --     RideCompletedCard.SkipButtonActionController act -> case act of 
  --       PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "ride_completed_screen_skip_button_action" "primary_button_on_click"
  --       PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "ride_completed_screen_skip_button_action" "primary_button_no_action"
  --     RideCompletedCard.ContactSupportPopUpAC act -> case act of 
  --       PopUpModal.OnButton1Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "ride_completed_screen_conatct_support_pop_up" "button1_click"
  --       PopUpModal.OnButton2Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "ride_completed_screen_conatct_support_pop_up" "button2_click"
  --       PopUpModal.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "ride_completed_screen_conatct_support_pop_up" "no_action"
  --       PopUpModal.OnImageClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "ride_completed_screen_conatct_support_pop_up" "image_click"
  --       PopUpModal.ETextController act -> trackAppTextInput appId (getScreen HOME_SCREEN) "ride_completed_screen_conatct_support_pop_up" "primary_edit_text"
  --       PopUpModal.CountDown arg1 arg2 arg3 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "ride_completed_screen_conatct_support_pop_up" "countdown_updated"
  --       PopUpModal.Tipbtnclick arg1 arg2 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "ride_completed_screen_conatct_support_pop_up" "tip_clicked"
  --       PopUpModal.DismissPopup -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "ride_completed_screen_conatct_support_pop_up" "popup_dismissed"
  --       PopUpModal.OptionWithHtmlClick -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "ride_completed_screen_conatct_support_pop_up" "options_with_html_click"
  --       PopUpModal.OnSecondaryTextClick -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "ride_completed_screen_conatct_support_pop_up" "secondary_text_click"
  --     RideCompletedCard.UpiQrRendered arg1 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "ride_completed_screen_conatct_support_pop_up" "upi_qr_rendered"
  --     RideCompletedCard.BannerAction act -> case act of 
  --       Banner.OnClick -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "ride_completed_banner" "banner_on_click"
  --       Banner.NoAction -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "ride_completed_banner" "banner_no_action"
  --     RideCompletedCard.HelpAndSupportAC -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "ride_completed_help_and_support" "help_and_support"
  --   LoadMessages -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "load_messages"
  --   KeyboardCallback val -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "key_board_callback"
  --   NotifyDriverStatusCountDown arg1 arg2 arg3 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "notify_driver_status_countdown"
  --   UpdateProfileButtonAC act -> case act of 
  --     PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "update_profile_button" "primary_button_on_click"
  --     PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "update_profile_button" "primary_button_no_action"
  --   SkipAccessibilityUpdateAC act -> case act of 
  --     PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "skip_accessibility_button" "primary_button_on_click"
  --     PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "skip_accessibility_button" "primary_button_no_action"
  --   SpecialZoneOTPExpiryAction arg1 arg2 arg3 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "special_zone_otp_expiry_action"
  --   TicketBookingFlowBannerAC act -> case act of 
  --     Banner.OnClick -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "ticket_booking_flow" "banner_on_click"
  --     Banner.NoAction -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "ticket_booking_flow" "banner_no_action"
  --   WaitingInfo -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "waiting_info"
  --   ShareRide -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "share_ride"
  --   ScrollStateChanged val -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "scroll_state_changed"
  --   RemoveNotification -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "remove_notification"
  --   MessageDriver -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "message_driver"
  --   SendQuickMessage val -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "send_quick_message"
  --   MessageExpiryTimer seconds status timerID -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "message_expiry_timer"
  --   NotificationAnimationEnd -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "notification_animation_end"
  --   RideSupport -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "ride_support"
  --   OpenEmergencyHelp -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "open_emergency_help"
  --   ShowCallDialer callType -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "show_call_dialer"
  --   CloseShowCallDialer -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "close_show_call_dialer"
  --   CheckAndAskNotificationPermission -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "check_and_ask_notification_permission"
  --   OpenChatScreen -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "open_chat_screen"
  --   PickUpFarFromCurrentLocAC act -> case act of
  --     PopUpModal.OnButton1Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "pick_up_far_from_curr_location_pop_up" "book_ride"
  --     PopUpModal.OnButton2Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "pick_up_far_from_curr_location_pop_up" "go_back"
  --     PopUpModal.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "pick_up_far_from_curr_location_pop_up" "no_action"
  --     PopUpModal.OnImageClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "pick_up_far_from_curr_location_pop_up" "image"
  --     PopUpModal.ETextController act -> trackAppTextInput appId (getScreen HOME_SCREEN) "pick_up_far_from_curr_location_pop_up" "primary_edit_text"
  --     PopUpModal.CountDown arg1 arg2 arg3 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "pick_up_far_from_curr_location_pop_up" "countdown_updated"
  --     PopUpModal.Tipbtnclick arg1 arg2 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "pick_up_far_from_curr_location_pop_up" "tip_clicked"
  --     PopUpModal.DismissPopup -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "pick_up_far_from_curr_location_pop_up" "popup_dismissed"
  --     PopUpModal.OptionWithHtmlClick -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "pick_up_far_from_curr_location_pop_up" "options_with_html_click"
  --     PopUpModal.OnSecondaryTextClick -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "pick_up_far_from_curr_location_pop_up" "secondary_text_click"
  --   IssueReportPopUpAC act -> case act of
  --     CancelRidePopUp.Button1 act -> case act of
  --       PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "issue_report_popup_button1_action" "primary_button_on_click"
  --       PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "issue_report_popup_button1_action" "primary_button_no_action"
  --     CancelRidePopUp.Button2 act -> case act of
  --       PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "issue_report_popup_button2_action" "primary_button_on_click"
  --       PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "issue_report_popup_button2_action" "primary_button_no_action"
  --     CancelRidePopUp.UpdateIndex index -> trackAppActionClick appId (getScreen HOME_SCREEN) "issue_report_popup" "update_index"
  --     CancelRidePopUp.OnGoBack -> trackAppActionClick appId (getScreen HOME_SCREEN) "issue_report_popup" "go_back"
  --     CancelRidePopUp.ClearOptions -> trackAppActionClick appId (getScreen HOME_SCREEN) "issue_report_popup" "clear_options"
  --     CancelRidePopUp.TextChanged valId newVal ->  trackAppTextInput appId (getScreen HOME_SCREEN) "issue_report_popup" "text_changed"
  --     CancelRidePopUp.NoAction ->  trackAppActionClick appId (getScreen HOME_SCREEN) "issue_report_popup" "no_action"
  --   CheckFlowStatusAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "check_flow_status_action"
  --   HideLiveDashboard arg -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "hide_dash_board_action"
  --   PopUpModalAction act -> case act of 
  --     PopUpModal.OnButton1Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen_pop_up" "button1_click"
  --     PopUpModal.OnButton2Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen_pop_up" "button2_click"
  --     PopUpModal.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen_pop_up" "no_action"
  --     PopUpModal.OnImageClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen_pop_up" "image_click"
  --     PopUpModal.ETextController act -> trackAppTextInput appId (getScreen HOME_SCREEN) "in_screen_pop_up" "primary_edit_text"
  --     PopUpModal.CountDown arg1 arg2 arg3 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen_pop_up" "countdown_updated"
  --     PopUpModal.Tipbtnclick arg1 arg2 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen_pop_up" "tip_clicked"
  --     PopUpModal.DismissPopup -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen_pop_up" "popup_dismissed"
  --     PopUpModal.OptionWithHtmlClick -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen_pop_up" "options_with_html_click"
  --     PopUpModal.OnSecondaryTextClick -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen_pop_up" "secondary_text_click"
  --   MessageViewAnimationEnd -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "message_view_animation_end"
  --   RepeatRide arg1 arg2 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "repeat_ride"
  --   Scroll arg1 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "scroll_action"
  --   WhereToClick -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "where_to_click"
  --   ShowMoreSuggestions -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "show_more_suggestions"
  --   SuggestedDestinationClicked arg1 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "suggested_destination_clicked"
  --   RepeatRideCountDown arg1 arg2 arg3 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "repeat_ride_count_down"
  --   StopRepeatRideTimer -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "stop_repeat_ride_timer"
  --   OpenLiveDashboard -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "open_live_dashboard"
  --   UpdatePeekHeight -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "update_peek_height"
  --   ReAllocate -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "reallocate_ride"
  --   AutoScrollCountDown arg1 arg2 arg3 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "auto_scroll_count_down"
  --   StopAutoScrollTimer -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "stop_auto_scroll_timer" 
  --   UpdateRepeatTrips arg1 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "update_repeat_trips"
  --   RemoveShimmer -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "remove_shimmer"
  --   ReportIssueClick -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "report_issue_click"
  --   ChooseSingleVehicleAction act -> case act of 
  --         ChooseVehicleController.NoAction -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "choose_your_ride_action" "no_action"
  --         ChooseVehicleController.ShowRateCard arg -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "choose_your_ride_action" "ShowRateCard"
  --         ChooseVehicleController.OnImageClick -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "choose_your_ride_action" "OnImageClick" 
  --         ChooseVehicleController.OnSelect arg -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "choose_your_ride_action" "OnSelect"
