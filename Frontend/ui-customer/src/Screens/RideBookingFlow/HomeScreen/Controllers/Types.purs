
module Screens.HomeScreen.Controllers.Types where

import Common.Types.App (EventPayload(..), GlobalPayload(..), LazyCheck(..), OptionButtonList, Payload(..), RateCardType(..), FeedbackAnswer(..), ProviderType(..))
import Accessor (_estimatedFare, _estimateId, _vehicleVariant, _status, _estimateFareBreakup, _title, _totalFareRange, _maxFare, _minFare, _nightShiftRate, _nightShiftEnd, _nightShiftMultiplier, _nightShiftStart, _selectedQuotes, _specialLocationTag, _contents, _toLocation, _lat, _lon, _otpCode, _list, _fareProductType, _stopLocation, _toLocation)
import Components.Banner as Banner
import Components.MessagingView as MessagingView
import Components.MessagingView.Controller as MessagingView
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
import Components.LocationTagBarV2 as LocationTagBarV2Controller
import Components.MenuButton as MenuButton
import Components.MenuButton as MenuButton
import Components.RideCompletedCard.Controller as RideCompletedCard
import Components.MenuButton.Controller (Action(..)) as MenuButtonController
import Components.PopUpModal.Controller as PopUpModal
import Components.PricingTutorialModel.Controller as PricingTutorialModelController
import Components.PrimaryButton.Controller as PrimaryButtonController
import Components.TipsView as TipsView
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
import Components.Referral as ReferralComponent
import Constants (defaultDensity, languageKey)
import Control.Monad.Except.Trans (runExceptT)
import Control.Monad.Except (runExcept)
import Control.Monad.Trans.Class (lift)
import Constants.Configs (getPolylineAnimationConfig)
import Helpers.Ride
import Control.Transformers.Back.Trans (runBackT)
import Effect (Effect)
import Effect.Aff (launchAff)
import Effect.Unsafe (unsafePerformEffect)
import Effect.Uncurried (runEffectFn1, runEffectFn9, runEffectFn2)
import Engineering.Helpers.Commons
import Engineering.Helpers.Events as Events
import Engineering.Helpers.LogEvent (logEvent, logEventWithTwoParams, logEventWithMultipleParams)
import Engineering.Helpers.Suggestions (getMessageFromKey, getSuggestionsfromKey, emChatSuggestion, chatSuggestion)
import Foreign (unsafeToForeign)
import Foreign.Class (encode)

import Language.Strings (getString, getVarString)
import Language.Types (STR(..))
import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, printLog, trackAppTextInput, trackAppScreenEvent, logInfo, logStatus)
import MerchantConfig.Utils (Merchant(..), getMerchant)
import Prelude (class Applicative, class Show, Unit, Ordering, bind, compare, discard, map, negate, pure, show, unit, not, ($), (&&), (-), (/=), (<>), (==), (>), (||), (>=), void, (<), (*), (<=), (/), (+), when, (<<<), (*>))
import Control.Monad (unless)
import Presto.Core.Types.API (ErrorResponse)
import PrestoDOM (BottomSheetState(..), Eval, update, ScrollState(..), Visibility(..), continue, continueWithCmd, defaultPerformLog, exit, payload, updateAndExit, updateWithCmdAndExit)
import PrestoDOM.Types.Core (class Loggable)
import Resources.Constants (encodeAddress, getAddressFromBooking, decodeAddress, cancelReasons, dummyCancelReason,  emergencyContactInitialChatSuggestionId, DecodeAddress(..))
import Constants (defaultDensity)
import Screens (ScreenName(..), getScreen)
import Screens.AddNewAddressScreen.Controller (validTag, getSavedTagsFromHome)
import Screens.HomeScreen.ScreenData (dummyAddress, dummyQuoteAPIEntity, dummyZoneType)
import Screens.HomeScreen.ScreenData as HomeScreenData
import Screens.HomeScreen.Transformer (dummyRideAPIEntity, getDriverInfo, getEstimateList, getQuoteList, getQuotesTransformer, transformContactList, getNearByDrivers, dummyEstimateEntity, filterSpecialZoneAndInterCityQuotes, getFareProductType, extractFareProductType)
import Screens.RideBookingFlow.HomeScreen.Config
import Screens.SuccessScreen.Handler as UI
import Screens.Types (CallType(..), CardType(..), CurrentLocationDetails, CurrentLocationDetailsWithDistance(..), HomeScreenState, LocationItemType(..), LocationListItemState, PopupType(..), RatingCard, SearchLocationModelType(..), SearchResultType(..), SheetState(..), SpecialTags, Stage(..), TipViewStage(..), ZoneType(..), Trip, BottomNavBarIcon(..), City(..), ReferralStatus(..), NewContacts(..), City(..), CancelSearchType(..))
import Services.API (BookingLocationAPIEntity(..), EstimateAPIEntity(..), FareRange, GetDriverLocationResp, GetQuotesRes(..), GetRouteResp, LatLong(..), OfferRes, PlaceName(..), QuoteAPIEntity(..), RideBookingRes(..), SelectListRes(..), GetEditLocResultResp(..), BookingUpdateRequestDetails(..),  SelectedQuotes(..), RideBookingAPIDetails(..), GetPlaceNameResp(..), RideBookingListRes(..), FollowRideRes(..), Followers(..), Route(..), RideAPIEntity(..))
import Services.Backend as Remote
import Services.Config (getDriverNumber, getSupportNumber)
import Storage (KeyStore(..), isLocalStageOn, updateLocalStage, getValueToLocalStore, setValueToLocalStore, getValueToLocalNativeStore, setValueToLocalNativeStore, deleteValueFromLocalStore)
import Control.Monad.Trans.Class (lift)
import Presto.Core.Types.Language.Flow (doAff)
import Effect.Class (liftEffect)
import Screens.HomeScreen.ScreenData as HomeScreenData
import Types.App (defaultGlobalState)
import Screens.RideBookingFlow.HomeScreen.Config (reportIssueOptions, metersToKm, safetyIssueOptions)
import Screens.Types (TipViewData(..) , TipViewProps(..), RateCardDetails, PermissionScreenStage(..), SuggestionsMap(..), SosBannerType(..), ReferralType(..), ReferralStage(..))
import Screens.Types as ST
import Engineering.Helpers.Suggestions (getMessageFromKey, getSuggestionsfromKey)
import PrestoDOM.Properties (sheetState) as PP
import Screens.RideBookingFlow.HomeScreen.Config(reportIssueOptions)
import Data.Function (const)
import Data.List ((:))
import Common.Resources.Constants (zoomLevel, pickupZoomLevel)
import Screens.RideBookingFlow.HomeScreen.Config
import Data.Function.Uncurried
import Data.Function.Uncurried (Fn3, runFn3, Fn1, runFn1)
import Screens.NammaSafetyFlow.Components.ContactCircle as ContactCircle
import Timers (clearTimerWithId)
import Mobility.Prelude (boolToInt, toBool)
import SuggestionUtils
import Data.Tuple (Tuple(..))
import PrestoDOM.Core (getPushFn)
import Components.BannerCarousel as BannerCarousel
import PrestoDOM.List
import PrestoDOM.Core
import Locale.Utils (getLanguageLocale)
import RemoteConfig as RC
import Screens.RideBookingFlow.HomeScreen.BannerConfig
import Components.PopupWithCheckbox.Controller as PopupWithCheckboxController
import LocalStorage.Cache (getValueFromCache, setValueToCache, getFromCache, setInCache, removeValueFromCache)
import DecodeUtil (getAnyFromWindow, stringifyJSON, decodeForeignAny, parseJSON, decodeForeignAnyImpl)
import JBridge as JB
import Helpers.SpecialZoneAndHotSpots (zoneLabelIcon,getSpecialTag)
import Engineering.Helpers.Utils as EHU
import Engineering.Helpers.Commons as EHC
import Components.ServiceTierCard.View as ServiceTierCard
import Components.ProviderModel as PM
import Common.Types.App as CTP
import Helpers.TipConfig
import Data.Maybe
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import PrestoDOM.Types.Core (class Loggable)

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
  | NotificationHandler String HomeScreenState
  | GetSelectList HomeScreenState
  | RideConfirmed HomeScreenState
  | SelectEstimate HomeScreenState
  | LocationSelected LocationListItemState Boolean HomeScreenState
  | EditDestLocationSelected LocationListItemState Boolean HomeScreenState
  | EditDestinationSoft HomeScreenState
  | SearchPlace String HomeScreenState
  | UpdateLocationName HomeScreenState Number Number
  | UpdatePickupName HomeScreenState Number Number
  | GoToHome HomeScreenState
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
  | RetryFindingQuotes Boolean HomeScreenState
  | RideDetailsScreen HomeScreenState
  | GoToTicketBookingFlow HomeScreenState
  | GoToMyTickets HomeScreenState
  | RepeatTrip HomeScreenState Trip
  | ExitToTicketing HomeScreenState
  | GoToHelpAndSupport HomeScreenState
  | ReAllocateRide HomeScreenState
  | GoToRentalsFlow HomeScreenState
  | GoToScheduledRides
  | Add_Stop HomeScreenState
  | SafetySupport HomeScreenState Boolean
  | GoToShareRide HomeScreenState
  | GoToNotifyRideShare HomeScreenState
  | ExitToFollowRide HomeScreenState
  | GoToReportSafetyIssue HomeScreenState
  | GoToMyMetroTickets HomeScreenState
  | GoToMetroTicketBookingFlow HomeScreenState
  | GoToSafetyEducation HomeScreenState
  | RepeatSearch HomeScreenState
  | ChangeVehicleVarient HomeScreenState
  | ExitToConfirmingLocationStage HomeScreenState
  | UpdateReferralCode HomeScreenState String
  | GoToSafetySettingScreen 
  | GoToRideRelatedIssues HomeScreenState
  | Go_To_Search_Location_Flow HomeScreenState Boolean
  | RideSearchSO
  | ConfirmRentalRideSO HomeScreenState
  | StayInHomeScreenSO HomeScreenState
  | GoToIssueReportChatScreenWithIssue HomeScreenState CTP.CustomerIssueTypes
  | ReloadFlowStatus HomeScreenState
  | ExitToPickupInstructions HomeScreenState Number Number String String
  | EditDestLocSelected HomeScreenState
  | EditDestBackPressed
  | ExitAndEnterHomeScreen HomeScreenState

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
  | NotificationListener String
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
  | UpdateCurrentStage String RideBookingRes
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
  | ChooseYourRideAction ChooseYourRideController.Action
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
  | UpdateChatWithEM Boolean
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


instance showAction :: Show Action where show _ = ""
instance loggableAction :: Loggable Action where
  performLog = defaultPerformLog