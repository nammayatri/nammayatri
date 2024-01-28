{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.Types where

import MerchantConfig.Types

import Common.Types.App (CountryCodeObj, OTPChannel, OptionButtonList, RateCardType, FeedbackAnswer, CarouselModal, CategoryListType)
import Common.Types.App as Common
import Components.ChatView.Controller (ChatComponentConfig, Config)
import Components.ChooseVehicle.Controller as ChooseVehicle
import Components.SettingSideBar.Controller (SettingSideBarState)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import Foreign (Foreign)
import Foreign.Class (class Decode, class Encode)
import Foreign.Object (Object)
import Halogen.VDom.DOM.Prop (PropValue)
import Prelude (class Eq, class Show)
import Presto.Core.Utils.Encoding (defaultEnumDecode, defaultEnumEncode, defaultDecode, defaultEncode)
import PrestoDOM (LetterSpacing, BottomSheetState(..), Visibility(..))
import Services.API (AddressComponents, BookingLocationAPIEntity, EstimateAPIEntity(..), QuoteAPIEntity, TicketPlaceResp, RideBookingRes, Route, BookingStatus(..), LatLong(..), PlaceType(..), ServiceExpiry(..), Chat, MetroTicketBookingStatus(..))
import Components.SettingSideBar.Controller as SideBar
import Components.MessagingView.Controller (ChatComponent)
import Screens(ScreenName)

type Contacts = {
  name :: String,
  number :: String
}

type NewContacts = {
  name :: String,
  number :: String,
  isSelected :: Boolean
}

type NewContactsProp = {
  name :: PropValue,
  number :: PropValue,
  isSelected :: PropValue,
  contactBackgroundColor :: PropValue,
  isSelectImage :: PropValue,
  visibilitySelectedImage :: PropValue,
  visibilityUnSelectedImage :: PropValue
}

type EditTextInLabelState =
 {
    topLabel :: String
  , showTopLabel :: Boolean
  , inLabel :: String
  , showInLabel :: Boolean
  , valueId :: String
  , hint :: String
  , pattern :: Maybe String
  , id :: String
  }

type LanguageItemState =
 {
    key :: String
  , language :: String
  , selected :: Boolean
  }

type NotificationItemState = {
    color :: String
  , color1 :: String
  , icon :: String
  , title :: String
  , description :: String
}

type PrimarySelectItemState =
 {
    label :: String
  , placeholder :: String
  , selectedItem :: String
  , screenName :: String
  }

type EditTextState =
 {
    title :: String
  , hint :: String
  }

type EditTextInImageState =
 {
    topLabel :: String
  , showTopLabel :: Boolean
  , inImage :: String
  , showInImage :: Boolean
  , hint :: String
  }

type DateEditTextState =
 {
    label :: String
  , hint :: String
  , id :: String
  , value :: String
  }

type SplashScreenState =  {
   data :: SplashScreenData
 }

type SplashScreenData =  {
   message :: String
 }

-- ############################################################# WelcomeScreen ################################################################################




type WelcomeScreenState = {
  data :: WelcomeScreenData
}

type WelcomeScreenData = {
  carouselModal :: CarouselModal,
  logField :: Object Foreign
}

type StepsHeaderModelState = {
  activeIndex :: Int,
  textArray :: Array String,
  backArrowVisibility :: Boolean
, config :: AppConfig
}

-- ############################################################# ChooseLanguageScreen ################################################################################

type ChooseLanguageScreenState = {
  data :: ChooseLanguageScreenData,
  props :: ChooseLanguageScreenProps
}

type ChooseLanguageScreenData =  {
  isSelected :: Boolean,
  config :: AppConfig
 }

type ChooseLanguageScreenProps =  {
  selectedLanguage :: String,
  btnActive :: Boolean,
  exitAnimation :: Boolean
 }


-- ################################################ EnterMobileNumberScreenState #############################################
type EnterMobileNumberScreenState =
  {
    props :: EnterMobileNumberScreenStateProps,
    data :: EnterMobileNumberScreenStateData
  }

type EnterMobileNumberScreenStateProps = {
  enterOTP :: Boolean,
  btnActiveMobileNumber :: Boolean,
  btnActiveOTP :: Boolean,
  isValidMobileNumber :: Boolean,
  wrongOTP :: Boolean,
  resendEnable :: Boolean,
  isReadingOTP :: Boolean,
  capturedOtp :: String,
  letterSpacing :: LetterSpacing,
  mNumberEdtFocused :: Boolean,
  otpEdtFocused :: Boolean,
  editTextVal :: String,
  attemptLeft :: String,
  countryCodeOptionExpanded :: Boolean
}

type EnterMobileNumberScreenStateData = {
    mobileNumber :: String
  , countryObj :: CountryCodeObj
  , tokenId :: String
  , attempts :: Int
  , otp :: String
  , timer :: Int
  , timerID :: String
  , config :: AppConfig
  , logField :: Object Foreign
  , otpChannel :: OTPChannel
}
-- ################################################ AccountSetUpScreenState ##################################################

data Gender = MALE | FEMALE | OTHER | PREFER_NOT_TO_SAY

derive instance genericGender :: Generic Gender _
instance eqGender :: Eq Gender where eq = genericEq

type AccountSetUpScreenState =
  { props :: AccountSetUpScreenStateProps ,
    data :: AccountSetUpScreenStateData
  }


type AccountSetUpScreenStateProps =
  {   btnActive :: Boolean
    , backPressed :: Boolean
    , genderSelected :: Maybe String
    , genderOptionExpanded :: Boolean
    , expandEnabled :: Boolean
    , showOptions :: Boolean
    , activeField :: Maybe ActiveFieldAccountSetup
    , isNameValid :: Boolean
    , isSpecialAssistList :: Boolean
  }



data ActiveFieldAccountSetup = DropDown | NameSection

derive instance genericActiveFieldAccountSetup :: Generic ActiveFieldAccountSetup _
instance eqActiveFieldAccountSetup :: Eq ActiveFieldAccountSetup where eq = genericEq

type AccountSetUpScreenStateData =
  {   name :: String
    , email :: String
    , gender :: Maybe Gender
    , nameErrorMessage :: Maybe ErrorType
    , config :: AppConfig
    , disabilityOptions :: DisabilityData
  }



 -- ######################################  TripDetailsScreenState   ######################################
type TripDetailsScreenState =
  {
    data :: TripDetailsScreenData,
    props :: TripDetailsScreenProps
  }

data PaymentMode = CASH | ONLINE

derive instance genericPaymentMode :: Generic PaymentMode _
instance showPaymentMode :: Show PaymentMode where show = genericShow
instance eqPaymentMode :: Eq PaymentMode where eq = genericEq
instance encodePaymentMode :: Encode PaymentMode where encode = defaultEnumEncode
instance decodePaymentMode :: Decode PaymentMode where decode = defaultEnumDecode

type TripDetailsScreenData =
  {
    message :: String,
    driverName :: String,
    date :: String,
    time :: String,
    source :: String,
    destination :: String,
    totalAmount :: String,
    paymentMode :: PaymentMode,
    rating :: Int,
    selectedItem :: IndividualRideCardState,
    tripId :: String,
    config :: AppConfig,
    vehicleVariant :: Maybe VehicleVariant,
    categories :: Array CategoryListType
    -- bookingId :: String
  }

type TripDetailsScreenProps =
  {
    reportIssue :: Boolean,
    issueReported :: Boolean,
    activateSubmit :: Boolean,
    fromMyRides :: TripDetailsGoBackType,
    showConfirmationPopUp :: Boolean,
    canConnectWithDriver :: Boolean,
    triggerUIUpdate :: Boolean,
    showIssueOptions :: Boolean
  }

data TripDetailsGoBackType = Home | MyRides | HelpAndSupport
derive instance genericTripDetailsGoBackType :: Generic TripDetailsGoBackType _
instance showTripDetailsGoBackType :: Show TripDetailsGoBackType where show = genericShow
instance eqTripDetailsGoBackType :: Eq TripDetailsGoBackType where eq = genericEq

-- ######################################  InvoiceScreenState   ######################################

type InvoiceScreenState =
  {
    data :: InvoiceScreenData,
    props :: InvoiceScreenProps
  }

type InvoiceScreenData =
  {
    tripCharges :: String,
    promotion :: Number,
    gst :: Number,
    totalAmount :: String,
    date :: String ,
    selectedItem :: IndividualRideCardState,
    config :: AppConfig,
    logField :: Object Foreign,
    pdfHeading :: String
  }

type InvoiceScreenProps =
  {
    paymentMode :: String
  , fromHomeScreen :: Boolean
  }

-- ################################################ ContactUsScreen ##################################################

type ContactUsScreenState =
  {
    data :: ContactUsScreenData,
    props :: ContactUsScreenProps
  }

type ContactUsScreenData =
  {
    email :: String,
    subject :: String,
    description :: String,
    bookingId :: String,
    errorMessage :: Maybe ErrorType,
    config :: AppConfig,
    logField :: Object Foreign
  }

type ContactUsScreenProps =
  {
    btnActive :: Boolean,
    isSubmitted :: Boolean
  }

-- ################################################  HelpAndSuportScreenState   ################################################

type HelpAndSupportScreenState =
  {
    data :: HelpAndSupportScreenData,
    props :: HelpAndSuportScreenProps
  }

type HelpAndSupportScreenData =
  {
    date :: String,
    time :: String,
    rating :: Int,
    source :: String,
    destination :: String,
    driverName :: String,
    totalAmount :: String,
    isNull :: Boolean,
    faresList :: Array FareComponent,
    status :: String,
    rideStartTime :: String,
    rideEndTime :: String,
    rideId :: String,
    vehicleNumber :: String,
    tripId :: String,
    bookingId :: String,
    email :: String,
    description :: String,
    accountStatus :: DeleteStatus ,
    config :: AppConfig,
    vehicleVariant :: Maybe VehicleVariant,
    logField :: Object Foreign,
    issueList :: Array IssueInfo,
    ongoingIssueList :: Array IssueInfo,
    resolvedIssueList :: Array IssueInfo,
    issueListType :: IssueModalType,
    categories :: Array CategoryListType,
    merchantExoPhone :: String
  }

type RideSelectionScreenState =
  {
    shimmerLoader :: AnimationState,
    prestoListArrayItems :: Array ItemState,
    itemsRides :: Array IndividualRideCardState,
    props :: RideSelectionScreenProps,
    data :: RideSelectionScreenData,
    selectedCategory :: CategoryListType,
    selectedItem :: Maybe IndividualRideCardState
  }  

type RideSelectionScreenData = {
    offsetValue :: Int,
    loadMoreText :: Boolean,
    config :: AppConfig,
    logField :: Object Foreign,
    isSrcServiceable :: Boolean
  }

type RideSelectionScreenProps = {
  loaderButtonVisibility :: Boolean,
  loadMoreDisabled :: Boolean,
  receivedResponse :: Boolean,
  apiFailure :: Boolean,
  fromNavBar :: Boolean,
  optionsVisibility :: Boolean
}

type ReportIssueChatScreenState = {
    data :: ReportIssueChatScreenData,
    props :: ReportIssueChatScreenProps
}

type ReportIssueChatScreenData = {
  tripId :: Maybe String,
  categoryName :: String,
  messageToBeSent :: String,
  issueId :: Maybe String,
  chatConfig :: Config,
  selectedOption :: Maybe Option,
  addedImages :: Array { image :: String, imageName :: String },
  categoryId :: String,
  recordAudioState :: RecordAudioState,
  addImagesState :: AddImageState,
  viewImageState :: ViewImageState,
  recordedAudioUrl :: Maybe String,
  addAudioState :: AddAudioState,
  uploadedImagesIds :: Array String,
  uploadedAudioId :: Maybe String,
  options :: Array Option,
  chats :: Array Chat,
  showStillHaveIssue :: Boolean,
  merchantExoPhone :: Maybe String,
  selectedRide :: Maybe IndividualRideCardState,
  entryPoint :: ReportIssueChatScreenEntryPoint,
  config :: AppConfig
}
data ReportIssueChatScreenEntryPoint = TripDetailsScreenEntry | RideSelectionScreenEntry | HelpAndSupportScreenEntry | OldChatEntry
derive instance genericReportIssueChatScreenEntryPoint :: Generic ReportIssueChatScreenEntryPoint _
instance showReportIssueChatScreenEntryPoint :: Show ReportIssueChatScreenEntryPoint where show = genericShow
instance eqReportIssueChatScreenEntryPoint :: Eq ReportIssueChatScreenEntryPoint where eq = genericEq
instance encodeReportIssueChatScreenEntryPoint :: Encode ReportIssueChatScreenEntryPoint where encode = defaultEnumEncode
instance decodeReportIssueChatScreenEntryPoint :: Decode ReportIssueChatScreenEntryPoint where decode = defaultEnumDecode

type RecordAudioState = {
  timer         :: String,
  isRecording   :: Boolean,
  isUploading   :: Boolean,
  recordedFile  :: Maybe String,
  recordingDone :: Boolean,
  openAddAudioModel :: Boolean
}

type AddImageState = {
  images :: Array Image,
  stateChanged :: Boolean,
  isLoading :: Boolean,
  imageMediaIds :: Array String
}

type ViewImageState = {
   image :: String,
   imageName :: Maybe String
}

type AddAudioState = {
  audioFile :: Maybe String,
  stateChanged :: Boolean
}

type Image = {
  image :: String, 
  imageName :: String
}

type Option = { 
  issueOptionId :: String
, option :: String
, label :: String
}

type ReportIssueChatScreenProps = {
  showSubmitComp :: Boolean,
  showImageModel :: Boolean,
  showAudioModel :: Boolean,
  showRecordModel :: Boolean,
  showCallDriverModel :: Boolean,
  showCallSupportModel :: Boolean,
  showViewImageModel :: Boolean,
  isPopupModelOpen :: Boolean,
  isKeyboardOpen :: Boolean, 
  timerId :: String, 
  initalizedCallbacks :: Boolean,
  isResolved :: Boolean,
  isEndFlow :: Boolean
}

type IssueInfo = {
    issueReportId :: String,
    status :: String,
    category :: String,
    createdAt :: String
}

type HelpAndSuportScreenProps =
  {
    apiFailure :: Boolean
  , isCallConfirmation :: Boolean
  , showDeleteAccountView :: Boolean
  , btnActive :: Boolean
  , needIssueListApiCall :: Boolean
  }
-- ################################################ MyRidesScreenState ##################################################
data AnimationState
  = AnimatedIn
  | AnimatingIn
  | AnimatingOut
  | AnimatedOut

derive instance genericAnimationState :: Generic AnimationState _
instance showAnimationState :: Show AnimationState where show = genericShow
instance eqAnimationState :: Eq AnimationState where eq = genericEq
instance encodeAnimationState :: Encode AnimationState where encode = defaultEnumEncode
instance decodeAnimationState :: Decode AnimationState where decode = defaultEnumDecode

type MyRidesScreenState =
  {
    shimmerLoader :: AnimationState,
    prestoListArrayItems :: Array ItemState,
    itemsRides :: Array IndividualRideCardState,
    props :: MyRideScreenProps,
    data :: MyRideScreenData
  }

type MyRideScreenData = {
    selectedItem :: IndividualRideCardState,
    offsetValue :: Int,
    loadMoreText :: Boolean,
    config :: AppConfig,
    logField :: Object Foreign,
    isSrcServiceable :: Boolean
  }

type MyRideScreenProps = {
  loaderButtonVisibility :: Boolean,
  loadMoreDisabled :: Boolean,
  receivedResponse :: Boolean,
  apiFailure :: Boolean,
  fromNavBar :: Boolean,
  optionsVisibility :: Boolean
}
-- ################################################ IndividualRideCardState ##################################################

type IndividualRideCardState =
  {
    date :: String,
    time :: String,
    source :: String,
    destination :: String,
    totalAmount :: String,
    cardVisibility :: String,
    shimmerVisibility :: String,
    driverImage :: String,
    isCancelled :: String,
    isSuccessfull :: String,
    rating :: Int,
    driverName :: String,
    rideStartTime :: String,
    rideEndTime :: String,
    vehicleNumber :: String,
    rideId :: String,
    status :: String,
    shortRideId :: String,
    bookingId :: String,
    rideEndTimeUTC :: String,
    sourceLocation :: BookingLocationAPIEntity,
    destinationLocation :: BookingLocationAPIEntity,
    alpha :: String,
    fareBreakUpList :: Fares, -- Added only For Backward Compatibility
    faresList :: Array FareComponent ,
    baseFare :: String -- Added only For Backward Compatibility
  , pickupCharges :: String
  , extraFare :: String
  , waitingCharges :: String
  , baseDistance :: String
  , extraDistance :: String
  , referenceString :: String
  , isSpecialZone :: Boolean
  , nightCharges :: Boolean
  , zoneType :: ZoneType
  , vehicleVariant :: Maybe VehicleVariant
  , isSrcServiceable :: Boolean
  , optionsVisibility :: Boolean
  , merchantExoPhone :: String
  }


data VehicleVariant = SUV | SEDAN | HATCHBACK | AUTO_RICKSHAW | TAXI | TAXI_PLUS

derive instance genericVehicleVariant :: Generic VehicleVariant _
instance eqVehicleVariant :: Eq VehicleVariant where eq = genericEq
instance showVehicleVariant :: Show VehicleVariant where show = genericShow

type ItemState =
  {
    date :: PropValue,
    time :: PropValue,
    source :: PropValue,
    destination :: PropValue,
    totalAmount :: PropValue,
    cardVisibility :: PropValue,
    shimmerVisibility :: PropValue,
    driverImage :: PropValue,
    isCancelled :: PropValue,
    isSuccessfull :: PropValue,
    rating :: PropValue,
    driverName :: PropValue,
    rideStartTime :: PropValue,
    rideEndTime :: PropValue,
    vehicleNumber :: PropValue,
    rideId :: PropValue,
    status :: PropValue,
    rideEndTimeUTC :: PropValue,
    alpha :: PropValue,
    zoneVisibility :: PropValue
  }

-- ################################################ PermissionScreenState ##################################################

type PermissionScreenState = {
    appConfig :: AppConfig,
    logField :: Object Foreign,
    stage :: PermissionScreenStage
}

data PermissionScreenStage = NORMAL | LOCATION_DISABLED | INTERNET_ACTION | LOCATION_DENIED 
derive instance genericPermissionScreenStage :: Generic PermissionScreenStage _
instance eqPermissionScreenStage :: Eq PermissionScreenStage where eq = genericEq
instance showPermissionScreenStage :: Show PermissionScreenStage where show = genericShow
-- ######################################  HomeScreenState   ######################################

data Stage = HomeScreen
           | SettingPrice
           | FindingEstimate
           | ConfirmingRide
           | RideAccepted
           | ReAllocated
           | RideStarted
           | RideCompleted
           | PricingTutorial
           | SearchLocationModel
           | FindingQuotes
           | QuoteList
           | PreviousRating
           | ConfirmingLocation
           | DistanceOutsideLimits
           | ShortDistance
           | TryAgain
           | RideRating
           | FavouriteLocationModel
           | ChatWithDriver
           | FindEstimateAndSearch
           | RetryFindingQuote
           | PickUpFarFromCurrentLocation
           | LoadMap

derive instance genericStage :: Generic Stage _
instance eqStage :: Eq Stage where eq = genericEq
instance showStage :: Show Stage where show = genericShow

data SearchLocationModelType = SearchLocation | LocateOnMap | NoView

data PopupType = Logout | ConfirmBack | NoPopUp | ActiveQuotePopUp | TipsPopUp

derive instance genericPopupType :: Generic PopupType _
instance eqPopupType :: Eq PopupType where eq = genericEq

derive instance genericSearchLocationModelType :: Generic SearchLocationModelType _
instance eqSearchLocationModelType :: Eq SearchLocationModelType where eq = genericEq

type HomeScreenState =
  {
    data :: HomeScreenStateData,
    props :: HomeScreenStateProps
  }

type HomeScreenStateData =
  {
    suggestedAmount :: Int
  , currentSearchResultType :: SearchResultType
  , finalAmount :: Int
  , startedAt :: String
  , endedAt :: String
  , source :: String
  , destination :: String
  , eta :: String
  , vehicleDetails :: String
  , registrationNumber :: String
  , rating :: Number
  , locationList :: Array LocationListItemState
  , savedLocations :: Array LocationListItemState
  , recentSearchs :: RecentlySearchedObject
  , destinationSuggestions :: Array LocationListItemState
  , tripSuggestions :: Array Trip
  , selectList :: Array QuoteAPIEntity
  , quoteListModelState :: Array QuoteListItemState
  , driverInfoCardState :: DriverInfoCard
  , rideRatingState :: RatingCard
  , settingSideBar :: SettingSideBarState
  , sourceAddress :: Address
  , destinationAddress :: Address
  , route :: Maybe Route
  , startedAtUTC :: String
  , rateCard :: RateCard
  , speed :: Int
  , selectedLocationListItem :: Maybe LocationListItemState
  , saveFavouriteCard :: SaveFavouriteCardState
  , rideDistance :: String
  , rideDuration :: String
  , showPreferences :: Boolean
  , previousCurrentLocations:: PreviousCurrentLocations
  , messages :: Array ChatComponentConfig
  , messagesSize :: String
  , chatSuggestionsList :: Array String
  , messageToBeSent :: String
  , nearByPickUpPoints :: Array Location
  , polygonCoordinates :: String
  , specialZoneQuoteList :: Array ChooseVehicle.Config
  , specialZoneSelectedQuote :: Maybe String
  , specialZoneSelectedVariant :: Maybe String
  , selectedEstimatesObject :: ChooseVehicle.Config
  , lastMessage :: ChatComponentConfig
  , cancelRideConfirmationData :: CancelRideConfirmationData
  , pickUpCharges :: Int
  , ratingViewState :: RatingViewState
  , config :: AppConfig
  , logField :: Object Foreign
  , nearByDrivers :: Maybe Int
  , disability :: Maybe DisabilityT
  , searchLocationModelData :: SearchLocationModelData
  , waitTimeInfo :: Boolean
  , lastSentMessage :: ChatComponent
  , lastReceivedMessage :: ChatComponent
  , triggerPatchCounter :: Int
  , infoCardPeekHeight :: Int
  , suggestionsData :: SuggestionsData
  , peekHeight :: Int
  , rideHistoryTrip :: Maybe Trip
  }

type QuoteListItemState = 
  {
    seconds :: Int
  , id :: String  
  , timer :: String
  , timeLeft :: Int
  , driverRating :: Number
  , profile :: String
  , price :: String
  , vehicleType :: String
  , driverName :: String
  , selectedQuote :: Maybe String
  , appConfig :: AppConfig
  , city :: City
  }


type LocationDetails = {
    formattedAddress :: String,
    location :: LatLong,
    plusCode :: Maybe String,
    addressComponents :: Array AddressComponents,
    placeId :: Maybe String
  }


type DisabilityT = 
  {
    id :: String
  , tag :: String
  , description :: String
  }

type DisabilityData = {
    activeIndex :: Int
  , specialAssistActiveIndex :: Int
  , disabilityOptionList :: Array DisabilityT
  , selectedDisability :: Maybe DisabilityT
  , otherDisabilityReason :: Maybe String 
  , editedDisabilityReason :: String
}

type HomeScreenStateProps =
  {
    currentStage :: Stage
  , showCallPopUp :: Boolean
  , rideRequestFlow :: Boolean
  , isSearchLocation :: SearchLocationModelType
  , isSource :: Maybe Boolean
  , sourceLat :: Number
  , sourceLong :: Number
  , destinationLat :: Number
  , destinationLong :: Number
  , sourcePlaceId :: Maybe String
  , destinationPlaceId :: Maybe String
  , estimateId :: String
  , selectedQuote :: Maybe String
  , locationRequestCount :: Int
  , searchId :: String
  , bookingId :: String
  , customerTip :: CustomerTipProps
  , expiredQuotes :: Array String
  , isCancelRide :: Boolean
  , cancellationReasons :: Array OptionButtonList
  , cancelRideActiveIndex :: Maybe Int
  , cancelDescription :: String
  , cancelReasonCode :: String
  , isPopUp :: PopupType
  , forFirst :: Boolean
  , callbackInitiated :: Boolean
  , isLocationTracking :: Boolean
  , isInApp :: Boolean
  , locateOnMap :: Boolean
  , sourceSelectedOnMap :: Boolean
  , distance :: Int
  , isSrcServiceable :: Boolean
  , isDestServiceable :: Boolean
  , isRideServiceable :: Boolean
  , showlocUnserviceablePopUp :: Boolean
  , isMockLocation :: Boolean
  , autoSelecting :: Boolean
  , searchExpire :: Int
  , isEstimateChanged :: Boolean
  , showRateCard :: Boolean
  , showRateCardIcon :: Boolean
  , emergencyHelpModal :: Boolean
  , sendMessageActive :: Boolean
  , chatcallbackInitiated :: Boolean
  , estimatedDistance :: Maybe Int
  , waitingTimeTimerIds :: Array String
  , tagType :: Maybe CardType
  , isSaveFavourite :: Boolean
  , showShareAppPopUp :: Boolean
  , showMultipleRideInfo :: Boolean
  , hasTakenRide :: Boolean
  , isReferred :: Boolean
  , storeCurrentLocs :: Boolean
  , unReadMessages :: Boolean
  , openChatScreen :: Boolean
  , emergencyHelpModelState :: EmergencyHelpModelState
  , showLiveDashboard :: Boolean
  , isBanner :: Boolean
  , callSupportPopUp :: Boolean
  , defaultPickUpPoint :: String
  , isSpecialZone :: Boolean
  , showChatNotification :: Boolean
  , cancelSearchCallDriver :: Boolean
  , zoneType :: SpecialTags
  , cancelRideConfirmationPopup :: Boolean
  , searchAfterEstimate :: Boolean
  , tipViewProps :: TipViewProps
  , timerId :: String
  , findingRidesAgain :: Boolean
  , routeEndPoints :: Maybe RouteEndPoints
  , findingQuotesProgress :: Number
  , confirmLocationCategory :: String
  , zoneTimerExpired :: Boolean
  , canSendSuggestion :: Boolean
  , sheetState :: BottomSheetState
  , showOfferedAssistancePopUp :: Boolean
  , showDisabilityPopUp :: Boolean
  , isChatNotificationDismissed :: Boolean
  , searchLocationModelProps :: SearchLocationModelProps
  , flowWithoutOffers :: Boolean
  , showEducationalCarousel :: Boolean
  , locateOnMapLocation :: LocateOnMapLocation
  , specialZoneType :: String
  , currentLocation :: Location
  , isShorterTrip :: Boolean
  , isNotificationExpanded :: Boolean
  , bottomSheetState :: SheetState
  , removeNotification :: Boolean
  , city :: City
  , isHomescreenExpanded :: Boolean
  , isRepeatRide :: Boolean
  , currSlideIndex :: Number
  , suggestionsListExpanded :: Boolean
  , repeatRideTimer :: String
  , repeatRideTimerId :: String
  , showShimmer :: Boolean
  , nightSafetyFlow :: Boolean 
  , reAllocation :: ReAllocationProp
  , homeScreenSheetState :: BottomSheetState
  , autoScrollTimer :: String
  , autoScrollTimerId :: String
  , autoScroll :: Boolean
  , enableChatWidget :: Boolean
  }

data City
  = Bangalore
  | Kolkata
  | Paris
  | Kochi
  | Delhi
  | Hyderabad
  | Mumbai
  | Chennai
  | Coimbatore
  | Pondicherry
  | Goa
  | Pune
  | Mysore
  | Tumakuru
  | AnyCity

derive instance genericCity :: Generic City _
instance showCity :: Show City where show = genericShow
instance eqCity :: Eq City where eq = genericEq
instance encodeCity :: Encode City where encode = defaultEnumEncode
instance decodeCity :: Decode City where decode = defaultEnumDecode

type SearchLocationModelProps = {
    isAutoComplete :: Boolean
  , showLoader :: Boolean
  , crossBtnSrcVisibility :: Boolean
  , crossBtnDestVisibility :: Boolean
  , findPlaceIllustration :: Boolean
}

type SearchLocationModelData = {
    prevLocation :: String
}

type LocateOnMapLocation = {
    source :: String
  , sourceAddress :: Address
  , sourceLat :: Number
  , sourceLng :: Number
  , destination :: String
  , destinationAddress :: Address
  , destinationLat :: Number
  , destinationLng :: Number
}

type RouteEndPoints = {
    source :: Location
  , destination :: Location
}

type SpecialTags = {
    sourceTag :: ZoneType
  , destinationTag :: ZoneType
  , priorityTag :: ZoneType
}

type CancelRideConfirmationData = {
  delayInSeconds :: Int,
  timerID :: String,
  enableTimer :: Boolean,
  continueEnabled :: Boolean
}
type RatingViewState = {
    selectedYesNoButton :: Int,
    selectedRating :: Int,
    issueReportActiveIndex :: Maybe Int,
    issueReasonCode :: Maybe String,
    openReportIssue :: Boolean,
    issueFacedView :: Boolean,
    doneButtonVisibility :: Boolean,
    issueReason :: Maybe String,
    issueDescription :: String,
    rideBookingRes :: RideBookingRes,
    wasOfferedAssistance :: Maybe Boolean
}

type CustomerTipProps = {
    enableTips :: Boolean
  , tipActiveIndex :: Int
  , tipForDriver :: Int
  , isTipSelected :: Boolean
}

data SheetState = STATE_DRAGGING | STATE_SETTLING | STATE_EXPANDED | STATE_COLLAPSED | STATE_HIDDEN | STATE_HALF_EXPANDED

derive instance genericSheetState :: Generic SheetState _
instance showSheetState :: Show SheetState where show = genericShow
instance eqSheetState :: Eq SheetState where eq = genericEq
instance encodeSheetState :: Encode SheetState where encode = defaultEnumEncode
instance decodeSheetState :: Decode SheetState where decode = defaultEnumDecode

data TipViewStage = DEFAULT | TIP_AMOUNT_SELECTED | TIP_ADDED_TO_SEARCH | RETRY_SEARCH_WITH_TIP

derive instance genericTipViewStage :: Generic TipViewStage _
instance showTipViewStage :: Show TipViewStage where show = genericShow
instance eqTipViewStage :: Eq TipViewStage where eq = genericEq
instance encodeTipViewStage :: Encode TipViewStage where encode = defaultEncode
instance decodeTipViewStage :: Decode TipViewStage where decode = defaultDecode

type TipViewProps = {
    stage :: TipViewStage
  , isVisible :: Boolean
  , onlyPrimaryText :: Boolean
  , isprimaryButtonVisible :: Boolean
  , primaryText :: String
  , secondaryText :: String
  , customerTipArray :: Array String
  , customerTipArrayWithValues :: Array Int
  , activeIndex :: Int
  , primaryButtonText :: String
}

type Contact = {
     name :: String,
     phoneNo :: String
}

type RateCard =
  {
    baseFare :: Int,
    extraFare :: Int,
    pickUpCharges :: Int,
    additionalFare :: Int,
    nightShiftMultiplier :: Number,
    nightCharges :: Boolean,
    currentRateCardType :: RateCardType,
    onFirstPage :: Boolean,
    vehicleVariant :: String
  }

type RateCardDetails = {
  title :: String ,
  description :: String
}

type EmergencyHelpModelState = {
   currentlySelectedContact :: Contact,
   showCallSuccessfulPopUp :: Boolean,
   showCallContactPopUp :: Boolean,
   sosId :: String,
   sosStatus :: String,
   isSelectEmergencyContact :: Boolean,
   showContactSupportPopUp :: Boolean,
   showCallPolicePopUp :: Boolean,
   emergencyContactData :: Array Contact,
   waitingDialerCallback :: Boolean
}

type RecentlySearchedObject =
  {
    predictionArray :: Array LocationListItemState
  }

type ReferralScreenState =
  {
      referralCode :: String
    , btnActive :: Boolean
    , showThanks :: Boolean
    , isInvalidCode :: Boolean
    , isExpandReference :: Boolean
    , config :: AppConfig
    , logField :: Object Foreign
  }

type EstimateInfo = {
  additionalFare :: Int,
  estimatedPrice :: Int, 
  quoteList :: Array ChooseVehicle.Config,
  defaultQuote :: ChooseVehicle.Config,
  estimateId :: String,
  estimatedVarient :: Array EstimateAPIEntity,
  pickUpCharges :: Int,
  nightShiftMultiplier :: Number,
  nightCharges :: Boolean,
  baseFare :: Int,
  extraFare :: Int,
  showRateCardIcon :: Boolean,
  zoneType :: SpecialTags
}

-- ################################## SelectLanguageScreenState ###############################

type SelectLanguageScreenState = {
  data :: SelectLanguageScreenData,
  props :: SelectLanguageScreenProps
}

type SelectLanguageScreenData =  {
  isSelected :: Boolean,
  config :: AppConfig
 }

type SelectLanguageScreenProps =  {
  selectedLanguage :: String,
  btnActive :: Boolean
 }

-- ############################################## EmergencyContactsScreenState #############################


type EmergencyContactsScreenState = {
  data :: EmergencyContactsScreenData,
  props :: EmergencyContactsScreenProps
}

type EmergencyContactsScreenData = {
  contactInfoState :: Array Contacts,
  contactsCount :: Int,
  contactsList :: Array NewContacts,
  contactsNewList :: Array NewContacts,
  contactsUpdatedNewList :: Array NewContacts,
  prestoListArrayItems :: Array NewContactsProp,
  loadMoreDisabled :: Boolean,
  removedContactDetail :: NewContacts,
  offsetForEmergencyContacts :: Int,
  limitForEmergencyContacts :: Int,
  editedText :: String,
  logField :: Object Foreign
}

type EmergencyContactsScreenProps = {
  showContactList :: Boolean,
  showInfoPopUp :: Boolean
}

type ContactDetail = {
  name :: String,
  phoneNumber :: String
}

-- ############################################## AboutUsScreenState #############################

type AboutUsScreenState = {
    appConfig :: AppConfig
}

-- ############################################## MyProfileScreenState #############################

type MyProfileScreenState = {
  props :: MyProfileScreenProps,
  data :: MyProfileScreenData
}
data DeleteStatus = CONFIRM_REQ | DEL_REQUESTED | ACTIVE

derive instance genericDeleteStatus :: Generic DeleteStatus _
instance showDeleteStatus :: Show DeleteStatus where show = genericShow
instance eqDeleteStatus :: Eq DeleteStatus where eq = genericEq
instance encodeDeleteStatus :: Encode DeleteStatus where encode = defaultEnumEncode
instance decodeDeleteStatus :: Decode DeleteStatus where decode = defaultEnumDecode


type MyProfileScreenProps = {
  updateProfile :: Boolean,
  genderOptionExpanded :: Boolean,
  expandEnabled :: Boolean,
  isEmailValid :: Boolean,
  isNameValid :: Boolean,
  isBtnEnabled :: Boolean,
  showOptions :: Boolean,
  fromHomeScreen :: Boolean,
  showAccessibilityPopUp :: Boolean,
  changeAccessibility :: Boolean,
  isSpecialAssistList :: Boolean
}

data FieldType = NAME | EMAILID_ | GENDER_ | MOBILE | DISABILITY_TYPE

derive instance genericFieldType :: Generic FieldType _
instance eqFieldType :: Eq FieldType where eq = genericEq

type MyProfileScreenData = {
  name :: String,
  mobileNumber :: String,
  editedName :: String,
  emailId :: Maybe String,
  gender :: Maybe Gender,
  editedEmailId :: Maybe String,
  editedGender :: Maybe Gender,
  emailErrorMessage :: Maybe ErrorType,
  nameErrorMessage :: Maybe ErrorType,
  config :: AppConfig,
  logField :: Object Foreign,
  disabilityType :: Maybe DisabilityT,
  disabilityOptions :: DisabilityData,
  editedDisabilityOptions :: DisabilityData,
  hasDisability :: Maybe Boolean
}

data ErrorType = INVALID_EMAIL | EMAIL_EXISTS | EMAIL_CANNOT_BE_BLANK | INVALID_NAME | NAME_CANNOT_BE_BLANK


derive instance genericErrorType :: Generic ErrorType _
instance eqErrorType :: Eq ErrorType where eq = genericEq

type Location = {
  place :: String,
  lat :: Number,
  lng :: Number,
  address :: Maybe String,
  city :: Maybe String
}

type DriverInfoCard =
  { otp :: String
  , driverName :: String
  , currentSearchResultType :: SearchResultType
  , eta :: Maybe Int
  , vehicleDetails :: String
  , registrationNumber :: String
  , rating :: Number
  , startedAt :: String
  , endedAt :: String
  , source :: String
  , destination :: String
  , rideId :: String
  , price :: Int
  , sourceLat :: Number
  , sourceLng :: Number
  , destinationLat :: Number
  , destinationLng :: Number
  , driverLat :: Number
  , driverLng :: Number
  , distance :: Int
  , waitingTime :: String
  , driverArrived :: Boolean
  , estimatedDistance :: String
  , driverArrivalTime :: Int
  , bppRideId :: String
  , driverNumber :: Maybe String
  , merchantExoPhone :: String
  , createdAt :: String
  , initDistance :: Maybe Int
  , config :: AppConfig
  , vehicleVariant :: String
  , sourceAddress :: Address
  , destinationAddress :: Address
  }

type RatingCard =
  {
    rideId :: String
  , rating :: Int
  , driverName :: String
  , finalAmount :: Int
  , rideStartTime :: String
  , rideEndTime :: String
  , source :: String
  , destination :: String
  , rideStartDate :: String
  , vehicleNumber :: String
  , status :: String
  , shortRideId :: String
  , bookingId :: String
  , rideEndTimeUTC :: String
  , dateDDMMYY :: String
  , offeredFare :: Int
  , distanceDifference :: Int
  , feedback :: String
  , feedbackList :: Array FeedbackAnswer
  }

type Address =
  { area :: Maybe String
  , state :: Maybe String
  , country :: Maybe String
  , building  :: Maybe String
  , door :: Maybe String
  , street :: Maybe String
  , city :: Maybe String
  , areaCode :: Maybe String
  , ward :: Maybe String
  , placeId :: Maybe String
  }


type SavedLocationScreenState =
  {
      data :: SavedLocationScreenData
    , props :: SavedLocationScreenProps
  }

type SavedLocationScreenProps =
  {
    showDeleteLocationModel :: Boolean
  , apiRespReceived :: Boolean
  }

type SavedLocationScreenData =
  {
    savedLocations :: Array LocationListItemState
  , deleteTag :: Maybe String
  , config :: AppConfig
  , logField :: Object Foreign
  }

type DistInfo =
  { locationName :: String
  , distanceDiff :: Number
  }

type SavedLocationData =
  {
    address :: String
  , lat :: Number
  , lon :: Number
  , tag :: String
  , placeName :: String
  , placeId :: Maybe String
  }

data CallType = ANONYMOUS_CALLER | DIRECT_CALLER
derive instance genericCallType :: Generic CallType _
instance eqCallType :: Eq CallType where eq = genericEq
instance showCallType :: Show CallType where show = genericShow
instance encodeCallType :: Encode CallType where encode = defaultEnumEncode
instance decodeCallType :: Decode CallType where decode = defaultEnumDecode


data CardType = HOME_TAG | WORK_TAG | OTHER_TAG

derive instance genericCardType :: Generic CardType _
instance eqCardType :: Eq CardType where eq = genericEq
instance showCardType :: Show CardType where show = genericShow
instance encodeCardType :: Encode CardType where encode = defaultEnumEncode
instance decodeCardType :: Decode CardType where decode = defaultEnumDecode

type AddNewAddressScreenState =
  {
    data :: AddNewAddressScreenData
  , props :: AddNewAddressScreenProps
  }

type AddNewAddressScreenData =
  {
    locationList :: Array LocationListItemState
  , savedLocations :: Array LocationListItemState
  , selectedItem :: LocationListItemState
  , activeIndex :: Maybe Int
  , selectedTag :: Maybe CardType
  , savedTags :: Array String
  , addressSavedAs :: String
  , placeName :: String
  , lat :: Number
  , lon :: Number
  , editTag :: String
  , existsAs :: String
  , currentLocation :: String
  , currLat :: Maybe Number
  , currLon :: Maybe Number
  , address :: String
  , recentSearchs :: RecentlySearchedObject
  , locSelectedFromMap :: String
  , latSelectedFromMap :: Number
  , lonSelectedFromMap :: Number
  , addressComponents :: Array AddressComponents
  , polygonCoordinates :: String
  , nearByPickUpPoints :: Array Location
  , config :: AppConfig
  }

type AddNewAddressScreenProps =
  {
    showSavePlaceView :: Boolean
  , isBtnActive :: Boolean
  , editLocation :: Boolean
  , tagExists :: Boolean
  , placeNameExists :: Boolean
  , isLocateOnMap :: Boolean
  , isLocationServiceable :: Boolean
  , fromHome :: Boolean
  , selectFromCurrentOrMap :: Boolean
  , isSearchedLocationServiceable :: Boolean
  , editSavedLocation :: Boolean
  , isSpecialZone :: Boolean
  , defaultPickUpPoint :: String
  , isServiceable :: Boolean
  }

type AppUpdatePopUpState =
 { version :: Int ,
   logField :: Object Foreign,
   updatePopup :: UpdatePopupType,
   appUpdatedView :: AppUpdatedViewState,
   config :: AppConfig
 }

type AppUpdatedViewState = {
  primaryText :: String,
  secondaryText :: String,
  optionTwoText :: String,
  coverImageUrl :: String
}

data UpdatePopupType =  AppVersion
                      | DateAndTime
                      | NoUpdatePopup
                      | AppUpdated

derive instance genericUpdatePopupType :: Generic UpdatePopupType _
instance showUpdatePopupType :: Show UpdatePopupType where show = genericShow
instance eqUpdatePopupType :: Eq UpdatePopupType where eq = genericEq

data NotifyFlowEventType = RATE_DRIVER_SKIPPED | SEARCH_CANCELLED

derive instance genericNotifyFlowEventType :: Generic NotifyFlowEventType _
instance showNotifyFlowEventType :: Show NotifyFlowEventType where show = genericShow
data LocItemType = LOC_LIST | CURR_LOC | LOCATE_ON_MAP

derive instance genericLocItemType :: Generic LocItemType _
instance eqLocItemType :: Eq LocItemType where eq = genericEq
instance showLocItemType :: Show LocItemType where show = genericShow
instance encodeLocItemType :: Encode LocItemType where encode = defaultEnumEncode
instance decodeLocItemType:: Decode LocItemType where decode = defaultEnumDecode

data SearchResultType = QUOTES | ESTIMATES

derive instance genericSearchResultType :: Generic SearchResultType _
instance eqSearchResultType :: Eq SearchResultType where eq = genericEq
instance showSearchResultType :: Show SearchResultType where show = genericShow

type LocationTagBarState =
  { savedLocations :: Array LocationListItemState }

type LocationListItemState = {
    prefixImageUrl :: String
  , postfixImageUrl :: String
  , postfixImageVisibility :: Boolean
  , title :: String
  , subTitle :: String
  , placeId :: Maybe String
  , lat :: Maybe Number
  , lon :: Maybe Number
  , description :: String
  , tag :: String
  , tagType :: Maybe String
  , cardType :: Maybe String
  , address :: String
  , tagName :: String
  , isEditEnabled :: Boolean
  , savedLocation :: String
  , placeName :: String
  , isClickable :: Boolean
  , alpha :: Number
  , fullAddress :: Address
  , locationItemType :: Maybe LocationItemType
  , distance :: Maybe String
  , showDistance :: Maybe Boolean
  , actualDistance :: Maybe Int
  , frequencyCount :: Maybe Int
  , recencyDate :: Maybe String
  , locationScore :: Maybe Number
}

type SuggestionsMap = Map SourceGeoHash Suggestions
type SourceGeoHash = String
type DestinationGeoHash = String

type Suggestions = {
    destinationSuggestions :: Array LocationListItemState
  , tripSuggestions :: Array Trip
}

type Trip = {
    sourceLat :: Number
  , source :: String 
  , destination :: String
  , sourceAddress :: Address 
  , destinationAddress :: Address
  , sourceLong :: Number
  , destLat :: Number
  , destLong :: Number
  , frequencyCount :: Maybe Int
  , recencyDate :: Maybe String
  , locationScore :: Maybe Number
  , isSpecialZone :: Boolean
}
type SuggestionsData =  {
    suggestionsMap :: SuggestionsMap
}

data LocationItemType = RECENTS | PREDICTION | SAVED_LOCATION | SUGGESTED_DESTINATIONS

derive instance genericLocationItemType :: Generic LocationItemType _
instance eqLocationItemType :: Eq LocationItemType where eq = genericEq
instance showLocationItemType :: Show LocationItemType where show = genericShow
instance encodeLocationItemType :: Encode LocationItemType where encode = defaultEnumEncode
instance decodeLocationItemType:: Decode LocationItemType where decode = defaultEnumDecode
instance encodeJsonLocationItemType :: EncodeJson LocationItemType where
  encodeJson = genericEncodeJson
instance decodeJsonLocationItemType :: DecodeJson LocationItemType where
  decodeJson = genericDecodeJson
  

type SaveFavouriteCardState =
  {
    address :: String
  , tag :: String
  , tagExists :: Boolean
  , selectedItem :: LocationListItemState
  , isBtnActive :: Boolean
  }

type Fares = {
  baseFare :: String
, pickupCharges :: String
, nominalFare :: String
, waitingCharges :: String
}

type FareComponent = {
  fareType :: String
, price :: String
, title :: String
}

type SuccessScreenState = {
    title :: String
  , subTitle :: String
}
type CurrentLocationDetails =  {
    lat :: Number
  , lon :: Number
  , placeName :: String
  }

type PreviousCurrentLocations =  {
    pastCurrentLocations :: Array CurrentLocationDetails
  }
type CurrentLocationDetailsWithDistance =  {
    locationDetails :: CurrentLocationDetails
  , distance :: Number
}

newtype FlowStatusData = FlowStatusData {
    source :: Location
  , destination :: Location
  , sourceAddress :: Address
  , destinationAddress :: Address
}

derive instance genericFlowStatusData :: Generic FlowStatusData _
instance showFlowStatusData :: Show FlowStatusData where show = genericShow
instance encodeFlowStatusData :: Encode FlowStatusData where encode = defaultEncode
instance decodeFlowStatusData :: Decode FlowStatusData where decode = defaultDecode

data ZoneType = METRO
              | HOSPITAL
              | AIRPORT
              | SCHOOL
              | RAILWAY
              | NOZONE
              | AUTO_BLOCKED

derive instance genericZoneType :: Generic ZoneType _
instance showZoneType :: Show ZoneType where show = genericShow
instance eqZoneType :: Eq ZoneType where eq = genericEq
instance encodeZoneType :: Encode ZoneType where encode = defaultEncode
instance decodeZoneType :: Decode ZoneType where decode = defaultDecode


newtype TipViewData = TipViewData {
    stage :: TipViewStage
  , activeIndex :: Int
  , isVisible :: Boolean
}

derive instance genericTipViewData :: Generic TipViewData _
instance showTipViewData :: Show TipViewData where show = genericShow
instance encodeTipViewData :: Encode TipViewData where encode = defaultEncode
instance decodeTipViewData :: Decode TipViewData where decode = defaultDecode

-- ###################################### TicketInfoScreen ######################################

type TicketInfoScreenState =
  { data :: TicketInfoScreenData,
    props :: TicketInfoScreenProps
  }

type TicketInfoScreenData = {
  selectedBookingInfo :: IndividualBookingItem
}

type TicketInfoScreenProps = {
  activeListItem :: TicketBookingServiceDetails,
  activeIndex :: Int,
  leftButtonDisable :: Boolean,
  rightButtonDisable :: Boolean
}


-- ######################################  TicketBookingScreen   ######################################

type TicketBookingScreenState =
  { data :: TicketBookingScreenData,
    props :: TicketBookingScreenProps
  }


----- data for the screen internally (transformed data from the response) ------
type Ticket = 
  { title :: String
  , shortDesc :: Maybe String
  , ticketID :: String
  , isExpanded :: Boolean
  , businessHours :: Array TicketBusinessHoursOptionData
  , timeIntervals :: Array TimeInterval
  , slot :: Array SlotInterval
  , selectedBHid :: Maybe String
  , selectedSlot :: Maybe String
  , expiry :: ServiceExpiry
  }

type TicketBusinessHoursOptionData =
  { ticketID :: String,
    bhourId :: String,
    categories :: Array TicketCategoriesOptionData,
    operationalDays :: Array String
  }

type TicketCategoriesOptionData =
  {   ticketID :: String,
      categoryName :: String, -- (SEAT-TYPES, DESTINATION, ZOO)
      categoryId :: String,
      availableSeats :: Maybe Int,
      allowedSeats :: Maybe Int,
      bookedSeats :: Int,
      peopleCategories :: Array TicketPeopleCategoriesOptionData,
      isSelected :: Boolean
  }

type TicketPeopleCategoriesOptionData =
  { ticketID :: String,
    title ::String,
    subcategory :: String,
    currentValue :: Int,
    pricePerUnit :: Int,
    ticketLimitCrossed :: Boolean,
    peopleCategoryId :: String
  }

---- data for the --------------------------------------------------------
type TicketServiceData =
  { id :: String,
    serviceName :: String,
    allowFutureBooking :: Boolean,
    shortDesc :: Maybe String,
    expiry :: ServiceExpiry,
    businessHours :: Array BusinessHoursData,
    timeIntervalData :: Array SlotsAndTimeIntervalData,
    isExpanded :: Boolean,
    selectedBHid :: Maybe String,
    selectedSlot :: Maybe String
  }

type BusinessHoursData = {
  bhourId :: String,
  slot :: Maybe String,
  startTime :: Maybe String, -- TimeOfDay -- OpenTimings CloseTimings
  endTime :: Maybe String,
  categories :: Array TicketCategoriesData,
  operationalDays :: Array String
}

type SlotsAndTimeIntervalData = {
  operationalDays :: Array String,
  slot :: Array SlotInterval,
  timeIntervals :: Array TimeInterval
}

type SlotInterval = {
  bhourId :: String,
  slot :: String
}
type TimeInterval = {
  bhourId :: String,
  startTime :: String,
  endTime :: String
}

type TicketCategoriesData = {
  categoryName :: String, -- (SEAT-TYPES, DESTINATION, ZOO)
  categoryId :: String,
  availableSeats :: Maybe Int,
  allowedSeats :: Maybe Int,
  bookedSeats :: Int,
  peopleCategories :: Array PeopleCategoriesRespData,
  isSelected :: Boolean
}

type PeopleCategoriesRespData =
  { peopleCategoryName :: String,
    pricePerUnit :: Int,
    currentValue :: Int,
    peopleCategoryId :: String,
    ticketLimitCrossed :: Boolean
  }

type TiketingListTransformedData =
  { timings :: Array KeyVal,
    fees :: Array KVPairArr
  }

type KVPairArr =
  { key :: String
  , val :: Array KeyVal
  , disableCategory :: Boolean
  }

type TicketBookingScreenData = {
  servicesAvailing :: Array TicketServiceI, -- TODO:: Use this for generic handling
  dateOfVisit :: String,
  keyValArray :: Array KeyVal,
  transactionId :: String,
  bookedForArray :: Array String,
  zooName :: String,
  totalAmount :: Int,
  placeInfo :: Maybe TicketPlaceResp,
  servicesInfo :: Array TicketServiceData,
  shortOrderId :: String,
  selectedPlaceType :: PlaceType
}

type TicketServiceI = {
  id :: String,
  peopleCategoryName :: String,
  numberOfUnits :: Int
}

type KeyVal = {
  key :: String,
  val :: String
}

type TicketBookingItem = 
  { shortId :: String,
    ticketPlaceName :: String,
    amount :: Number,
    visitDate :: String,
    status :: BookingStatus,
    ticketPlaceId ::  String,
    personId :: String
  }

type TicketBookings = 
  { pendingBooking :: Array TicketBookingItem,
    booked :: Array TicketBookingItem
  }

type TicketBookingScreenProps = {
  currentStage :: TicketBookingScreenStage,
  previousStage :: TicketBookingScreenStage,
  termsAndConditionsSelected :: Boolean,
  validDate :: Boolean,
  showShimmer :: Boolean,
  paymentStatus :: Common.PaymentStatus,
  ticketBookingList :: TicketBookings,
  selectedBookingId :: String,
  selectedBookingInfo :: IndividualBookingItem,
  activeListItem :: TicketBookingServiceDetails,
  activeIndex :: Int,
  rightButtonDisable :: Boolean,
  leftButtonDisable :: Boolean,
  navigateToHome :: Boolean,
  selectedOperationalDay :: String
}

type TicketItem = {
  ticketType :: String,
  fare :: Int,
  time :: String,
  serviceId :: String,
  validTill :: String
  }

type IndividualBookingItem =
  { shortId :: String,
    ticketPlaceId :: String,
    ticketPlaceName :: String,
    personId :: String,
    amount :: Number,
    visitDate :: String,
    status :: BookingStatus,
    services :: Array TicketBookingServiceDetails
  }

type TicketBookingServiceDetails =
  { amount :: Number,
    status :: String,
    verificationCount :: Int,
    expiryDate :: Maybe String,
    ticketServiceName :: String,
    categories :: Array TicketBookingCategoryDetails,
    ticketServiceShortId :: String,
    slot :: Maybe String
  }

type TicketBookingCategoryDetails =
  { amount :: Number,
    bookedSeats :: Int,
    name :: String,
    peopleCategories :: Array TicketBookingPeopleCategoryDetails
  }

type TicketBookingPeopleCategoryDetails =
  { name :: String,
    numberOfUnits ::Int,
    pricePerUnit :: Number
  }

data TicketBookingScreenStage = DescriptionStage 
                              | ChooseTicketStage
                              | BookingConfirmationStage
                              | ViewTicketStage
                              | MyTicketsStage
                              | TicketInfoStage

derive instance genericTicketBookingScreenStage :: Generic TicketBookingScreenStage _
instance showTicketBookingScreenStage :: Show TicketBookingScreenStage where show = genericShow
instance eqTicketBookingScreenStage :: Eq TicketBookingScreenStage where eq = genericEq


-- ######################################### TicketingScreenState ####################################################

type TicketingScreenState = {
  data :: TicketingScreenData ,
  props :: TicketingScreenProps
}

type TicketingScreenData = {
  placeInfoArray :: Array TicketPlaceResp
} 

type TicketingScreenProps = {
  hideMyTickets :: Boolean
} 
type ReAllocationProp =
  { showPopUp :: Boolean
  }

-- ######################################### RideScheduledState ####################################################
type RideScheduledScreenState = {
    primaryButtonText :: String
  , source :: String
  , destination :: Maybe String
  , startDate :: String
  , startTime :: String
  , finalPrice :: String
  , baseDuration :: String
  , baseDistance :: String
  , driverAllocationTime :: String
}
data IssueModalType = HELP_AND_SUPPORT_SCREEN_MODAL | REPORTED_ISSUES_MODAL | RESOLVED_ISSUES_MODAL

derive instance genericIssueModalType :: Generic IssueModalType _
instance eqIssueModalType :: Eq IssueModalType where eq = genericEq


-- ######################################### MetroTicketDetailsState ####################################################
type MetroTicketDetailsScreenState = {
    data :: MetroTicketDetailsScreenData
  , props :: MetroTicketDetailsScreenProps
}

type MetroTicketDetailsScreenData = {
  dummyData :: String
, metroRoute :: Array MetroRoute
, ticketsInfo :: Array MetroTicketInfo
}

type MetroTicketInfo = {
  qrString :: String
, ticketNumber :: String 
, validUntil :: String
, status :: String
}

type MetroRoute = {
  name :: String
, line :: MetroLine 
, stops :: Array MetroStop
, listExpanded :: Boolean
}
data MetroLine = BlueLine 
               | GreenLine 
               | RedLine
               | NoColorLine

derive instance genericMetroLine :: Generic MetroLine _                                  
instance showMetroLine :: Show MetroLine where show = genericShow
instance eqMetroLine :: Eq MetroLine where eq = genericEq 

type MetroStop = {
  name :: String
}

type MetroTicketDetailsScreenProps = {
  dummyProps :: String
, stage :: MetroTicketDetailsScreenStage
, currentTicketIndex :: Int
, previousScreenStage :: PreviousMetroTicketDetailsStage
}

data PreviousMetroTicketDetailsStage = MetroMyTicketsStage 
                                     | SearchMetroLocationStage 
                                     | MetroTicketSelectionStage

derive instance genericPreviousMetroTicketDetailsStage :: Generic PreviousMetroTicketDetailsStage _                                  
instance showPreviousMetroTicketDetailsStage :: Show PreviousMetroTicketDetailsStage where show = genericShow
instance eqPreviousMetroTicketDetailsStage :: Eq PreviousMetroTicketDetailsStage where eq = genericEq 

data MetroTicketDetailsScreenStage = MetroTicketDetailsStage 
                                   | MetroMapStage 
                                   | MetroRouteDetailsStage 

derive instance genericMetroTicketDetailsScreenStage :: Generic MetroTicketDetailsScreenStage _                                  
instance showMetroTicketDetailsScreenStage :: Show MetroTicketDetailsScreenStage where show = genericShow
instance eqMetroTicketDetailsScreenStage :: Eq MetroTicketDetailsScreenStage where eq = genericEq 


-- ######################################### MetroMyTicket ####################################################
type MetroMyTicketsScreenState = {
    data :: MetroMyTicketsScreenData
  , props :: MetroMyTicketsScreenProps
}

type MetroMyTicketsScreenData = {
  activeTickets :: Array MetroTicketCardData
, pastTickets :: Array MetroTicketCardData
}

type MetroMyTicketsScreenProps = {
  dummyProps :: String
, showShimmer :: Boolean
}

type MetroTicketCardData = {
  sourceName :: String
  , destinationName :: String
  , createdAt :: String
  , noOfTickets :: Int
  , metroTicketStatusApiResp :: MetroTicketBookingStatus
  , status :: String
  , validUntill :: String
}


-- ######################################### TicketBookingStatus #################################################### 

type TicketStatusScreenState =
  { data :: TicketStatusScreenData,
    props :: TicketStatusScreenProps
  }

type TicketStatusScreenData = {
  servicesAvailing :: Array TicketServiceI, -- TODO:: Use this for generic handling
  dateOfVisit :: String,
  keyValArray :: Array KeyVal,
  transactionId :: String,
  bookedForArray :: Array String,
  ticketName :: String,
  totalAmount :: Int,
  placeInfo :: Maybe TicketPlaceResp,
  servicesInfo :: Array TicketServiceData,
  shortOrderId :: String,
  selectedPlaceType :: PlaceType
}

type TicketStatusScreenProps = {
  currentStage :: TicketBookingScreenStage,
  previousStage :: TicketBookingScreenStage,
  termsAndConditionsSelected :: Boolean,
  validDate :: Boolean,
  showShimmer :: Boolean,
  paymentStatus :: Common.PaymentStatus,
  ticketBookingList :: TicketBookings,
  selectedBookingId :: String,
  selectedBookingInfo :: IndividualBookingItem,
  activeListItem :: TicketBookingServiceDetails,
  activeIndex :: Int,
  rightButtonDisable :: Boolean,
  leftButtonDisable :: Boolean,
  navigateToHome :: Boolean,
  selectedOperationalDay :: String,
  actionType :: TicketStatusEntry
}


data TicketStatusEntry = MetroTicketToPaymentStatusEntry
                       | ZooTicketToPaymentStatusEntry
derive instance genericTicketStatusEntry :: Generic TicketStatusEntry _ 
instance showTicketStatusEntry :: Show TicketStatusEntry where show = genericShow
instance eqTicketStatusEntry :: Eq TicketStatusEntry where eq = genericEq
--- ######################################### Search Location Screen State ####################################################


type SearchLocationScreenState = 
  { data :: SearchLocationScreenData ,
    props :: SearchLocationScreenProps,
    appConfig :: AppConfig
  }

type SearchLocationScreenData = 
  {
    srcLoc :: Maybe LocationInfo,
    destLoc :: Maybe LocationInfo,
    currentLoc :: Maybe LocationInfo,
    locationList :: Array LocationListItemState,
    fromScreen :: String,
    saveFavouriteCard :: SaveFavouriteCardState,
    mapLoc :: LocationInfo,
    defaultGate :: String,
    nearByGates :: Array Location,
    specialZoneCoordinates :: String,
    confirmLocCategory :: String,
    metroStations :: Array Station,
    updatedMetroStations :: Array Station
  }

type Station = {
  stationName :: String,
  stationCode :: String
}

type SearchLocationScreenProps = 
  { searchLocStage :: SearchLocationStage
  , focussedTextField :: Maybe SearchLocationTextField
  , actionType :: SearchLocationActionType
  , showSaveFavCard :: Boolean
  , areBothLocMandatory :: Boolean
  , canSelectFromFav :: Boolean
  , showLoader :: Boolean
  , canClearText :: Boolean 
  , locUnserviceable :: Boolean
  , isAutoComplete :: Boolean  }

data SearchLocationActionType = AddingStopAction 
                              | SearchLocationAction
                              | MetroStationSelectionAction

derive instance genericSearchLocationActionType :: Generic SearchLocationActionType _
instance eqSearchLocationActionType :: Eq SearchLocationActionType where eq = genericEq

data SearchLocationTextField =  SearchLocPickup
                              | SearchLocDrop

derive instance genericSearchLocationTextField :: Generic SearchLocationTextField _
instance showSearchLocationTextField :: Show SearchLocationTextField where show = genericShow
instance eqSearchLocationTextField :: Eq SearchLocationTextField where eq = genericEq

data SearchLocationStage =  ConfirmLocationStage 
                          | PredictionsStage 
                          | LocateOnMapStage
                          | AllFavouritesStage

derive instance genericSearchLocationStage :: Generic SearchLocationStage _
instance eqSearchLocationStage :: Eq SearchLocationStage where eq = genericEq

type GlobalProps = 
  { savedLocations :: Array LocationListItemState
  , recentSearches :: Array LocationListItemState
  , cachedSearches :: Array LocationListItemState
  }

type LocationInfo = 
  { lat :: Maybe Number ,
    lon :: Maybe Number ,
    placeId :: Maybe String ,
    address :: String ,
    addressComponents :: Address ,
    city :: Maybe City,
    metroInfo :: Maybe Station,
    stationCode :: String
  }

type MetroStation = {
  code :: String
  , name :: String
  , lat :: Maybe Number
  , lon :: Maybe Number
  , address :: Maybe String
  , stationType :: Maybe String
  , color :: Maybe String
}
-- ######################################### MetroTicketBookingScreenState ####################################################

type MetroTicketBookingScreenState = {
  data :: MetroTicketBookingScreenData,
  props :: MetroTicketBookingScreenProps
}

type MetroTicketBookingScreenData = {
  ticketType :: TicketType
  , ticketCount :: Int
  , srcLoc :: String
  , destLoc :: String
  , srcCode :: String
  , destCode :: String
  , searchId :: String
  , ticketPrice :: Int
  , bookingId :: String
  , quoteId :: String
}

type MetroTicketBookingScreenProps = {
  isLimitExceeded :: Boolean
, termsAndConditionsSelected :: Boolean
, currentStage :: MetroTicketBookingStage
, isButtonActive :: Boolean
}

data MetroTicketBookingStage = MetroTicketSelection | GetMetroQuote | ConfirmMetroQuote

derive instance genericMetroTicketBookingStage :: Generic MetroTicketBookingStage _
instance eqMetroTicketBookingStage :: Eq MetroTicketBookingStage where eq = genericEq
instance showMetroTicketBookingStage :: Show MetroTicketBookingStage where show = genericShow

data TicketType = ONE_WAY | ROUND_TRIP

derive instance genericTicketType :: Generic TicketType _
instance eqTicketType :: Eq TicketType where eq = genericEq

data LocationActionId = Src | Dest

derive instance genericLocationActionId :: Generic LocationActionId _
instance eqLocationActionId :: Eq LocationActionId where eq = genericEq
instance showLocationActionId :: Show LocationActionId where show = genericShow


-- ######################################### MetroTicketStatusScreenState ####################################################
type MetroTicketStatusScreenState = {
  data :: MetroTicketStatusScreenData,
  props :: MetroTicketStatusScreenProps
}

type MetroTicketStatusScreenData = {
  shortOrderId :: String,
  keyValArray :: Array KeyVal,
  ticketName :: String,
  validUntil :: String
}

type MetroTicketStatusScreenProps = {
  showShimmer :: Boolean
, paymentStatus :: Common.PaymentStatus
}
