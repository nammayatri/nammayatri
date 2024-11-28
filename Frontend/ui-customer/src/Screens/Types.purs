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
import PrestoDOM.List

import Common.Types.App as CTA
import Components.ChatView.Controller (ChatComponentConfig, Config)
import Components.ChooseVehicle.Controller as ChooseVehicle
import Components.SettingSideBar.Controller (SettingSideBarState)
import Components.SettingSideBar.Controller as SideBar
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import Domain.Payments as PP
import Foreign (Foreign,unsafeFromForeign)
import Foreign.Class (class Decode, class Encode)
import Foreign.Object (Object)
import Halogen.VDom.DOM.Prop (PropValue)
import JBridge (Location)
import Language.Types (STR(..))
import Prelude (class Eq, class Show,($))
import Presto.Core.Utils.Encoding (defaultEnumDecode, defaultEnumEncode, defaultDecode, defaultEncode)
import PrestoDOM (LetterSpacing, BottomSheetState(..), Visibility(..), Accessiblity(..))
import RemoteConfig as RC
import Services.API (DeadKmFare, AddressComponents, BookingLocationAPIEntity, EstimateAPIEntity(..), QuoteAPIEntity, TicketPlaceResp, RideBookingRes, Route, BookingStatus(..), LatLong(..), PlaceType(..), ServiceExpiry(..), Chat, SosFlow(..), FRFSTicketBookingStatusAPIRes(..),FRFSStationAPI(..),TicketCategoriesResp(..), FrfsQuote, RideShareOptions(..), SavedLocationsListRes,  Route(..), FRFSConfigAPIRes, RideShareOptions, DeliveryDetails(..), PersonLocationAndInstruction(..), InitiatedAs(..), InstructionAndAddress(..), LocationAPIEntity(..), DeadKmFare(..),PriceAPIEntityDecimals(..),DistanceWithUnit(..),RideAPIEntity(..), RideBookingListRes, FRFSRouteAPI)
import Components.SettingSideBar.Controller as SideBar
import Components.MessagingView.Controller (ChatComponent, ChatContacts)
import Screens(ScreenName)
import PrestoDOM.List
import JBridge (Location, Locations)
import Data.HashMap as DHM
import Data.Map as DM
import MerchantConfig.Types as MRC
import Services.API (DeadKmFare)
import Services.API as API
import Common.RemoteConfig.Types as CRT
import Common.Types.App (FeedbackAnswer)
import Styles.Types
import Control.Monad.Except (runExcept, except)
import Data.Either (Either(..))

type Contacts = {
  name :: String,
  number :: String
}

type FeedbackItem = { 
    id :: String
  , text :: String
}

type NewContacts = {
  name :: String,
  number :: String,
  isSelected :: Boolean,
  enableForFollowing :: Boolean,
  enableForShareRide:: Boolean,
  onRide :: Boolean,
  priority :: Int,
  contactPersonId :: Maybe String,
  notifiedViaFCM :: Maybe Boolean,
  shareTripWithEmergencyContactOption :: DropDownOptions,
  isFollowing :: Maybe Boolean
}

type NewContactsProp = {
  name :: PropValue,
  number :: PropValue,
  contactBackgroundColor :: PropValue,
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
  carouselModal :: CTA.CarouselModal,
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
  countryCodeOptionExpanded :: Boolean,
  autoFillOTPEnabled ::  Boolean
}

type EnterMobileNumberScreenStateData = {
    mobileNumber :: String
  , countryObj :: CTA.CountryCodeObj
  , tokenId :: String
  , attempts :: Int
  , otp :: String
  , timer :: Int
  , timerID :: String
  , config :: AppConfig
  , logField :: Object Foreign
  , otpChannel :: CTA.OTPChannel
}
-- ################################################ AccountSetUpScreenState ##################################################

data Gender = MALE | FEMALE | OTHER | PREFER_NOT_TO_SAY

derive instance genericGender :: Generic Gender _
instance showGender :: Show Gender where show = genericShow
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



data ActiveFieldAccountSetup = DropDown | NameSection | ReferralSection

data ReferralEnum = Verified | NotVerified | ReferralFailed | Verifying

derive instance eqReferralEnum :: Eq ReferralEnum
derive instance genericActiveFieldAccountSetup :: Generic ActiveFieldAccountSetup _
instance eqActiveFieldAccountSetup :: Eq ActiveFieldAccountSetup where eq = genericEq

type AccountSetUpScreenStateData =
  {   name :: String
    , email :: String
    , gender :: Maybe Gender
    , nameErrorMessage :: Maybe ErrorType
    , config :: AppConfig
    , disabilityOptions :: DisabilityData
    , isReferred :: ReferralEnum
    , referralTextFocussed :: Boolean
    , referralTextDisabled :: Boolean
    , referralCode :: String
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
    categories :: Array CTA.CategoryListType
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
    showIssueOptions :: Boolean,
    isContactSupportPopUp :: Boolean
  }

data TripDetailsGoBackType = Home | MyRides | HelpAndSupport | ReportIssueChat | RideCompletedScreen
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
    pdfHeading :: String,
    rideType :: FareProductType
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




type IssueInfo = {
    issueReportId :: String,
    status :: String,
    category :: String,
    createdAt :: String,
    issueReportShortId :: Maybe String,
    optionLabel :: Maybe String,
    rideId :: Maybe String
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
  optionsVisibility :: Boolean,
  fromBanner :: Boolean,
  refreshLoader :: Boolean,
  scrollEnable :: Boolean
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
    driverPhoneNumber :: Maybe String,
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
  , serviceTierName :: Maybe String
  , totalTime :: String
  , vehicleModel :: String
  , rideStartTimeUTC :: String
  , providerName :: String
  , providerType :: CTA.ProviderType
  , showRepeatRide :: String
  , rideType :: FareProductType
  , estimatedDistance :: Int
  , isScheduled :: String
  , estimatedDuration :: Int
  , estimatedFare :: Int
  , showDestination :: String
  , rideScheduledTime :: String
  , rideCreatedAt :: String
  , rideStatus :: String
  , isAirConditioned :: Maybe Boolean
  }

data VehicleVariant = SUV | SEDAN | HATCHBACK | AUTO_RICKSHAW | TAXI | TAXI_PLUS | BIKE | AMBULANCE_TAXI | AMBULANCE_TAXI_OXY | AMBULANCE_AC | AMBULANCE_AC_OXY | AMBULANCE_VENTILATOR | SUV_PLUS | DELIVERY_BIKE | EV_AUTO_RICKSHAW | HERITAGE_CAB

derive instance genericVehicleVariant :: Generic VehicleVariant _
instance eqVehicleVariant :: Eq VehicleVariant where eq = genericEq
instance showVehicleVariant :: Show VehicleVariant where show = genericShow
instance encodeVehicleVariant :: Encode VehicleVariant where encode = defaultEncode
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
    isScheduled :: PropValue,
    rating :: PropValue,
    driverName :: PropValue,
    rideStartTime :: PropValue,
    rideEndTime :: PropValue,
    vehicleNumber :: PropValue,
    rideId :: PropValue,
    status :: PropValue,
    rideEndTimeUTC :: PropValue,
    alpha :: PropValue,
    zoneVisibility :: PropValue,
    variantImage :: PropValue,
    showVariantImage :: PropValue,
    showRepeatRide :: PropValue,
    showDestination :: PropValue,
    itemRideType :: PropValue,
    rideTypeVisibility :: PropValue,
    rideTypeBackground :: PropValue,
    cornerRadius :: PropValue
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


-- ################################################ Rider RideCompletedScreen ##################################################

type RiderRideCompletedScreenState =
  {
    topCard :: TopCard
  , favDriverInfoCard :: Boolean
  , isFreeRide :: Boolean
  , accessibility :: Accessiblity
  , isRatingCard :: Boolean 
  , ratingCard :: RiderRatingCard
  , driverInfoCardState :: DriverInfoCard
  , needHelpText :: String
  , recordedView :: Boolean
  , rideId :: String
  , timerId :: String
  , timerValue :: String
  , countDownValue :: String
  , showRentalRideDetails :: Boolean
  , rentalRowDetails :: RentalRowConfig
  , rentalBookingData :: CTA.RentalBookingConfig
  , rideRatingState :: RatingCard
  , ratingViewState :: RatingViewState
  , isSafetyCenterDisabled :: Boolean
  , bookingId :: String
  , config :: AppConfig
  , rideDuration :: Maybe Int
  , additionalCharges :: Array AdditionalCharges
  , customerIssue :: CustomerIssueReportData
  , showSafetyCenter :: Boolean
  , isKeyBoardOpen :: Boolean
  , goToLastBanner :: Boolean
  }

type CustomerIssueReportData = {
  currentPageIndex :: Int
, showIssueBanners :: Boolean
, hasAccessibilityIssue :: Boolean
, hasSafetyIssue :: Boolean
, demandExtraTollAmountIssue :: Boolean
, customerResponse :: Array {issueType :: CTA.CustomerIssueTypes, selectedYes :: Maybe Boolean}
, respondedValidIssues :: Boolean
, bannerComputedView :: Maybe ListItem
, buttonActive :: Boolean
}

type AdditionalCharges = {
  text :: String
, visibility :: Visibility
, textColor :: Color
, image :: String
}

type RentalRowConfig = {
    rideTime :: String
  , rideDistance :: String
  , rideDistanceInfo :: String
  , rideStartedAt :: String
  , rideEndedAt :: String
  , estimatedFare :: String
  , extraTimeFare :: String
  , extraDistanceFare :: String
  , totalFare :: String
  , rideDetailsTitle :: String
  , fareUpdateTitle :: String
  , surcharges :: String
}

type TopCard = {
  title :: String,
  finalAmount :: Int,
  initialAmount :: Int,
  fareUpdatedVisiblity :: Boolean,
  infoPill :: FareUpdatePill
}

type TopPill = {
  visible :: Boolean,
  background :: String,
  text :: String,
  textColor :: String,
  icon :: Maybe String
}

type RatingState = {
    rideBookingRes :: RideBookingRes
}

type FareUpdatePill =
  {
    text :: String
  , imageVis :: Visibility
  , visible :: Visibility
  }

type RiderRatingCard =
  {
    rating :: Int 
  , rideId :: String
  , isRecording :: Boolean
  , favDriver :: Boolean
  , distanceDifference :: Int
  , feedbackList :: Array FeedbackAnswer
  , feedbackPillData :: Array (Array (Array FeedbackItem)) 
  , recordAudioState :: RecordAudioState
  , feedbackText :: String
  }

type RecordAudioState = {
    isRecording :: Boolean 
  , timer :: String
  , recordingDone :: Boolean
  , isUploading   :: Boolean
  , recordedFile  :: Maybe String
  , openAddAudioModel :: Boolean
  , uploadedAudioId :: Maybe String
  , recordedAudioUrl :: Maybe String
  , isListening :: Boolean
  , pauseLootie :: Boolean
}

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
           | GoToConfirmLocation
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
           | EditPickUpLocation
           | ProviderSelection
           | RideSearch
           | ConfirmRentalRide
           | ChangeToRideAccepted
           | ChangeToRideStarted
           | ConfirmingQuotes
           | EditingDestinationLoc
           | ConfirmEditDestinationLoc
           | ConfirmingEditDestinationLoc
           | RevisedEstimate
           | FavouriteLocationModelEditDest
           | GoToTripSelect
           | GoToConfirmgDelivery

derive instance genericStage :: Generic Stage _
instance eqStage :: Eq Stage where eq = genericEq
instance showStage :: Show Stage where show = genericShow

data SearchLocationModelType = SearchLocation | LocateOnMap | NoView | RouteMap | SelectTripType

data PopupType = Logout | ConfirmBack | NoPopUp | ActiveQuotePopUp | TipsPopUp | CancelConfirmingQuotes

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
  , isBookingUpdated :: Boolean
  , source :: String
  , destination :: String
  , eta :: String
  , vehicleDetails :: String
  , registrationNumber :: String
  , newEstimatedDistance :: Maybe Number
  , newEstimatedFare :: Maybe Int
  , rating :: Number
  , locationList :: Array LocationListItemState
  , savedLocations :: Array LocationListItemState
  , recentSearchs :: RecentlySearchedObject
  , destinationSuggestions :: Array LocationListItemState
  , tripSuggestions :: Array Trip
  , selectList :: Array QuoteAPIEntity
  , quoteListModelState :: Array QuoteListItemState
  , driverInfoCardState :: DriverInfoCard 
  , activeRidesList :: Array DriverInfoCard
  , rideRatingState :: RatingCard
  , settingSideBar :: SettingSideBarState
  , sourceAddress :: Address
  , destinationAddress :: Address
  , route :: Maybe Route
  , startedAtUTC :: String
  , rateCard :: CTA.RateCard
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
  , quoteList :: Array ChooseVehicle.Config
  , selectedQuoteId :: Maybe String
  , selectedQuoteVariant :: Maybe String
  , selectedEstimatesObject :: ChooseVehicle.Config
  , lastMessage :: ChatComponentConfig
  , cancelRideConfirmationData :: CancelRideConfirmationData
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
  , rentalsInfo :: Maybe RentalsInfo
  , bannerData :: BannerCarousalData
  , contactList :: Maybe (Array NewContacts)
  , followers :: Maybe (Array Followers)
  , manuallySharedFollowers :: Maybe (Array Followers)
  , vehicleVariant :: String
  , hotSpotInfo :: Array HotSpotData
  , startTimeUTC :: String
  , returnTimeUTC :: String
  , estReturnTimeUTC :: String
  , selectedDateTimeConfig :: DateTimeConfig
  , fareProductType :: FareProductType
  , invalidBookingId :: Maybe String
  , iopState :: InteroperabilityState
  , currentCityConfig :: MRC.CityConfig
  , otherSelectedEstimates :: Array String
  , rateCardCache :: Maybe CTA.RateCard
  , maxEstimatedDuration :: Int
  , invalidBookingPopUpConfig :: Maybe InvalidBookingPopUpConfig
  , rideCompletedData :: RideCompletedData -- put necesssary data which is required in ride completed screen
  , routeCacheForAdvancedBooking :: Maybe Route
  , previousRideDrop :: Boolean
  , famousDestinations :: Array LocationListItemState
  , chatPersonId :: String
  , parking :: ParkingData
  , toll :: TollData
  , channelIdFromFCM :: String
  , personIdFromFCM :: String
  , sourceFromFCM :: String
  , suggestedVehicalVarient :: Array (Maybe String)
  , tripTypeDataConfig :: TripTypeConfig
  , tripEstDuration :: Int
  , latestScheduledRides :: Maybe RideBookingListRes
  , overLappingBooking :: Maybe RideBookingRes
  , upcomingRideDetails :: Maybe UpcomingRideDetails
  , selectedService :: Maybe RC.Service
  , intercityBus :: IntercityBusData
  , deliveryImage :: Maybe String
  , deliveryDetailsInfo :: Maybe API.DeliveryDetails
  , requestorPartyRoles :: Maybe (Array String)
  , boostSearchEstimate :: ChooseVehicle.Config
  , cancellationRate :: Maybe Number
}

type UpcomingRideDetails = {
  bookingId :: String,
  rideScheduledAt :: String
}

type TollData = {
  confidence :: Maybe CTA.Confidence
, showAmbiguousPopUp :: Boolean
, estimatedCharges :: Number 
, showIncludedPopUp :: Boolean
}

type ParkingData = {
  estimatedCharge :: Maybe Number
}
  

type IntercityBusData = {
  showPermissionPopUp :: Boolean
, showWebView :: Boolean
, hasPhoneNumberPermission :: Boolean
, url :: Maybe String
}

type InteroperabilityState = {
  timerId :: String,
  timerVal :: String,
  showMultiProvider :: Boolean,
  providerPrefVisible :: Boolean,
  providerSelectionStage :: Boolean,
  showPrefButton :: Boolean,
  providerPrefInfo :: Boolean,
  hasTopProviderEstimate :: Boolean
 }
 
type InvalidBookingPopUpConfig = {
    fromLocation :: String
  , toLocation :: String
  , bookingId :: String
  , rideScheduledTime :: String
  , maxEstimatedDuration :: Int
  , fareProductType :: FareProductType
}

type RentalsInfo = 
  { rideScheduledAtUTC :: String 
  , bookingId :: String
  , multipleScheduled :: Boolean
  , fareProductType :: FareProductType
  , nearestRideScheduledAtUTC :: String
  , vehicleVariant :: String
  , driverInformation :: Maybe ScheduledRideDriverInfo
  }

type ScheduledRideDriverInfo = {
  driverName :: String,
  vehicleNumber :: String
}
  
type Followers = {
  name :: Maybe String,
  bookingId :: String,
  mobileNumber :: String,
  priority :: Int,
  isManualFollower :: Boolean,
  personId :: Maybe String
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
  , city :: CTA.City
  , vehicleImage :: String
  , serviceTierName :: Maybe String
  }


type LocationDetails = {
    formattedAddress :: String,
    location :: LatLong,
    plusCode :: Maybe String,
    addressComponents :: Array AddressComponents,
    placeId :: Maybe String
  }

type BannerCarousalData = {
  bannerItem :: Maybe ListItem,
  currentBanner :: Int,
  bannerScrollState :: String,
  currentPage :: Int
}


type RideCompletedData = {
  issueReportData :: IssueReportData
}

type IssueReportData = {
  bannerItem :: Maybe ListItem
, currentBannerIndex :: Int
, currentPageIndex :: Int
, showIssueBanners :: Boolean
, hasAccessibilityIssue :: Boolean
, hasTollIssue :: Boolean
, hasSafetyIssue :: Boolean
, customerResponse :: Array {issueType :: CTA.CustomerIssueTypes, selectedYes :: Maybe Boolean}
, respondedValidIssues :: Boolean

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
  , mapLottieViewVisibility :: Boolean
  , showCallPopUp :: Boolean
  , homeScreenPrimaryButtonLottie :: Boolean
  , rideRequestFlow :: Boolean
  , isSearchLocation :: SearchLocationModelType
  , isSource :: Maybe Boolean
  , sourceLat :: Number
  , sourceLong :: Number
  , destinationLat :: Number
  , destinationLong :: Number
  , canScheduleRide :: Boolean
  , stopLoc :: Maybe {
      lat :: Number
    , lng :: Number
    , stopLocAddress :: String
    }
  , sourcePlaceId :: Maybe String
  , destinationPlaceId :: Maybe String
  , estimateId :: String
  , bookingUpdateRequestId :: Maybe String
  , showConfirmEditDestPopUp :: Boolean
  , selectedQuote :: Maybe String
  , locationRequestCount :: Int
  , searchId :: String
  , bookingId :: String
  , customerTip :: CustomerTipProps
  , expiredQuotes :: Array String
  , isCancelRide :: Boolean
  , cancellationReasons :: Array CTA.OptionButtonList
  , cancelRideActiveIndex :: Maybe Int
  , cancelDescription :: String
  , cancelReasonCode :: String
  , isPopUp :: PopupType
  , forFirst :: Boolean
  , callbackInitiated :: Boolean
  , isLocationTracking :: Boolean
  , isInApp :: Boolean
  , locateOnMap :: Boolean
  , distance :: Int
  , isSrcServiceable :: Boolean
  , isDestServiceable :: Boolean
  , isRideServiceable :: Boolean
  , userBlocked :: Boolean
  , showlocUnserviceablePopUp :: Boolean
  , isMockLocation :: Boolean
  , autoSelecting :: Boolean
  , searchExpire :: Int
  , isEstimateChanged :: Boolean
  , showRateCard :: Boolean
  , showRevisedFareDetails :: Boolean
  , showRateCardIcon :: Boolean
  , markerLabel :: String
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
  , confirmLocationCategory :: ZoneType
  , zoneTimerExpired :: Boolean
  , canSendSuggestion :: Boolean
  , sheetState :: Maybe BottomSheetState
  , currentSheetState :: BottomSheetState
  , showDisabilityPopUp :: Boolean
  , isChatNotificationDismissed :: Boolean
  , searchLocationModelProps :: SearchLocationModelProps
  , flowWithoutOffers :: Boolean
  , showEducationalCarousel :: Boolean
  , locateOnMapLocation :: LocateOnMapLocation
  , currentLocation :: Location
  , isShorterTrip :: Boolean
  , isNotificationExpanded :: Boolean
  , bottomSheetState :: SheetState
  , removeNotification :: Boolean
  , city :: CTA.City
  , destCity :: Maybe CTA.City
  , isHomescreenExpanded :: Boolean
  , isRepeatRide :: Boolean
  , currSlideIndex :: Number
  , suggestionsListExpanded :: Boolean
  , repeatRideTimer :: String
  , repeatRideTimerId :: String
  , showShimmer :: Boolean
  , reAllocation :: ReAllocationProp
  , homeScreenSheetState :: BottomSheetState
  , autoScrollTimer :: String
  , autoScrollTimerId :: String
  , autoScroll :: Boolean
  , enableChatWidget :: Boolean
  , focussedBottomIcon :: BottomNavBarIcon
  , sosBannerType :: Maybe SosBannerType
  , showShareRide :: Boolean
  , followsRide :: Boolean
  , isChatWithEMEnabled :: Boolean
  , referral :: ReferralStatusProp
  , showBookingPreference :: Boolean
  , safetyAlertType :: Maybe SafetyAlertType
  , rideSearchProps :: RideSearchProps
  , selectedEstimateHeight :: Int
  , suggestedRideFlow :: Boolean
  , isSafetyCenterDisabled :: Boolean
  , locateOnMapProps :: LocateOnMapProps
  , showSpecialZoneInfoPopup :: Boolean
  , hotSpot :: HotSpotProps
  , isBannerDataComputed :: Boolean
  , repeatRideVariant :: String
  , repeatRideServiceTierName :: Maybe String
  , isSearchCancelled :: Boolean
  , referralComponentProps :: ReferralComponentState
  , showAcWorkingPopup :: Boolean
  , repeateRideTimerStoped :: Boolean
  , currentEstimateHeight :: Int
  , showEndOTP :: Boolean
  , rideDurationTimer :: String
  , rideDurationTimerId :: String
  , showRentalInfo :: Boolean
  , maxDateBooking :: Int
  , showIntercityUnserviceablePopUp :: Boolean
  , showNormalRideNotSchedulablePopUp :: Boolean
  , zoneOtpExpired :: Boolean
  , stageBeforeChatScreen :: Stage
  , scheduledRidePollingDelay :: Number
  , startScheduledRidePolling :: Boolean
  , showScheduledRideExistsPopUp :: Boolean
  , isOffline :: Boolean
  , hasEstimateBackpoint :: Boolean
  , shimmerViewTimer :: Int
  , shimmerViewTimerId :: String
  , isKeyBoardOpen :: Boolean
  , isContactSupportPopUp :: Boolean
  , showChatListPopUp :: Boolean
  , isSharedLocationFlow :: Boolean
  , isOtpRideFlow :: Boolean
  , safetySettings :: Maybe API.GetEmergencySettingsRes
  , editedPickUpLocation :: EditedLocation
  , showEditPickupPopupOnCancel :: Boolean
  , isIntercityFlow :: Boolean 
  , isTripSchedulable :: Boolean
  , isConfirmSourceCurrentLocation :: Boolean
  , showDeliveryImageAndOtpModal :: Boolean
  , loadingDeliveryImage :: Boolean
  , showBookAnyOptions :: Boolean
  , showBoostSearch :: Boolean
  , busClicked :: Boolean
  , ticketServiceType :: API.TicketServiceType
  , bookAmbulanceModal :: Boolean
  , firstTimeAmbulanceSearch :: Boolean
  , searchType :: Maybe String
  }

type EditedLocation = {
  gps :: LatLong ,
  address :: Address
}
data BottomNavBarIcon = TICKETING | MOBILITY | BUS_

type BookingTime = {
  rideStartTime :: String,
  bookingId :: String,
  estimatedDuration :: Int
}

derive instance genericBottomNavBarIcon :: Generic BottomNavBarIcon _
instance showBottomNavBarIcon :: Show BottomNavBarIcon where show = genericShow
instance eqBottomNavBarIcon :: Eq BottomNavBarIcon where eq = genericEq

data SafetyAlertType = DEVIATION | STATIONARY_VEHICLE

derive instance genericSafetyAlertType :: Generic SafetyAlertType _
instance showSafetyAlertType :: Show SafetyAlertType where show = genericShow
instance eqSafetyAlertType :: Eq SafetyAlertType where eq = genericEq

type SearchLocationModelProps = {
    isAutoComplete :: Boolean
  , showLoader :: Boolean
  , crossBtnSrcVisibility :: Boolean
  , crossBtnDestVisibility :: Boolean
  , tripType :: CTA.TicketType
  , totalRideDistance :: Int
  , totalRideDuration :: Int
  , showRideInfo :: Boolean
}

type TripTypeConfig = {
  tripPickupData :: Maybe TripTypeData,
  tripReturnData :: Maybe TripTypeData
}

type TripTypeData =  {
  tripDateTimeConfig :: DateTimeConfig,
  tripDateUTC :: String,
  tripDateReadableString :: String 
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
    selectedYes :: Maybe Boolean,
    selectedRating :: Int,
    issueReportActiveIndex :: Maybe Int,
    issueReasonCode :: Maybe String,
    openReportIssue :: Boolean,
    doneButtonVisibility :: Boolean,
    issueReason :: Maybe String,
    issueDescription :: String,
    rideBookingRes :: RideBookingRes,
    wasOfferedAssistance :: Maybe Boolean,
    nightSafety :: Maybe Boolean
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
  , suggestedActiveIndex :: Maybe Int
}

type Contact = {
     name :: String,
     phoneNo :: String
}

type RateCard =
  {
    additionalFare :: Int,
    createdTime :: String,
    tollCharge :: Number,
    waitingTimeInfo :: CTA.WaitingTimeInfo,
    currentRateCardType :: CTA.RateCardType,
    driverAdditions :: Array CTA.FareList,
    extraFare :: Array CTA.FareList,
    fareInfoDescription :: Array String,
    isNightShift :: Boolean,
    nightChargeFrom :: String,
    nightChargeTill :: String,
    onFirstPage :: Boolean,
    pickUpCharges :: Number,
    vehicleVariant :: String,
    baseFare :: Int
  }


type RateCardDetails = {
  title :: String ,
  description :: String
}

data SosBannerType = SETUP_BANNER | MOCK_DRILL_BANNER

derive instance genericSosBannerType :: Generic SosBannerType _
instance showSosBannerType :: Show SosBannerType where show = genericShow
instance eqSosBannerType :: Eq SosBannerType where eq = genericEq

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
  {   referralCode :: String
    , btnActive :: Boolean
    , showThanks :: Boolean
    , isInvalidCode :: Boolean
    , isExpandReference :: Boolean
    , config :: AppConfig
    , logField :: Object Foreign
    , referralType :: ReferralType
    , showQRCodePopUp :: Boolean
    , referralComponentProps :: ReferralComponentState
  }


type EstimatesAndQuotesInfo = {
    defaultQuote :: ChooseVehicle.Config
  , nearByDrivers :: Maybe Int
  , zoneType :: SpecialTags
  , hasToll :: Boolean
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
  emergencyContactsList :: Array NewContacts,
  storedContactsList :: Array NewContacts,
  selectedContacts :: Array NewContacts,
  selectedContact :: NewContacts,
  searchResult :: Array NewContacts,
  prestoListArrayItems :: Array NewContactsProp,
  loadMoreDisabled :: Boolean,
  removedContactDetail :: NewContacts,
  offsetForEmergencyContacts :: Int,
  limitForEmergencyContacts :: Int,
  editedText :: String,
  logField :: Object Foreign,
  manualContactName :: String,
  manualContactNumber :: String
}

type EmergencyContactsScreenProps = {
  showContactList :: Boolean,
  validManualContact :: Boolean,
  validManualName :: Boolean,
  showAddContactOptions :: Boolean,
  addContactsManually :: Boolean,
  showInfoPopUp :: Boolean,
  fromSosFlow :: Boolean,
  fromNewSafetyFlow :: Boolean,
  appName :: String,
  showDropDown :: Boolean,
  getDefaultContacts :: Boolean,
  saveEmergencyContacts :: Boolean,
  isKeyBoardOpen :: Boolean
}

type ContactDetail = {
  name :: String,
  phoneNumber :: String
}

-- ############################################## AboutUsScreenState #############################

type AboutUsScreenState = {
    appConfig :: AppConfig,
    props :: AboutUsScreenProps
}

type AboutUsScreenProps = {
    enableDemoModeCount :: Int,
    demoModePopup :: Boolean
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
  isSpecialAssistList :: Boolean,
  profileLoaded :: Boolean
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

type DriverInfoCard =
  { otp :: String
  , driverName :: String
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
  , initialPickupLat :: Number
  , initialPickupLon :: Number
  , destinationLat :: Number
  , destinationLng :: Number
  , driverLat :: Number
  , driverLng :: Number
  , distance :: Int
  , waitingTime :: String
  , driverArrived :: Boolean
  , estimatedDistance :: String
  , driverArrivalTime :: Int
  , destinationReached :: Boolean
  , destinationReachedAt :: Int
  , bppRideId :: String
  , driverNumber :: Maybe String
  , merchantExoPhone :: String
  , createdAt :: String
  , initDistance :: Maybe Int
  , config :: AppConfig
  , vehicleVariant :: String
  , sourceAddress :: Address
  , destinationAddress :: Address
  , editPickupAttemptsLeft :: Int
  , status :: String
  , serviceTierName :: Maybe String
  , vehicleModel :: String
  , vehicleColor :: String
  , providerType :: CTA.ProviderType
  , providerName :: String
  , rentalData :: CTA.RentalBookingConfig
  , fareProductType :: FareProductType
  , driversPreviousRideDropLocLat :: Maybe Number
  , driversPreviousRideDropLocLon :: Maybe Number
  , spLocationName :: Maybe String
  , addressWard :: Maybe String
  , currentChatRecipient :: ChatContacts
  , hasToll :: Boolean
  , isAirConditioned :: Maybe Boolean
  , isAlreadyFav :: Boolean
  , favCount :: Int
  , rideDuration :: Maybe Int
  , rideScheduledAtUTC :: Maybe String
  , senderDetails :: Maybe PersonDeliveryDetails
  , receiverDetails :: Maybe PersonDeliveryDetails
  , estimatedTimeToReachDestination :: Maybe String 
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
  , feedbackList :: Array CTA.FeedbackAnswer
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

type FavouriteDriverTripsState =
  {
    data :: FavouriteDriverTripsData
  }

type FavouriteDriverTripsData =
  {
    driverNumber :: String
  , driverName :: String
  , driverId :: Maybe String
  , details :: Array Details 
  }

type Details =
  {    
    rideRating :: Maybe Int
  , fromLocation :: LocationAPIEntity
  , toLocation :: Maybe LocationAPIEntity
  , totalFare :: Maybe Int
  , startTime :: Maybe String
  , id :: Maybe String
  }

type SavedLocationScreenData =
  {
    savedLocations :: Array LocationListItemState
  , deleteTag :: Maybe String
  , config :: AppConfig
  , logField :: Object Foreign
  , favouriteDriversList :: Array FavouriteDriverListItemState
  , current :: String 
  , driverNo :: String
  , driverName :: String
  , driverId :: Maybe String
  }

type FavouriteDriverListItemState = {
    driverName :: String
  , driverPhone :: String
  , driverRating :: Number
  , favCount :: Int
  , id :: Maybe String
}

data FavouriteDriverTripsType = SavedLocations

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
  , fromScreen :: String
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

data SearchResultType = QUOTES FareProductType | ESTIMATES

derive instance genericSearchResultType :: Generic SearchResultType _
instance eqSearchResultType :: Eq SearchResultType where eq = genericEq
instance showSearchResultType :: Show SearchResultType where show = genericShow
instance encodeSearchResultType :: Encode SearchResultType where encode = defaultEncode
instance decodeSearchResultType:: Decode SearchResultType where decode = defaultDecode

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
  , dynamicAction :: Maybe CRT.RemoteAC
  , types :: Maybe (Array String)
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
  , vehicleVariant :: Maybe String
  , serviceTierNameV2 :: Maybe String
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
  , sourceLabelIcon :: Maybe String
  , destLabelIcon :: Maybe String
  , sourceGeoJson :: Maybe String
  , sourceGates :: Maybe (Array Location)
}

derive instance genericFlowStatusData :: Generic FlowStatusData _
instance showFlowStatusData :: Show FlowStatusData where show = genericShow
instance encodeFlowStatusData :: Encode FlowStatusData where encode = defaultEncode
instance decodeFlowStatusData :: Decode FlowStatusData where decode = defaultDecode

data ZoneType = METRO
              | SHOPPING_MALL
              | HOSPITAL
              | AIRPORT
              | SCHOOL
              | RAILWAY
              | NOZONE
              | AUTO_BLOCKED
              | SPECIAL_PICKUP
              | HOTSPOT Boolean

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

type TicketServiceData =
  { id :: String,
    serviceName :: String,
    allowFutureBooking :: Boolean,
    shortDesc :: Maybe String,
    expiry :: ServiceExpiry,
    isExpanded :: Boolean,
    serviceCategories :: Array ServiceCategory,
    selectedBHId :: Maybe String
  }

type ServiceCategory =
  { categoryId :: String,
    categoryName :: String,
    availableSeats :: Maybe Int,
    allowedSeats :: Maybe Int,
    bookedSeats :: Int,
    isSelected :: Boolean,
    isClosed :: Maybe Boolean,
    peopleCategories :: Array PeopleCategoriesData,
    operationalDays :: Array OperationalDaysData,
    operationalDate :: Maybe OperationalDate,
    validOpDay :: Maybe OperationalDaysData
  }

type OperationalDate = {
  startDate :: String,
  endDate :: String
}

type FlattenedBusinessHourData =
  { id :: String,
    slot :: Maybe String, -- array of slots
    startTime :: Maybe String,
    endTime :: Maybe String,
    specialDayDescription :: Maybe String,
    specialDayType :: Maybe String,
    operationalDays :: Array String,
    operationalDate :: Maybe OperationalDate,
    category :: TicketCategoriesResp
  }

type PeopleCategoriesData =
  { peopleCategoryName :: String,
    pricePerUnit :: Int,
    currentValue :: Int,
    peopleCategoryId :: String,
    ticketLimitCrossed :: Boolean
  }

type OperationalDaysData = 
  { operationalDays :: Array String,
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

type TiketingListTransformedData =
  { timings :: Array KeyVal,
    fees :: Array KeyVal
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

type KeyVal2 = {
  key :: Array String,
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
    booked :: Array TicketBookingItem,
    cancelled :: Array TicketBookingItem
  }

type TicketBookingScreenProps = {
  currentStage :: TicketBookingScreenStage,
  previousStage :: TicketBookingScreenStage,
  termsAndConditionsSelected :: Boolean,
  validDate :: Boolean,
  showShimmer :: Boolean,
  paymentStatus :: PP.PaymentStatus,
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
    data :: RideScheduledScreenData,
    props :: RideScheduledScreenProps
}

type RideScheduledScreenData = {
  primaryButtonText :: String
  , source :: LocationInfo
  , destination :: Maybe LocationInfo
  , startDate :: String
  , startTime :: String
  , finalPrice :: String
  , baseDuration :: String
  , baseDistance :: String
  , bookingId :: String
  , cancellationReasons :: Array CTA.OptionButtonList
  , config :: AppConfig
  , fareProductType :: FareProductType
  , fromScreen :: String
}

type RideScheduledScreenProps = {
    cancelRideActiveIndex :: Maybe Int
  , isCancelRide :: Boolean
  , cancelDescription :: String 
  , cancelReasonCode :: String
  , driverAllocationTime :: String

}
-- ######################################### SearchLocationScreenState ####################################################

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
, bookingId :: String
, city :: CTA.City
, bookingUpdatedAt :: String
, metroRoute :: Array MetroRoute
, ticketsInfo :: Array MetroTicketInfo
, ticketType :: String
, ticketPrice :: Number
, noOfTickets :: Int
, vehicleType :: String
, route :: Maybe (Array FRFSRouteAPI)
, transactionId :: String
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
, isBookingCancellable :: Maybe Boolean
, cancellationCharges :: Maybe Number
, refundAmount :: Maybe Number
, showLoader :: Boolean
, fromScreen :: Maybe String
, paymentDetailsExpanded :: Boolean
}

data PreviousMetroTicketDetailsStage = MetroMyTicketsStage 
                                     | SearchMetroLocationStage 
                                     | MetroTicketSelectionStage
                                     | MetroTicketStatusStage

derive instance genericPreviousMetroTicketDetailsStage :: Generic PreviousMetroTicketDetailsStage _                                  
instance showPreviousMetroTicketDetailsStage :: Show PreviousMetroTicketDetailsStage where show = genericShow
instance eqPreviousMetroTicketDetailsStage :: Eq PreviousMetroTicketDetailsStage where eq = genericEq 

data MetroTicketDetailsScreenStage = MetroTicketDetailsStage 
                                   | MetroMapStage 
                                   | MetroRouteDetailsStage
                                   | MetroSoftCancelStatusStage
                                   | MetroHardCancelStatusStage
                                   | MetroBookingCancelledStage

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
, userBlocked :: Boolean
}

type MetroMyTicketsScreenProps = {
  dummyProps :: String
, showShimmer :: Boolean
, entryPoint :: MetroMyTicketsEntry
, fromScreen :: Maybe String
, ticketServiceType :: API.TicketServiceType
}

type MetroTicketCardData = {
  sourceName :: String
  , destinationName :: String
  , createdAt :: String
  , noOfTickets :: Int
  , metroTicketStatusApiResp :: FRFSTicketBookingStatusAPIRes
  , status :: String
  , validUntill :: String
}

data MetroMyTicketsEntry = HomeScreenToMetroMyTickets | MetroTicketBookingToMetroMyTickets


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
  paymentStatus :: PP.PaymentStatus,
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
                       | BusTicketToPaymentStatusEntry
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
    route :: Maybe Route,
    currentLoc :: LocationInfo,
    locationList :: Array LocationListItemState,
    fromScreen :: String,
    saveFavouriteCard :: SaveFavouriteCardState,
    latLonOnMap :: LocationInfo,
    selectedQuote :: Maybe QuotesList,
    defaultGate :: String,
    nearByGates :: Array Location,
    specialZoneCoordinates :: String,
    confirmLocCategory :: ZoneType,
    metroStations :: Array Station,
    updatedMetroStations :: Array Station,
    predictionSelectedFromHome :: LocationListItemState,
    quotesList :: Array QuotesList,
    rideDetails :: RideDetails,
    listItem :: Maybe ListItem,
    routeSearchedList :: Array FRFSRouteAPI,
    stopsSearchedList :: Array FRFSStationAPI,
    updatedStopsSearchedList :: Array FRFSStationAPI,
    updatedRouteSearchedList :: Array FRFSRouteAPI,
    ticketServiceType :: API.TicketServiceType,
    rideType :: RideType,
    searchRideType :: API.SearchRideType
  }

type RideDetails = {
  searchId :: String ,
  rideDistance :: Int ,
  rideDuration :: Int ,
  rideScheduledDate :: String,
  rideScheduledTime :: String,
  rideScheduledTimeUTC :: String
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
  , isSpecialZone :: Boolean
  , isAutoComplete :: Boolean
  , pickUpSelectedOnMap :: Boolean
  , showRateCard :: Boolean 
  , tipViewProps :: TipViewProps 
  , customerTip :: CustomerTipProps
  , fareProductType :: FareProductType
  , currentEstimateHeight :: Int
  , routeSearch :: Boolean
  , routeSelected :: String
  , stopCodeSelected :: String
  , stopNameSelected :: String
  , autoCompleteBusStop :: Boolean
  , srcLat :: Number 
  , srcLong :: Number
  , routeName :: String
  , selectedEstimateHeight :: Int }

data SearchLocationActionType = AddingStopAction 
                              | SearchLocationAction
                              | MetroStationSelectionAction
                              | BusStationSelectionAction
                              | BusSearchSelectionAction
                              | BusRouteSelectionAction
                              | BusStopSelectionAction
                              | NoBusRouteSelectionAction
                              

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
                          | PredictionSelectedFromHome
                          | ChooseYourRide

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
    metroInfo :: Maybe Station,
    busStopInfo :: Maybe Station,
    stationCode :: String,
    city :: CTA.City
  }
  
data RideType = ROUTES | STOP

derive instance genericRideType :: Generic RideType _
instance eqRideType :: Eq RideType where eq = genericEq
instance showRideType :: Show RideType where
  show ROUTES = "Routes"
  show STOP = "Stops"
-- ############################################## NammaSafetyScreenState #############################


data SafetySetupStage =  SetNightTimeSafetyAlert
                        | SetDefaultEmergencyContacts
                        | SetPersonalSafetySettings

data NammaSafetyStage = 
    TrustedContacts (Array SafetyStageConfig)
  | SafetyCheckIn (Array SafetyStageConfig)
  | EmergencyActions (Array SafetyStageConfig)
  | SafetyDrill (Array SafetyStageConfig)
  | TrustedContactsActions (Array SafetyStageConfig)
  | DriverSafetyStandards (Array SafetyStageConfig)

data Component --TODO:: Discuss if this concept can be used and moved to a new component file all the components can be defined :)
  = BoxContainer BoxContainerConfig
  | DropDownWithHeader DropDownWithHeaderConfig
  | NoteBox NoteBoxConfig
  | Title TitleConfig
  | SubTitle SubTitleConfig
  | CheckBoxSelection CheckBoxSelectionConfig 
  | ImageComponent ImageComponentConfig

type CheckBoxSelectionConfig = {
  title :: String,
  contacts :: Array NewContacts,
  selectedContact :: NewContacts
}

type ImageComponentConfig = {
  imageUrl :: String
}

type BoxContainerConfig = {
  title :: String,
  subTitle :: String,
  toggleButton :: Boolean,
  noteText :: String,
  noteImageIcon :: String
}

type NoteBoxConfig = {
  noteText :: String,
  noteImageIcon :: String,
  noteSubTitle :: String
}

type TitleConfig = {
  titleText :: String
}

type SubTitleConfig = {
  subTitleText :: String
}

type DropDownWithHeaderConfig = {
  headerText :: String,
  currentValue :: DropDownOptions,
  dropDownItems :: Array DropDownOptions
}

type SafetyStageConfig = {
  dynamicViewData :: Array Component,
  imageUrl :: String,
  primaryButtonText :: String,
  primaryButtonAction :: String
}

type DropDownOptions = {
  key :: RideShareOptions,
  value :: String
}

derive instance genericSafetySetupStage :: Generic SafetySetupStage _
instance eqSafetySetupStage :: Eq SafetySetupStage where eq = genericEq
instance showSafetySetupStage :: Show SafetySetupStage where show = genericShow

type NammaSafetyScreenState = {
  data :: NammaSafetyScreenData,
  props :: NammaSafetyScreenProps
}

type NammaSafetyScreenData =  {
  shareToEmergencyContacts :: Boolean,
  nightSafetyChecks :: Boolean,
  settingsAPIResponse :: API.GetEmergencySettingsRes,
  hasCompletedMockSafetyDrill :: Boolean,
  shareTripWithEmergencyContactOption :: RideShareOptions,
  shareOptionCurrent :: RideShareOptions,
  hasCompletedSafetySetup :: Boolean,
  emergencyContactsList :: Array NewContacts,
  sosId :: String,
  rideId :: String,
  videoPath :: String,
  updateActionType :: String,
  removedContactDetail :: NewContacts,
  currentLocation :: String,
  vehicleDetails :: String,
  videoList :: Array RC.SafetyVideoConfig,
  sosType :: Maybe SosFlow,
  config :: AppConfig,
  lastRideDetails :: Maybe IndividualRideCardState,
  bannerData :: BannerCarousalData,
  safetySetupSteps :: Array SafetyStepsConfig,
  extraSafetyExplaination :: Array SafetyStepsConfig,
  autoCallDefaultContact :: Boolean,
  currentLatLon :: Maybe LatLong
 }

type NammaSafetyScreenProps =  {
  setupStage :: SafetySetupStage,
  confirmPopup :: Boolean,
  timerId :: String,
  timerValue :: Int,
  enableLocalPoliceSupport :: Boolean,
  isFromSafetyCenter :: Boolean,
  showInfoPopUp :: Boolean,
  localPoliceNumber :: String,
  showShimmer :: Boolean,
  showTestDrill :: Boolean,
  triggeringSos :: Boolean,
  confirmTestDrill :: Boolean,
  educationViewIndex :: Maybe Int,
  showCallPolice :: Boolean,
  shouldCallAutomatically :: Boolean,
  fromDeepLink :: Boolean,
  showRideShareOptionsPopup :: Boolean,
  showVideoView :: Boolean,
  isSafetyCenterDisabled :: Boolean,
  fromBannerLink :: Boolean,
  showPastRidePopUp :: Boolean,
  checkPastRide :: Boolean,
  reportPastRide :: Boolean,
  appName :: String,
  isOffUs :: Boolean,
  triggerSiren :: Boolean,
  isAudioRecordingActive :: Boolean,
  audioRecordingStatus :: CTA.RecordingState,
  recordingTimerId :: String,
  recordingTimer :: String,
  recordedAudioUrl :: Maybe String,
  showMenu :: Boolean,
  policeCallTimerValue :: Int,
  policeCallTimerId :: String,
  defaultCallPopup :: Boolean,
  fromScreen :: Maybe TripDetailsGoBackType
}

type DataFetchScreenState = {
  data :: DataFetchScreenData,
  props :: DataFetchScreenProps,
  config :: DataFetchScreenConfigs
}

type DataFetchScreenConfigs = {
  appConfig :: AppConfig,
  stage :: NammaSafetyStage,
  stageSteps :: Int,
  currentStep :: Int
}

type DataFetchScreenData = {
  headerValue :: String,
  bannerData :: BannerCarousalData,
  emergencyContactsList :: Array NewContacts,
  defaultSelectedContact :: NewContacts,
  unExpectedEventChecks :: RideShareOptions,
  postRideCheck :: RideShareOptions,
  notifySafetyTeam :: Boolean,
  emergencySOSShake :: Boolean,
  autoCallDefaultContact :: Boolean,
  informPoliceSos :: Boolean,
  notifySosWithEmergencyContacts :: Boolean,
  hasCompletedMockSafetyDrill :: Boolean
}

type DataFetchScreenProps = {
    showLoader :: Boolean
  , showDropDown :: Boolean
  , dropDownAction :: String
  , stageSetUpCompleted :: Boolean
}

type SafetyStepsConfig
  = { title :: STR
    , prefixImage :: String
    , stepNumber :: String
    , isCompleted :: Boolean
    , prefixImageCompleted :: String
    , labelText :: Maybe STR
    , navigation :: NammaSafetyStage
    }


data FollowRideScreenStage = PersonList | FollowingRide | ChatWithEM | MockFollowRide | RideCompletedStage

derive instance genericFollowRideScreenStage :: Generic FollowRideScreenStage _
instance showFollowRideScreenStage :: Show FollowRideScreenStage where show = genericShow
instance eqFollowRideScreenStage :: Eq FollowRideScreenStage where eq = genericEq

type FollowRideScreenState = {
  data :: FollowRideScreenData,
  props :: FollowRideScreenProps
}

type FollowRideScreenData = {
  driverInfoCardState :: Maybe DriverInfoCard
, currentStage :: FollowRideScreenStage
, currentFollower :: Maybe Followers
, followers :: Array Followers
, zoneType :: SpecialTags
, route :: Maybe Route
, speed :: Int
, config :: AppConfig
, messages :: Array ChatComponentConfig
, messagesSize :: String
, chatSuggestionsList :: Array String
, lastMessage :: ChatComponentConfig
, lastSentMessage :: ChatComponent
, lastReceivedMessage :: ChatComponent
, logField :: Object Foreign
, messageToBeSent:: String
, sosStatus :: Maybe CTA.SosStatus
, emergencyAudioStatus :: EmAudioPlayStatus
, counter :: Int
}

type FollowRideScreenProps = {
  city :: CTA.City
, showChatNotification :: Boolean
, canSendSuggestion :: Boolean
, isChatNotificationDismissed :: Boolean
, unReadMessages :: Boolean
, removeNotification :: Boolean
, enableChatWidget :: Boolean
, chatCallbackInitiated :: Boolean
, openChatScreen:: Boolean
, sendMessageActive :: Boolean
, sheetState :: Maybe BottomSheetState
, currentSheetState :: BottomSheetState
, isNotificationExpanded :: Boolean
, startMapAnimation :: Boolean
, isRideStarted :: Boolean
, isMock :: Boolean
, currentUserOnRide :: Boolean
}


data EmAudioPlayStatus = STOPPED | STARTED | COMPLETED | RESTARTED

derive instance genericEmAudioPlayStatus :: Generic EmAudioPlayStatus _
instance eqEmAudioPlayStatus :: Eq EmAudioPlayStatus where eq = genericEq

type ReferralStatusProp = {
  referralStatus :: ReferralStatus,
  referralCode :: Maybe String,
  showAddReferralPopup :: Boolean
}

data ReferralStatus = NO_REFERRAL | REFERRAL_APPLIED | REFERRAL_INVALID | REFERRAL_ALREADY_APPLIED

derive instance genericReferralStatus :: Generic ReferralStatus _
instance eqReferralStatus :: Eq ReferralStatus where eq = genericEq

data ReferralType = GIVE_REFERRAL | GET_REFERRED

derive instance genericReferralType :: Generic ReferralType _
instance eqReferralType :: Eq ReferralType where eq = genericEq

type MetroStation = {
  code :: String
  , name :: String
  , lat :: Maybe Number
  , lon :: Maybe Number
  , address :: Maybe String
  , stationType :: Maybe String
  , color :: Maybe String
  , distance :: Maybe Int
  , sequenceNum :: Maybe Int
}

type MetroStations = {
  city :: CTA.City,
  stations :: Array FRFSStationAPI,
  lastUpdatedAt :: String
}

-- ######################################### MetroTicketBookingScreenState ####################################################

type MetroTicketBookingScreenState = {
  data :: MetroTicketBookingScreenData,
  props :: MetroTicketBookingScreenProps, 
  config :: AppConfig
}

type MetroTicketBookingScreenData = {
  ticketType :: TicketType
  , ticketCount :: Int
  , srcLoc :: String
  , destLoc :: String
  , srcCode :: String
  , destCode :: String
  , searchId :: String
  , ticketPrice :: Number
  , bookingId :: String
  , quoteId :: String
  , quoteResp :: Array FrfsQuote
  , routeSearchedList :: Array FRFSRouteAPI
  , routeList :: Array FRFSRouteAPI
  , stopsSearchedList :: Array FRFSStationAPI
  , metroBookingConfigResp :: FRFSConfigAPIRes
  , eventDiscountAmount :: Maybe Int
  , searchRideType :: API.SearchRideType
  , discounts :: Array API.FRFSDiscountRes
  , applyDiscounts :: Maybe (Array API.FRFSDiscountReq)
}

type MetroTicketBookingScreenProps = {
  isLimitExceeded :: Boolean
, termsAndConditionsSelected :: Boolean
, currentStage :: TicketBookingStage
, isButtonActive :: Boolean
, showMetroBookingTimeError :: Boolean
, showShimmer :: Boolean
, busClicked :: Boolean
, routeList :: Boolean
, showRouteOptions :: Boolean
, isEmptyRoute :: String
, ticketServiceType :: API.TicketServiceType
, srcLat :: Number 
, srcLong :: Number
, routeName :: String
, isRepeatRide :: Boolean
}

data TicketBookingStage = MetroTicketSelection | GetMetroQuote | ConfirmMetroQuote | PaymentSDKPooling | BusTicketSelection | OfferSelection

derive instance genericTicketBookingStage :: Generic TicketBookingStage _
instance eqMetroTicketBookingStage :: Eq TicketBookingStage where eq = genericEq
instance showTicketBookingStage :: Show TicketBookingStage where show = genericShow

data TicketType = ONE_WAY_TICKET | ROUND_TRIP_TICKET

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
  validUntil :: String,
  bookingId :: String,
  resp :: FRFSTicketBookingStatusAPIRes,
  timerId :: String,
  quoteId :: String
}

type MetroTicketStatusScreenProps = {
  showShimmer :: Boolean
, paymentStatus :: PP.PaymentStatus
, entryPoint :: MetroTicketStatusScreenEntry
}

data MetroTicketStatusScreenEntry = HomescreenToMetroTicketStatus | MyMetroTicketsToMetroTicketStatus | BusTicketToMetroTicketStatus

type RideSearchProps = {
    sessionId :: String
  , sourceManuallyMoved :: Boolean
  , destManuallyMoved :: Boolean
  , autoCompleteType :: Maybe AutoCompleteReqType
  , sourceSelectType :: LocationSelectType
  , cachedPredictions :: DHM.HashMap String (Array LocationListItemState)
}

data AutoCompleteReqType = PICKUP | DROP
derive instance genericAutoCompleteReqType :: Generic AutoCompleteReqType _
instance showAutoCompleteReqType :: Show AutoCompleteReqType where show = genericShow

data LocationType = Source | Destination 
derive instance genericLocationType :: Generic LocationType _
instance eqLocationType :: Eq LocationType where eq = genericEq

type GlobalFlowCache = {
    savedLocations :: Maybe SavedLocationsListRes
  , savedScheduledRides :: Maybe RideBookingListRes
}

type LocateOnMapProps = {
    sourceLocationName :: Maybe String
  , sourceGeoJson :: Maybe String
  , sourceGates :: Maybe (Array Location)
  , isSpecialPickUpGate :: Boolean
  , cameraAnimatedToSource :: Boolean
}

data NavigationMode = WALK | DRIVE

derive instance genericNavigationMode :: Generic NavigationMode _
instance showNavigationMode :: Show NavigationMode where show = genericShow

type HotSpotProps = {
    selectedSpot :: Maybe Location
  , centroidPoint :: Maybe CTA.Paths
}

type HotSpotData = {
    lat :: Number
  , lon :: Number
}

data LocationSelectType = SEARCH | MAP | FAVOURITE | REPEAT_RIDE | RETRY_SEARCH | SUGGESTION

derive instance genericLocationSelectType :: Generic LocationSelectType _
instance eqLocationSelectType :: Eq LocationSelectType where eq = genericEq

type ReferralComponentState =
  { stage :: ReferralStage
  , referralCode :: Maybe String
  , applyButtonActive :: Boolean
  , showReferredUserInfoPopup :: Boolean
  , showReferralProgramInfoPopup :: Boolean
  , isInvalidCode :: Boolean
  }

data ReferralStage = ENTER_REFERRAL_CODE
                   | INVALID_POPUP
                   | APPLIED_POPUP
                   | ALREADY_APPLIED_POPUP
                   | NO_REFERRAL_STAGE

derive instance genericReferralStage :: Generic ReferralStage _
instance eqReferralStage :: Eq ReferralStage where eq = genericEq

type SpecialLocationMap = DM.Map String SpecialLocationList

type SpecialLocationList = {
    geoJson :: String
  , gates :: Array Location
  , locationName :: String
  , category :: String
  , city :: String
}

type SpecialZoneTagConfig = {
    icon :: String
  , text :: String
  , infoPopUpConfig :: Maybe SpecialZoneInfoPopUp
  , backgroundColor :: String
}

type SpecialZoneInfoPopUp = {
    title :: String
  , primaryText :: String
  , secondaryText :: String
  , primaryButtonText :: String
  , icon :: String
}

-- ######################################### RentalScreenState ####################################################

type RentalScreenState = {
  data :: RentalScreenData,
  props :: RentalScreenProps
}

type RentalScreenData = {
    rentalBookingData :: CTA.RentalBookingConfig
  , startTimeUTC :: String
  , currentStage :: RentalScreenStage
  , rentalsQuoteList :: Array QuotesList
  , selectedQuote :: Maybe QuotesList
  , endOTP :: Maybe String
  , nextStop :: Maybe String
  , selectedDateTimeConfig :: DateTimeConfig
  , pickUpLoc :: LocationInfo 
  , dropLoc :: Maybe LocationInfo
  , searchId :: String
  , bookingId :: String
  , config :: AppConfig
  , latestScheduledRides :: Maybe RideBookingListRes
  , overLappingBooking :: Maybe RideBookingRes
}

type QuotesList = {
  quoteDetails :: ChooseVehicle.Config,
  index :: Int,
  activeIndex :: Int,
  fareDetails :: FareDetails
}

type IntercityFareDetails = {
  perDayMaxHourAllowance :: Int,
  baseFare :: Maybe PriceAPIEntityDecimals ,
  kmPerPlannedExtraHour :: Maybe DistanceWithUnit ,
  perExtraKmRate :: Maybe PriceAPIEntityDecimals ,
  perExtraMinRate :: Maybe PriceAPIEntityDecimals ,
  perHourCharge :: Maybe PriceAPIEntityDecimals ,
  plannedPerKmRateOneWay :: Maybe PriceAPIEntityDecimals ,
  plannedPerKmRateRoundTrip :: Maybe PriceAPIEntityDecimals,
  tollCharges :: Maybe PriceAPIEntityDecimals ,
  deadKmFare :: Maybe PriceAPIEntityDecimals
}
type FareDetails = {
  plannedPerKmRate ::  Int,
  baseFare :: Int,
  includedKmPerHr :: Int,
  perExtraKmRate :: Int,
  perExtraMinRate :: Int,
  perHourCharge :: Int,
  nightShiftCharge :: Int,
  tollCharges :: Maybe Number,
  deadKmFare :: Maybe DeadKmFare
}

data RentalScreenStage = RENTAL_SELECT_PACKAGE | RENTAL_SELECT_VARIANT | RENTAL_CONFIRMATION

derive instance genericRentalScreenStage :: Generic RentalScreenStage _
instance showRentalScreenStage :: Show RentalScreenStage where show = genericShow
instance eqRentalScreenStage :: Eq RentalScreenStage where eq = genericEq

type RentalScreenProps = {
    minDuration :: Int
  , maxDuration :: Int
  , minDistance :: Int
  , maxDistance :: Int
  , farePerKm :: String
  , maxDateBooking :: Int
  , showRateCard :: Boolean
  , showShimmer :: Boolean
  , showPrimaryButton :: Boolean
  , showPopUpModal :: Boolean
  , showRentalPolicy :: Boolean
  , isOtpRideFlow :: Boolean
  , showScheduledRideExistsPopUp :: Boolean
}

type DateTimeConfig = {
    year :: Int
  , month :: Int
  , day :: Int
  , hour :: Int
  , minute :: Int
}

----------------------------------------------------------------------

data FareProductType = RENTAL | INTER_CITY | ONE_WAY | ONE_WAY_SPECIAL_ZONE | DRIVER_OFFER | AMBULANCE | DELIVERY

derive instance genericFareProductType :: Generic FareProductType _
instance showFareProductType :: Show FareProductType where show = genericShow
instance eqFareProductType :: Eq FareProductType where eq = genericEq
instance encodeFareProductType :: Encode FareProductType where encode = defaultEncode
instance decodeFareProductType :: Decode FareProductType where decode = defaultDecode

data CancelSearchType = NORMAL_RIDE_CANCEL | RENTAL_SEARCH_CANCEL

derive instance genericCancelSearchType :: Generic CancelSearchType _
instance showCancelSearchType :: Show CancelSearchType where show = genericShow
instance eqCancelSearchType :: Eq CancelSearchType where eq = genericEq

data VehicleViewType = LEFT_VIEW | RIGHT_VIEW

derive instance genericVehicleViewType :: Generic VehicleViewType _
instance showVehicleViewType :: Show VehicleViewType where show = genericShow
instance eqVehicleViewType :: Eq VehicleViewType where eq = genericEq

type PickupInstructionsScreenState = {
  data :: PickupInstructionsScreenData,
  props :: PickupInstructionsScreenProps
}

type PickupInstructionsScreenData = {
  pickupLat :: Number,
  pickupLong :: Number,
  pickupInstructions :: Array RC.PickupInstructions
}

type PickupInstructionsScreenProps = {
  
}

-- ######################################### SelectFaqScreenState ####################################################


type SelectFaqScreenState =
  {
    data :: SelectFaqScreenData,
    props :: SelectFaqScreenProps
  }

type SelectFaqScreenData =
  {
    config :: AppConfig,
    issueList :: Array IssueInfo,
    issueListType :: IssueModalType,
    categories :: Array CTA.CategoryListType,
    categoryName :: String
  }

type SelectFaqScreenProps =
  {
    apiFailure :: Boolean
  , needIssueListApiCall :: Boolean
  }

-- ######################################### FaqScreenState ####################################################

type FaqScreenState =
  {
    data :: FaqScreenData,
    props :: FaqScreenProps
  }

type FaqScreenData =
  {
    config :: AppConfig,
    dropDownList :: Array CTA.FaqCardDropDownInfo,
    issueListType :: IssueModalType,
    categories :: Array CTA.CategoryListType,
    categoryName :: String,
    maxAllowedRideAge :: Maybe Int
  }

type FaqScreenProps =
  {
    apiFailure :: Boolean
  , needIssueListApiCall :: Boolean
  }


-- ######################################### ParcelDeliveryFlow ####################################################

data ParcelDeliveryScreenStage = DELIVERY_INSTRUCTIONS | SENDER_DETAILS | RECEIVER_DETAILS | FINAL_DETAILS

derive instance genericParcelDeliveryScreenStage :: Generic ParcelDeliveryScreenStage _
instance showParcelDeliveryScreenStage :: Show ParcelDeliveryScreenStage where show = genericShow
instance eqParcelDeliveryScreenStage :: Eq ParcelDeliveryScreenStage where eq = genericEq

type ParcelDeliveryScreenState = {
  data :: ParcelDeliveryScreenData,
  props :: ParcelDeliveryScreenProps
}

type ParcelDeliveryScreenData = {
    currentStage :: ParcelDeliveryScreenStage
  , sourceAddress :: Address
  , destinationAddress :: Address
  , route :: Maybe Route
  , sourceLat :: Number
  , sourceLong :: Number
  , destinationLat :: Number
  , destinationLong :: Number
  , senderDetails :: PersonDeliveryDetails
  , receiverDetails :: PersonDeliveryDetails
  , initiatedAs :: API.InitiatedAs
  , selectedQuote :: Maybe QuotesList
  , parcelQuoteList :: ChooseVehicle.Config
  , rateCard :: CTA.RateCard
  , config :: AppConfig
  , tipForDriver :: Maybe Int
}

type ParcelDeliveryScreenProps = {
  editDetails :: PersonDeliveryDetails,
  showRateCard :: Boolean,
  isEditModal :: Boolean,
  focusField :: String,
  isValidInputs :: Boolean
}

type PersonDeliveryDetails = {
  name :: String,
  phone :: String,
  extras :: String,
  instructions :: Maybe String
}


newtype BookingAPIEntity = BookingAPIEntity {
  currency :: Currency,
  estimatedDistance ::Maybe String,
  estimatedDuration ::Maybe Int,
  estimatedFare :: String ,
  fromLocation :: LocationInformation,
  id :: String,
  isAirConditioned ::Maybe Boolean ,
  isScheduled :: Boolean,
  returnTime :: Maybe String,
  roundTrip :: Maybe Boolean,
  startTime :: String ,
  toLocation :: Maybe LocationInformation,
  tripCategory :: CTA.TripCategory,
  vehicleServiceTier :: String,
  vehicleServiceTierAirConditioned :: Maybe Number ,
  vehicleServiceTierName :: String,
  vehicleServiceTierSeatingCapacity ::  Maybe Int
}
newtype LocationInformation =  LocationInformation {
  address :: Address,
  placeId  :: String,
  fullAddress :: String
}
data Currency  = INR | USD | EUR 

type NotificationBody = {
  rideTime :: Maybe String,
  bookingId :: Maybe String
}

-- ######################################### BusTicketBookingFlow ####################################################

type BusTicketBookingState = {
  data :: BusTicketBookingData,
  props :: BusTicketBookingProps
}

type BusTicketBookingData = {
    routeList :: Boolean
  , showRouteOptions :: Boolean
  , isEmptyRoute :: String
  , ticketServiceType :: API.TicketServiceType
  , ticketDetailsState :: Maybe MetroMyTicketsScreenState
}

type BusTicketBookingProps = {
   srcLat :: Number
 , srcLong :: Number
 , showAllTickets :: Boolean
}

-- ######################################### MultiModalFlow ####################################################

type BusTrackingScreenState = {
  data :: BusTrackingScreenData,
  props :: BusTrackingScreenProps
}

type BusTrackingScreenData = {
  appConfig :: AppConfig,
  busRouteCode :: String,
  stopsList :: Array FRFSStationAPI,
  sourceStation :: Maybe Station,
  destinationStation :: Maybe Station,
  bookingId :: String,
  previousStopsMap :: DM.Map String FRFSStationAPI,
  vehicleTrackingData :: DM.Map String (Array Number),
  rideType :: Maybe RideType,
  vehicleData :: Array VehicleData,
  stationResponse :: Maybe (Array FRFSStationAPI),
  routeShortName :: String,
  routePts :: Locations
}

type VehicleData
  = { vehicleId :: String
    , nextStop :: String
    , nextStopDistance :: Number
    , vehicleLat :: Number
    , vehicleLon :: Number
    , nextStopLat ::Number
    , nextStopLon ::Number
    , nextStopSequence :: Int
    , nextStopTravelTime :: Maybe Int
    }

type BusTrackingScreenProps = {
  showRouteDetailsTab :: Boolean, 
  expandStopsView :: Boolean, 
  verticalLineHeight :: Int,
  srcLat :: Number,
  srcLon :: Number,
  gotMapReady :: Boolean,
  busNearSourceData :: Maybe VehicleData,
  showShimmer :: Boolean,
  individualBusTracking :: Boolean,
  userAndBuslocationMatchCount :: Int,
  vehicleTrackingId :: Maybe String,
  previousScreen :: PreviousScreenForTracking,
  destinationSequenceNumber :: Maybe Int
}

data PreviousScreenForTracking = PreStopRouteSelection | BusHomeScreen

derive instance genericPreviousScreenForTracking :: Generic PreviousScreenForTracking _
instance eqPreviousScreenForTracking :: Eq PreviousScreenForTracking where eq = genericEq


type AlertWidgetConfig = {}

--------------------------------------------------------------- AadhaarVerificationScreenState -----------------------------------------------------------------------------

type AadhaarVerificationScreenState = {
  data :: EnterAadhaarNumberScreenStateData,
  props :: EnterAadhaarNumberScreenStateProps
}

type EnterAadhaarNumberScreenStateData = {
    aadhaarNumber :: String
  , timer :: String
  , otp :: String
  , driverName :: String
  , driverGender :: String
  , driverDob :: String
}

type EnterAadhaarNumberScreenStateProps = {
  btnActive :: Boolean
, isValid :: Boolean
, resendEnabled :: Boolean
, currentStage :: AadhaarStage
, showErrorAadhaar :: Boolean
, fromHomeScreen :: Boolean
, showLogoutPopup :: Boolean
, isDateClickable :: Boolean
}

data AadhaarStage = EnterAadhaar | VerifyAadhaar | AadhaarDetails

derive instance genericAadhaarStage :: Generic AadhaarStage _
instance eqAadhaarStage :: Eq AadhaarStage where eq = genericEq
