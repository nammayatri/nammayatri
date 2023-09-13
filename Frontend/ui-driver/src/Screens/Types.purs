{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.Types where

import Common.Types.App (OptionButtonList)
import Components.ChooseVehicle.Controller (Config) as ChooseVehicle
import Data.Generic.Rep (class Generic)
import Data.Eq.Generic (genericEq)
import Data.Show.Generic (genericShow)
import Data.Maybe (Maybe)
import Foreign.Class (class Decode, class Encode)
import Halogen.VDom.DOM.Prop (PropValue)
import Prelude (class Eq, class Show)
import Presto.Core.Utils.Encoding (defaultDecode, defaultEncode)
import Presto.Core.Utils.Encoding (defaultEnumDecode, defaultEnumEncode)
import PrestoDOM (Visibility, LetterSpacing)
import Services.APITypes (Route, Status, MediaType)
import Styles.Types (FontSize)
import Components.ChatView.Controller as ChatView
import Components.RecordAudioModel.Controller as RecordAudioModel

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

type NoInternetScreenState =  { }

-- ############################################################# ChooseLanguageScreen ################################################################################

type ChooseLanguageScreenState = {
  data :: ChooseLanguageScreenData,
  props :: ChooseLanguageScreenProps
}

type ChooseLanguageScreenData =  {
  languages :: Array Language,
  isSelected :: Boolean
 }

type ChooseLanguageScreenProps =  {
  selectedLanguage :: String,
  btnActive :: Boolean
 }

-- ############################################################# AddVehicleDetailsScreen ################################################################################

type AddVehicleDetailsScreenState = {
  data :: AddVehicleDetailsScreenData,
  props :: AddVehicleDetailsScreenProps
}

type AddVehicleDetailsScreenData =  {
  vehicle_type :: String,
  vehicle_model_name :: String,
  vehicle_color :: String,
  vehicle_registration_number :: String,
  reEnterVehicleRegistrationNumber :: String,
  rc_base64 :: String,
  vehicle_rc_number :: String,
  referral_mobile_number :: String,
  rcImageID :: String,
  errorMessage :: String,
  dateOfRegistration :: Maybe String,
  dateOfRegistrationView :: String,
  driverMobileNumber :: String
 }

type AddVehicleDetailsScreenProps =  {
  rcAvailable :: Boolean,
  vehicleTypes :: Array VehicalTypes,
  openSelectVehicleTypeModal :: Boolean,
  openRegistrationModal :: Boolean,
  rc_name :: String,
  input_data :: String,
  enable_upload :: Boolean,
  openRCManual :: Boolean,
  openReferralMobileNumber :: Boolean,
  isValid :: Boolean,
  btnActive :: Boolean,
  referralViewstatus :: Boolean,
  isEdit :: Boolean,
  isValidState :: Boolean,
  limitExceedModal :: Boolean,
  errorVisibility :: Boolean,
  openRegistrationDateManual :: Boolean,
  openHowToUploadManual :: Boolean,
  logoutModalView :: Boolean,
  validateProfilePicturePopUp :: Boolean,
  imageCaptureLayoutView :: Boolean,
  fileCameraOption :: Boolean,
  fileCameraPopupModal :: Boolean
 }

data VehicalTypes = Sedan | Hatchback | SUV | Auto

type Language =  {
  name :: String,
  value :: String,
  subtitle :: String
 }

 -- ############################################################# UploadingDrivingLicenseScreen ################################################################################
type UploadDrivingLicenseState = {
  data :: UploadDrivingLicenseStateData,
  props :: UploadDrivingLicenseStateProps

}

type UploadDrivingLicenseStateData = {
  driver_license_number :: String
  , reEnterDriverLicenseNumber :: String
  , imageFront :: String
  , imageBack :: String
  , imageNameFront :: String
  , imageNameBack :: String
  , dob :: String
  , dobView :: String
  , imageIDFront :: String
  , imageIDBack :: String
  , rcVerificationStatus :: String
  , errorMessage :: String
  , dateOfIssue :: Maybe String
  , dateOfIssueView :: String
  , imageFrontUrl :: String
  , mobileNumber :: String
}

type UploadDrivingLicenseStateProps = {
    openRegistrationModal :: Boolean
  , openLicenseManual :: Boolean
  , input_data :: String
  , clickedButtonType :: String
  , openGenericMessageModal :: Boolean
  , errorVisibility :: Boolean
  , openDateOfIssueManual :: Boolean
  , openHowToUploadManual :: Boolean
  , logoutPopupModal :: Boolean
  , validateProfilePicturePopUp :: Boolean
  , imageCaptureLayoutView :: Boolean 
  , fileCameraPopupModal :: Boolean
  , fileCameraOption :: Boolean
}

 -- ############################################################# RegistrationScreen ################################################################################
type RegistrationScreenState = {
  data :: RegistrationScreenData,
  props :: RegistrationScreenProps
}
type RegistrationScreenData = {
  activeIndex :: Int,
  stepsArray :: Array String
}
type RegistrationScreenProps = {}

 -- ############################################################# UploadAdhaarScreen ################################################################################

type UploadAdhaarScreenState = {
  data :: UploadAdhaarScreenData,
  props :: UploadAdhaarScreenProps
}
type UploadAdhaarScreenData = {
  imageFront :: String,
  imageBack :: String,
  imageName :: String
}

type UploadAdhaarScreenProps = {
    openRegistrationModal :: Boolean,
    clickedButtonType :: String
}
 ----------------------------------------------------  PrimaryEditTextState   ------------------------------------------------
type PrimaryEditTextState = {
  title :: String,
  type :: String,
  hint :: String,
  valueId :: String,
  pattern :: Maybe String,
  isinValid :: Boolean,
  error :: Maybe String,
  text :: String,
  fontSize :: FontSize,
  letterSpacing :: LetterSpacing,
  id :: String
}

----------------------------------------------------- DriverProfileScreen ------------------------------------------------
type DriverProfileScreenState = {
  data :: DriverProfileScreenData,
  props :: DriverProfileScreenProps
}

type DriverProfileScreenData = {
  driverName :: String,
  driverVehicleType :: String,
  driverRating :: Maybe Int,
  base64Image :: String,
  drivingLicenseNo :: String,
  driverMobile :: Maybe String,
  vehicleRegNumber :: String,
  vehicleModelName :: String,
  vehicleColor :: String,
  driverAlternateNumber :: Maybe String,
  capacity :: Int,
  downgradeOptions :: Array String,
  vehicleSelected :: Array VehicleP,
  driverGender :: Maybe String
}

type VehicleP = {
  vehicleName :: String,
  isSelected :: Boolean
}

type DriverProfileScreenProps = {
  logoutModalView :: Boolean,
  showLiveDashboard :: Boolean
}
-----------------------------------------------ApplicationStatusScreen ---------------------------------------
type ApplicationStatusScreenState = {
  data :: ApplicationStatusScreenData,
  props :: ApplicationStatusScreenProps
}
type ApplicationStatusScreenData =  {
  rcVerificationStatus :: String,
  dlVerificationStatus :: String,
  mobileNumber ::  String,
  otpValue :: String,
  activeIndex :: Int,
  stepsArray :: Array String
}
type ApplicationStatusScreenProps =  {
  isSelected :: Boolean,
  onBoardingFailure :: Boolean,
  isVerificationFailed :: Boolean,
  popupview :: Boolean,
  enterMobileNumberView :: Boolean,
  alternateNumberAdded :: Boolean,
  isValidAlternateNumber :: Boolean,
  buttonVisibilty :: Boolean,
  enterOtp :: Boolean,
  isValidOtp :: Boolean,
  isAlternateMobileNumberExists :: Boolean,
  isPermissionGranted :: Boolean,
  logoutModalView :: Boolean,
  lottieStatus :: Boolean
}

--------------------------------------------------------------- EnterMobileNumberScreenState -----------------------------------------------------------------------------
type EnterMobileNumberScreenState = {
  data :: EnterMobileNumberScreenStateData,
  props :: EnterMobileNumberScreenStateProps
}

type EnterMobileNumberScreenStateData = {
    mobileNumber :: String,
    stepsArray :: Array String,
    activeIndex :: Int
}

type EnterMobileNumberScreenStateProps = {
  btnActive :: Boolean,
  isValid :: Boolean,
  mobileNumberEditFocused :: Boolean
}

--------------------------------------------------------------- BankDetailScreenState -----------------------------------------------------------------------------
type BankDetailScreenState = {
  data :: BankDetailScreenStateData,
  props :: BankDetailScreenStateProps
}

type BankDetailScreenStateData =  {
  beneficiaryNumber :: String,
  ifsc :: String
}

type BankDetailScreenStateProps =  {
  openRegistrationModal :: Boolean,
  inputData :: String,
  isBeneficiaryMatching :: Boolean
}

-------------------------------------------------------EnterOTPScreenState ------------------------------
type EnterOTPScreenState = {
  data :: EnterOTPScreenStateData,
  props :: EnterOTPScreenStateProps
}

type EnterOTPScreenStateData = {
  otp :: String,
  tokenId :: String,
  attemptCount :: Int,
  mobileNo :: String,
  timer :: String,
  capturedOtp :: String,
  focusedIndex :: Int,
  editTextId :: String
}

type EnterOTPScreenStateProps = {
  btnActive :: Boolean,
  isValid :: Boolean,
  resendEnabled :: Boolean,
  otpTmp :: Boolean
}

---------------------PrimaryButtonState----------------------------------------
type PrimaryButtonState = {
  title :: String,
  active :: Boolean,
  size :: Boolean,
  screenName :: String,
  isEndScreen :: Boolean,
  specificLog :: String
  }


-- ################################################ RideHistoryScreenState ##################################################
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

type RideHistoryScreenState =
  {
    shimmerLoader :: AnimationState,
    prestoListArrayItems :: Array ItemState,
    rideList :: Array IndividualRideCardState,
    selectedItem :: IndividualRideCardState,
    currentTab :: String,
    offsetValue :: Int,
    loaderButtonVisibility :: Boolean,
    loadMoreDisabled :: Boolean,
    recievedResponse :: Boolean
  }

type RideSelectionScreenState =
  {
    shimmerLoader :: AnimationState,
    prestoListArrayItems :: Array ItemState,
    rideList :: Array IndividualRideCardState,
    selectedItem :: Maybe IndividualRideCardState,
    offsetValue :: Int,
    loaderButtonVisibility :: Boolean,
    loadMoreDisabled :: Boolean,
    recievedResponse :: Boolean,
    selectedCategory :: CategoryListType
  }
------------------------------------------- ReferralScreenState -----------------------------------------

type ReferralScreenState = {
    data :: ReferralScreenStateData
  , props :: ReferralScreenStateProps
}

type ReferralScreenStateData = {
    referralCode :: String
  , confirmReferralCode :: String
  , password :: String
  , driverInfo :: {
      driverName :: String,
      driverMobile :: Maybe String,
      vehicleRegNumber :: String,
      referralCode     :: Maybe String
    }
  , driverPerformance :: {
      referrals :: {
        totalActivatedCustomers :: Int,
        totalReferredCustomers :: Int
      }
    }
}

type ReferralScreenStateProps = {
    primarybtnActive :: Boolean
  , passwordPopUpVisible  :: Boolean
  , callSupportPopUpVisible :: Boolean
  , confirmBtnActive :: Boolean
  , enableReferralFlowCount :: Int
  , stage :: ReferralType
  , seconds :: Int
  , id :: String
  , firstTime :: Boolean
  , leaderBoardType :: LeaderBoardType
  , showDateSelector :: Boolean
  , days :: Array LeaderBoardDay
  , weeks :: Array LeaderBoardWeek
  , selectedDay :: LeaderBoardDay
  , selectedWeek :: LeaderBoardWeek
  , rankersData :: Array RankCardData
  , currentDriverData :: RankCardData
  , showShimmer :: Boolean
  , noData :: Boolean
  , lastUpdatedAt :: String
}

-- ################################################ IndividualRideCardState ##################################################

type IndividualRideCardState =
  {
    date :: String,
    time :: String,
    total_amount :: Int,
    card_visibility :: String,
    shimmer_visibility :: String,
    rideDistance :: String,
    status :: String,
    vehicleModel :: String,
    shortRideId :: String,
    vehicleNumber :: String,
    driverName :: String,
    driverSelectedFare :: Int,
    vehicleColor :: String,
    id :: String,
    updatedAt :: String,
    source :: String,
    destination :: String
  }


type ItemState =
  {
    date :: PropValue,
    time :: PropValue,
    total_amount :: PropValue,
    card_visibility :: PropValue,
    shimmer_visibility :: PropValue,
    rideDistance :: PropValue,
    status :: PropValue,
    vehicleModel :: PropValue,
    shortRideId :: PropValue,
    vehicleNumber :: PropValue,
    driverName :: PropValue,
    driverSelectedFare :: PropValue,
    vehicleColor :: PropValue,
    id :: PropValue,
    updatedAt :: PropValue,
    source :: PropValue,
    destination :: PropValue,
    amountColor :: PropValue,
    riderName :: PropValue,
    metroTagVisibility :: PropValue,
    specialZoneText :: PropValue,
    specialZoneImage :: PropValue,
    specialZoneLayoutBackground :: PropValue
  }
-----------------------------------------------ApplicationStatusScreen -------------------

type DriverDetailsScreenState = {
  data :: DriverDetailsScreenStateData,
  props :: DriverDetailsScreenStateProps
}

data KeyboardModalType = MOBILE__NUMBER | OTP | NONE

derive instance genericKeyboardModalType :: Generic KeyboardModalType _
instance eqKeyboardModalType :: Eq KeyboardModalType where eq = genericEq
type DriverDetailsScreenStateData =  {
  driverName :: String,
  driverVehicleType :: String,
  driverRating :: Maybe Int,
  base64Image :: String,
  drivingLicenseNo :: String,
  driverMobile :: Maybe String,
  driverAlternateMobile :: Maybe String,
  driverEditAlternateMobile :: Maybe String,
  genderSelectionModal ::  GenderSelectionModalData,
  driverGender :: Maybe String,
  otpLimit :: Int,
  otpBackAlternateNumber :: Maybe String
}

type DriverDetailsScreenStateProps =  {
  keyboardModalType :: KeyboardModalType,
  checkAlternateNumber :: Boolean,
  enterOtpFocusIndex :: Int,
  otpIncorrect :: Boolean,
  otpAttemptsExceeded :: Boolean,
  genderSelectionModalShow :: Boolean,
  alternateMobileOtp :: String,
  removeNumberPopup :: Boolean,
  isEditAlternateMobile :: Boolean,
  numberExistError :: Boolean
}


----------------------------------------------- VehicleDetailsScreen -------------------

type VehicleDetailsScreenState = {
  data :: VehicleDetailsScreenData,
  props :: VehicleDetailsScreenProps
}

type VehicleDetailsScreenData =  {
  imageName :: String,
  vehicleTypes :: Array VehicalTypes,
  base64Image :: String,
  vehicleRegNumber :: String,
  vehicleType :: String,
  vehicleModel :: String,
  vehicleColor :: String
}

type VehicleDetailsScreenProps =  {
  isInEditVehicleDetailsView :: Boolean,
  openSelectVehicleTypeModal :: Boolean,
  isModalVisible :: Boolean,
  deleteButtonVisibility :: Boolean,
  isValid :: Boolean
}

--------------------------------------------- AboutUsScreenState ---------------------------
type AboutUsScreenState = {
  data :: AboutUsScreenData,
  props :: AboutUsScreenProps
}

type AboutUsScreenData = {
  versionNumber :: String
}

type AboutUsScreenProps = {
  demoModePopup :: Boolean,
  enableConfirmPassword :: Boolean,
  enableDemoModeCount :: Int
}

--------------------------------------------- SelectLanguageScreenState ---------------------------
type SelectLanguageScreenState = {
  data :: SelectLanguageScreenData,
  props :: SelectLanguageScreenProps
}

type SelectLanguageScreenData = {
  languages :: Array Language,
  isSelected :: Boolean

}

type SelectLanguageScreenProps = {
  selectedLanguage :: String,
  btnActive :: Boolean
}

----------------------------------------------- HomeScreenState ---------------------------------------------

type HomeScreenState = {
  data :: HomeScreenData,
  props :: HomeScreenProps
}

-- check something here as well

type HomeScreenData =  {
  driverName :: String,
  vehicleType :: String,
  activeRide :: ActiveRide,
  cancelRideModal :: CancelRideModalData,
  currentDriverLat :: Number,
  currentDriverLon :: Number,
  locationLastUpdatedTime :: String,
  totalRidesOfDay :: Int,
  totalEarningsOfDay :: Int,
  bonusEarned :: Int ,
  route :: Array Route,
  cancelRideConfirmationPopUp :: CancelRidePopUpData,
  messages :: Array ChatView.ChatComponent,
  messagesSize :: String,
  suggestionsList :: Array String,
  messageToBeSent :: String,
  driverAlternateMobile :: Maybe String
 }

type CancelRidePopUpData = {
  delayInSeconds :: Int,
  timerID :: String,
  continueEnabled :: Boolean,
  enableTimer :: Boolean
}

type CancelRideModalData = {
  selectionOptions :: Array OptionButtonList,
  activeIndex ::Maybe Int,
  selectedReasonCode :: String,
  selectedReasonDescription :: String,
  isMandatoryTextHidden :: Boolean,
  isSelectButtonActive :: Boolean
}

type GenderSelectionModalData = {
  selectionOptions :: Array OptionButtonList,
  activeIndex ::Maybe Int,
  selectedReasonCode :: String,
  selectedReasonDescription :: String,
  isSelectButtonActive :: Boolean
}

type Rides = {
  id :: String,
  timer :: Int,
  seconds :: Int,
  pickupDistance :: Int,
  journeyDistance :: Int,
  sourceAddress :: String,
  destinationAddress :: String,
  totalAmount :: Number,
  baseAmount :: Number ,
  increasePrice :: Number,
  decreasePrice :: Number,
  destinationArea :: String
}

type ActiveRide = {
  id :: String,
  source :: String,
  destination :: String,
  src_lat :: Number,
  src_lon :: Number,
  dest_lat :: Number,
  dest_lon :: Number,
  actualRideDistance :: Number,
  status :: Status,
  distance :: Number,
  exoPhone :: String,
  duration :: Int,
  riderName :: String,
  estimatedFare :: Int,
  isDriverArrived :: Boolean,
  notifiedCustomer :: Boolean,
  specialLocationTag :: Maybe String
}

type HomeScreenProps =  {
  statusOnline :: Boolean,
  goOfflineModal :: Boolean,
  screenName :: String,
  rideActionModal :: Boolean,
  enterOtpModal :: Boolean,
  rideOtp :: String,
  enterOtpFocusIndex :: Int,
  time :: Int,
  otpIncorrect :: Boolean,
  endRidePopUp :: Boolean,
  cancelRideModalShow :: Boolean,
  routeVisible :: Boolean,
  otpAttemptsExceeded :: Boolean,
  refreshAnimation :: Boolean,
  showDottedRoute :: Boolean,
  currentStage :: HomeScreenStage,
  mapRendered :: Boolean,
  cancelConfirmationPopup :: Boolean,
  chatcallbackInitiated :: Boolean,
  sendMessageActive :: Boolean,
  unReadMessages :: Boolean,
  openChatScreen :: Boolean,
  updatedArrivalInChat :: Boolean,
  driverStatusSet :: DriverStatus,
  silentPopUpView :: Boolean,
  zoneRideBooking :: Boolean,
  showGenderBanner :: Boolean,
  notRemoveBanner :: Boolean,
  showBonusInfo :: Boolean
 }

data DriverStatus = Online | Offline | Silent

type PillButtonState = {
  status :: DriverStatus,
  background :: String,
  imageUrl :: String,
  textColor :: String
}

data DriverStatusResult = ACTIVE | DEFAULT | DEMO_

derive instance genericDriverStatus :: Generic DriverStatus _
instance showDriverStatus :: Show DriverStatus where show = genericShow
instance eqDriverStatus :: Eq DriverStatus where eq = genericEq
instance encodeDriverStatus :: Encode DriverStatus where encode = defaultEnumEncode
instance decodeDriverStatus :: Decode DriverStatus where decode = defaultEnumDecode

type Location = {
  place :: String,
  lat :: Number,
  lon :: Number
}

data LocationType = LATITUDE | LONGITUDE

derive instance genericLocationType :: Generic LocationType _
instance eqLocationType :: Eq LocationType where eq = genericEq

-- ############################################################# BottomNavBarState ################################################################################

type BottomNavBarState = {
  activeIndex :: Int,
  navButton :: Array NavIcons
}

type NavIcons = {
  activeIcon :: String,
  defaultIcon :: String,
  text :: String
}
 -- ######################################  TripDetailsScreenState   ######################################

type TripDetailsScreenState =
  {
    data :: TripDetailsScreenData,
    props :: TripDetailsScreenProps
}

data PaymentMode = CASH | PAYMENT_ONLINE

derive instance genericPaymentMode :: Generic PaymentMode _
instance showPaymentMode :: Show PaymentMode where show = genericShow
instance eqPaymentMode :: Eq PaymentMode where eq = genericEq
instance encodePaymentMode :: Encode PaymentMode where encode = defaultEnumEncode
instance decodePaymentMode :: Decode PaymentMode where decode = defaultEnumDecode

type TripDetailsScreenData =
  {
    message :: String,
    tripId :: String,
    rider :: String,
    date :: String,
    time :: String,
    timeTaken :: String,
    source :: String,
    destination :: String,
    totalAmount :: Int,
    paymentMode :: PaymentMode,
    distance :: String,
    status :: String
  }

type TripDetailsScreenProps =
  {
    rating :: Int,
    reportIssue :: Boolean,
    issueReported :: Boolean
  }

--------------------------------------------- AboutUsScreenState ---------------------------
type HelpAndSupportScreenState = {
  data :: HelpAndSupportScreenData,
  props :: HelpAndSupportScreenProps
}

type HelpAndSupportScreenData = {
  categories :: Array CategoryListType,
  issueList :: Array IssueInfo,
  ongoingIssueList :: Array IssueInfo,
  resolvedIssueList :: Array IssueInfo,
  issueListType :: IssueModalType
}

type CategoryListType = {
    categoryName :: String
  , categoryImageUrl :: String
  , categoryAction :: String
  , categoryId :: String
  }

type HelpAndSupportScreenProps = {
  isNoRides :: Boolean

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
  chatConfig :: ChatView.Config,
  selectedOptionId :: Maybe String,
  categoryAction :: String,
  addedImages :: Array { image :: String, imageName :: String },
  categoryId :: String,
  recordAudioState :: RecordAudioModel.RecordAudioModelState,
  addImagesState :: { images :: Array { image :: String, imageName :: String }, stateChanged :: Boolean, isLoading :: Boolean, imageMediaIds :: Array String },
  viewImageState :: { image :: String, imageName :: Maybe String },
  recordedAudioUrl :: Maybe String,
  addAudioState :: { audioFile :: Maybe String, stateChanged :: Boolean },
  uploadedImagesIds :: Array String,
  uploadedAudioId :: Maybe String,
  options :: Array
             { issueOptionId :: String
             , option :: String
             , label :: String
             }
}

type ReportIssueChatScreenProps = {
  showSubmitComp :: Boolean,
  showImageModel :: Boolean,
  showAudioModel :: Boolean,
  showRecordModel :: Boolean,
  showCallCustomerModel :: Boolean,
  isReversedFlow :: Boolean,
  showViewImageModel :: Boolean,
  isPopupModelOpen :: Boolean
}

type IssueInfo = {

    issueReportId :: String,
    status :: String,
    category :: String,
    createdAt :: String

}

data IssueModalType = HELP_AND_SUPPORT_SCREEN_MODAL | ONGOING_ISSUES_MODAL | RESOLVED_ISSUES_MODAL | BACKPRESSED_MODAL

derive instance genericIssueModalType :: Generic IssueModalType _
instance eqIssueModalType :: Eq IssueModalType where eq = genericEq
--------------------------------------------- AboutUsScreenState ---------------------------
type WriteToUsScreenState = {
  data :: WriteToUsScreenData,
  props :: WriteToUsScreenProps
}

type WriteToUsScreenData = {

}

type WriteToUsScreenProps = {
  isThankYouScreen :: Boolean
}


------------------------------------------- PermissionsScreenState ---------------------------
type PermissionsScreenState = {
  data :: PermissionsScreenData
  , props :: PermissionsScreenProps
}

type PermissionsScreenData = {
    driverMobileNumber :: String
}

type PermissionsScreenProps = {
  isLocationPermissionChecked :: Boolean
  , isOverlayPermissionChecked :: Boolean
  , isAutoStartPermissionChecked :: Boolean
  , androidVersion :: Int
  , isBatteryOptimizationChecked :: Boolean
  , logoutModalView :: Boolean
}
--------------------------------------------- RideDetailScreenState ---------------------------------------------

type RideDetailScreenState = {
  data :: RideDetailScreenData,
  props :: RideDetailScreenProps
}

type RideDetailScreenData =  {
  sourceAddress :: Location,
  destAddress :: Location,
  rideStartTime :: String,
  rideEndTime :: String,
  bookingDateAndTime :: String,
  totalAmount :: Int,
  customerName :: String
 }

type RideDetailScreenProps =  {
  cashCollectedButton :: Boolean
 }

--------------------------------------------- EditBankDetailsScreen ---------------------------
type EditBankDetailsScreenState = {
  data :: EditBankDetailsScreenData,
  props :: EditBankDetailsScreenProps
}

type EditBankDetailsScreenData = {

}

type EditBankDetailsScreenProps = {
  isInEditBankDetailsScreen :: Boolean
}

--------------------------------------------- EditAadhaarDetailsScreen ---------------------------
type EditAadhaarDetailsScreenState = {
  data :: EditAadhaarDetailsScreenData,
  props :: EditAadhaarDetailsScreenProps
}

type EditAadhaarDetailsScreenData = {

}

type EditAadhaarDetailsScreenProps = {
  isInEditAadharDetailsScreen :: Boolean
}


-- ######################################  InvoiceScreenState   ######################################

type InvoiceScreenState =
  {
    data :: InvoiceScreenData,
    props :: InvoiceScreenProps
  }

type InvoiceScreenData =
  {
    tripCharges :: Number,
    promotion :: Number,
    gst :: Number,
    totalAmount :: Number,
    date :: String
  }

type InvoiceScreenProps =
  {
    paymentMode :: String
  }

---------------------------------------------DriverWelcomeScreen -------------------------------
type CarouselModel = {
  image :: String,
  title :: String,
  description :: String
}

type WelcomeScreenState = {
  data :: WelcomeScreenData
}

type WelcomeScreenData = {
  carouselModel :: Array CarouselModel
}

type StepsHeaderModelState = {
  activeIndex :: Int,
  textArray :: Array String,
  backArrowVisibility :: Boolean
}

--------------------------------------------- PopUpScreenState ---------------------------
type PopUpScreenState = {
  data :: PopUpScreenData,
  props :: PopUpScreenProps
}

type PopUpScreenData = {
  availableRides :: Array Rides
}

type PopUpScreenProps = {}

type AllocationData = {
  searchRequestValidTill :: String,
  searchRequestId :: String,
  startTime :: String,
  baseFare :: Number,
  distance :: Int,
  distanceToPickup :: Int,
  fromLocation :: {
    area :: String,
    state :: String,
    full_address :: String,
    createdAt :: String,
    country :: String,
    building :: String,
    street :: String,
    lat :: Number,
    city :: String,
    areaCode :: String,
    id :: String,
    lon :: Number,
    updatedAt :: String
  },
  toLocation :: {
    area :: String,
    state :: String,
    full_address :: String,
    createdAt :: String,
    country :: String,
    building :: String,
    street :: String,
    lat :: Number,
    city :: String,
    areaCode :: String,
    id :: String,
    lon :: Number,
    updatedAt :: String
  },
  durationToPickup :: Int
}

type RegCardDetails =  {
  title :: String,
  reason :: String,
  image :: String,
  verificationStatus :: String,
  visibility :: String,
  docType :: String,
  status :: String,
  titleImage :: String,
  backgroundColor :: String,
  strokeColor :: String
}


type DriverRideRatingScreenState = {
  data :: DriverRideRatingScreenData,
  props :: DriverRideRatingScreenProps
}

type DriverRideRatingScreenData = {
    rating :: Int
  , rideId :: String
  , feedback :: String
  , customerName :: String
  , activeFeedBackOption :: Maybe FeedbackSuggestions
  , selectedFeedbackOption :: String
}

type DriverRideRatingScreenProps = {

}

type AppUpdatePopUpScreenState = {
  version :: Int
  , updatePopup :: UpdatePopupType
}

data UpdatePopupType =  AppVersion
                      | DateAndTime
                      | NoUpdatePopup

derive instance genericUpdatePopupType :: Generic UpdatePopupType _
instance showUpdatePopupType :: Show UpdatePopupType where show = genericShow
instance eqUpdatePopupType :: Eq UpdatePopupType where eq = genericEq

data FeedbackSuggestions
 = CUSTOMER_RUDE_BEHAVIOUR
  | LONG_WAIT_TIME
  | DIDNT_COME_TO_PICUP
  | NOTHING

derive instance genericFeedbackSuggestions :: Generic FeedbackSuggestions _
instance eqFeedbackSuggestions :: Eq FeedbackSuggestions where eq = genericEq

data HomeScreenStage =  HomeScreen
                      | RideRequested
                      | RideAccepted
                      | RideStarted
                      | RideCompleted
                      | ChatWithCustomer

derive instance genericHomeScreenStage :: Generic HomeScreenStage _
instance showHomeScreenStage :: Show HomeScreenStage where show = genericShow
instance eqHomeScreenStage :: Eq HomeScreenStage where eq = genericEq
instance decodeHomeScreenStage :: Decode HomeScreenStage where decode = defaultEnumDecode
instance encodeHomeScreenStage :: Encode HomeScreenStage where encode = defaultEnumEncode

data NotificationType =  DRIVER_REACHED
                      | CANCELLED_PRODUCT
                      | DRIVER_ASSIGNMENT
                      | RIDE_REQUESTED

derive instance genericNotificationType :: Generic NotificationType _
instance showNotificationType :: Show NotificationType where show = genericShow
instance eqNotificationType :: Eq NotificationType where eq = genericEq


------------------------------------- NotificationScreen ------------------------------
type NotificationsScreenState = {
  shimmerLoader :: AnimationState,
  prestoListArrayItems :: Array NotificationCardPropState,
  notificationList :: Array NotificationCardState,
  selectedItem :: NotificationCardState,
  offsetValue :: Int,
  loaderButtonVisibility :: Boolean,
  loadMoreDisabled :: Boolean,
  recievedResponse :: Boolean,
  notificationDetailModelState :: NotificationDetailModelState,
  notifsDetailModelVisibility :: Visibility,
  loadMore :: Boolean,
  selectedNotification :: Maybe String,
  deepLinkActivated :: Boolean
}

type NotificationCardState = {
  mediaUrl :: String,
  title :: String,
  description :: String,
  action1Text :: String,
  action2Text :: String,
  notificationLabel :: String,
  timeLabel :: String,
  messageId :: String,
  notificationNotSeen :: Boolean,
  comment :: Maybe String,
  imageUrl :: String,
  mediaType :: Maybe MediaType,
  likeCount :: Int,
  viewCount :: Int,
  likeStatus :: Boolean
}

type NotificationCardPropState = {
  mediaUrl :: PropValue,
  title :: PropValue,
  action1Text :: PropValue,
  action2Text :: PropValue,
  notificationLabel :: PropValue,
  timeLabel :: PropValue,
  description :: PropValue,
  cardVisibility :: PropValue,
  shimmerVisibility :: PropValue,
  notificationLabelColor :: PropValue,
  action1Visibility :: PropValue,
  action2Visibility :: PropValue,
  descriptionVisibility :: PropValue,
  illustrationVisibility :: PropValue,
  notificationNotSeen :: PropValue,
  playBtnVisibility :: PropValue,
  imageUrl :: PropValue,
  playButton :: PropValue,
  previewImage :: PropValue,
  previewImageTitle :: PropValue,
  imageVisibility :: PropValue,
  messageId :: PropValue,
  imageWithUrl :: PropValue,
  imageWithUrlVisibility :: PropValue,
  likeCount :: PropValue,
  viewCount :: PropValue
}

type NotificationDetailModelState = {
  mediaUrl :: String,
  title :: String,
  timeLabel :: String,
  description :: Array String,
  actionText :: String,
  actionVisibility :: Visibility,
  addCommentModelVisibility :: Visibility,
  comment :: Maybe String,
  commentBtnActive :: Boolean,
  messageId :: String,
  notificationNotSeen :: Boolean,
  imageUrl :: String,
  mediaType :: Maybe MediaType,
  likeCount :: Int,
  likeStatus :: Boolean,
  viewCount :: Int
}

type YoutubeData = {
    videoTitle :: String
  , setVideoTitle :: Boolean
  , showMenuButton :: Boolean
  , showDuration :: Boolean
  , showSeekBar :: Boolean
  , videoId :: String
  , videoType :: String
}

data YoutubeVideoStatus = PLAY | PAUSE

derive instance genericYoutubeVideoStatus:: Generic YoutubeVideoStatus _
instance showYoutubeVideoStatus :: Show YoutubeVideoStatus where show = genericShow
instance eqYoutubeVideoStatus :: Eq YoutubeVideoStatus where eq = genericEq


data ReferralType = SuccessScreen | ComingSoonScreen | ReferralFlow | QRScreen | LeaderBoard

derive instance genericReferralType :: Generic ReferralType _
instance eqReferralType :: Eq ReferralType where eq = genericEq


type BookingOptionsScreenState = {
  data :: BookingOptionsScreenData,
  props :: BookingOptionsScreenProps
}

type BookingOptionsScreenData = {
  vehicleType :: String,
  vehicleNumber :: String,
  vehicleName :: String,
  vehicleCapacity :: Int,
  downgradeOptions :: Array ChooseVehicle.Config
}

type BookingOptionsScreenProps = {
  isBtnActive :: Boolean
}

data LeaderBoardType = Daily | Weekly

derive instance genericLeaderBoardType :: Generic LeaderBoardType _
instance eqLeaderBoardType :: Eq LeaderBoardType where eq = genericEq

type LeaderBoardDay = {
    date :: Int
  , utcDate :: String
  , month :: String
  , year :: Int
}

type LeaderBoardWeek = {
    startDate :: Int
  , utcStartDate :: String
  , endDate :: Int
  , utcEndDate :: String
  , startMonth :: String
  , endMonth :: String
}

data DateSelector = DaySelector LeaderBoardDay | WeekSelector LeaderBoardWeek

type RankCardData = {
    goodName :: String
  , profileUrl :: Maybe String
  , rank :: Int
  , rides :: Int
}