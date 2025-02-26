{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.Types 
  ( module Screens.Types
  , module ReExport
  ) where

import Common.Types.Config

import Common.Types.App as Common
import Components.ChatView.Controller as ChatView
import Components.ChooseVehicle.Controller (Config) as ChooseVehicle
import Components.GoToLocationModal.Controller as GoToModal
import Components.PaymentHistoryListItem.Controller as PaymentHistoryListItem
import Components.RecordAudioModel.Controller as RecordAudioModel
import Data.Eq.Generic (genericEq)
import Data.Either as Either
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Domain.Payments as PP
import Foreign (Foreign)
import Foreign (Foreign)
import Foreign.Class (class Decode, class Encode)
import Foreign.Object (Object)
import Control.Alt ((<|>))
import Foreign (ForeignError(..), fail) 
import Halogen.VDom.DOM.Prop (PropValue)
import MerchantConfig.Types (AppConfig, BottomNavConfig, GradientConfig, SubscriptionConfig, Language(..))
import Prelude (class Eq, class Show, ($), (<$>))
import Foreign.Index (readProp)
import Foreign.Generic (class Decode, decode)
import Presto.Core.Types.API (class StandardEncode, standardEncode)
import Presto.Core.Utils.Encoding (defaultDecode, defaultEncode,defaultEnumDecode, defaultEnumEncode)
import PrestoDOM (LetterSpacing, Visibility, visibility)
import PrestoDOM.List (ListItem)
import Prim.TypeError as String
import RemoteConfig.Types as RC
import Screens (ScreenName)
import Services.API (LmsTranslatedModuleInfoRes(..), QuizQuestion(..), QuizOptions(..), LmsQuizHistory(..), LmsQuestionRes(..), LmsModuleRes(..), LmsVideoRes(..), LmsEntityCompletionStatus(..), LmsBonus(..), LmsReward(..), LmsCategory(..), ModuleCompletionStatus(..), AutopayPaymentStage, BankError(..), FeeType, GetDriverInfoResp(..), MediaType, PaymentBreakUp, Route, Status, DriverProfileStatsResp(..), LastPaymentType(..), RidesSummary, RidesInfo(..), GetAllRcDataResp(..), GetAllRcDataRecords(..), TripCategory(..), QuestionConfirmRes(..), ServiceTierType(..))
import Services.API (QuestionConfirmRes(..), GetDriverInfoResp(..), Route, Status, MediaType, PaymentBreakUp, BookingTypes(..))
import Services.API as API
import Styles.Types (FontSize)
import Control.Monad.Except (runExcept)
import Components.ChatView.Controller as ChatView
import Foreign.Object (Object)
import Foreign (Foreign)
import Screens (ScreenName)
import Services.API (LmsTranslatedModuleInfoRes(..), QuizQuestion(..), QuizOptions(..), LmsQuizHistory(..), LmsQuestionRes(..), LmsModuleRes(..), LmsVideoRes(..), LmsEntityCompletionStatus(..), LmsBonus(..), LmsReward(..), LmsCategory(..), ModuleCompletionStatus(..), AutopayPaymentStage, BankError(..), FeeType, GetDriverInfoResp(..), MediaType, PaymentBreakUp, Route, Status, DriverProfileStatsResp(..), LastPaymentType(..), RidesSummary, RidesInfo(..), GetAllRcDataResp(..), GetAllRcDataRecords(..), TripCategory(..), QuestionConfirmRes(..),CoinEntity(..), PayoutVpaStatus(..))
import Styles.Types (FontSize)
import MerchantConfig.Types
import RemoteConfig.Types as RC
import Services.API as API
import Styles.Types (FontSize)
import Common.Types.App (CalendarDate)
import Common.RemoteConfig.Types as CommonRC
import Common.RemoteConfig.Types (OfferBanner(..)) as ReExport
import Data.Tuple(Tuple(..))


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
  config :: AppConfig,
  isSelected :: Boolean,
  logField :: Object Foreign
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
  logField :: Object Foreign,
  driverMobileNumber :: String,
  cityConfig :: CityConfig,
  vehicleCategory :: Maybe VehicleCategory,
  config :: AppConfig,
  rcNumberPrefixList :: Array String,
  ventilator :: Maybe Boolean,
  airConditioned :: Maybe Boolean,
  oxygen :: Maybe Boolean
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
  addRcFromProfile :: Boolean,
  isDateClickable :: Boolean,
  openHowToUploadManual :: Boolean,
  logoutModalView :: Boolean,
  validateProfilePicturePopUp :: Boolean,
  imageCaptureLayoutView :: Boolean,
  fileCameraOption :: Boolean,
  fileCameraPopupModal :: Boolean,
  validating :: Boolean,
  successfulValidation :: Boolean,
  multipleRCstatus :: StageStatus,
  menuOptions :: Boolean,
  confirmChangeVehicle :: Boolean,
  contactSupportModal :: AnimType,
  buttonIndex :: Maybe Int,
  acModal :: Boolean,
  facilities :: Boolean,
  showIssueOptions :: Boolean,
  isvariant :: String,
  ambulanceModal :: Boolean,
  agreeTermsModal :: Boolean
 }

data ValidationStatus  =  Success | Failure | InProgress | None

derive instance genericValidationStatus :: Generic ValidationStatus _
instance showValidationStatus :: Show ValidationStatus where show = genericShow
instance eqValidationStatus :: Eq ValidationStatus where eq = genericEq


data VehicalTypes = Sedan | Hatchback | SUV | Auto | Bike | Ambulance_Taxi | Ambulance_AC | Ambulance_AC_Oxy | Ambulance_Taxi_Oxy | Ambulance_Ventilator | Suv_Plus | EV_Auto | HERITAGE_CAB

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
  , logField :: Object Foreign
  , mobileNumber :: String
  , vehicleCategory :: Maybe VehicleCategory
  , cityConfig :: CityConfig
  , config :: AppConfig
}

type UploadDrivingLicenseStateProps = {
    openRegistrationModal :: Boolean
  , openLicenseManual :: Boolean
  , input_data :: String
  , clickedButtonType :: String
  , openGenericMessageModal :: Boolean
  , errorVisibility :: Boolean
  , openDateOfIssueManual :: Boolean
  , isDateClickable :: Boolean
  , openHowToUploadManual :: Boolean
  , logoutPopupModal :: Boolean
  , validateProfilePicturePopUp :: Boolean
  , imageCaptureLayoutView :: Boolean 
  , fileCameraPopupModal :: Boolean
  , fileCameraOption :: Boolean
  , validating :: Boolean
  , successfulValidation :: Boolean
  , menuOptions :: Boolean
  , confirmChangeVehicle :: Boolean
  , contactSupportModal :: AnimType
}

 -- ############################################################# RegistrationScreen ################################################################################
type RegistrationScreenState = {
  data :: RegistrationScreenData,
  props :: RegistrationScreenProps
}
type RegistrationScreenData = {
  activeIndex :: Int,
  registerationStepsAuto :: Array StepProgress,
  registerationStepsCabs :: Array StepProgress,
  registerationStepsBike :: Array StepProgress,
  registerationStepsAmbulance :: Array StepProgress,
  registerationStepsTruck :: Array StepProgress,
  registerationStepsBus :: Array StepProgress,
  phoneNumber :: String,
  drivingLicenseStatus :: StageStatus,
  vehicleDetailsStatus :: StageStatus,
  permissionsStatus :: StageStatus,
  documentStatusList :: Array DocumentStatus,
  variantList :: Array VehicleCategory,
  lastUpdateTime :: String,
  cityConfig :: CityConfig,
  config :: AppConfig,
  referralCode :: String,
  referral_code_input_data :: String,
  logField :: Object Foreign,
  enteredDL :: String,
  enteredRC :: String,
  vehicleCategory :: Maybe VehicleCategory,
  vehicleTypeMismatch :: Boolean,
  linkedRc :: Maybe String,
  accessToken :: String,
  hvTxnId :: Maybe String,
  hvFlowId :: Maybe String
}

type DocumentStatus = {
  vehicleType :: Maybe VehicleCategory,
  verifiedVehicleCategory :: Maybe VehicleCategory,
  status :: StageStatus,
  docType :: RegisterationStep,
  verificationMessage :: Maybe String,
  regNo :: Maybe String
}

type VehicleInfo = {
  vehicleType :: VehicleCategory,
  vehicleImage :: String,
  vehicleName :: String
}

type StepProgress = {
  stageName :: String,
  stage :: RegisterationStep,
  subtext :: String,
  isMandatory :: Boolean,
  isDisabled :: Boolean,
  disableWarning :: String,
  isHidden :: Boolean,
  dependencyDocumentType :: Array RegisterationStep,
  rcNumberPrefixList :: Array String
}

type RegistrationScreenProps = {
  logoutModalView :: Boolean,
  limitReachedFor :: Maybe String,
  isValidReferralCode :: Boolean,
  enterOtpFocusIndex :: Int,
  enterReferralCodeModal :: Boolean,
  referralCodeSubmitted :: Boolean,
  contactSupportView :: Boolean,
  contactSupportModal :: AnimType,
  selectedVehicleIndex :: Maybe Int,
  optionalDocsExpanded :: Boolean,
  confirmChangeVehicle :: Boolean,
  refreshAnimation :: Boolean,
  driverEnabled :: Boolean,
  menuOptions :: Boolean,
  manageVehicle :: Boolean,
  manageVehicleCategory :: Maybe VehicleCategory,
  dontAllowHvRelaunch :: Boolean
}

data AnimType = HIDE | SHOW | ANIMATING
derive instance genericAnimType :: Generic AnimType _
instance eqAnimType :: Eq AnimType where eq = genericEq

data RegisterationStep = 
    DRIVING_LICENSE_OPTION 
  | VEHICLE_DETAILS_OPTION 
  | GRANT_PERMISSION 
  | SUBSCRIPTION_PLAN
  | PROFILE_PHOTO
  | AADHAAR_CARD
  | PAN_CARD 
  | VEHICLE_PERMIT 
  | FITNESS_CERTIFICATE 
  | VEHICLE_INSURANCE
  | VEHICLE_PUC
  | NO_OPTION

derive instance genericRegisterationStep :: Generic RegisterationStep _
instance eqRegisterationStep :: Eq RegisterationStep where eq = genericEq

data StageStatus = COMPLETED | IN_PROGRESS | NOT_STARTED | FAILED | MANUAL_VERIFICATION_REQUIRED
derive instance genericStageStatus :: Generic StageStatus _
instance eqStageStatus :: Eq StageStatus where eq = genericEq

data VehicleCategory = AutoCategory | CarCategory | BikeCategory | AmbulanceCategory | TruckCategory | BusCategory | UnKnown

derive instance genericVehicleCategory :: Generic VehicleCategory _
instance eqVehicleCategory :: Eq VehicleCategory where eq = genericEq
instance showVehicleCategory :: Show VehicleCategory where show = genericShow

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
  vehicleDetails :: Array DriverVehicleDetails,
  driverName :: String,
  driverVehicleType :: String,
  driverRating :: Maybe Number,
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
  genderTypeSelect :: Maybe String,
  alterNumberEditableText :: Boolean,
  driverEditAlternateMobile :: Maybe String,
  otpLimit :: Int,
  otpBackAlternateNumber :: Maybe String,
  languagesSpoken :: Array String,
  gender :: Maybe String,
  driverGender :: Maybe String,
  languageList :: Array Common.CheckBoxOptions,
  vehicleAge :: Int,
  vehicleName :: String,
  rcDataArray :: Array RcData,
  inactiveRCArray :: Array RcData,
  activeRCData :: RcData,
  rcNumber :: String,
  isRCActive :: Boolean,
  openInactiveRCViewOrNotArray :: Array Int,
  logField :: Object Foreign,
  analyticsData :: AnalyticsData,
  fromHomeScreen :: Boolean,
  profileImg :: Maybe String,
  payerVpa :: String,
  autoPayStatus :: AutoPayStatus,
  config :: AppConfig,
  goHomeActive :: Boolean,
  cachedVehicleCategory :: VehicleCategory,
  cancellationRate :: Int,
  assignedRides :: Int,
  cancelledRides :: Int,
  cancellationWindow :: Maybe Int,
  missedEarnings :: Int,
  driverInfoResponse :: Maybe GetDriverInfoResp,
  profileCompletedModules :: Int,
  driverBlocked :: Boolean,
  blockedExpiryTime :: String,
  favCount :: Maybe Int
}

type CompletingProfileRes = {
    completed :: Int
  , pledge :: Array String
  , vehicalOffer :: Array String
  , languages :: Array String
  , aspirations :: Array String
  , homeTown :: Maybe String
  , calendarState :: CalendarState
  , drivingSince :: Maybe Int
  , viewImageState :: ViewImageState
  , addImagesState :: {
    images :: Array Image,
    stateChanged :: Boolean,
    isLoading :: Boolean,
    imageMediaIds :: Array String
  }
  , datePickerState :: DatePickersState
  , uploadedImagesIds :: Array String
  , addedImages :: Array { image :: String, imageName :: String }
  , inputTextState :: InputTextState
}

type RcData = {
  rcStatus  :: Boolean,
  rcDetails :: RcDetails
  }

type RcDetails = {
    certificateNumber :: String,
    vehicleModel      :: Maybe String,
    vehicleColor      :: Maybe String
    }

type DriverVehicleDetails = {
    registrationNo :: String,
    userSelectedVehicleCategory :: VehicleCategory,
    verifiedVehicleCategory :: Maybe VehicleCategory,
    isVerified :: Boolean,
    vehicleModel :: Maybe String,
    isActive :: Boolean
}

type AnalyticsData = {
    totalEarnings :: String
  , bonusEarned :: String
  , totalCompletedTrips :: Int
  , totalUsersRated :: Int
  , rating :: Maybe Number
  , lateNightTrips :: Int
  , lastRegistered :: String
  , badges :: Array Badge
  , missedEarnings :: Int
  , ridesCancelled :: Int
  , cancellationRate :: Int
  , totalRidesAssigned :: Int
  , totalDistanceTravelled :: String
}

type ChipRailData = {
    mainTxt :: String
  , subTxt :: String
}

type Badge =  {
    badgeImage :: String
  , primaryText :: String
  , subText :: String
  }

type VehicleP = {
  vehicleName :: String,
  isSelected :: Boolean
}

type DriverProfileScreenProps = {
  manageVehicleVisibility :: Boolean,
  logoutModalView :: Boolean,
  showLiveDashboard :: Boolean,
  screenType :: DriverProfileScreenType,
  openSettings :: Boolean,
  updateDetails :: Boolean,
  showGenderView :: Boolean,
  alternateNumberView :: Boolean,
  removeAlternateNumber :: Boolean,
  enterOtpModal :: Boolean,
  enterOdometerFocusIndex :: Int,
  enterOtpFocusIndex :: Int,
  otpIncorrect :: Boolean,
  otpAttemptsExceeded :: Boolean,
  alternateMobileOtp :: String,
  checkAlternateNumber :: Boolean,
  isEditAlternateMobile :: Boolean,
  numberExistError :: Boolean,
  mNumberEdtFocused :: Boolean,
  updateLanguages :: Boolean,
  activateRcView :: Boolean,
  activateOrDeactivateRcView :: Boolean,
  activeRcIndex :: Int,
  deleteRcView :: Boolean,
  alreadyActive :: Boolean,
  callDriver :: Boolean,
  openRcView :: Boolean,
  detailsUpdationType :: Maybe UpdateType,
  btnActive :: Boolean,
  showBookingOptionForTaxi :: Boolean,
  upiQrView :: Boolean,
  paymentInfoView :: Boolean,
  enableGoto :: Boolean,
  isRideActive :: Boolean,
  canSwitchToRental :: Maybe Boolean,
  canSwitchToIntraCity :: Maybe Boolean,
  canSwitchToInterCity :: Maybe Boolean,
  showDriverBlockedPopup :: Boolean
}
data Gender = MALE | FEMALE | OTHER | PREFER_NOT_TO_SAY

data DriverProfileScreenType = DRIVER_DETAILS | VEHICLE_DETAILS | SETTINGS

derive instance genericDriverProfileScreenType :: Generic DriverProfileScreenType _
instance showDriverProfileScreenType :: Show DriverProfileScreenType where show = genericShow
instance eqDriverProfileScreenType :: Eq DriverProfileScreenType where eq = genericEq


data UpdateType = LANGUAGE | HOME_TOWN | VEHICLE_AGE | VEHICLE_NAME | PAYMENT

derive instance genericUpdateType :: Generic UpdateType _
instance showUpdateType :: Show UpdateType where show = genericShow
instance eqUpdateType :: Eq UpdateType where eq = genericEq

-----------------------------------------------ApplicationStatusScreen ---------------------------------------
type ApplicationStatusScreenState = {
  data :: ApplicationStatusScreenData,
  props :: ApplicationStatusScreenProps
}
type ApplicationStatusScreenData =  {
  rcVerificationStatus :: String,
  dlVerificationStatus :: String,
  mobileNumber :: String,
  otpValue :: String
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
  isAlternateMobileNumberExists :: Boolean
}

--------------------------------------------------------------- EnterMobileNumberScreenState -----------------------------------------------------------------------------
type EnterMobileNumberScreenState = {
  data :: EnterMobileNumberScreenStateData,
  props :: EnterMobileNumberScreenStateProps
}

type EnterMobileNumberScreenStateData = {
    mobileNumber :: String,
    logField :: Object Foreign,
    config :: AppConfig
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
  editTextId :: String,
  config :: AppConfig
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
    recievedResponse :: Boolean,
    logField :: Object Foreign
  , datePickerState :: DatePickerState
  , props :: RideHistoryScreenStateProps
  , data :: RideHistoryScreenStateData
  }
type DatePickerState = {
  activeIndex :: Int
, selectedItem :: Common.CalendarDate
}
type RideHistoryScreenStateProps = {
    showDatePicker :: Boolean
 , showPaymentHistory :: Boolean
}
type RideHistoryScreenStateData = {
    pastDays :: Int
  , paymentHistory :: PaymentHistoryModelState
  , config :: AppConfig
}

data EditRc = DEACTIVATING_RC | DELETING_RC | ACTIVATING_RC

derive instance genericEditRc :: Generic EditRc _
instance eqEditRc :: Eq EditRc where eq = genericEq

data CallOptions = CALLING_DRIVER | CALLING_CUSTOMER_SUPPORT
derive instance genericCallOptions :: Generic CallOptions _
instance eqCallOptions :: Eq CallOptions where eq = genericEq
instance showCallOptions :: Show CallOptions where show = genericShow
instance encodeCallOptions :: Encode CallOptions where encode = defaultEnumEncode
instance decodeCallOptions :: Decode CallOptions where decode = defaultEnumDecode

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
    selectedCategory :: Common.CategoryListType
  }

type VehicleDetails = { rcStatus :: Boolean
                , rcDetails :: { certificateNumber  :: String
                , vehicleModel :: Maybe String
                , vehicleColor :: Maybe String
                }}

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
      referralCode     :: Maybe String,
      vehicleVariant :: String
    }
  , driverPerformance :: {
      referrals :: {
        totalActivatedCustomers :: Int,
        totalReferredCustomers :: Int,
        totalReferredDrivers :: Int
      }
    }
  , logField :: Object Foreign
  , config :: AppConfig
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
  , days :: Array Common.CalendarDate
  , weeks :: Array Common.CalendarWeek
  , months :: Array Common.CalendarMonth
  , selectedDay :: Common.CalendarDate
  , selectedWeek :: Common.CalendarWeek
  , selectedMonth :: Common.CalendarMonth
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
    destination :: String,
    vehicleType :: String,
    riderName :: String,
    customerExtraFee :: Maybe Int,
    purpleTagVisibility :: Boolean,
    gotoTagVisibility :: Boolean,
    specialZoneLayoutBackground :: String,
    specialZoneImage :: String,
    specialZoneText :: String,
    spLocTagVisibility :: Boolean,
    specialZonePickup :: Boolean,
    tripType :: TripType,
    tollCharge :: Number,
    rideType :: String,
    tripStartTime :: Maybe String,
    tripEndTime :: Maybe String,
    acRide :: Maybe Boolean,
    vehicleServiceTier :: String,
    parkingCharge :: Number,
    stops :: Array API.Stop
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
    spLocTagVisibility :: PropValue,
    specialZoneText :: PropValue,
    specialZoneImage :: PropValue,
    specialZoneLayoutBackground :: PropValue,
    gotoTagVisibility :: PropValue,
    purpleTagVisibility :: PropValue,
    tipTagVisibility :: PropValue,
    specialZonePickup :: PropValue
  }
-----------------------------------------------ApplicationStatusScreen -------------------

type DriverDetailsScreenState = {
  data :: DriverDetailsScreenStateData,
  props :: DriverDetailsScreenStateProps
}

data KeyboardModalType = MOBILE__NUMBER | OTP | ODOMETER | NONE

derive instance genericKeyboardModalType :: Generic KeyboardModalType _
instance eqKeyboardModalType :: Eq KeyboardModalType where eq = genericEq
type DriverDetailsScreenStateData =  {
  driverName :: String,
  driverVehicleType :: String,
  driverRating :: Maybe Number,
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
  appConfig :: AppConfig,
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

--------------------------------------------- CancellationRateScreenState -------------------------

type CancellationRateScreenState = {
  appConfig :: AppConfig,
  data :: CancellationRateScreenData
}

type CancellationRateScreenData = {
  cancellationRate :: Int,
  assignedRides :: Int,
  cancelledRides :: Int,
  cancellationWindow :: Maybe Int,
  missedEarnings :: Int
}

--------------------------------------------- SelectLanguageScreenState ---------------------------
type SelectLanguageScreenState = {
  data :: SelectLanguageScreenData,
  props :: SelectLanguageScreenProps
}

type SelectLanguageScreenData = {
  isSelected :: Boolean
, config :: AppConfig
, logField :: Object Foreign
, languageList:: Array Language
}

type SelectLanguageScreenProps = {
  selectedLanguage :: String,
  btnActive :: Boolean,
  onlyGetTheSelectedLanguage :: Boolean,
  selectLanguageForScreen :: String,
  fromOnboarding :: Boolean
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
  advancedRideData :: Maybe ActiveRide,
  currentRideData :: Maybe ActiveRide,
  driverStats :: Boolean,
  cancelRideModal :: CancelRideModalData,
  currentDriverLat :: Number,
  currentDriverLon :: Number,
  locationLastUpdatedTime :: String,
  totalRidesOfDay :: Int,
  totalEarningsOfDay :: Int,
  earningPerKm :: Maybe Int,
  totalValidRidesOfDay :: Int,
  bonusEarned :: Int ,
  route :: Array Route,
  cancelRideConfirmationPopUp :: CancelRidePopUpData,
  messages :: Array ChatView.ChatComponentConfig,
  messagesSize :: String,
  chatSuggestionsList :: Array String,
  messageToBeSent :: String,
  driverAlternateMobile :: Maybe String,
  logField :: Object Foreign,
  paymentState :: PaymentState,
  profileImg :: Maybe String, 
  endRideData :: EndRideData,
  config :: AppConfig,
  triggerPatchCounter :: Int,
  peekHeight :: Int,
  driverGotoState :: DriverGoToState,
  snappedOrigin :: Maybe Location,
  gender :: String,
  coinBalance :: Int,
  subsRemoteConfig :: CommonRC.RCSubscriptionDues,
  bannerData :: BannerCarousalData,
  prevLatLon :: Maybe Location,
  noOfLocations :: Int,
  isVehicleSupported :: Boolean,
  linkedVehicleCategory :: String,
  linkedVehicleVariant :: String,
  cityConfig :: CityConfig,
  parking :: ParkingData,
  toll :: TollState,
  payoutVpa :: Maybe String,
  payoutVpaStatus :: Maybe PayoutVpaStatus,
  isPayoutEnabled :: Maybe Boolean,
  payoutRewardAmount :: Maybe Int,
  payoutVpaBankAccount :: Maybe String
, cancellationRate :: Int
, coinsEarned :: Array API.CoinsEarned
, plansState :: PlansState
, scheduledRideListResponse ::  Int
, upcomingRide :: Maybe ActiveRide 
, homeScreenBannerTimerID :: String 
, homeScreenBannerTimer :: Int
, onRideBannerTimerID :: String
, onRideBannerTimer :: Int
, blockExpiryTime :: String
, scheduleRideCount :: Maybe (Tuple Int String)
, completingProfileRes :: CompletingProfileRes
, favPopUp :: FavouritePopUp
, isSpecialLocWarrior :: Boolean
, bus_number :: String

}

type FavouritePopUp = {
  visibility :: Boolean,
  title :: String,
  message :: String
}

type PlansState = {
  showSwitchPlanModal :: Boolean,
  plansList :: Array PlanCardState,
  selectedPlan :: Maybe PlanCardState,
  cityOrVehicleChanged :: Boolean,
  freeTrialRides :: Maybe Int,
  totalRidesTaken :: Maybe Int
}

type PlanCardState = {
    id :: String
    , title :: String
    , description :: String
    , isSelected :: Boolean
    , offers :: Array PromoConfig
    , priceBreakup :: Array API.PaymentBreakUp
    , frequency :: String
    , freeRideCount :: Int
    , showOffer :: Boolean

    , clickable :: Boolean
    , showBanner :: Boolean
    , isMyPlan :: Boolean
    , isSelectedLangTamil :: Boolean
    , isActivePlan :: Boolean
    , offerBannerProps :: Maybe CommonRC.OfferBanner
    , isIntroductory :: Boolean
    , offerBannerPlans :: Array String
    , mbCoinDiscountUpto :: Maybe Number
}
type ParkingData = {
  estimatedCharge :: Maybe Number
, finalCharge :: Maybe Number
}

type BannerCarousalData = {
  bannerItem :: Maybe ListItem,
  currentBanner :: Int,
  bannerScrollState :: String,
  currentPage :: Int
}


type DriverGoToState = {
  gotoCount :: Int,
  goToInfo :: Boolean,
  selectedGoTo :: String,
  showGoto :: Boolean,
  savedLocationsArray :: Array GoToLocation,
  gotoValidTill :: String,
  timerInMinutes :: String,
  isGotoEnabled :: Boolean,
  timerId :: String,
  gotoReducedCount :: Maybe Int,
  gotoLocInRange :: Boolean,
  goToPopUpType :: GoToPopUpType,
  gotoEnabledForMerchant :: Boolean,
  confirmGotoCancel :: Boolean,
  savedLocationCount :: Int
}


type EndRideData = {
    actualRideDuration :: Maybe Int,
    actualRideDistance :: Maybe Int,
    rideId :: String,
    zeroCommision :: Int,
    tip :: Maybe Int,
    finalAmount :: Int, 
    riderName :: String,
    rating :: Int,
    feedback :: String,
    disability :: Maybe String,
    payerVpa :: String,
    specialZonePickup :: Maybe Boolean,
    capacity :: Maybe Int,
    serviceTier :: String,
    tollAmbigous :: Boolean,
    tripStartTime :: Maybe String,
    tripEndTime :: Maybe String,
    specialLocationTag :: Maybe String,
    metroRideCoinData :: Maybe MetroRideCoinData
}

type MetroRideCoinData = {
  coinsEarned :: Int,
  metroRideType :: API.MetroRideType
}

type PaymentState = {
  rideCount :: Int,
  totalMoneyCollected :: Int,
  payableAndGST :: Int,
  platFromFee :: Int,
  date :: String,
  makePaymentModal :: Boolean,
  showRateCard :: Boolean,
  paymentStatusBanner :: Boolean,
  paymentStatus :: PP.PaymentStatus,
  invoiceId :: String,
  bannerBG :: String,
  bannerTitle :: String,
  bannerTitleColor :: String,
  banneActionText :: String,
  actionTextColor :: String,
  bannerImage :: String,
  showBannerImage :: Boolean,
  chargesBreakup :: Array PaymentBreakUp,
  blockedDueToPayment :: Boolean,
  dateObj :: String,
  laterButtonVisibility :: Boolean,
  orderId :: String,
  subscribed :: Boolean,
  showShimmer :: Boolean,
  driverBlocked :: Boolean,
  showBlockingPopup :: Boolean,
  totalPendingManualDues :: Number,
  autoPayStatus :: AutoPayStatus
}

type CancelRidePopUpData = {
  delayInSeconds :: Int,
  timerID :: String,
  continueEnabled :: Boolean,
  enableTimer :: Boolean
}

type CancelRideModalData = {
  selectionOptions :: Array Common.OptionButtonList,
  activeIndex ::Maybe Int,
  selectedReasonCode :: String,
  selectedReasonDescription :: String,
  isMandatoryTextHidden :: Boolean,
  isSelectButtonActive :: Boolean
}

type GenderSelectionModalData = {
  selectionOptions :: Array Common.OptionButtonList,
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
  sourceArea :: Maybe String,
  destination :: Maybe String,
  destinationArea :: Maybe String,
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
  waitTimerId :: String,
  notifiedCustomer :: Boolean,
  waitTimeInfo :: Boolean,
  rideCreatedAt :: String,
  specialLocationTag :: Maybe String,
  requestedVehicleVariant :: Maybe String,
  disabilityTag :: Maybe DisabilityType,
  waitTimeSeconds :: Int,
  enableFrequentLocationUpdates :: Boolean,
  tripScheduledAt :: Maybe String,
  tripType :: TripType,
  tripStartTime :: Maybe String,
  tripEndTime :: Maybe String,
  tripDuration :: Maybe Int,
  actualRideDuration :: Maybe Int,
  nextStopAddress :: Maybe String,
  lastStopAddress :: Maybe String,
  nextStopLat :: Maybe Number,
  nextStopLon :: Maybe Number,
  tripActualDistance :: Maybe Int,
  lastStopLat :: Maybe Number,
  lastStopLon :: Maybe Number,
  startOdometerReading :: Maybe Number,
  endOdometerReading :: Maybe Number,
  driverVehicle :: String,
  serviceTier :: String,
  capacity :: Maybe Int,
  estimatedTollCharges :: Number,
  acRide :: Maybe Boolean,
  bapName :: String,
  bookingFromOtherPlatform :: Boolean,
  sourceCity :: String,
  destinationCity :: Maybe String,
  roundTrip :: Boolean,
  returnTime :: String,
  parkingCharge :: Number,
  extraFromLocationInfo :: Maybe String,
  extraToLocationInfo :: Maybe String,
  senderInstructions :: Maybe String,
  receiverInstructions :: Maybe String,
  senderPersonDetails :: Maybe API.PersonDetails,
  receiverPersonDetails :: Maybe API.PersonDetails,
  notifiedReachedDestination :: Boolean,
  stops :: Array API.Stop
}

type HomeScreenProps =  {
  isFreeRide :: Boolean,
  statusOnline :: Boolean,
  goOfflineModal :: Boolean,
  screenName :: String,
  rideActionModal :: Boolean,
  enterOtpModal :: Boolean,
  endRideOtpModal :: Boolean,
  rideOtp :: String,
  odometerValue :: String,
  enterOdometerReadingModal :: Boolean,
  endRideOdometerReadingModal :: Boolean,
  isInvalidOdometer :: Boolean,
  enterOtpFocusIndex :: Int,
  enterOdometerFocusIndex :: Int,
  time :: Int,
  otpIncorrect :: Boolean,
  wrongVehicleVariant :: Boolean,
  endRidePopUp :: Boolean,
  cancelRideModalShow :: Boolean,
  routeVisible :: Boolean,
  otpAttemptsExceeded :: Boolean,
  refreshAnimation :: Boolean,
  showDottedRoute :: Boolean,
  currentStage :: HomeScreenStage,
  advancedRideStage :: HomeScreenStage,
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
  showBonusInfo :: Boolean,
  showlinkAadhaarPopup :: Boolean,
  showAadharPopUp :: Boolean,
  canSendSuggestion :: Boolean,
  showOffer :: Boolean,
  autoPayBanner :: SubscriptionBannerType,
  subscriptionPopupType :: SubscriptionPopupType,
  rcActive :: Boolean, 
  rcDeactivePopup :: Boolean,
  showAccessbilityPopup :: Boolean,
  rentalInfoPopUp :: Boolean,
  showRideCompleted :: Boolean,
  showRideRating :: Boolean,
  showContactSupportPopUp :: Boolean,
  showChatBlockerPopUp :: Boolean,
  showGenericAccessibilityPopUp :: Boolean,
  waitTimeStatus :: TimerStatus,
  isMockLocation :: Boolean,
  accountBlockedPopup :: Boolean,
  accountBlockedPopupDueToCancellations :: Boolean,
  showCoinsPopup :: Boolean,
  isStatsModelExpanded :: Boolean,
  tobeLogged :: Boolean,
  safetyAudioAutoPlay :: Boolean,
  vehicleNSPopup :: Boolean,
  bgLocationPopup :: Boolean,
  specialZoneProps :: SpecialZoneProps,
  coinPopupType :: CoinEarnedPopupType,
  startRideOdometerImage :: Maybe String,
  endRideOdometerImage :: Maybe String,
  arrivedAtStop :: Boolean,
  rideStartRemainingTime :: Int,
  odometerFileId :: Maybe String,
  odometerUploadAttempts :: Int,
  odometerImageUploading :: Boolean,
  showAcWorkingPopup :: Maybe Boolean,
  acExplanationPopup :: Boolean,
  isOdometerReadingsRequired :: Boolean,
  bookingStage :: BookingTypes,
  showAdvancedRidePopUp :: Boolean,
  showInterOperablePopUp :: Boolean,
  showReferralEarnedPopUp :: Boolean,
  showReferNowPopUp :: Boolean,
  showAddUPIPopUp :: Boolean,
  showVerifyUPIPopUp :: Boolean,
  chatServiceKilled :: Boolean,
  isSourceDetailsExpanded  :: Boolean,
  showDeliveryCallPopup :: Boolean,
  checkUpcomingRide :: Boolean,
  homeScreenBannerVisibility :: Boolean,
  rideRequestPill :: RideRequestPill,
  showIntercityRateCard :: Boolean,
  intercityInfoPopUp :: Boolean,
  retryRideList :: Boolean,
  showParcelIntroductionPopup :: Boolean,
  showMetroWarriorWarningPopup :: Boolean,
  setBusOnline :: Boolean,
  bus_input_data :: String,
  showEndRideWithStopPopup :: Boolean,
  triggerGMapsIntent :: Boolean
 }

type RideRequestPill = {
  isPillClickable ::  Boolean,
  pillShimmerVisibility :: Boolean,
  countVisibility ::Boolean
 }

type TollState = {
  showTollChargePopup :: Boolean
, showTollChargeAmbigousPopup :: Boolean
, finalCharge :: Number
, tollAmbigous :: Boolean
, estimatedCharge :: Number
}

data DeliverCallType = SENDER | RECEIVER
derive instance genericDeliverCallType :: Generic DeliverCallType _
instance eqDeliverCallType :: Eq DeliverCallType where eq = genericEq
instance showDeliverCallType :: Show DeliverCallType where show = genericShow
instance encodeDeliverCallType :: Encode DeliverCallType where encode = defaultEnumEncode
instance decodeDeliverCallType :: Decode DeliverCallType where decode = defaultEnumDecode

data SubscriptionBannerType = FREE_TRIAL_BANNER | SETUP_AUTOPAY_BANNER | CLEAR_DUES_BANNER | NO_SUBSCRIPTION_BANNER | DUE_LIMIT_WARNING_BANNER | LOW_DUES_BANNER

derive instance genericSubscriptionBannerType :: Generic SubscriptionBannerType _
instance eqSubscriptionBannerType :: Eq SubscriptionBannerType where eq = genericEq
instance showSubscriptionBannerType :: Show SubscriptionBannerType where show = genericShow
instance encodeSubscriptionBannerType :: Encode SubscriptionBannerType where encode = defaultEnumEncode
instance decodeSubscriptionBannerType :: Decode SubscriptionBannerType where decode = defaultEnumDecode

data SubscriptionPopupType = GO_ONLINE_BLOCKER | LOW_DUES_CLEAR_POPUP | SOFT_NUDGE_POPUP | FREE_TRIAL_POPUP | NO_SUBSCRIPTION_POPUP | FREE_TRIAL_RIDES_POPUP

derive instance genericSubscriptionPopupType :: Generic SubscriptionPopupType _
instance eqSubscriptionPopupType :: Eq SubscriptionPopupType where eq = genericEq
instance showSubscriptionPopupType :: Show SubscriptionPopupType where show = genericShow
instance encodeSubscriptionPopupType :: Encode SubscriptionPopupType where encode = defaultEnumEncode
instance decodeSubscriptionPopupType :: Decode SubscriptionPopupType where decode = defaultEnumDecode

data TripType = OneWay | RoundTrip | Rental | Intercity | RideShare | Delivery

derive instance genericTripType :: Generic TripType _
instance eqTripType :: Eq TripType where eq = genericEq
instance showTripType :: Show TripType where show = genericShow
instance encodeTripType :: Encode TripType where encode = defaultEnumEncode
instance decodeTripType :: Decode TripType where decode = defaultEnumDecode

data DisabilityType = BLIND_AND_LOW_VISION | HEAR_IMPAIRMENT | LOCOMOTOR_DISABILITY | OTHER_DISABILITY | SAFETY | SPECIAL_ZONE_PICKUP

derive instance genericPwdType :: Generic DisabilityType _
instance eqPwdType :: Eq DisabilityType where eq = genericEq
instance showPwdType :: Show DisabilityType where show = genericShow
instance encodePwdType :: Encode DisabilityType where encode = defaultEnumEncode
instance decodePwdType :: Decode DisabilityType where decode = defaultEnumDecode

data DriverStatus = Online | Offline | Silent

data TimerStatus = Scheduled | Triggered | PostTriggered | NoStatus | NotTriggered | DestinationReachedTriggered

derive instance genericTimerStatus :: Generic TimerStatus _
instance eqTimerStatus :: Eq TimerStatus where eq = genericEq
instance showTimerStatus :: Show TimerStatus where show = genericShow
instance encodeTimerStatus :: Encode TimerStatus where encode = defaultEnumEncode
instance decodeTimerStatus :: Decode TimerStatus where decode = defaultEnumDecode

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
  lon :: Number,
  driverInsideThreshold :: Boolean
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
  text :: String,
  screenName :: ScreenName,
  showNewBanner :: Boolean,
  isVisible :: Boolean
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
    status :: String,
    vehicleType :: String,
    customerExtraFee :: Maybe Int,
    purpleTagVisibility :: Boolean,
    gotoTagVisibility :: Boolean,
    spLocTagVisibility :: Boolean,
    specialZoneLayoutBackground :: String,
    specialZoneImage :: String,
    specialZoneText :: String,
    config :: AppConfig,
    goBackTo :: GoBackToScreen,
    specialZonePickup :: Boolean,
    tollCharge :: Number,
    rideType :: String,
    tripStartTime :: Maybe String,
    tripEndTime :: Maybe String,
    vehicleModel :: String,
    acRide :: Maybe Boolean,
    vehicleServiceTier :: String,
    parkingCharge :: Number,
    tripType :: TripType,
    stops :: Array String
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
  categories :: Array Common.CategoryListType,
  issueList :: Array IssueInfo,
  ongoingIssueList :: Array IssueInfo,
  resolvedIssueList :: Array IssueInfo,
  issueListType :: IssueModalType,
  timerId :: String,
  goBackTo :: ScreenName,
  cityConfig :: CityConfig
}

type HelpAndSupportScreenProps = {
  isNoRides :: Boolean,
  enableDummyPopup :: Boolean,
  startTimerforDummyRides :: Boolean,
  popupType :: UpdateDummyTestPopUpType
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
  recordAudioState :: {
    timer         :: String,
    isRecording   :: Boolean,
    isUploading   :: Boolean,
    recordedFile  :: Maybe String,
    recordingDone :: Boolean,
    openAddAudioModel :: Boolean
  },
  addImagesState :: {
    images :: Array Image,
    stateChanged :: Boolean,
    isLoading :: Boolean,
    imageMediaIds :: Array String
  },
  viewImageState :: {
    image :: String,
    imageName :: Maybe String
  },
  recordedAudioUrl :: Maybe String,
  addAudioState :: {
    audioFile :: Maybe String,
    stateChanged :: Boolean
  },
  uploadedImagesIds :: Array String,
  uploadedAudioId :: Maybe String,
  options :: Array
             { issueOptionId :: String
             , option :: String
             , label :: String
             }
}
type Image = {
  image :: String, 
  imageName :: String
}

type ReportIssueChatScreenProps = {
  showSubmitComp :: Boolean,
  showImageModel :: Boolean,
  showAudioModel :: Boolean,
  showRecordModel :: Boolean,
  showCallCustomerModel :: Boolean,
  isReversedFlow :: Boolean,
  showViewImageModel :: Boolean,
  isPopupModelOpen :: Boolean,
  submitIsInProgress :: Boolean,
  timerId :: String
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

data IssueModalType = HELP_AND_SUPPORT_SCREEN_MODAL | ONGOING_ISSUES_MODAL | RESOLVED_ISSUES_MODAL | BACKPRESSED_MODAL

derive instance genericIssueModalType :: Generic IssueModalType _
instance eqIssueModalType :: Eq IssueModalType where eq = genericEq

data UpdateDummyTestPopUpType = TEST_RIDE_RECIEVED | PROBLEM_WITH_TEST | EVERYTHING_OK

derive instance genericUpdateDummyTestPopUpType :: Generic UpdateDummyTestPopUpType _
instance showUpdateDummyTestPopUpType :: Show UpdateDummyTestPopUpType where show = genericShow
instance eqUpdateDummyTestPopUpType :: Eq UpdateDummyTestPopUpType where eq = genericEq
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
  logField :: Object Foreign,
  driverMobileNumber :: String,
  config :: AppConfig
}

type PermissionsScreenProps = {
  isNotificationPermissionChecked :: Boolean
  , isOverlayPermissionChecked :: Boolean
  , isAutoStartPermissionChecked :: Boolean
  , androidVersion :: Int
  , isBatteryOptimizationChecked :: Boolean
  , isLocationPermissionChecked :: Boolean
  , logoutModalView :: Boolean
  , isDriverEnabled :: Boolean
}

------------------------------------------- OnBoardingSubscriptionScreenState ---------------------------
type OnBoardingSubscriptionScreenState = {
  data :: OnBoardingSubscriptionScreenData
  , props :: OnBoardingSubscriptionScreenProps
}

type OnBoardingSubscriptionScreenData = {
  plansList :: Array PlanCardConfig,
  selectedPlanItem :: Maybe PlanCardConfig,
  subscriptionConfig :: SubscriptionConfig,
  reelsData :: Array RC.ReelItem,
  vehicleCategory :: Maybe VehicleCategory,
  freeTrialDays :: Maybe Int,
  freeTrialRides :: Maybe Int,
  totalRidesTaken :: Maybe Int,
  vehicleAndCityConfig :: CommonRC.SubscriptionConfigVariantLevelEntity,
  config :: AppConfig
}

type OnBoardingSubscriptionScreenProps = {
  isSelectedLangTamil :: Boolean,
  screenCount :: Int,
  supportPopup :: Boolean,
  choosePlanSelected :: Boolean
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
  status :: String
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
  , appUpdatedView :: AppUpdatedViewState
}

type AppUpdatedViewState = {
  primaryText :: String,
  secondaryText :: String,
  optionTwoText :: String,
  coverImageUrl :: String,
  popupFlowType :: AppUpdatePoppupFlowType
}

data UpdatePopupType =  AppVersion
                      | DateAndTime
                      | NoUpdatePopup
                      | AppUpdated

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
                      | NotAssigned

derive instance genericHomeScreenStage :: Generic HomeScreenStage _
instance showHomeScreenStage :: Show HomeScreenStage where show = genericShow
instance eqHomeScreenStage :: Eq HomeScreenStage where eq = genericEq
instance decodeHomeScreenStage :: Decode HomeScreenStage where decode = defaultEnumDecode
instance encodeHomeScreenStage :: Encode HomeScreenStage where encode = defaultEnumEncode

data NotificationType =  DRIVER_REACHED
                      | CANCELLED_PRODUCT
                      | DRIVER_ASSIGNMENT
                      | RIDE_REQUESTED
                      | TRIP_STARTED
                      | EDIT_LOCATION
                      | USER_FAVOURITE_DRIVER
                      | DRIVER_REACHED_DESTINATION
                      | TO_METRO_COINS
                      | FROM_METRO_COINS

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
  deepLinkActivated :: Boolean,
  config :: AppConfig
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
  likeStatus :: Boolean,
  shareable :: Boolean
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
  backgroundHolder :: PropValue,
  likeCount :: PropValue,
  viewCount :: PropValue,
  likeCountVisibility :: PropValue,
  shareCountVisibility :: PropValue,
  viewCountVisibility :: PropValue
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
  viewCount :: Int,
  shareable :: Boolean
}

data YoutubeVideoStatus = PLAY | PAUSE

derive instance genericYoutubeVideoStatus:: Generic YoutubeVideoStatus _
instance showYoutubeVideoStatus :: Show YoutubeVideoStatus where show = genericShow
instance eqYoutubeVideoStatus :: Eq YoutubeVideoStatus where eq = genericEq


data ReferralType = SuccessScreen | ComingSoonScreen | ReferralFlow | QRScreen | LeaderBoard

derive instance genericReferralType :: Generic ReferralType _
instance eqReferralType :: Eq ReferralType where eq = genericEq

type DocumentDetailsScreenState = {
  data :: DocumentDetailsScreenData,
  props :: DocumentDetailsScreenProps
}

type DocumentDetailsScreenData = {

}

type DocumentDetailsScreenProps = {

}

type DriverCompleteProfileScreenState = {
  data :: DriverCompleteProfileScreenData,
  props :: DriverCompleteProfileScreenProps
}

type DriverCompleteProfileScreenData = {
    pledge :: Array String
  , vehicalOffer :: Array String
  , languages :: Array String
  , aspirations :: Array String
  , homeTown :: Maybe String
  , calendarState :: CalendarState
  , drivingSince :: Maybe Int
  , viewImageState :: ViewImageState
  , addImagesState :: {
    images :: Array Image,
    stateChanged :: Boolean,
    isLoading :: Boolean,
    imageMediaIds :: Array String
  }
  , datePickerState :: DatePickersState
  , uploadedImagesIds :: Array String
  , addedImages :: Array { image :: String, imageName :: String }
  , inputTextState :: InputTextState
  , vehicleType :: VehicleCategory
}

data Component = Pledge | Aspirations | Empty

derive instance genericComponent :: Generic Component _
instance eqComponent :: Eq Component where eq = genericEq
type InputTextState = {
  feedback :: String,
  component :: Component,
  others :: Others
}

type Others = {
  pledge :: String,
  aspirations :: String
}

type DriverCompleteProfileScreenProps = {
  showImageModel :: Boolean,
  showViewImageModel :: Boolean,
  showInputTextView :: Boolean
}

type DatePickersState = {
    activeIndex :: Int 
  , dates :: Array CalendarDate
  , id :: String
}

type ViewImageState = {
   image :: String,
   imageName :: Maybe String
}

type BookingOptionsScreenState = {
  data :: BookingOptionsScreenData,
  props :: BookingOptionsScreenProps
}

type BookingOptionsScreenData = {
  vehicleType :: String,
  vehicleNumber :: String,
  vehicleName :: String,
  vehicleCapacity :: Int,
  downgradeOptions :: Array ChooseVehicle.Config,
  ridePreferences :: Array RidePreference,
  defaultRidePreference :: RidePreference,
  airConditioned :: Maybe API.AirConditionedTier,
  config :: AppConfig,
  rateCard :: Common.RateCard
}

type RidePreference = {
  airConditioned :: Maybe Number,
  driverRating :: Maybe Number,
  isDefault :: Boolean,
  isSelected :: Boolean,
  longDescription :: Maybe String,
  luggageCapacity :: Maybe Int,
  name :: String,
  seatingCapacity :: Maybe Int,
  serviceTierType :: API.ServiceTierType,
  shortDescription :: Maybe String,
  vehicleRating :: Maybe Number,
  isUsageRestricted :: Boolean,
  priority :: Int,
  rateCardData :: Maybe Common.BreakupList,
  perKmRate :: Maybe Number,
  farePolicyHour :: Maybe API.FarePolicyHour
}

type BookingOptionsScreenProps = {
  isBtnActive :: Boolean,
  downgraded :: Boolean,
  canSwitchToRental :: Maybe Boolean,
  acExplanationPopup :: Boolean,
  fromDeepLink :: Boolean,
  canSwitchToInterCity :: Maybe Boolean,
  canSwitchToIntraCity :: Maybe Boolean,
  showRateCard :: Boolean,
  rateCardLoaded :: Boolean,
  peakTime :: Boolean
}

data LeaderBoardType = Daily | Weekly | Monthly

derive instance genericLeaderBoardType :: Generic LeaderBoardType _
instance eqLeaderBoardType :: Eq LeaderBoardType where eq = genericEq


data DateSelector = DaySelector Common.CalendarDate | WeekSelector Common.CalendarWeek |  MonthSelector Common.CalendarMonth

type RankCardData = {
    goodName :: String
  , profileUrl :: Maybe String
  , rank :: Int
  , rides :: Int
  , gender :: String
}

type AcknowledgementScreenState = {
  data :: AcknowledgementScreenData,
  props :: AcknowledgementScreenProps
}

type AcknowledgementScreenData = {
  illustrationAsset :: String,
  title :: Maybe String,
  description ::Maybe String,
  primaryButtonText :: Maybe String,
  orderId  :: Maybe String,
  amount :: String
}

type AcknowledgementScreenProps = {
  primaryButtonVisibility :: Visibility,
  paymentStatus :: PP.PaymentStatus,
  illustrationType :: IllustrationType
}

data IllustrationType = Image | Lottie

derive instance genericIllustrationType:: Generic IllustrationType _
instance showIllustrationType :: Show IllustrationType where show = genericShow
instance eqIllustrationType :: Eq IllustrationType where eq = genericEq

type PaymentHistoryModelState = {
  paymentHistoryList :: Array PaymentHistoryListItem.Config
}
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

type GlobalProps = {
  aadhaarVerificationRequired :: Boolean,
  driverInformation :: Maybe GetDriverInfoResp,
  driverRideStats :: Maybe DriverProfileStatsResp,
  callScreen :: ScreenName,
  gotoPopupType :: GoToPopUpType,
  addTimestamp :: Boolean,
  bgLocPopupShown :: Boolean,
  onBoardingDocs :: Maybe API.OnboardingDocsRes,
  firstTimeOnboardingStatus :: Boolean
}

--------------------------------------------------------------- SubscriptionScreenState ---------------------------------------------------

type SubscriptionScreenState = {
  data :: SubscriptionScreenData,
  props :: SubscriptionScreenProps
}

type SubscriptionScreenData = {
  myPlanData :: MyPlanData,
  reelsData :: Array RC.ReelItem,
  managePlanData :: ManagePlanData,
  joinPlanData :: JoinPlanData,
  autoPayDetails :: AutoPayDetails,
  driverId :: String,
  paymentMode :: String,
  planId :: String,
  orderId :: Maybe String,
  errorMessage :: String,
  config :: AppConfig,
  switchPlanModalState :: SwitchPlanModalState,
  vehicleAndCityConfig :: CommonRC.SubscriptionConfigVariantLevelEntity,
  linkedVehicleVariant :: String,
  subscriptionDown :: Maybe Boolean
}

type SwitchPlanModalState = {
  showSwitchPlanModal :: Boolean,
  plansList :: Array PlanCardState,
  selectedPlan :: Maybe PlanCardState
}

type AutoPayDetails = {
  isActive :: Boolean,
  detailsList :: Array KeyValType,
  payerUpiId :: Maybe String,
  pspLogo :: String
}

type KeyValType = {
  key :: String,
  val :: String
}

type SubscriptionScreenProps = {
  subView :: SubscriptionSubview,
  myPlanProps :: MyPlanProps,
  managePlanProps :: ManagePlanProps,
  joinPlanProps :: JoinPlanProps,
  popUpState :: Maybe SubscribePopupType,
  resumeBtnVisibility :: Boolean,
  showError :: Boolean,
  showShimmer :: Boolean,
  refreshPaymentStatus :: Boolean,
  confirmCancel :: Boolean,
  isSelectedLangTamil :: Boolean,
  currentLat :: Number,
  currentLon :: Number,
  destLat :: Number,
  destLon :: Number,
  kioskLocation :: Array KioskLocation,
  prevSubView :: SubscriptionSubview,
  noKioskLocation :: Boolean,
  optionsMenuState :: OptionsMenuState,
  redirectToNav :: String,
  lastPaymentType :: Maybe LastPaymentType,
  offerBannerProps :: CommonRC.OfferBanner,
  isEndRideModal :: Boolean
}

type JoinPlanData = {
  allPlans :: Array PlanCardConfig,
  subscriptionStartDate :: String
}

type JoinPlanProps = {
  paymentMode :: String,
  selectedPlanItem :: Maybe PlanCardConfig,
  isIntroductory :: Boolean
}

type ManagePlanData = {
  currentPlan :: PlanCardConfig,
  alternatePlans :: Array PlanCardConfig
}

type ManagePlanProps = {
  selectedPlanItem :: PlanCardConfig
}

type MyPlanData = {
  dueItems :: Array DueItem,
  planEntity :: PlanCardConfig,
  autoPayStatus :: AutoPayStatus,
  lowAccountBalance :: Maybe Number,
  switchAndSave :: Boolean,
  paymentMethodWarning :: Boolean,
  maxDueAmount :: Number,
  totalDueAmount :: Number,
  autoPayDueAmount :: Number,
  manualDueAmount :: Number,
  mandateStatus :: String,
  selectedDue :: String,
  dueBoothCharges :: Maybe Number,
  coinEntity :: Maybe CoinEntity
}

type MyPlanProps = {
  isDuesExpanded :: Boolean,
  isDueViewExpanded :: Boolean,
  overDue :: Boolean,
  multiTypeDues :: Boolean,
  dueType :: FeeType
}

type DueItem = {
  randomId :: String,
  tripDate :: String,
  amount :: Number,
  earnings :: Number,
  noOfRides :: Int,
  scheduledAt :: String,
  paymentStatus :: String,
  feeBreakup :: String,
  plan :: String,
  mode :: FeeType,
  autoPayStage :: Maybe AutopayPaymentStage,
  isSplit :: Boolean,
  specialZoneRideCount :: Maybe Int,
  totalSpecialZoneCharges :: Maybe Number,
  amountPaidByYatriCoins :: Maybe Number 
}

type KioskLocation = {
  longitude :: Number,
  address :: String,
  contact :: Maybe String,
  latitude :: Number,
  landmark :: String,
  distance :: Number
}

type PlanCardConfig = {
    id :: String
  , title :: String
  , description :: String
  , isSelected :: Boolean
  , offers :: Array PromoConfig
  , priceBreakup :: Array PaymentBreakUp
  , frequency :: String
  , freeRideCount :: Int
  , showOffer :: Boolean
}

type PromoConfig = {
    title :: Maybe String
  , isGradient :: Boolean
  , gradient :: Array String
  , hasImage :: Boolean
  , imageURL :: String
  , offerDescription :: Maybe String
  , addedFromUI :: Boolean
  , isPaidByYatriCoins :: Boolean
}

data SubscribePopupType = SuccessPopup | FailedPopup | DuesClearedPopup | CancelAutoPay | SwitchedPlan | SupportPopup | PaymentSuccessPopup

derive instance genericSubscribePopupType :: Generic SubscribePopupType _
instance showSubscribePopupType :: Show SubscribePopupType where show = genericShow
instance eqSubscribePopupType :: Eq SubscribePopupType where eq = genericEq
instance decodeSubscribePopupType :: Decode SubscribePopupType where decode = defaultEnumDecode
instance encodeSubscribePopupType :: Encode SubscribePopupType where encode = defaultEnumEncode

data AutoPayStatus = ACTIVE_AUTOPAY | SUSPENDED | PAUSED_PSP | CANCELLED_PSP | NO_AUTOPAY | PENDING | MANDATE_FAILED | RESUME_PENDING

derive instance genericAutoPayStatus:: Generic AutoPayStatus _
instance showAutoPayStatus:: Show AutoPayStatus where show = genericShow
instance eqAutoPayStatus:: Eq AutoPayStatus where eq = genericEq
instance encodeAutoPayStatus :: Encode AutoPayStatus where encode = defaultEnumEncode

data SubscriptionSubview = JoinPlan | ManagePlan | MyPlan | PlanDetails | FindHelpCentre | DuesView | DueDetails | NoSubView 

derive instance genericSubscriptionSubview :: Generic SubscriptionSubview _
instance showSubscriptionSubview :: Show SubscriptionSubview where show = genericShow
instance eqSubscriptionSubview :: Eq SubscriptionSubview where eq = genericEq
instance decodeSubscriptionSubview :: Decode SubscriptionSubview where decode = defaultEnumDecode
instance encodeSubscriptionSubview :: Encode SubscriptionSubview where encode = defaultEnumEncode

data OptionsMenuState = ALL_COLLAPSED | PLAN_MENU  -- SUPPORT_MENU  | CALL_MENU disabled for now.

derive instance genericOptionsMenuState :: Generic OptionsMenuState _
instance showOptionsMenuState :: Show OptionsMenuState where show = genericShow
instance eqOptionsMenuState :: Eq OptionsMenuState where eq = genericEq

type LocalStoreSubscriptionInfo = {
  projectedInvoice :: Number,
  maxDueLimit :: Number,
  expiry :: String
}

---------------------------------------------------- PaymentHistoryScreen ----------------------------------

type PaymentHistoryScreenState = {
  data :: PaymentHistoryScreenData,
  props :: PaymentHistoryScreenProps
}

type PaymentHistoryScreenData = {
  transactionDetails :: TransactionInfo,
  planData :: PlanCardConfig,
  autoPayList :: Array PaymentListItem,
  manualPayList :: Array PaymentListItem,
  gradientConfig :: Array GradientConfig,
  autoPayStatus :: AutoPayStatus
}

type TransactionInfo = {
  notificationStatus :: Maybe AutopayPaymentStage,
  paymentStatus :: PP.PaymentStatus,
  statusTime :: String,
  details :: Array TransactionListItem,
  manualSpecificDetails :: Array DueCard,
  isSplit :: Boolean,
  isAutoPayFailed :: Boolean,
  feeType :: FeeType,
  numOfDriverFee :: Int,
  isCoinDiscountApplied :: Boolean,
  isCoinCleared :: Boolean
}
type PaymentListItem = {
  transactionDate :: String,
  invoiceId :: String,
  paymentStatus :: PP.PaymentStatus,
  amount :: Number,
  feeType :: FeeType,
  description :: String,
  ridesTakenDate :: String,
  isPaidByYatriCoins :: Boolean
}

type ChargeBreakupItem = {
  amount :: Number,
  component :: String
}

type TransactionListItem = {
  key :: String,
  title :: String,
  val :: String
}

type DueCard = {
  date :: String,
  planType :: String,
  offerApplied :: Maybe PromoConfig,
  noOfRides :: Int,
  totalEarningsOfDay :: Number,
  dueAmount :: Number,
  fareBreakup :: String,
  expanded :: Boolean,
  isAutoPayFailed :: Boolean,
  isSplitPayment :: Boolean,
  id :: String,
  scheduledAt :: Maybe String,
  paymentMode :: FeeType,
  paymentStatus :: Maybe String,
  boothCharges :: Maybe String,
  isDue :: Boolean,
  amountPaidByYatriCoins :: Maybe Number
}

type PaymentHistoryScreenProps = {
  subView :: PaymentHistorySubview,
  autoPayHistory :: Boolean,
  autoPaySetup :: Boolean,
  selectedDue :: String,
  offset :: Int,
  enableLoadMore :: Boolean
}

data PaymentHistorySubview = PaymentHistory | TransactionDetails | RideDetails

derive instance genericPaymentHistorySubview :: Generic PaymentHistorySubview _
instance showPaymentHistorySubview :: Show PaymentHistorySubview where show = genericShow
instance eqPaymentHistorySubview :: Eq PaymentHistorySubview where eq = genericEq
instance decodePaymentHistorySubview :: Decode PaymentHistorySubview where decode = defaultEnumDecode
instance encodePaymentHistorySubview :: Encode PaymentHistorySubview where encode = defaultEnumEncode


type DriverSavedLocationScreenState = {
  data :: DriverSavedLocationScreenData,
  props :: DriverSavedLocationScreenProps
}

type DriverSavedLocationScreenData = {
  address :: String,
  currentLat :: Maybe String,
  currentLon :: Maybe String,
  savedLocationsArray :: Array GoToLocation,
  predictions :: Array PredictionItem,
  saveLocationObject :: SaveLocationObject,
  maxGotoLocations :: Int,
  locationSelectType :: LocationSelectType
}

data LocationSelectType = SET_LOC | CURRENT_LOC

derive instance genericLocationSelectType :: Generic LocationSelectType _
instance showLocationSelectType :: Show LocationSelectType where show = genericShow
instance eqLocationSelectType :: Eq LocationSelectType where eq = genericEq
instance decodeLocationSelectType :: Decode LocationSelectType where decode = defaultEnumDecode
instance encodeLocationSelectType :: Encode LocationSelectType where encode = defaultEnumEncode

type GoToLocation = {
  id :: String,
  lat :: Number,
  lon :: Number,
  address :: String,
  tag :: String,
  disabled :: Boolean
}

type PredictionItem = {
 description :: String,
 title :: String,
 placeId :: Maybe String,
 distance :: Maybe Int
}


type DriverSavedLocationScreenProps = {
  viewType :: SavedLocationScreenType,
  selectedPrediction :: PredictionItem,
  confirmDelete :: Boolean,
  selectedLocation :: GoToModal.GoToModalConfig,
  fromEditButton :: Maybe GoToScrEntryType,
  gotBackToHomeScreen :: Boolean,
  errorText :: Maybe String,
  defTag :: String
}

data GoToScrEntryType = FromEdit | FromPrediction

derive instance genericGoToScrEntryType :: Generic GoToScrEntryType _
instance eqGoToScrEntryType :: Eq GoToScrEntryType where eq = genericEq

data SavedLocationScreenType = GoToList | SearchLocation | LOCATE_ON_MAP | ConfirmLocation

derive instance genericSavedLocationScreenType :: Generic SavedLocationScreenType _
instance eqSavedLocationScreenType :: Eq SavedLocationScreenType where eq = genericEq

data GoToPopUpType = REDUCED Int | MORE_GOTO_RIDES | VALIDITY_EXPIRED | REACHED_HOME | NO_POPUP_VIEW

derive instance genericGoToPopUpType :: Generic GoToPopUpType _
instance showGoToPopUpType :: Show GoToPopUpType where show = genericShow
instance eqGoToPopUpType :: Eq GoToPopUpType where eq = genericEq
instance standardEncodeGoToPopUpType :: StandardEncode GoToPopUpType where standardEncode _ = standardEncode {}
instance decodeGoToPopUpType :: Decode GoToPopUpType where decode = defaultDecode
instance encodeGoToPopUpType  :: Encode GoToPopUpType where encode = defaultEncode

data HomeScreenPopUpTypes = KnowMore | DisableGotoPopup | LocInRange | AccountBlocked | VehicleNotSupported | BgLocationPopup | TopAcDriver | ReferralEarned | ReferNow | AddUPI | VerifyUPI | AccountBlockedDueToCancellations | MetroWarriorWarning

derive instance genericHomeScreenPopUpTypes :: Generic HomeScreenPopUpTypes _
instance showHomeScreenPopUpTypes :: Show HomeScreenPopUpTypes where show = genericShow
instance eqHomeScreenPopUpTypes :: Eq HomeScreenPopUpTypes where eq = genericEq
instance standardEncodeHomeScreenPopUpTypes :: StandardEncode HomeScreenPopUpTypes where standardEncode _ = standardEncode {}
instance decodeHomeScreenPopUpTypes :: Decode HomeScreenPopUpTypes where decode = defaultDecode
instance encodeHomeScreenPopUpTypes  :: Encode HomeScreenPopUpTypes where encode = defaultEncode

data MenuOptions = DRIVER_PRESONAL_DETAILS |DRIVER_BANK_DETAILS | DRIVER_VEHICLE_DETAILS | ABOUT_APP | MULTI_LANGUAGE | HELP_AND_FAQS | DRIVER_LOGOUT | DRIVER_BOOKING_OPTIONS | REFER | APP_INFO_SETTINGS | LIVE_STATS_DASHBOARD | GO_TO_LOCATIONS | DOCUMENTS
derive instance genericMenuoptions :: Generic MenuOptions _
instance eqMenuoptions :: Eq MenuOptions where eq = genericEq

type Listtype =
    { icon :: String,
      menuOptions :: MenuOptions
    }

type SaveLocationObject = {
  position :: Location,
  address :: String,
  tag :: String
}

type Tag = {
  background :: String, 
  image :: String, 
  visibility :: Boolean, 
  text :: String, 
  textColor :: String
}

---------------------------------------------ChooseCityScreen -------------------------------------

type ChooseCityScreenState = {
  data :: ChooseCityScreenData,
  props :: ChooseCityScreenProps
}

type ChooseCityScreenData = {
  config :: AppConfig,
  locationSelected :: Maybe String,
  merchantOperatingCityConfig :: Array CityConfig,
  logField :: Object Foreign
}

type ChooseCityScreenProps = {
  selectedLanguage :: String,
  currentStage :: ChooseCityScreenStage,
  isLocationPermissionGiven :: Boolean,
  radioMenuFocusedLang :: String,
  radioMenuFocusedCity :: String,
  locationUnserviceable :: Boolean,
  locationDetectionFailed :: Boolean,
  isMockLocation :: Boolean,
  lat :: Number,
  lon :: Number
}

data ChooseCityScreenStage = SELECT_LANG | SELECT_CITY | ENABLE_PERMISSION | DETECT_LOCATION

derive instance genericChooseCityScreenStage :: Generic ChooseCityScreenStage _
instance showChooseCityScreenStage :: Show ChooseCityScreenStage where show = genericShow
instance eqChooseCityScreenStage :: Eq ChooseCityScreenStage where eq = genericEq

---------------------------------------------WelcomeScreen -------------------------------------

type WelcomeScreenState = {
  data :: WelcomeScreenData
}

type WelcomeScreenData = {
  logField :: Object Foreign
}
---------------------------------------------------- DriverEarningsScreen ----------------------------------

type DriverEarningsScreenState = {
  data :: DriverEarningsScreenData,
  props :: DriverEarningsScreenProps
}

type DriverEarningsScreenData = {
  coinsEarned :: Int,
  coinsUsed :: Int,
  coinBalance :: Int,
  coinsEarnedPreviousDay :: Int,
  coinHistoryItems :: Array CoinHistoryItem,
  usageHistoryItems :: Array CoinHistoryItem,
  coinsEarnedToday :: Int,
  expiringCoins :: Int,
  expiringDays :: Int,
  hasActivePlan :: Boolean,
  timerID :: String,
  timer :: Int,
  totalCoinConvertedToCash :: Number,
  coinConvertedToCashUsedForLatestDues :: Maybe Int,
  coinConvertedTocashLeft :: Number,
  coinConversionRate :: Number,
  coinsToUse :: Int,
  config :: AppConfig,
  earningHistoryItems :: Array CoinHistoryItem,
  rideHistoryItems :: Array RidesInfo,
  selectedRideHistoryItem :: IndividualRideCardState,
  weeklyEarningData :: Array WeeklyEarning,
  anyRidesAssignedEver :: Boolean,
  logField :: Object Foreign
, coinInfoRes :: Maybe (Array API.CoinInfo)
}

type DriverEarningsScreenProps = {
  subView :: DriverEarningsSubView,
  date :: String,
  popupType :: DriverEarningsPopupType,
  showCoinsRedeemedAnim :: String,
  showCoinsEarnedAnim :: Maybe Int,
  calendarState :: CalendarState,
  showCoinsUsagePopup :: Boolean,
  selectedBarIndex :: Int,
  weekIndex :: Int,
  totalEarningsData :: TotalEarningsData,
  currWeekData :: Array WeeklyEarning,
  weekDay :: Array String,
  currentWeekMaxEarning :: Int,
  showShimmer :: Boolean,
  startDate :: String,
  endDate :: String,
  gotDataforWeek :: Array Boolean,
  coinConvertedSuccess :: Boolean,
  individualQuestion :: FaqQuestions,
  callRideSummaryApi :: Boolean,
  loadMoreButtonVisibility :: Boolean,
  offsetValue :: Int
}

type CalendarState = { 
  calendarPopup :: Boolean,
  endDate :: Maybe Common.CalendarModalDateObject,
  selectedTimeSpan :: Common.CalendarModalDateObject,
  startDate :: Maybe Common.CalendarModalDateObject,
  weeks  :: Array Common.CalendarModalWeekObject
}

type FaqQuestions = {
  question :: String,
  videoLink :: Maybe String,
  answer :: Array AnswerConfig,
  showTable :: Boolean,
  tag :: CoinsQuestionTag
}

data CoinsQuestionTag = HowEarnLosePoints | DiscountPoints | PointsValidity | HowEarnPoints | HowUsePoints | PointsEarnEligibility | NothingCoinsQuestionTag

derive instance genericCoinsQuestionTag :: Generic CoinsQuestionTag _
instance eqCoinsQuestionTag :: Eq CoinsQuestionTag where eq = genericEq


type AnswerConfig = {
  answer :: String,
  hyperLinkText :: Maybe String,
  hyperLinkUrl :: Maybe String,
  hyperLinkColor :: Maybe String
}

type WeeklyEarning = {
  earnings :: Int,
  rideDistance :: Int,
  rideDate :: String,
  noOfRides :: Int,
  percentLength :: Number
}

type TotalEarningsData = {
  fromDate :: String,
  toDate :: String,
  totalEarnings :: Int,
  totalRides :: Int,
  totalDistanceTravelled :: Int
}

newtype CachedEarningsForDriver = CachedEarningsForDriver {
  id :: String,
  earningsData :: Array WeeklyEarning
}

derive instance genericCachedEarningsForDriver :: Generic CachedEarningsForDriver _
derive instance newtypeCachedEarningsForDriver :: Newtype CachedEarningsForDriver _
instance showCachedEarningsForDriver :: Show CachedEarningsForDriver where show = genericShow
instance decodeCachedEarningsForDriver :: Decode CachedEarningsForDriver where decode = defaultDecode
instance encodeCachedEarningsForDriver :: Encode CachedEarningsForDriver where encode = defaultEncode

data DriverEarningsSubView = EARNINGS_VIEW | YATRI_COINS_VIEW | USE_COINS_VIEW | FAQ_VIEW | FAQ_QUESTON_VIEW

derive instance genericDriverEarningsSubView :: Generic DriverEarningsSubView _
instance showDriverEarningsSubView :: Show DriverEarningsSubView where show = genericShow
instance eqDriverEarningsSubView :: Eq DriverEarningsSubView where eq = genericEq
instance decodeDriverEarningsSubView :: Decode DriverEarningsSubView where decode = defaultEnumDecode
instance encodeDriverEarningsSubView :: Encode DriverEarningsSubView where encode = defaultEnumEncode

type CoinHistoryItem = {
  event :: String,
  destination :: Maybe String,
  timestamp :: String,
  coins :: Int,
  earnings ::  Maybe Int,
  status :: Maybe String,
  tagImages :: Array String,
  cash :: Number,
  vehicleVariant :: String,
  isValueAddNP :: Boolean,
  bapName :: String
}

type TableItem = {
  key :: String,
  value :: String
}

data DriverEarningsPopupType = COIN_TO_CASH_POPUP | COIN_TO_CASH_FAIL_POPUP | NO_COINS_POPUP | COINS_EXPIRING_POPUP | NO_POPUP

derive instance genericDriverEarningsPopupType :: Generic DriverEarningsPopupType _
instance showDriverEarningsPopupType :: Show DriverEarningsPopupType where show = genericShow
instance eqDriverEarningsPopupType :: Eq DriverEarningsPopupType where eq = genericEq


--------------------------------------------- Benefits.BenefitsScreen -------------------------------------

type BenefitsScreenState = {
  data :: BenefitsScreenData,
  props :: BenefitsScreenProps
}

type BenefitsScreenData = {
    logField :: Object Foreign
  , config :: AppConfig
  , totalReferredDrivers :: Int
  , totalActivatedCustomers :: Int
  , totalReferredCustomers :: Int
  , referralCode :: String
  , rank :: Maybe Int
  , totalEligibleDrivers :: Maybe Int
  , moduleList :: LmsModuleList
  , bannerData :: BannerCarousalData
  , cityConfig :: CityConfig
  , eligiblePayoutAmount :: Int
  , lastPayoutAt :: Maybe String
  , payoutAmountPaid :: Int
  , payoutVpa :: Maybe String
  , payoutRewardAmount :: Maybe Int
}

type BenefitsScreenProps = {
  showDriverReferralQRCode :: Boolean
, showNewDriverReferralText :: Boolean
, driverReferralType :: DriverReferralType
, referralInfoPopType :: ReferralInfoPopType
, selectedModule :: Maybe LmsModuleRes
, showShimmer :: Boolean
, isPayoutEnabled :: Maybe Boolean
, bannerLength :: Int
, glBannerClickable :: Boolean
}

type LmsModuleList =
  { completed :: Array LmsModuleRes,
    remaining :: Array LmsModuleRes
  }

data DriverReferralType = DRIVER | CUSTOMER

derive instance genericDriverReferralType :: Generic DriverReferralType _
instance showDriverReferralType :: Show DriverReferralType where show = genericShow
instance eqDriverReferralType :: Eq DriverReferralType where eq = genericEq

data ReferralInfoPopType = REFERRED_DRIVERS_POPUP | REFERRED_CUSTOMERS_POPUP | ACTIVATED_CUSTOMERS_POPUP | NO_REFERRAL_POPUP

derive instance genericReferralInfoPopType :: Generic ReferralInfoPopType _
instance showReferralInfoPopType :: Show ReferralInfoPopType where show = genericShow
instance eqReferralInfoPopType :: Eq ReferralInfoPopType where eq = genericEq

data GoBackToScreen = Earning | Home

derive instance genericGoBackToScreen :: Generic GoBackToScreen _
instance showGoBackToScreen :: Show GoBackToScreen where show = genericShow
instance eqGoBackToScreen :: Eq GoBackToScreen where eq = genericEq
instance encodeGoBackToScreen :: Encode GoBackToScreen where encode = defaultEnumEncode
instance decodeGoBackToScreen :: Decode GoBackToScreen where decode = defaultEnumDecode


--------------------------------------------- Benefits.LmsVideoScreen -------------------------------------

type LmsVideoScreenState = {
  data :: LmsVideoScreenData,
  props :: LmsVideoScreenProps
}

type LmsVideoScreenData = {
  config :: AppConfig,
  logField :: Object Foreign,
  videosScreenData :: LmsGetVideo
}

type LmsGetVideo = {
  quizEnabled :: Boolean,
  completed :: Array LmsVideoRes,
  pending :: Array LmsVideoRes,
  quizStatus :: LmsEntityCompletionStatus,
  selectedTranslatedModule :: Maybe LmsTranslatedModuleInfoRes
}

type LmsVideoScreenProps = {
  selectedModule :: Maybe LmsModuleRes,
  showShimmer :: Boolean,
  showError :: Boolean,
  selectedLanguage :: String,
  isFetchingQuiz :: Boolean
}

--------------------------------------------- Benefits.LmsQuizScreen -------------------------------------

type LmsQuizScreenState = {
  data :: LmsQuizScreenData,
  props :: LmsQuizScreenProps
}

type LmsQuizScreenData = {
  config :: AppConfig,
  logField :: Object Foreign,
  questions :: Array LmsQuestion
}

type LmsQuizScreenProps = {
  selectedTranslatedModule :: Maybe LmsTranslatedModuleInfoRes,
  currentQuestionSelectedOptionsData :: CurrentQuestionSelectedOptionData,
  currentQuestionIndex :: Int,
  isRetryEnabled :: Boolean,
  showShimmer :: Boolean,
  selectedLanguage :: String,
  languageUpdated :: Boolean,
  animationVisibilty :: Boolean,
  exitPopupVisible :: Boolean,
  isConfirming :: Boolean,
  isConfirmed :: Boolean,
  bottomButtonVisibility :: Boolean
}

type LmsQuestion = {
    questionId :: String
  , moduleId :: String
  , language :: String
  , question :: QuizQuestion
  , options :: QuizOptions
  , previousHistory :: Maybe LmsQuizHistory
  , questionStatusDuringQuiz :: QuestionStatus
  , validationRes :: Maybe QuestionConfirmRes
}

data QuestionStatus = QUESTION_NOT_ATTEMPTED | QUESTION_CORRECT | QUESTION_INCORRECT | QUESTION_ATTEMPTING

instance eqQuestionStatus :: Eq QuestionStatus where eq = genericEq
derive instance genericQuestionStatus :: Generic QuestionStatus _
instance showQuestionStatus :: Show QuestionStatus where show = genericShow

type CurrentQuestionSelectedOptionData = {
  selectedSingleOption :: Maybe SelectedOption,
  selectedMultipleOptions :: Array SelectedOption
}

type SelectedOption = {
  optionId :: String,
  isCorrect :: Boolean,
  validated :: Boolean
}

type SpecialZoneProps = {
    specialZonePopup :: Boolean
  , nearBySpecialZone :: Boolean
  , currentGeoHash :: String
}

type DocumentCaptureScreenState = {
  data :: DocumentCaptureScreenData ,
  props :: DocumentCaptureScreenProps
}

type DocumentCaptureScreenData = {
  imageBase64 :: String,
  docType :: RegisterationStep,
  errorMessage :: Maybe String,
  vehicleCategory :: Maybe VehicleCategory,
  docId :: String,
  linkedRc :: Maybe String,
  cityConfig :: CityConfig
} 

type DocumentCaptureScreenProps = {
  validateDocModal :: Boolean,
  logoutModalView :: Boolean,
  validating :: Boolean,
  menuOptions :: Boolean,
  confirmChangeVehicle :: Boolean,
  contactSupportModal :: AnimType
} 


type UpdateRouteSrcDestConfig = {
  srcLat :: Number,
  srcLon :: Number,
  destLat :: Number,
  destLon :: Number,
  source :: String,
  destination :: String
}

data CoinEarnedPopupType = 
    RIDE_MORE_EARN_COIN 
  | TWO_MORE_RIDES 
  | ONE_MORE_RIDE 
  | TWO_RIDE_COMPLETED 
  | FIVE_RIDE_COMPLETED 
  | EIGHT_RIDE_COMPLETED
  | TEN_RIDE_COMPLETED 
  | REFER_AND_EARN_COIN 
  | CONVERT_COINS_TO_CASH 
  | NO_COIN_POPUP
  | SIX_RIDE_COMPLETED

derive instance genericCoinEarnedPopupType :: Generic CoinEarnedPopupType _
instance showCoinEarnedPopupType :: Show CoinEarnedPopupType where show = genericShow
instance eqCoinEarnedPopupType :: Eq CoinEarnedPopupType where eq = genericEq

type CoinEarnedPopupTypeShown = {
  rideMoreEarnCoin :: String,
  twoMoreRides :: String,
  oneMoreRide :: String,
  twoRideCompleted :: String,
  fiveRideCompleted :: String,
  sixRideCompleted :: String,
  eightRideCompleted :: String,
  referAndEarnCoin :: String,
  convertCoinsToCash :: String,
  tenRideCompleted :: String
}

type RateCardScreenState = {
  data :: RateCardScreenData,
  props :: RateCardScreenProps
}

type RateCardScreenData = {
  ridePreferences :: Array RidePreference,
  rateCard :: Common.RateCard,
  cityConfig :: CityConfig,
  config :: AppConfig
}

type RateCardScreenProps = {
  sliderVal :: Int,
  showRateCard :: Boolean,
  sliderDefVal :: Int,
  incrementUnit :: Int,
  sliderMinValue :: Int,
  sliderMaxValue :: Int,
  sliderLoading :: Boolean
}


type RideRequestCardState =
  {
    date :: String,
    time :: String,
    source :: String,
    distance :: String,
    destination :: String,
    totalAmount :: String,
    cardVisibility :: String,
    shimmerVisibility :: String,
    carImage :: String,
    rideType :: String,
    vehicleType :: String,
    srcLat ::  Number,
    srcLong :: Number,
    desLat :: Number,
    desLong :: Number,
    id :: String,
    image ::  String,
    visible ::  Boolean,
    pillColor :: String,
    overlayVisiblity :: String,
    visiblePill :: String,
    cornerRadius :: String,
    imageType :: String,
    estimatedDuration :: String
  }
type RideCardItemState =
  {
    date :: PropValue,
    time :: PropValue,
    source :: PropValue,
    distance ::  PropValue,
    destination :: PropValue,
    totalAmount :: PropValue,
    cardVisibility :: PropValue,
    shimmerVisibility :: PropValue,
    carImage :: PropValue,
    rideType :: PropValue,
    vehicleType :: PropValue,
    srcLat ::  PropValue,
    srcLong :: PropValue,
    desLat :: PropValue,
    desLong :: PropValue,
    id :: PropValue,
    image :: PropValue,
    visible :: PropValue,
    pillColor :: PropValue,
    overlayVisiblity :: PropValue,
    visiblePill :: PropValue,
    cornerRadius :: PropValue,
    imageType :: PropValue,
    estimatedDuration :: PropValue


  }
-------------------------------------------------- Onboarding LiveSelfie, PAN and Aadhaar Integration ------------------------------------
data HyperVergeKycResult = HyperVergeKycResult
  { status :: Maybe String,
    transactionId :: Maybe String,
    details :: Maybe Details,
    errorCode :: Maybe Int,
    errorMessage :: Maybe String
  }
derive instance genericHyperVergeKycResult :: Generic HyperVergeKycResult _
instance decodeHyperVergeKycResult :: Decode HyperVergeKycResult where decode = defaultDecode
instance encodeHyperVergeKycResult  :: Encode HyperVergeKycResult where encode = defaultEncode
instance showHyperVergeKycResult :: Show HyperVergeKycResult where show = genericShow

data Details =
  LIVE_SELFIE LiveSelfie
  | PAN_DETAILS PanDetails
  | AADHAAR_DETAILS AadhaarCardDetails

derive instance genericDetails :: Generic Details _
instance decodeDetails :: Decode Details
  where
   decode body = (AADHAAR_DETAILS <$> decode body) <|> (PAN_DETAILS <$> decode body) <|> (LIVE_SELFIE <$> decode body) <|> (fail $ ForeignError "Unknown response")
instance encodeDetails  :: Encode Details where encode = defaultEncode
instance showDetails :: Show Details where show = genericShow

data LiveSelfie = LiveSelfie
  { selfieImage :: String,
    selfieURL :: Maybe String,
    declineReason :: Maybe String,
    errorCode :: Maybe String
  }
  
derive instance genericLiveSelfie :: Generic LiveSelfie _
instance decodeLiveSelfie :: Decode LiveSelfie where decode = defaultDecode
instance encodeLiveSelfie  :: Encode LiveSelfie where encode = defaultEncode
instance showLiveSelfie :: Show LiveSelfie where show = genericShow


data PanDetails = PanDetails
  { panImage :: String,
    panURL :: Maybe String,
    pan :: Maybe String,
    name :: Maybe String,
    dob :: Maybe String,
    gender :: Maybe String,
    panDB_name :: Maybe String,
    declineReason :: Maybe String,
    errorCode :: Maybe String
  }

derive instance genericPanDetails :: Generic PanDetails _
instance decodePanDetails :: Decode PanDetails where decode = defaultDecode
instance encodePanDetails  :: Encode PanDetails where encode = defaultEncode
instance showPanDetails :: Show PanDetails where show = genericShow


data AadhaarCardDetails = AadhaarCardDetails
  { aadhaarFrontImage :: String,
    aadhaarBackImage :: Maybe String,
    aadhaarFrontURL :: Maybe String,
    aadhaarBackURL :: Maybe String,
    idNumber :: Maybe String,
    fullName :: Maybe String,
    dob :: Maybe String,
    address :: Maybe String,
    city :: Maybe String,
    pincode :: Maybe String,
    declineReason :: Maybe String,
    errorCode :: Maybe String
  }

derive instance genericAadhaarCardDetails :: Generic AadhaarCardDetails _
instance decodeAadhaarCardDetails :: Decode AadhaarCardDetails where decode = defaultDecode
instance encodeAadhaarCardDetails  :: Encode AadhaarCardDetails where encode = defaultEncode
instance showAadhaarCardDetails :: Show AadhaarCardDetails where show = genericShow

data HvErrorCode = HvErrorCode
  { errorCode :: Int
  }

derive instance genericHvErrorCode :: Generic HvErrorCode _
instance decodeHvErrorCode :: Decode HvErrorCode where decode = defaultDecode
instance encodeHvErrorCode  :: Encode HvErrorCode where encode = defaultEncode

data AppUpdatePoppupFlowType = REG_PROF_PAN_AADHAAR | NORMAL

derive instance genericAppUpdatePoppupFlowType :: Generic AppUpdatePoppupFlowType _
instance showAppUpdatePoppupFlowType :: Show AppUpdatePoppupFlowType where show = genericShow
instance eqAppUpdatePoppupFlowType :: Eq AppUpdatePoppupFlowType where eq = genericEq

type GullakSDKResp = {
  amount :: Number,
  quantity :: String,
  responseMessage :: String,
  responseCode :: Int,
  isNewUser :: Boolean
}
------------------------------------------------------- HOTSPOT_SCREEN ------------------------------------------------------------------------

type HotspotScreenState = {
  data :: HotspotScreenData,
  props :: HotspotScreenProps
}

type HotspotScreenData = {
  pointsWithWeight :: Array PointsWithWeight,
  dataExpiryAt :: String,
  currentDriverLat :: Number,
  currentDriverLon :: Number,
  config :: AppConfig,
  logField :: Object Foreign
}

type HotspotScreenProps = {
  lastUpdatedTime :: String,
  showNavigationSheet :: Boolean,
  refreshAnimation :: Boolean,
  selectedCircleColor :: String,
  selectedCircleLatLng :: API.LatLong,
  isAnyCircleSelected :: Boolean,
  mapCorners :: {
    leftPoint :: String,
    topPoint :: String,
    rightPoint :: String,
    bottomPoint :: String
  }
}

type PointsWithWeight = {
  latlong :: API.LatLong,
  weight :: Number,
  id :: String
}

type NotificationBody = {
  title :: String,
  message :: String
} 

-------------------------------------------------- Parcel Image Upload Screen ------------------------------------

type UploadParcelImageScreenState = {
  data :: UploadParcelImageScreenData ,
  props :: UploadParcelImageScreenProps
}

type UploadParcelImageScreenData = {
  rideId :: String,
  imagePath :: String,
  errorMessage :: Maybe String,
  imageId :: String
} 

type UploadParcelImageScreenProps = {
  showConfirmAndUploadButton :: Boolean,
  isStartRideActive :: Boolean,
  uploading :: Boolean
} 

type MetroWarriorsScreenState = {
  data :: MetroWarriorsScreenData,
  props :: MetroWarriorsScreenProps
}

type MetroWarriorsScreenData = {
  listItem :: Maybe ListItem,
  stationList :: Array API.SpecialLocation,
  searchString :: Maybe String,
  stationData :: MetroWarriorData,
  remoteConfigData :: RC.MetroWarriorConfigEntity
}

type MetroWarriorsScreenProps = {
  showStationList :: Boolean,
  showShimmer :: Boolean
}

type MetroWarriorData = {
  primaryStation :: Maybe API.SpecialLocationWarrior,
  secondaryStationsData :: Array String,
  isSpecialLocWarrior :: Boolean
}