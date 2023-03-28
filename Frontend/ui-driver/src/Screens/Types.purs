{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.Types where

import Common.Types.App (CancellationReasons)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Foreign.Class (class Decode, class Encode)
import Halogen.VDom.DOM.Prop (PropValue)
import Prelude (class Eq, class Show)
import Presto.Core.Utils.Encoding (defaultDecode, defaultEncode)
import Presto.Core.Utils.Encoding (defaultEnumDecode, defaultEnumEncode)
import PrestoDOM (Visibility)
import Services.APITypes (Route, Status, MediaType)
import Styles.Types (FontSize)

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
  dateOfRegistrationView :: String
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
  openRegistrationDateManual :: Boolean
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
}

type UploadDrivingLicenseStateProps = {
    openRegistrationModal :: Boolean
  , openLicenseManual :: Boolean
  , input_data :: String
  , clickedButtonType :: String
  , openGenericMessageModal :: Boolean
  , errorVisibility :: Boolean
  , openDateOfIssueManual :: Boolean
}

 -- ############################################################# RegistrationScreen ################################################################################
type RegistrationScreenState = {
  data :: RegistrationScreenData,
  props :: RegistrationScreenProps
}
type RegistrationScreenData = {}
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
  letterSpacing :: Number,
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
  vehicleColor :: String
}

type DriverProfileScreenProps = {
  logoutModalView :: Boolean
}
-----------------------------------------------ApplicationStatusScreen --------------------------------------- 
type ApplicationStatusScreenState = {
  data :: ApplicationStatusScreenData,
  props :: ApplicationStatusScreenProps
}

type ApplicationStatusScreenData =  {
  rcVerificationStatus :: String,
  dlVerificationStatus :: String
}

type ApplicationStatusScreenProps =  {
  isSelected :: Boolean
}

--------------------------------------------------------------- EnterMobileNumberScreenState -----------------------------------------------------------------------------
type EnterMobileNumberScreenState = {
  data :: EnterMobileNumberScreenStateData,
  props :: EnterMobileNumberScreenStateProps
}

type EnterMobileNumberScreenStateData = {
    mobileNumber :: String
}

type EnterMobileNumberScreenStateProps = {
  btnActive :: Boolean,
  isValid :: Boolean
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
  capturedOtp :: String
}

type EnterOTPScreenStateProps = {
  btnActive :: Boolean,
  isValid :: Boolean,
  resendEnabled :: Boolean
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
    ride_distance_visibility :: PropValue,
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
    amountColor :: PropValue
  }
-----------------------------------------------ApplicationStatusScreen -------------------

type DriverDetailsScreenState = {
  data :: DriverDetailsScreenStateData,
  props :: DriverDetailsScreenStateProps
}

type DriverDetailsScreenStateData =  {
  driverName :: String,
  driverVehicleType :: String,
  driverRating :: Maybe Int,
  base64Image :: String,
  drivingLicenseNo :: String,
  driverMobile :: Maybe String
}

type DriverDetailsScreenStateProps =  {
  
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
  route :: Array Route,
  cancelRideConfirmationPopUp :: CancelRidePopUpData
 }

type CancelRidePopUpData = {
  delayInSeconds :: Int,
  timerID :: String,
  continueEnabled :: Boolean,
  enableTimer :: Boolean
}

type CancelRideModalData = {
  cancelRideReasons :: Array CancellationReasons,
  activeIndex ::Maybe Int,
  selectedReasonCode :: String,
  selectedReasonDescription :: String,
  isMandatoryTextHidden :: Boolean,
  isCancelButtonActive :: Boolean
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
  duration :: Int,
  riderName :: String,
  estimatedFare :: Int,
  isDriverArrived :: Boolean,
  notifiedCustomer :: Boolean
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
  zoneRideBooking :: Boolean
 }

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
  navButton :: Array NavIcons,
  screenName :: String
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

data PaymentMode = CASH | ONLINE

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
  mapImage :: String,
  date :: String,
  time :: String,
  source :: String,
  destination :: String,
  fare :: Int,
  tripId :: String,
  customerName :: String,
  coveredDistance :: String,
  durationOfTrip :: String,
  rating :: Int
}

type HelpAndSupportScreenProps = {
  isNoRides :: Boolean

}

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

}

type PermissionsScreenProps = {
  isLocationPermissionChecked :: Boolean
  , isOverlayPermissionChecked :: Boolean
  , isAutoStartPermissionChecked :: Boolean
  , androidVersion :: Int
  , isBatteryOptimizationChecked :: Boolean
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
}

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
  loadMore :: Boolean
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
  mediaType :: Maybe MediaType
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
  messageId :: PropValue
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
  mediaType :: Maybe MediaType
}

type YoutubeData = {
    videoTitle :: String 
  , setVideoTitle :: Boolean
  , showMenuButton :: Boolean
  , showDuration :: Boolean
  , showSeekBar :: Boolean
  , videoId :: String
}

data YoutubeVideoStatus = PLAY | PAUSE 

derive instance genericYoutubeVideoStatus:: Generic YoutubeVideoStatus _
instance showYoutubeVideoStatus :: Show YoutubeVideoStatus where show = genericShow
instance eqYoutubeVideoStatus :: Eq YoutubeVideoStatus where eq = genericEq


data ReferralType = SuccessScreen | ComingSoonScreen | ReferralFlow | QRScreen

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
  vehicleCapacity :: String
}

type BookingOptionsScreenProps = {
  
}