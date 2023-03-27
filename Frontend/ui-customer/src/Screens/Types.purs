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
import Components.QuoteListItem.Controller (QuoteListItemState)
import Components.SettingSideBar.Controller (SettingSideBarState)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Foreign.Class (class Decode, class Encode)
import Halogen.VDom.DOM.Prop (PropValue)
import Prelude (class Eq, class Show)
import Presto.Core.Utils.Encoding (defaultEnumDecode, defaultEnumEncode)
import Services.API (AddressComponents, BookingLocationAPIEntity, QuoteAPIEntity, Route)

type Contacts = {
  name :: String,
  number :: String
}

type NewContacts = {
  name :: String,
  number :: String,
  isSelected :: Boolean
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
  btnActive :: Boolean,
  exitAnimation :: Boolean
 }


type Language =  {
  name :: String,
  value :: String,
  subTitle :: String
 }


-- ################################################ EnterMobileNumberScreenState #############################################
type EnterMobileNumberScreenState = 
  {
    props :: EnterMobileNumberScreenStateProps,
    data :: EnterMobileNumberScreenStateData
  }

type EnterMobileNumberScreenStateProps = {
  enterOTP :: Boolean,
  btnActiveMobileNuber :: Boolean,
  btnActiveOTP :: Boolean,
  isValidMobileNumber :: Boolean,
  wrongOTP :: Boolean,
  resendEnable :: Boolean,
  isReadingOTP :: Boolean,
  capturedOtp :: String,
  letterSpacing :: Number
}

type EnterMobileNumberScreenStateData = {
    mobileNumber :: String
  , tokenId :: String
  , attempts :: Int
  , otp :: String
  , timer :: String
  , timerID :: String
}
-- ################################################ AccountSetUpScreenState ##################################################

type AccountSetUpScreenState = 
  { props :: AccountSetUpScreenStateProps ,
    data :: AccountSetUpScreenStateData
  }


type AccountSetUpScreenStateProps =
  {   btnActive :: Boolean
    , backPressed :: Boolean
  }

type AccountSetUpScreenStateData = 
  {  name :: String
    , email :: String
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
    tripId :: String
    -- bookingId :: String
  }

type TripDetailsScreenProps =
  {
    reportIssue :: Boolean,
    issueReported :: Boolean,
    activateSubmit :: Boolean,
    fromMyRides :: Boolean,
    showConfirmationPopUp :: Boolean,
    canConnectWithDriver :: Boolean
  }

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
    selectedItem :: IndividualRideCardState
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
    bookingId :: String
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
    status :: String,
    rideStartTime :: String,
    rideEndTime :: String,
    rideId :: String,
    vehicleNumber :: String,
    tripId :: String,
    bookingId :: String
  }

type HelpAndSuportScreenProps = 
  {
    apiFailure :: Boolean
  , isCallConfirmation :: Boolean
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
    loadMoreText :: String
  }

type MyRideScreenProps = {
  loaderButtonVisibility :: Boolean,
  loadMoreDisabled :: Boolean,
  receivedResponse :: Boolean,
  apiFailure :: Boolean,
  fromNavBar :: Boolean
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
    fareBreakUpList :: Fares,
    baseFare :: String -- Added only For Backward Compatibility
  , pickupCharges :: String
  , extraFare :: String
  , waitingCharges :: String
  , baseDistance :: String
  , extraDistance :: String
  }

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
    alpha :: PropValue
  }

-- ################################################ PermissionScreenState ##################################################

type PermissionScreenState = 
  {}
-- ######################################  HomeScreenState   ######################################

data Stage = HomeScreen 
           | SettingPrice 
           | FindingEstimate 
           | ConfirmingRide 
           | RideAccepted 
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

derive instance genericStage :: Generic Stage _
instance eqStage :: Eq Stage where eq = genericEq
instance showStage :: Show Stage where show = genericShow

data SearchLocationModelType = SearchLocation | LocateOnMap | NoView

data PopupType = Logout | ConfirmBack | NoPopUp | ActiveQuotePopUp

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
  , selectList :: Array QuoteAPIEntity
  , quoteListModelState :: Array QuoteListItemState
  , driverInfoCardState :: DriverInfoCard
  , previousRideRatingState :: RatingCard
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
  }

type HomeScreenStateProps =
  {
    currentStage :: Stage
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
  , expiredQuotes :: Array String
  , isCancelRide :: Boolean
  , cancellationReasons :: Array CancellationReasons
  , cancelRideActiveIndex :: Maybe Int
  , cancelDescription :: String
  , cancelReasonCode :: String
  , isPopUp :: PopupType
  , forFirst :: Boolean
  , ratingModal :: Boolean
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
  , autoSelecting :: Boolean
  , searchExpire :: Int
  , isEstimateChanged :: Boolean
  , showRateCard :: Boolean
  , showRateCardIcon :: Boolean
  , emergencyHelpModal :: Boolean
  , estimatedDistance :: Maybe Int
  , waitingTimeTimerId :: String
  , tagType :: Maybe CardType
  , isSaveFavourite :: Boolean
  , showShareAppPopUp :: Boolean
  , showMultipleRideInfo :: Boolean
  , hasTakenRide :: Boolean
  , isReferred :: Boolean
  , storeCurrentLocs :: Boolean
  , emergencyHelpModelState :: EmergencyHelpModelState
  , showLiveDashboard :: Boolean
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
    nightCharges :: Boolean
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
   emergencyContactData :: Array Contact
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
  }

-- ################################## SelectLanguageScreenState ###############################

type SelectLanguageScreenState = {
  data :: SelectLanguageScreenData,
  props :: SelectLanguageScreenProps
}

type SelectLanguageScreenData =  {
  isSelected :: Boolean
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
  removedContactDetail :: NewContacts,
  editedText :: String
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

type AboutUsScreenState = {}

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
  accountStatus :: DeleteStatus
}


type MyProfileScreenData = {
  name :: String,
  mobileNumber :: String,
  editedName :: String
}

type Location = {
  place :: String,
  lat :: Number,
  lng :: Number
}

type DriverInfoCard =
  { otp :: String
  , driverName :: String
  , eta :: Int
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
  , createdAt :: String
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
  }

type AppUpdatePopUpState = 
 { version :: Int }


data NotifyFlowEventType = RATE_DRIVER_SKIPPED | SEARCH_CANCELLED

derive instance genericNotifyFlowEventType :: Generic NotifyFlowEventType _
instance showNotifyFlowEventType :: Show NotifyFlowEventType where show = genericShow
data LocItemType = LOC_LIST | CURR_LOC | LOCATE_ON_MAP

derive instance genericLocItemType :: Generic LocItemType _
instance eqLocItemType :: Eq LocItemType where eq = genericEq
instance showLocItemType :: Show LocItemType where show = genericShow
instance encodeLocItemType :: Encode LocItemType where encode = defaultEnumEncode
instance decodeLocItemType:: Decode LocItemType where decode = defaultEnumDecode

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
}

data LocationItemType = RECENTS | PREDICTION | SAVED_LOCATION 

derive instance genericLocationItemType :: Generic LocationItemType _
instance eqLocationItemType :: Eq LocationItemType where eq = genericEq
instance showLocationItemType :: Show LocationItemType where show = genericShow
instance encodeLocationItemType :: Encode LocationItemType where encode = defaultEnumEncode
instance decodeLocationItemType:: Decode LocationItemType where decode = defaultEnumDecode

type SaveFavouriteCardState = 
  { 
    address :: String
  , tag :: String
  , tagExists :: Boolean
  , selectedItem :: LocationListItemState
  , tagData :: Array String
  , isBtnActive :: Boolean
  }

type Fares = {
  baseFare :: String
, pickupCharges :: String
, nominalFare :: String
, waitingCharges :: String
}

data FareTypes = BASE_FARE | EXTRA_DISTANCE_FARE | DRIVER_SELECTED_FARE | TOTAL_FARE | WAITING_CHARGES | DEAD_KILOMETER_FARE

derive instance genericFareTypes :: Generic FareTypes _
instance eqFareTypes :: Eq FareTypes where eq = genericEq
instance showFareTypes :: Show FareTypes where show = genericShow

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

