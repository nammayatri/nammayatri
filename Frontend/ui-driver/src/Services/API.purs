{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Services.API where

import Data.Maybe
import PaymentPage

import Common.Types.App as CTA
import Common.Types.App as Common
import Data.Either
import Domain.Payments as PP
import Control.Alt ((<|>))
import Control.Monad.Except (except, runExcept)
import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Either as Either
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Debug (spy)
import Foreign (ForeignError(..), fail, unsafeFromForeign, typeOf, unsafeToForeign, Foreign)
import Foreign.Class (class Decode, class Encode, decode, encode)
import Foreign.Generic (decodeJSON)
import Foreign.Generic.EnumEncoding (genericDecodeEnum, genericEncodeEnum, defaultGenericEnumOptions)
import Foreign.Index (readProp)
import Prelude (class Eq, class Show, bind, show, ($), (<$>), (>>=), (==))
import Presto.Core.Types.API (class RestEndpoint,class StandardEncode, ErrorResponse, Method(..), defaultMakeRequestWithoutLogs, standardEncode, defaultDecodeResponse, defaultMakeRequestString)
import Presto.Core.Utils.Encoding (defaultDecode, defaultEncode, defaultEnumDecode, defaultEnumEncode)
import Services.EndPoints as EP
import Screens.CustomerReferralTrackerScreen.Types as CRST

newtype ErrorPayloadWrapper = ErrorPayload ErrorResponse

instance decodeErrorPayloadWrapper :: Decode ErrorPayloadWrapper
  where decode payload = ErrorPayload <$> decode payload


data APIResponse a b = APISuccess a | APIError b

instance showAPIResponse :: (Show a, Show b) => Show (APIResponse a b) where
  show (APISuccess a) = show a
  show (APIError b) = show b

instance encodeAPIResponse :: (Encode a, Encode b) => Encode (APIResponse a b) where
  encode (APISuccess a) = encode a
  encode (APIError b) = encode b

instance decodeAPIResponse :: (Decode a, Decode b) => Decode (APIResponse a b) where
  decode fgn = (APISuccess <$> decode fgn) <|> (APIError <$> decode fgn) <|> (fail $ ForeignError "Unknown response")

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Trigger OTP API request, response types

newtype TriggerOTPReq = TriggerOTPReq {
  mobileNumber :: String,
  mobileCountryCode :: String,
  merchantId :: String,
  merchantOperatingCity :: Maybe String,
  registrationLat :: Maybe Number,
  registrationLon :: Maybe Number,
  packageName :: String
}

newtype TriggerOTPResp = TriggerOTPResp {
  authId :: String,
  attempts :: Int
}

instance makeTriggerOTPReq :: RestEndpoint TriggerOTPReq where
 makeRequest reqBody headers = defaultMakeRequestWithoutLogs POST (EP.triggerOTP "") headers reqBody Nothing
 encodeRequest req = standardEncode req


derive instance genericTriggerOTPReq :: Generic TriggerOTPReq _
derive instance newtypeTriggerOTPReq :: Newtype TriggerOTPReq _
instance standardEncodeTriggerOTPReq :: StandardEncode TriggerOTPReq where standardEncode (TriggerOTPReq reqBody) = standardEncode reqBody
instance showTriggerOTPReq :: Show TriggerOTPReq where show = genericShow
instance decodeTriggerOTPReq :: Decode TriggerOTPReq where decode = defaultDecode
instance encodeTriggerOTPReq :: Encode TriggerOTPReq where encode = defaultEncode

derive instance genericTriggerOTPResp :: Generic TriggerOTPResp _
derive instance newtypeTriggerOTPResp :: Newtype TriggerOTPResp _
instance standardEncodeTriggerOTPResp :: StandardEncode TriggerOTPResp where standardEncode (TriggerOTPResp id) = standardEncode id
instance showTriggerOTPResp :: Show TriggerOTPResp where show = genericShow
instance decodeTriggerOTPResp :: Decode TriggerOTPResp where decode = defaultDecode
instance encodeTriggerOTPResp :: Encode TriggerOTPResp where encode = defaultEncode
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Verify OTP API request, response types

data VerifyTokenRequest = VerifyTokenRequest String VerifyTokenReq

data WhatsappOptMethods = OPT_IN | OPT_OUT

newtype VerifyTokenReq = VerifyTokenReq {
  otp :: String,
  deviceToken :: String,
  whatsappNotificationEnroll :: WhatsappOptMethods
}

newtype VerifyTokenResp = VerifyTokenResp {
  token :: String,
  person :: User
}
newtype User = User
    { maskedDeviceToken :: Maybe String
    , firstName :: Maybe String
    , middleName :: Maybe String
    , id :: String
    , lastName :: Maybe String
    , maskedMobileNumber :: String
    , role :: String
    }

instance makeVerifyTokenReq :: RestEndpoint VerifyTokenRequest where
 makeRequest reqBody@(VerifyTokenRequest token (VerifyTokenReq rqBody)) headers = defaultMakeRequestWithoutLogs POST (EP.verifyToken token) headers reqBody Nothing
 encodeRequest req = standardEncode req

derive instance genericWhatsappOptMethods :: Generic WhatsappOptMethods _
instance showWhatsappOptMethods :: Show WhatsappOptMethods where show = genericShow
instance decodeWhatsappOptMethods :: Decode WhatsappOptMethods where decode = defaultDecode
instance encodeWhatsappOptMethods :: Encode WhatsappOptMethods where encode = defaultEncode
instance standardEncodeWhatsappOptMethods :: StandardEncode WhatsappOptMethods
  where
  standardEncode OPT_IN = standardEncode $ show OPT_IN
  standardEncode OPT_OUT = standardEncode $ show OPT_OUT

derive instance genericVerifyTokenReq :: Generic VerifyTokenReq _
derive instance newtypeVerifyTokenReq :: Newtype VerifyTokenReq _
instance standardEncodeVerifyTokenReq :: StandardEncode VerifyTokenReq where standardEncode (VerifyTokenReq reqBody) = standardEncode reqBody -- FOR REVIEW
instance showVerifyTokenReq :: Show VerifyTokenReq where show = genericShow
instance decodeVerifyTokenReq :: Decode VerifyTokenReq where decode = defaultDecode
instance encodeVerifyTokenReq :: Encode VerifyTokenReq where encode = defaultEncode

derive instance genericVerifyTokenResp :: Generic VerifyTokenResp _
derive instance newtypeVerifyTokenResp :: Newtype VerifyTokenResp _
instance standardEncodeVerifyTokenResp :: StandardEncode VerifyTokenResp where standardEncode (VerifyTokenResp id) = standardEncode id
instance showVerifyTokenResp :: Show VerifyTokenResp where show = genericShow
instance decodeVerifyTokenResp :: Decode VerifyTokenResp where decode = defaultDecode
instance encodeVerifyTokenResp :: Encode VerifyTokenResp where encode = defaultEncode

derive instance genericVerifyTokenRequest :: Generic VerifyTokenRequest _
instance decodeVerifyTokenRequest :: Decode VerifyTokenRequest where decode = defaultDecode
instance standardEncodeVerifyTokenRequest :: StandardEncode VerifyTokenRequest where standardEncode (VerifyTokenRequest token req) = standardEncode req
instance encodeVerifyTokenRequest :: Encode VerifyTokenRequest where encode = defaultEncode

derive instance genericUser :: Generic User _
derive instance newtypeUser :: Newtype User _
instance standardEncodeUser :: StandardEncode User where standardEncode (User req) = standardEncode req
instance showUser :: Show User where show = genericShow
instance decodeUser :: Decode User where decode = defaultDecode
instance encodeUser :: Encode User where encode = defaultEncode

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Resend OTP API request, response types
data ResendOTPRequest = ResendOTPRequest String

newtype ResendOTPResp = ResendOTPResp {
  authId :: String,
  attempts :: Int
}

instance makeResendOTPReq :: RestEndpoint ResendOTPRequest where
    makeRequest reqBody@(ResendOTPRequest token) headers = defaultMakeRequestWithoutLogs POST (EP.resendOTP token) headers reqBody Nothing
    encodeRequest req = standardEncode req

derive instance genericResendOTPResp :: Generic ResendOTPResp _
derive instance newtypeResendOTPResp :: Newtype ResendOTPResp _
instance standardEncodeResendOTPResp :: StandardEncode ResendOTPResp where standardEncode (ResendOTPResp req) = standardEncode req
instance showResendOTPResp :: Show ResendOTPResp where show = genericShow
instance decodeResendOTPResp :: Decode ResendOTPResp where decode = defaultDecode
instance encodeResendOTPResp :: Encode ResendOTPResp where encode = defaultEncode

derive instance genericResendOTPRequest :: Generic ResendOTPRequest _
instance decodeResendOTPRequest :: Decode ResendOTPRequest where decode = defaultDecode
instance standardEncodeResendOTPRequest :: StandardEncode ResendOTPRequest where standardEncode (ResendOTPRequest token) = standardEncode token
instance encodeResendOTPRequest :: Encode ResendOTPRequest where encode = defaultEncode

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--  Driver Activity API request, response types
data DriverActiveInactiveReq = DriverActiveInactiveReq String String

newtype DriverActiveInactiveResp = DriverActiveInactiveResp ApiSuccessResult

newtype ApiSuccessResult = ApiSuccessResult {
  result :: String
}

instance makeDriverActiveInactiveReq :: RestEndpoint DriverActiveInactiveReq  where
    makeRequest reqBody@(DriverActiveInactiveReq status status_n) headers = defaultMakeRequestWithoutLogs POST (EP.driverActiveInactiveSilent status status_n) headers reqBody Nothing
    encodeRequest req = standardEncode req

-- instance makeDriverActiveInactiveSilentReq :: RestEndpoint DriverActiveInactiveReq  where
--     makeRequest reqBody@(DriverActiveInactiveReq status ) headers = defaultMakeRequestWithoutLogs POST (EP.driverActiveInactiveSilent status status_n) headers reqBody
    -- decodeResponse = decodeJSON
--     encodeRequest req = standardEncode req

derive instance genericDriverActiveInactiveReq :: Generic DriverActiveInactiveReq _
instance standardEncodeDriverActiveInactiveReq :: StandardEncode DriverActiveInactiveReq where standardEncode (DriverActiveInactiveReq req status) = standardEncode req
instance showDriverActiveInactiveReq :: Show DriverActiveInactiveReq where show = genericShow
instance decodeDriverActiveInactiveReq :: Decode DriverActiveInactiveReq where decode = defaultDecode
instance encodeDriverActiveInactiveReq :: Encode DriverActiveInactiveReq where encode = defaultEncode

derive instance genericDriverActiveInactiveResp :: Generic DriverActiveInactiveResp _
derive instance newtypeDriverActiveInactiveResp :: Newtype DriverActiveInactiveResp _
instance standardEncodeDriverActiveInactiveResp :: StandardEncode DriverActiveInactiveResp where standardEncode (DriverActiveInactiveResp req) = standardEncode req
instance showDriverActiveInactiveResp :: Show DriverActiveInactiveResp where show = genericShow
instance decodeDriverActiveInactiveResp :: Decode DriverActiveInactiveResp where decode = defaultDecode
instance encodeDriverActiveInactiveResp :: Encode DriverActiveInactiveResp where encode = defaultEncode

newtype ErrorResponseDriverActivity = ErrorResponseDriverActivity { 
    blockExpiryTime :: String, 
    blockReason :: String
}

derive instance genericErrorResponseDriverActivity :: Generic ErrorResponseDriverActivity _
derive instance newtypeErrorResponseDriverActivity :: Newtype ErrorResponseDriverActivity _
instance encodeErrorResponseDriverActivity :: Encode ErrorResponseDriverActivity where encode = defaultEncode
instance decodeErrorResponseDriverActivity :: Decode ErrorResponseDriverActivity where decode = defaultDecode
instance eqErrorResponseDriverActivity :: Eq ErrorResponseDriverActivity where eq = genericEq
----------------------------------------------------------------------------------------------------------------------------START RIDE----------------------------------------------------------------------------------------------------------------------------------------------
-- StartRide API request, response types
data StartRideRequest = StartRideRequest String StartRideReq

derive instance genericStartRideRequest :: Generic StartRideRequest _
instance standardEncodeStartRideRequest :: StandardEncode StartRideRequest where standardEncode (StartRideRequest rideId req) = standardEncode req
instance showStartRideRequest :: Show StartRideRequest where show = genericShow
instance decodeStartRideRequest :: Decode StartRideRequest where decode = defaultDecode
instance encodeStartRideRequest :: Encode StartRideRequest where encode = defaultEncode

newtype StartRideReq = StartRideReq
    {
        rideOtp :: String,
        odometer :: Maybe Odometer,
        point :: Point
    }

derive instance genericStartRideReq :: Generic StartRideReq _
instance showStartRideReq :: Show StartRideReq where show = genericShow
instance standardEncodeStartRideReq :: StandardEncode StartRideReq where standardEncode (StartRideReq req) = standardEncode req
instance decodeStartRideReq :: Decode StartRideReq where decode = defaultDecode
instance encodeStartRideReq :: Encode StartRideReq where encode = defaultEncode


newtype Point = Point
    {
        lat :: Number
    ,   lon :: Number
    ,   ts :: String
    }

newtype Odometer = Odometer 
  {
    value :: Number,
    fileId :: Maybe String
  }

derive instance genericOdometer :: Generic Odometer _
derive instance newtypeOdometer :: Newtype Odometer _
instance standardOdometer :: StandardEncode Odometer where standardEncode (Odometer req) = standardEncode req
instance showOdometer :: Show Odometer where show = genericShow
instance decodeOdometer :: Decode Odometer where decode = defaultDecode
instance encodeOdometer :: Encode Odometer where encode = defaultEncode

derive instance genericPoint :: Generic Point _
derive instance newtypePoint :: Newtype Point _
instance standardEncodePoint :: StandardEncode Point where standardEncode (Point req) = standardEncode req
instance showPoint :: Show Point where show = genericShow
instance decodePoint :: Decode Point where decode = defaultDecode
instance encodePoint :: Encode Point where encode = defaultEncode


derive instance genericApiSuccessResult :: Generic ApiSuccessResult _
derive instance newtypeApiSuccessResult :: Newtype ApiSuccessResult _
instance standardEncodeApiSuccessResult :: StandardEncode ApiSuccessResult where standardEncode (ApiSuccessResult req) = standardEncode req
instance showApiSuccessResult :: Show ApiSuccessResult where show = genericShow
instance decodeApiSuccessResult :: Decode ApiSuccessResult where decode = defaultDecode
instance encodeApiSuccessResult :: Encode ApiSuccessResult where encode = defaultEncode


instance makeStartRideReq :: RestEndpoint StartRideRequest where
    makeRequest reqBody@(StartRideRequest rideId (StartRideReq rqBody)) headers = defaultMakeRequestWithoutLogs POST (EP.startRide rideId) headers reqBody Nothing
    encodeRequest req = standardEncode req



----------------------------------------------------------------------------------------------------------------------------END RIDE----------------------------------------------------------------------------------------------------------------------------------------------
-- EndRide API request, response types
data EndRideRequest = EndRideRequest String EndRideReq

newtype EndRideReq = EndRideReq
    {
      endRideOtp :: Maybe String,
      odometer :: Maybe Odometer,
      point :: Point,
      numberOfDeviation :: Maybe Boolean,
      uiDistanceCalculationWithAccuracy :: Int,
      uiDistanceCalculationWithoutAccuracy :: Int
    }

newtype EndRideResponse = EndRideResponse {
  result :: String,
  homeLocationReached :: Maybe Boolean
}

instance makeEndRideReq :: RestEndpoint EndRideRequest where
    makeRequest reqBody@(EndRideRequest rideId (EndRideReq rqBody)) headers = defaultMakeRequestWithoutLogs POST (EP.endRide rideId) headers reqBody Nothing
    encodeRequest req = standardEncode req

derive instance genericEndRideReq :: Generic EndRideReq _
instance showEndRideReq :: Show EndRideReq where show = genericShow
instance standardEncodeEndRideReq :: StandardEncode EndRideReq where standardEncode (EndRideReq req) = standardEncode req
instance decodeEndRideReq :: Decode EndRideReq where decode = defaultDecode
instance encodeEndRideReq :: Encode EndRideReq where encode = defaultEncode

derive instance genericEndRideRequest :: Generic EndRideRequest _
instance standardEncodeEndRideRequest :: StandardEncode EndRideRequest where standardEncode (EndRideRequest rideId req) = standardEncode req
instance showEndRideRequest :: Show EndRideRequest where show = genericShow
instance decodeEndRideRequest :: Decode EndRideRequest where decode = defaultDecode
instance encodeEndRideRequest :: Encode EndRideRequest where encode = defaultEncode

derive instance genericEndRideResponse :: Generic EndRideResponse _
derive instance newtypeEndRideResponse :: Newtype EndRideResponse _
instance standardEncodeEndRideResponse :: StandardEncode EndRideResponse where standardEncode (EndRideResponse req) = standardEncode req
instance showEndRideResponse :: Show EndRideResponse where show = genericShow
instance decodeEndRideResponse :: Decode EndRideResponse where decode = defaultDecode
instance encodeEndRideResponse :: Encode EndRideResponse where encode = defaultEncode

--------------------------------------------------------Arrived At Stop------------------------------------------------------------------------------------------------------------------------------------------
-- ArrivedAtStop API request, response types
data ArrivedAtStopRequest = ArrivedAtStopRequest String LatLong

instance makeArrivedStopReq :: RestEndpoint ArrivedAtStopRequest where
    makeRequest reqBody@(ArrivedAtStopRequest rideId (LatLong rqBody)) headers = defaultMakeRequestWithoutLogs POST (EP.arrivedAtStop rideId) headers reqBody Nothing
    encodeRequest req = standardEncode req

derive instance genericArrivedAtStopRequest :: Generic ArrivedAtStopRequest _
instance standardEncodeArrivedAtStopRequest :: StandardEncode ArrivedAtStopRequest where standardEncode (ArrivedAtStopRequest rideId req) = standardEncode req
instance showArrivedAtStopRequest :: Show ArrivedAtStopRequest where show = genericShow
instance decodeArrivedAtStopRequest :: Decode ArrivedAtStopRequest where decode = defaultDecode
instance encodeArrivedAtStopRequest :: Encode ArrivedAtStopRequest where encode = defaultEncode



--------------------------------------------------------CANCEL RIDE----------------------------------------------------------------------------------------------------------------------------------------------
-- Cancel Ride API request, response types
newtype DriverCancelRideResponse = DriverCancelRideResponse {
  isGoHomeDisabled :: Maybe Boolean,
  goHomeCancellationCount :: Maybe Int,
  result :: String
}


newtype DriverCancelRideReq = DriverCancelRideReq
    {
      additionalInfo :: String,
      reasonCode :: String,
      doCancellationRateBasedBlocking :: Boolean
    }

data DriverCancelRideRequest = DriverCancelRideRequest String DriverCancelRideReq

derive instance genericDriverCancelRideReq :: Generic DriverCancelRideReq _
instance showDriverCancelRideReq :: Show DriverCancelRideReq where show = genericShow
instance standardEncodeDriverCancelRideReq :: StandardEncode DriverCancelRideReq where standardEncode (DriverCancelRideReq req) = standardEncode req
instance decodeDriverCancelRideReq :: Decode DriverCancelRideReq where decode = defaultDecode
instance encodeDriverCancelRideReq :: Encode DriverCancelRideReq where encode = defaultEncode

instance makeDriverCancelRideReq :: RestEndpoint DriverCancelRideRequest where
    makeRequest reqBody@(DriverCancelRideRequest rideId (DriverCancelRideReq rqBody)) headers = defaultMakeRequestWithoutLogs POST (EP.cancelRide rideId) headers reqBody Nothing
    encodeRequest req = standardEncode req

derive instance genericDriverCancelRideRequest :: Generic DriverCancelRideRequest _
instance standardEncodeDriverCancelRideRequest :: StandardEncode DriverCancelRideRequest where standardEncode (DriverCancelRideRequest rideId req) = standardEncode req
instance showDriverCancelRideRequest :: Show DriverCancelRideRequest where show = genericShow
instance decodeDriverCancelRideRequest :: Decode DriverCancelRideRequest where decode = defaultDecode
instance encodeDriverCancelRideRequest :: Encode DriverCancelRideRequest where encode = defaultEncode

derive instance genericDriverCancelRideResponse :: Generic DriverCancelRideResponse _
derive instance newtypeDriverCancelRideResponse :: Newtype DriverCancelRideResponse _
instance standardEncodeDriverCancelRideResponse :: StandardEncode DriverCancelRideResponse where standardEncode (DriverCancelRideResponse req) = standardEncode req
instance showDriverCancelRideResponse :: Show DriverCancelRideResponse where show = genericShow
instance decodeDriverCancelRideResponse :: Decode DriverCancelRideResponse where decode = defaultDecode
instance encodeDriverCancelRideResponse :: Encode DriverCancelRideResponse where encode = defaultEncode

--------------------------------------------------------------LOGOUT----------------------------------------------------------------------------------------------------------------------------------------------
-- Logout API request, response types
data LogOutReq = LogOutReq


instance makeLogOutReq  :: RestEndpoint LogOutReq where
    makeRequest reqBody headers = defaultMakeRequestWithoutLogs POST (EP.logout "") headers reqBody Nothing
    encodeRequest req = defaultEncode req

derive instance genericLogOutReq :: Generic LogOutReq _
instance showLogOutReq :: Show LogOutReq where show = genericShow
instance standardEncodeLogOutReq :: StandardEncode LogOutReq where standardEncode (LogOutReq) = standardEncode{}
instance decodeLogOutReq :: Decode LogOutReq where decode = defaultDecode
instance encodeLogOutReq :: Encode LogOutReq where encode = defaultEncode


------------------------------------------------------------GET DRIVER PROFILE----------------------------------------------------------------------------------------------------------------------------------------------

-- GetDriverInfo API request, response types
newtype GetDriverInfoReq = GetDriverInfoReq {
 }

newtype DriverInfoReq = DriverInfoReq {
  isAdvancedBookingEnabled :: Maybe Boolean,
  isInteroperable :: Maybe Boolean,
  isCategoryLevelSubscriptionEnabled :: Maybe Boolean
 }

newtype GetDriverInfoResp = GetDriverInfoResp
    { id                    :: String
    , rating                :: Maybe Number
    , middleName            :: Maybe String
    , lastName              :: Maybe String
    , firstName             :: String
    , mobileNumber          :: Maybe String
    , active                :: Boolean
    , mode                  :: Maybe String
    , onRide                :: Boolean
    , linkedVehicle         :: Maybe Vehicle
    , organization          :: OrganizationInfo
    , enabled               :: Boolean
    , verified              :: Boolean
    , language              :: Maybe String
    , referralCode          :: Maybe String
    , alternateNumber       :: Maybe String
    , canDowngradeToHatchback :: Boolean
    , canDowngradeToSedan :: Boolean
    , canDowngradeToTaxi :: Boolean
    , clientVersion         :: Maybe CTA.Version
    , bundleVersion         :: Maybe CTA.Version
    , gender                :: Maybe String
    , blocked               :: Maybe Boolean
    , blockExpiryTime       :: Maybe String
    , numberOfRides         :: Maybe Int
    , paymentPending        :: Boolean
    , subscribed            :: Boolean
    , mediaUrl              :: Maybe String
    , autoPayStatus         :: Maybe String  
    , aadhaarCardPhoto      :: Maybe String
    , freeTrialDaysLeft     :: Maybe Int
    , payerVpa              :: Maybe String
    , currentDues           :: Maybe Number
    , manualDues            :: Maybe Number
    , driverGoHomeInfo      :: DriverGoHomeInfo
    , isGoHomeEnabled       :: Boolean
    , maskedDeviceToken     :: Maybe String
    , operatingCity         :: Maybe String
    , isVehicleSupported    :: Maybe Boolean
    , canSwitchToRental     :: Maybe Boolean
    , canSwitchToIntraCity  :: Maybe Boolean
    , checkIfACWorking      :: Maybe Boolean
    , canSwitchToInterCity  :: Maybe Boolean
    , payoutVpa             :: Maybe String 
    , payoutVpaStatus       :: Maybe PayoutVpaStatus
    , isPayoutEnabled       :: Maybe Boolean
    , payoutRewardAmount    :: Maybe Int
    , payoutVpaBankAccount  :: Maybe String
    , cancellationRateInWindow :: Maybe Int
    , cancelledRidesCountInWindow :: Maybe Int
    , assignedRidesCountInWindow :: Maybe Int
    , windowSize :: Maybe Int
    , favCount :: Maybe Int
    , isSubscriptionVehicleCategoryChanged :: Maybe Boolean
    , isOnFreeTrial :: Maybe Boolean
    , planMandatoryForCategory :: Maybe Boolean
    , isSubscriptionCityChanged :: Maybe Boolean
    , freeTrialDays :: Maybe Int
    , freeTrialRides :: Maybe Int
    , totalRidesTaken :: Maybe Int
    , subscriptionEnabledForVehicleCategory :: Maybe Boolean
    , isSubscriptionEnabledAtCategoryLevel :: Maybe Boolean
    , isSpecialLocWarrior :: Maybe Boolean
    , subscriptionDown :: Maybe Boolean
    }


newtype DriverGoHomeInfo = DriverGoHomeInfo {
  cnt :: Int,
  driverGoHomeRequestId :: Maybe String,
  validTill :: Maybe String,
  status :: Maybe String, -- ACTIVE, SUCCESS, FAILED
  isOnRide :: Boolean,
  goHomeReferenceTime :: String
}

newtype  OrganizationInfo = OrganizationInfo
    {   name          :: String,
        description   :: Maybe String,
        contactNumber :: String,
        status        :: String,
        enabled       :: Boolean,
        id            :: String
    }

newtype Vehicle = Vehicle
    {    driverId        :: String,
        category         :: Maybe String,
        model            :: String,
        variant          :: String,
        color            :: String,
        registrationNo   :: String,
        capacity         :: Maybe Int,
        createdAt        :: String,
        serviceTierType  :: Maybe String
    }

data PayoutVpaStatus = VIA_WEBHOOK | MANUALLY_ADDED | VERIFIED_BY_USER

derive instance genericPayoutVpaStatus :: Generic PayoutVpaStatus _
instance showPayoutVpaStatus :: Show PayoutVpaStatus where show = genericShow
instance decodePayoutVpaStatus :: Decode PayoutVpaStatus where decode = defaultEnumDecode
instance encodePayoutVpaStatus :: Encode PayoutVpaStatus where encode = defaultEnumEncode
instance eqPayoutVpaStatus :: Eq PayoutVpaStatus where eq = genericEq
instance standardEncodePayoutVpaStatus :: StandardEncode PayoutVpaStatus
  where
    standardEncode _ = standardEncode {}

instance makeDriverInfoReq :: RestEndpoint DriverInfoReq where
    makeRequest reqBody headers = defaultMakeRequestWithoutLogs POST (EP.getDriverInfoV2 "") headers reqBody Nothing
    encodeRequest req = defaultEncode req

derive instance genericDriverInfoReq :: Generic DriverInfoReq _
instance showDriverInfoReq :: Show DriverInfoReq where show = genericShow
instance standardEncodeDriverInfoReq :: StandardEncode DriverInfoReq where standardEncode (DriverInfoReq req) = standardEncode req
instance decodeDriverInfoReq :: Decode DriverInfoReq where decode = defaultDecode
instance encodeDriverInfoReq :: Encode DriverInfoReq where encode = defaultEncode

instance makeGetDriverInfoReq :: RestEndpoint GetDriverInfoReq where
    makeRequest reqBody headers = defaultMakeRequestWithoutLogs GET (EP.getDriverInfo "") headers reqBody Nothing
    encodeRequest req = defaultEncode req

derive instance genericGetDriverInfoReq :: Generic GetDriverInfoReq _
instance showGetDriverInfoReq :: Show GetDriverInfoReq where show = genericShow
instance standardEncodeGetDriverInfoReq :: StandardEncode GetDriverInfoReq where standardEncode (GetDriverInfoReq req) = standardEncode req
instance decodeGetDriverInfoReq :: Decode GetDriverInfoReq where decode = defaultDecode
instance encodeGetDriverInfoReq :: Encode GetDriverInfoReq where encode = defaultEncode

newtype UpdateFeatureInDInfoResp = UpdateFeatureInDInfoResp GetDriverInfoResp

derive instance genericUpdateFeatureInDInfoResp :: Generic UpdateFeatureInDInfoResp _
derive instance newtypeUpdateFeatureInDInfoResp :: Newtype UpdateFeatureInDInfoResp _
instance standardEncodeUpdateFeatureInDInfoResp :: StandardEncode UpdateFeatureInDInfoResp where standardEncode (UpdateFeatureInDInfoResp req) = standardEncode req
instance decodeUpdateFeatureInDInfoResp :: Decode UpdateFeatureInDInfoResp where decode = defaultDecode
instance encodeUpdateFeatureInDInfoResp :: Encode UpdateFeatureInDInfoResp where encode = defaultEncode

derive instance genericOrganizationInfo :: Generic OrganizationInfo _
derive instance newtypeOrganizationInfo :: Newtype OrganizationInfo _
instance standardEncodeOrganizationInfo :: StandardEncode OrganizationInfo where standardEncode (OrganizationInfo req) = standardEncode req
instance showOrganizationInfo :: Show OrganizationInfo where show = genericShow
instance decodeOrganizationInfo :: Decode OrganizationInfo where decode = defaultDecode
instance encodeOrganizationInfo :: Encode OrganizationInfo where encode = defaultEncode

derive instance genericVehicle :: Generic Vehicle _
derive instance newtypeVehicle :: Newtype Vehicle _
instance standardEncodeVehicle :: StandardEncode Vehicle where standardEncode (Vehicle req) = standardEncode req
instance showVehicle :: Show Vehicle where show = genericShow
instance decodeVehicle :: Decode Vehicle where decode = defaultDecode
instance encodeVehicle :: Encode Vehicle where encode = defaultEncode

derive instance genericDriverGoHomeInfo :: Generic DriverGoHomeInfo _
derive instance newtypeDriverGoHomeInfo :: Newtype DriverGoHomeInfo _
instance standardEncodeDriverGoHomeInfo :: StandardEncode DriverGoHomeInfo where standardEncode (DriverGoHomeInfo req) = standardEncode req
instance showDriverGoHomeInfo :: Show DriverGoHomeInfo where show = genericShow
instance decodeDriverGoHomeInfo :: Decode DriverGoHomeInfo where decode = defaultDecode
instance encodeDriverGoHomeInfo :: Encode DriverGoHomeInfo where encode = defaultEncode

derive instance genericGetDriverInfoResp :: Generic GetDriverInfoResp _
derive instance newtypeGetDriverInfoResp :: Newtype GetDriverInfoResp _
instance standardEncodeGetDriverInfoResp :: StandardEncode GetDriverInfoResp where standardEncode (GetDriverInfoResp req) = standardEncode req
instance showGetDriverInfoResp :: Show GetDriverInfoResp where show = genericShow
instance decodeGetDriverInfoResp :: Decode GetDriverInfoResp where decode = defaultDecode
instance encodeGetDriverInfoResp :: Encode GetDriverInfoResp where encode = defaultEncode

--------------------------------------------------Upload Driver Profile-----------------------------------------------------------------------

data UploadProfileReq = UploadProfileReq GetUploadProfileReq

newtype GetUploadProfileReq = GetUploadProfileReq {
  pledges :: Array String,
  vehicleTags :: Array String,
  languages :: Array String,
  aspirations :: Array String,
  drivingSince :: Maybe Int,
  hometown :: Maybe String,
  imageIds :: Array String
}

derive instance genericUploadProfileReq :: Generic UploadProfileReq _
instance standardEncodeUploadProfileReq :: StandardEncode UploadProfileReq where standardEncode (UploadProfileReq body) = standardEncode body
instance showUploadProfileReq :: Show UploadProfileReq where show = genericShow
instance decodeUploadProfileReq :: Decode UploadProfileReq where decode = defaultDecode
instance encodeUploadProfileReq :: Encode UploadProfileReq where encode = defaultEncode

derive instance genericGetUploadProfileReq :: Generic GetUploadProfileReq _
derive instance newtypeGetUploadProfileReq :: Newtype GetUploadProfileReq _
instance standardEncodeGetUploadProfileReq :: StandardEncode GetUploadProfileReq where standardEncode (GetUploadProfileReq body) = standardEncode body
instance showGetUploadProfileReq :: Show GetUploadProfileReq where show = genericShow
instance decodeGetUploadProfileReq :: Decode GetUploadProfileReq where decode = defaultDecode
instance encodeGetUploadProfileReq :: Encode GetUploadProfileReq where encode = defaultEncode

instance makeUploadProfileReq :: RestEndpoint UploadProfileReq where
    makeRequest reqBody@(UploadProfileReq (GetUploadProfileReq reqB)) headers = defaultMakeRequestWithoutLogs POST (EP.submitDriverProfile "") headers reqBody Nothing
    encodeRequest req = standardEncode req

-----------------------------------------------GET RIDES HISTORY---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

data InitiatedAs = AsSender | AsReciever | AsSomeoneElse

derive instance genericInitiatedAs :: Generic InitiatedAs _
instance showInitiatedAs :: Show InitiatedAs where show = genericShow
instance decodeInitiatedAs :: Decode InitiatedAs where decode = defaultEnumDecode
instance encodeInitiatedAs :: Encode InitiatedAs where encode = defaultEnumEncode
instance eqInitiatedAs :: Eq InitiatedAs where eq = genericEq
instance standardInitiatedAs :: StandardEncode InitiatedAs
  where
  standardEncode AsSender = standardEncode $ show AsSender
  standardEncode AsReciever = standardEncode $ show AsReciever
  standardEncode AsSomeoneElse = standardEncode $ show AsSomeoneElse
  standardEncode _ = standardEncode $ show AsSender

data GetRidesHistoryReq = GetRidesHistoryReq String String String String String

newtype GetRidesHistoryResp = GetRidesHistoryResp
    {
      list :: Array RidesInfo
    }
newtype RidesInfo = RidesInfo
  {
      status :: String,
      computedFare :: Maybe Int,
      vehicleModel :: String,
      createdAt :: String,
      driverNumber :: Maybe String,
      shortRideId :: String,
      vehicleNumber :: String,
      driverName :: String,
      driverSelectedFare :: Int,
      chargeableDistance :: Maybe Int,
      actualRideDistance :: Maybe Number,
      vehicleVariant :: String,
      estimatedBaseFare :: Int,
      vehicleColor :: String,
      tripStartTime :: Maybe String,
      id :: String,
      updatedAt :: String,
      riderName :: Maybe String,
      rideRating :: Maybe Int,
      fromLocation :: LocationInfo,
      toLocation :: Maybe LocationInfo,
      estimatedDistance :: Int,
      exoPhone :: String,
      specialLocationTag :: Maybe String,
      requestedVehicleVariant :: Maybe String,
      customerExtraFee :: Maybe Int,
      disabilityTag :: Maybe String,
      payerVpa :: Maybe String,
      autoPayStatus :: Maybe String,
      driverGoHomeRequestId :: Maybe String,
      isFreeRide :: Maybe Boolean,
      enableFrequentLocationUpdates :: Maybe Boolean,
      tripCategory :: Maybe TripCategory,
      nextStopLocation :: Maybe StopLocation,
      lastStopLocation :: Maybe StopLocation,
      tripEndTime :: Maybe String,
      stopLocationId :: Maybe String,
      tripScheduledAt :: Maybe String,
      estimatedDuration :: Maybe Int,
      actualDuration :: Maybe Int,
      startOdometerReading :: Maybe OdometerReading,
      endOdometerReading :: Maybe OdometerReading,
      isOdometerReadingsRequired :: Maybe Boolean,
      tollCharges :: Maybe Number,
      estimatedTollCharges :: Maybe Number,
      vehicleServiceTierName :: String,
      vehicleServiceTier :: String,
      isVehicleAirConditioned :: Maybe Boolean,
      vehicleCapacity :: Maybe Int,
      tollConfidence :: Maybe CTA.Confidence,
      bookingType :: Maybe BookingTypes,
      bapName :: Maybe String,
      isValueAddNP :: Boolean,
      parkingCharge :: Maybe Number, 
      coinsEarned :: Maybe (Array CoinsEarned),
      roundTrip :: Boolean,
      returnTime :: Maybe String,
      senderDetails :: Maybe PersonDetails,
      receiverDetails :: Maybe PersonDetails,
      stops :: Maybe (Array Stop)
}

newtype CoinsEarned = CoinsEarned CoinsEarnedType

type CoinsEarnedType = {
  coins :: Int,
  eventType :: String
}

newtype PersonDetails = PersonDetails
  {
    name :: String,
    primaryExophone :: String
  }

derive instance genericPersonDetails :: Generic PersonDetails _
derive instance newtypePersonDetails :: Newtype PersonDetails _
instance standardEncodePersonDetails :: StandardEncode PersonDetails where standardEncode (PersonDetails req) = standardEncode req
instance showPersonDetails :: Show PersonDetails where show = genericShow
instance eqPersonDetails :: Eq PersonDetails where eq = genericEq
instance decodePersonDetails :: Decode PersonDetails where decode = defaultDecode
instance encodePersonDetails :: Encode PersonDetails where encode = defaultEncode

newtype OdometerReading = OdometerReading
  {
    value :: Number,
    fileId :: Maybe String
  }

derive instance genericOdometerReading :: Generic OdometerReading _
derive instance newtypeOdometerReading :: Newtype OdometerReading _
instance standardEncodeOdometerReading :: StandardEncode OdometerReading where standardEncode (OdometerReading req) = standardEncode req
instance showOdometerReading :: Show OdometerReading where show = genericShow
instance decodeOdometerReading :: Decode OdometerReading where decode = defaultDecode
instance encodeOdometerReading :: Encode OdometerReading where encode = defaultEncode

newtype StopLocationAddress = StopLocationAddress
  { street :: Maybe String,
    door :: Maybe String,
    city :: Maybe String,
    state :: Maybe String,
    country :: Maybe String,
    building :: Maybe String,
    areaCode :: Maybe String,
    area :: Maybe String,
    fullAddress :: Maybe String
  }

derive instance genericStopLocationAddress :: Generic StopLocationAddress _
derive instance newtypeStopLocationAddress :: Newtype StopLocationAddress _
instance standardEncodeStopLocationAddress :: StandardEncode StopLocationAddress where standardEncode (StopLocationAddress req) = standardEncode req
instance showStopLocationAddress :: Show StopLocationAddress where show = genericShow
instance decodeStopLocationAddress :: Decode StopLocationAddress where decode = defaultDecode
instance encodeStopLocationAddress :: Encode StopLocationAddress where encode = defaultEncode

newtype StopLocation = StopLocation 
  { address :: StopLocationAddress,
    createdAt :: String,
    id :: String,
    lat :: Number,
    lon :: Number,
    updatedAt :: String
  }

derive instance genericStopLocation :: Generic StopLocation _
derive instance newtypeStopLocation :: Newtype StopLocation _
instance standardEncodeStopLocation :: StandardEncode StopLocation where standardEncode (StopLocation req) = standardEncode req
instance showStopLocation :: Show StopLocation where show = genericShow
instance decodeStopLocation :: Decode StopLocation where decode = defaultDecode
instance encodeStopLocation :: Encode StopLocation where encode = defaultEncode

newtype TripCategory = TripCategory
    { 
      contents :: Maybe String,
      tag :: String
    }

derive instance genericTripCategory :: Generic TripCategory _
derive instance newtypeTripCategory :: Newtype TripCategory _
instance standardEncodeTripCategory :: StandardEncode TripCategory where standardEncode (TripCategory req) = standardEncode req
instance showTripCategory :: Show TripCategory where show = genericShow     
instance decodeTripCategory :: Decode TripCategory where decode = defaultDecode
instance encodeTripCategory :: Encode TripCategory where encode = defaultEncode

newtype LocationInfo = LocationInfo
      {
        area :: Maybe String,
        state :: Maybe String,
        country :: Maybe String,
        building :: Maybe String,
        door ::Maybe  String,
        street :: Maybe String,
        lat :: Number,
        city :: Maybe String,
        areaCode :: Maybe String,
        lon :: Number,
        instructions :: Maybe String,
        extras :: Maybe String,
        id :: Maybe String
      }

data BookingTypes = CURRENT | ADVANCED

derive instance genericBookingTypesMethods :: Generic BookingTypes _
instance showBookingTypesMethods :: Show BookingTypes where show = genericShow
instance decodeBookingTypesMethods :: Decode BookingTypes where decode = defaultEnumDecode
instance encodeBookingTypesMethods :: Encode BookingTypes where encode = defaultEnumEncode
instance eqBookingTypes :: Eq BookingTypes where eq = genericEq
instance standardEncodeBookingTypesMethods :: StandardEncode BookingTypes
  where
  standardEncode CURRENT = standardEncode $ show CURRENT
  standardEncode ADVANCED = standardEncode $ show ADVANCED
  standardEncode _ = standardEncode $ show CURRENT

instance makeGetRidesHistoryReq :: RestEndpoint GetRidesHistoryReq where
    makeRequest reqBody@(GetRidesHistoryReq limit offset isActive status day) headers = defaultMakeRequestWithoutLogs GET (EP.getRideHistory limit offset isActive status day) headers reqBody Nothing
    encodeRequest req = defaultEncode req

derive instance genericGetRidesHistoryReq :: Generic GetRidesHistoryReq _
instance showGetRidesHistoryReq :: Show GetRidesHistoryReq where show = genericShow
instance standardEncodeGetRidesHistoryReq :: StandardEncode GetRidesHistoryReq where standardEncode _ = standardEncode {}
instance decodeGetRidesHistoryReq :: Decode GetRidesHistoryReq where decode = defaultDecode
instance encodeGetRidesHistoryReq :: Encode GetRidesHistoryReq where encode = defaultEncode

derive instance genericGetRidesHistoryResp :: Generic GetRidesHistoryResp _
derive instance newtypeGetRidesHistoryResp :: Newtype GetRidesHistoryResp _
instance standardEncodeGetRidesHistoryResp :: StandardEncode GetRidesHistoryResp where standardEncode (GetRidesHistoryResp req) = standardEncode req
instance showGetRidesHistoryResp :: Show GetRidesHistoryResp where show = genericShow
instance decodeGetRidesHistoryResp :: Decode GetRidesHistoryResp where decode = defaultDecode
instance encodeGetRidesHistoryResp :: Encode GetRidesHistoryResp where encode = defaultEncode

derive instance genericLocationInfo :: Generic LocationInfo _
derive instance newtypeLocationInfo :: Newtype LocationInfo _
instance standardEncodeLocationInfo :: StandardEncode LocationInfo where standardEncode (LocationInfo req) = standardEncode req
instance showLocationInfo :: Show LocationInfo where show = genericShow
instance decodeLocationInfo :: Decode LocationInfo where decode = defaultDecode
instance encodeLocationInfo :: Encode LocationInfo where encode = defaultEncode
instance eqLocationInfo :: Eq LocationInfo where eq = genericEq

derive instance genericRidesInfo :: Generic RidesInfo _
derive instance newtypeRidesInfo :: Newtype RidesInfo _
instance standardEncodeRidesInfo :: StandardEncode RidesInfo where standardEncode (RidesInfo req) = standardEncode req
instance showRidesInfo :: Show RidesInfo where show = genericShow
instance decodeRidesInfo :: Decode RidesInfo where decode = defaultDecode
instance encodeRidesInfo :: Encode RidesInfo where encode = defaultEncode

data VehicleVariant = SEDAN | SUV | HATCHBACK | AUTO_VARIANT | BIKE | AMBULANCE_TAXI | AMBULANCE_TAXI_OXY | AMBULANCE_AC | AMBULANCE_AC_OXY | AMBULANCE_VENTILATOR_ | SUV_PLUS | DELIVERY_LIGHT_GOODS_VEHICLE | BUS_NON_AC | BUS_AC | EV_AUTO_VARIANT | HERITAGE_CAB

derive instance genericVehicleVariant :: Generic VehicleVariant _
instance showVehicleVariant :: Show VehicleVariant where show = genericShow
instance decodeVehicleVariant :: Decode VehicleVariant where decode = defaultDecode
instance encodeVehicleVariant :: Encode VehicleVariant where encode = defaultEncode
instance standardEncodeVehicleVariant :: StandardEncode VehicleVariant
  where
 standardEncode SEDAN = standardEncode {}
 standardEncode SUV = standardEncode {}
 standardEncode HATCHBACK = standardEncode {}
 standardEncode AUTO_VARIANT = standardEncode {}
 standardEncode BIKE = standardEncode {}
 standardEncode AMBULANCE_TAXI = standardEncode {}
 standardEncode AMBULANCE_TAXI_OXY = standardEncode {}
 standardEncode AMBULANCE_AC = standardEncode {}
 standardEncode AMBULANCE_AC_OXY = standardEncode {}
 standardEncode AMBULANCE_VENTILATOR_ = standardEncode {}
 standardEncode SUV_PLUS = standardEncode {}
 standardEncode DELIVERY_LIGHT_GOODS_VEHICLE = standardEncode {}
 standardEncode BUS_NON_AC = standardEncode {}
 standardEncode BUS_AC = standardEncode {}
 standardEncode EV_AUTO_VARIANT = standardEncode {}
 standardEncode HERITAGE_CAB = standardEncode {}

data Status = NEW | INPROGRESS | COMPLETED | CANCELLED | NOTHING | UPCOMING

instance eqStatus :: Eq Status where eq = genericEq

derive instance genericStatus :: Generic Status _
instance showStatus :: Show Status where show = genericShow
instance decodeStatus :: Decode Status where decode = defaultDecode
instance encodeStatus :: Encode Status where encode = defaultEncode
instance standardEncodeStatus :: StandardEncode Status
  where
 standardEncode (NEW) = standardEncode {}
 standardEncode (INPROGRESS) = standardEncode {}
 standardEncode (COMPLETED) = standardEncode {}
 standardEncode (CANCELLED) = standardEncode {}
 standardEncode (UPCOMING)  = standardEncode {}
 standardEncode (NOTHING) = standardEncode {}

derive instance genericCoinsEarned :: Generic CoinsEarned _
derive instance newtypeCoinsEarned :: Newtype CoinsEarned _
instance standardEncodeCoinsEarned :: StandardEncode CoinsEarned where standardEncode (CoinsEarned req) = standardEncode req
instance showCoinsEarned :: Show CoinsEarned where show = genericShow
instance decodeCoinsEarned :: Decode CoinsEarned where decode = defaultDecode
instance encodeCoinsEarned :: Encode CoinsEarned where encode = defaultEncode

---------------------------------GET RIDES HISTORY WITHIN DATES---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

data GetRidesSummaryListReq = GetRidesSummaryListReq (Array String)

newtype GetRidesSummaryListResp = GetRidesSummaryListResp
    {
      list :: Array RidesSummary
    }

newtype RidesSummary = RidesSummary
  {
    earnings :: Int,
    rideDistance :: Int,
    rideDate :: String,
    noOfRides :: Int
  }

instance makeGetRidesSummarListReq :: RestEndpoint GetRidesSummaryListReq where
    makeRequest reqBody@(GetRidesSummaryListReq dateList) headers = defaultMakeRequestWithoutLogs POST (EP.getRidesSummaryList dateList) headers reqBody Nothing
    encodeRequest req = defaultEncode req

derive instance genericGetRidesSummaryListReq :: Generic GetRidesSummaryListReq _
instance showGetRidesSummaryListReq :: Show GetRidesSummaryListReq where show = genericShow
instance standardEncodeGetRidesSummaryListReq :: StandardEncode GetRidesSummaryListReq where standardEncode _ = standardEncode {}
instance decodeGetRidesSummaryListReq :: Decode GetRidesSummaryListReq where decode = defaultDecode
instance encodeGetRidesSummaryListReq :: Encode GetRidesSummaryListReq where encode = defaultEncode

derive instance genericGetRidesSummaryListResp :: Generic GetRidesSummaryListResp _
derive instance newtypeGetRidesSummaryListResp :: Newtype GetRidesSummaryListResp _
instance standardEncodeGetRidesSummaryListResp :: StandardEncode GetRidesSummaryListResp where standardEncode (GetRidesSummaryListResp req) = standardEncode req
instance showGetRidesSummaryListResp :: Show GetRidesSummaryListResp where show = genericShow
instance decodeGetRidesSummaryListResp :: Decode GetRidesSummaryListResp where decode = defaultDecode
instance encodeGetRidesSummaryListResp :: Encode GetRidesSummaryListResp where encode = defaultEncode

derive instance genericRidesSummary :: Generic RidesSummary _
derive instance newtypeRidesSummary :: Newtype RidesSummary _
instance standardEncodeRidesSummary :: StandardEncode RidesSummary where standardEncode (RidesSummary req) = standardEncode req
instance showRidesSummary :: Show RidesSummary where show = genericShow
instance decodeRidesSummary :: Decode RidesSummary where decode = defaultDecode
instance encodeRidesSummary :: Encode RidesSummary where encode = defaultEncode
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Request/Offer Ride API request, response types

newtype OfferRideReq = OfferRideReq {
  searchRequestId :: String,
  offeredFare :: Maybe Number
}


instance makeOfferRideReq :: RestEndpoint OfferRideReq where
  makeRequest reqBody headers = defaultMakeRequestWithoutLogs POST (EP.offerRide "") headers reqBody Nothing
  encodeRequest req = standardEncode req

derive instance genericOfferRideReq :: Generic OfferRideReq _
derive instance newtypeOfferRideReq :: Newtype OfferRideReq _
instance standardEncodeOfferRideReq :: StandardEncode OfferRideReq where standardEncode (OfferRideReq reqBody) = standardEncode reqBody
instance showOfferRideReq :: Show OfferRideReq where show = genericShow
instance decodeOfferRideReq :: Decode OfferRideReq where decode = defaultDecode
instance encodeOfferRideReq :: Encode OfferRideReq where encode = defaultEncode


----------------------------------------------------------------------------------------------------------------------------UPDATE DRIVER PROFILE----------------------------------------------------------------------------------------------------------------------------------------------

-- UpdateDriverInfo API request, response types
data UpdateDriverInfoRequest = UpdateDriverInfoRequest UpdateDriverInfoReq

newtype UpdateDriverInfoReq
  = UpdateDriverInfoReq UpdateDriverInfoReqEntity
  

type UpdateDriverInfoReqEntity = 
  { middleName :: Maybe String
  , firstName :: Maybe String
  , lastName :: Maybe String
  , deviceToken :: Maybe String
  , canDowngradeToSedan :: Maybe Boolean
  , canDowngradeToHatchback :: Maybe Boolean
  , canDowngradeToTaxi :: Maybe Boolean
  , language :: Maybe String
  , clientVersion :: Maybe CTA.Version
  , bundleVersion :: Maybe CTA.Version
  , gender :: Maybe String
  , languagesSpoken :: Maybe (Array String)
  , hometown :: Maybe String
  , vehicleName :: Maybe String
  , availableUpiApps :: Maybe String
  , canSwitchToRental :: Maybe Boolean
  , canSwitchToIntraCity :: Maybe Boolean
  , canSwitchToInterCity :: Maybe Boolean
  , isSpecialLocWarrior :: Maybe Boolean
  }

newtype UpdateDriverInfoResp = UpdateDriverInfoResp GetDriverInfoResp

instance makeUpdateDriverInfoReq :: RestEndpoint UpdateDriverInfoRequest where
    makeRequest reqBody@(UpdateDriverInfoRequest (UpdateDriverInfoReq rqBody)) headers = defaultMakeRequestWithoutLogs POST (EP.updateDriverInfo "") headers reqBody Nothing
    encodeRequest req = standardEncode req

derive instance genericUpdateDriverInfoRequest :: Generic UpdateDriverInfoRequest _
instance decodeUpdateDriverInfoRequest :: Decode UpdateDriverInfoRequest where decode = defaultDecode
instance standardEncodeUpdateDriverInfoRequest :: StandardEncode UpdateDriverInfoRequest where standardEncode (UpdateDriverInfoRequest req) = standardEncode req
instance encodeUpdateDriverInfoRequest :: Encode UpdateDriverInfoRequest where encode = defaultEncode


derive instance genericUpdateDriverInfoReq :: Generic UpdateDriverInfoReq _
derive instance newtypeUpdateDriverInfoReq :: Newtype UpdateDriverInfoReq _
instance standardEncodeUpdateDriverInfoReq :: StandardEncode UpdateDriverInfoReq where standardEncode (UpdateDriverInfoReq req) = standardEncode req
instance decodeUpdateDriverInfoReq :: Decode UpdateDriverInfoReq where decode = defaultDecode
instance encodeUpdateDriverInfoReq :: Encode UpdateDriverInfoReq where encode = defaultEncode

derive instance genericUpdateDriverInfoResp :: Generic UpdateDriverInfoResp _
derive instance newtypeUpdateDriverInfoResp :: Newtype UpdateDriverInfoResp _
instance standardEncodeUpdateDriverInfoResp :: StandardEncode UpdateDriverInfoResp where standardEncode (UpdateDriverInfoResp req) = standardEncode req
instance decodeUpdateDriverInfoResp :: Decode UpdateDriverInfoResp where decode = defaultDecode
instance encodeUpdateDriverInfoResp :: Encode UpdateDriverInfoResp where encode = defaultEncode


------------------------------------------------------------listCancelReason----------------------------------------------------------------------------------------------------------------------------------------------

-- listCancelReason API request, response types
data ListCancelReasonReq = ListCancelReasonReq { }

newtype ListCancelReasonResp = ListCancelReasonResp (Array RideCancellationReason)

newtype RideCancellationReason = RideCancellationReason {
  reasonCode :: String,
  description :: String
}

instance makeListCancelReasonReq :: RestEndpoint ListCancelReasonReq where
    makeRequest reqBody headers = defaultMakeRequestWithoutLogs GET (EP.listCancelReason "" ) headers reqBody Nothing
    encodeRequest req = defaultEncode req

derive instance genericListCancelReasonReq :: Generic ListCancelReasonReq _
instance showListCancelReasonReq :: Show ListCancelReasonReq where show = genericShow
instance standardEncodeListCancelReasonReq :: StandardEncode ListCancelReasonReq where standardEncode (ListCancelReasonReq req) = standardEncode req
instance decodeListCancelReasonReq :: Decode ListCancelReasonReq where decode = defaultDecode
instance encodeListCancelReasonReq :: Encode ListCancelReasonReq where encode = defaultEncode

derive instance genericListCancelReasonResp :: Generic ListCancelReasonResp _
derive instance newtypeListCancelReasonResp :: Newtype ListCancelReasonResp _
instance standardEncodeListCancelReasonResp :: StandardEncode ListCancelReasonResp where standardEncode (ListCancelReasonResp req) = standardEncode req
instance showListCancelReasonResp :: Show ListCancelReasonResp where show = genericShow
instance decodeListCancelReasonResp :: Decode ListCancelReasonResp where decode = defaultDecode
instance encodeListCancelReasonResp :: Encode ListCancelReasonResp where encode = defaultEncode

derive instance genericRideCancellationReason :: Generic RideCancellationReason _
derive instance newtypeRideCancellationReason :: Newtype RideCancellationReason _
instance standardEncodeRideCancellationReason :: StandardEncode RideCancellationReason where standardEncode (RideCancellationReason req) = standardEncode req
instance showRideCancellationReason :: Show RideCancellationReason where show = genericShow
instance decodeRideCancellationReason :: Decode RideCancellationReason where decode = defaultDecode
instance encodeRideCancellationReason :: Encode RideCancellationReason where encode = defaultEncode

------------------------------------------------------------GetRoute----------------------------------------------------------------------------------------------------------------------------------------------

-- getRoute API request, response types

data RouteReq = RouteReq String GetRouteReq

newtype GetRouteReq = GetRouteReq {
  waypoints :: Array LatLong
, mode :: Maybe String
, calcPoints :: Boolean
}

newtype GetRouteResp = GetRouteResp (Array Route)

newtype Route = Route
  {
    points :: Snapped
  , boundingBox :: Maybe (Array Number)
  , snappedWaypoints :: Snapped
  , duration :: Int
  , distance :: Int
  }

newtype Snapped = Snapped (Array LatLong)

newtype LatLong = LatLong {
  lat :: Number,
  lon :: Number
}

instance makeRouteReq :: RestEndpoint RouteReq where
  makeRequest reqBody@(RouteReq rType (GetRouteReq reqB)) headers = defaultMakeRequestWithoutLogs POST (EP.getRoute rType) headers reqBody Nothing
  encodeRequest req = standardEncode req

derive instance genericGetRouteReq :: Generic GetRouteReq _
derive instance newtypeGetRouteReq :: Newtype GetRouteReq _
instance standardEncodeGetRouteReq :: StandardEncode GetRouteReq where standardEncode (GetRouteReq body) = standardEncode body
instance showGetRouteReq :: Show GetRouteReq where show = genericShow
instance decodeGetRouteReq :: Decode GetRouteReq where decode = defaultDecode
instance encodeGetRouteReq  :: Encode GetRouteReq where encode = defaultEncode

derive instance genericRouteReq :: Generic RouteReq _
instance standardEncodeRouteReq :: StandardEncode RouteReq where standardEncode (RouteReq rType body) = standardEncode body
instance showRouteReq :: Show RouteReq where show = genericShow
instance decodeRouteReq :: Decode RouteReq where decode = defaultDecode
instance encodeRouteReq  :: Encode RouteReq where encode = defaultEncode

derive instance genericRoute :: Generic Route _
derive instance newtypeRoute :: Newtype Route _
instance standardEncodeRoute :: StandardEncode Route where standardEncode (Route body) = standardEncode body
instance showRoute :: Show Route where show = genericShow
instance decodeRoute :: Decode Route where decode = defaultDecode
instance encodeRoute:: Encode Route where encode = defaultEncode

derive instance genericGetRouteResp :: Generic GetRouteResp _
derive instance newtypeGetRouteResp :: Newtype GetRouteResp _
instance standardEncodeGetRouteResp :: StandardEncode GetRouteResp where standardEncode (GetRouteResp body) = standardEncode body
instance showGetRouteResp :: Show GetRouteResp where show = genericShow
instance decodeGetRouteResp :: Decode GetRouteResp where decode = defaultDecode
instance encodeGetRouteResp  :: Encode GetRouteResp where encode = defaultEncode

derive instance genericSnapped :: Generic Snapped _
derive instance newtypeSnapped :: Newtype Snapped _
instance standardEncodeSnapped :: StandardEncode Snapped where standardEncode (Snapped body) = standardEncode body
instance showSnapped :: Show Snapped where show = genericShow
instance decodeSnapped :: Decode Snapped where decode = defaultDecode
instance encodeSnapped  :: Encode Snapped where encode = defaultEncode

derive instance genericLatLong :: Generic LatLong _
derive instance newtypeLatLong :: Newtype LatLong _
instance standardEncodeLatLong :: StandardEncode LatLong where standardEncode (LatLong body) = standardEncode body
instance showLatLong :: Show LatLong where show = genericShow
instance decodeLatLong :: Decode LatLong where decode = defaultDecode
instance encodeLatLong:: Encode LatLong where encode = defaultEncode
instance eqLatLong :: Eq LatLong where eq = genericEq

------------------------------------------------------------OnBoarding Flow---------------------------------------------------------------------------------------------------------------------------------------------

-- registerDriverRC API request, response types
newtype DriverRCReq = DriverRCReq {
  vehicleRegistrationCertNumber :: String,
  operatingCity :: String,
  imageId :: String,
  dateOfRegistration :: Maybe String,
  vehicleCategory :: Maybe String,
  airConditioned :: Maybe Boolean,
  ventilator :: Maybe Boolean,
  oxygen :: Maybe Boolean
}


instance makeDriverRCReq :: RestEndpoint DriverRCReq where
  makeRequest reqBody headers = defaultMakeRequestWithoutLogs POST (EP.registerDriverRC "") headers reqBody Nothing
  encodeRequest req = standardEncode req

derive instance genericDriverRCReq :: Generic DriverRCReq _
derive instance newtypeDriverRCReq :: Newtype DriverRCReq _
instance standardEncodeDriverRCReq :: StandardEncode DriverRCReq where standardEncode (DriverRCReq body) = standardEncode body
instance showDriverRCReq :: Show DriverRCReq where show = genericShow
instance decodeDriverRCReq:: Decode DriverRCReq where decode = defaultDecode
instance encodeDriverRCReq  :: Encode DriverRCReq where encode = defaultEncode



-- registerDriverDL API request, response types
newtype DriverDLReq = DriverDLReq {
  driverLicenseNumber :: String,
  driverDateOfBirth :: String,
  operatingCity :: String,
  imageId1 :: String,
  imageId2 :: Maybe String,
  dateOfIssue :: Maybe String,
  vehicleCategory :: Maybe String
}


instance makeDriverDLReq :: RestEndpoint DriverDLReq where
  makeRequest reqBody headers = defaultMakeRequestWithoutLogs POST (EP.registerDriverDL "") headers reqBody Nothing
  encodeRequest req = standardEncode req

derive instance genericDriverDLReq :: Generic DriverDLReq _
derive instance newtypeDriverDLReq :: Newtype DriverDLReq _
instance standardEncodeDriverDLReq :: StandardEncode DriverDLReq where standardEncode (DriverDLReq body) = standardEncode body
instance showDriverDLReq :: Show DriverDLReq where show = genericShow
instance decodeDriverDLReq:: Decode DriverDLReq where decode = defaultDecode
instance encodeDriverDLReq  :: Encode DriverDLReq where encode = defaultEncode


-- validateImage API request, response types
newtype ValidateImageReq = ValidateImageReq {
  image :: String,
  imageType :: String,
  rcNumber :: Maybe String,
  validationStatus :: Maybe ValidationStatus,
  workflowTransactionId :: Maybe String,
  vehicleCategory :: Maybe String
}

newtype ValidateImageRes = ValidateImageRes {
  imageId :: String
}

instance makeValidateImageReq :: RestEndpoint ValidateImageReq where
  makeRequest reqBody headers = defaultMakeRequestWithoutLogs POST (EP.validateImage "") headers reqBody Nothing
  encodeRequest req = defaultEncode req

derive instance genericValidateImageReq :: Generic ValidateImageReq _
derive instance newtypeValidateImageReq :: Newtype ValidateImageReq _
instance standardEncodeValidateImageReq :: StandardEncode ValidateImageReq where standardEncode (ValidateImageReq body) = standardEncode body
instance showValidateImageReq :: Show ValidateImageReq where show = genericShow
instance decodeValidateImageReq:: Decode ValidateImageReq where decode = defaultDecode
instance encodeValidateImageReq  :: Encode ValidateImageReq where encode = defaultEncode

derive instance genericValidateImageRes :: Generic ValidateImageRes _
derive instance newtypeValidateImageRes :: Newtype ValidateImageRes _
instance standardEncodeValidateImageRes :: StandardEncode ValidateImageRes where standardEncode (ValidateImageRes body) = standardEncode body
instance showValidateImageRes :: Show ValidateImageRes where show = genericShow
instance decodeValidateImageRes:: Decode ValidateImageRes where decode = defaultDecode
instance encodeValidateImageRes  :: Encode ValidateImageRes where encode = defaultEncode


-- DriverRegistrationStatus API request, response types
data DriverRegistrationStatusReq = DriverRegistrationStatusReq Boolean

newtype DriverRegistrationStatusResp = DriverRegistrationStatusResp
    { dlVerificationStatus :: String
    , rcVerificationStatus :: String
    , aadhaarVerificationStatus :: String
    , driverDocuments :: Array DocumentStatusItem
    , vehicleDocuments :: Array VehicleDocumentItem
    , enabled :: Maybe Boolean
    }

newtype VehicleDocumentItem = VehicleDocumentItem
  { registrationNo :: String,
    userSelectedVehicleCategory :: String,
    verifiedVehicleCategory :: Maybe String,
    documents :: Array DocumentStatusItem,
    isVerified :: Boolean,
    vehicleModel :: Maybe String,
    isActive :: Boolean
  }

newtype DocumentStatusItem = DocumentStatusItem
  { documentType :: String,
    verificationStatus :: String,
    verificationMessage :: Maybe String
  }

instance makeDriverRegistrationStatusReq :: RestEndpoint DriverRegistrationStatusReq where
    makeRequest reqBody@(DriverRegistrationStatusReq queryParam) headers = defaultMakeRequestWithoutLogs GET (EP.driverRegistrationStatus queryParam) headers reqBody Nothing
    encodeRequest req = defaultEncode req

derive instance genericVehicleDocumentItem :: Generic VehicleDocumentItem _
instance standardEncodeVehicleDocumentItem :: StandardEncode VehicleDocumentItem where standardEncode (VehicleDocumentItem res) = standardEncode res
instance showVehicleDocumentItem :: Show VehicleDocumentItem where show = genericShow
instance decodeVehicleDocumentItem :: Decode VehicleDocumentItem where decode = defaultDecode
instance encodeVehicleDocumentItem  :: Encode VehicleDocumentItem where encode = defaultEncode

derive instance genericDocumentStatusItem :: Generic DocumentStatusItem _
instance standardEncodeDocumentStatusItem :: StandardEncode DocumentStatusItem where standardEncode (DocumentStatusItem res) = standardEncode res
instance showDocumentStatusItem :: Show DocumentStatusItem where show = genericShow
instance decodeDocumentStatusItem :: Decode DocumentStatusItem where decode = defaultDecode
instance encodeDocumentStatusItem  :: Encode DocumentStatusItem where encode = defaultEncode

derive instance genericDriverRegistrationStatusReq :: Generic DriverRegistrationStatusReq _
instance showDriverRegistrationStatusReq :: Show DriverRegistrationStatusReq where show = genericShow
instance standardEncodeDriverRegistrationStatusReq :: StandardEncode DriverRegistrationStatusReq where standardEncode (DriverRegistrationStatusReq req) = standardEncode req
instance decodeDriverRegistrationStatusReq :: Decode DriverRegistrationStatusReq where decode = defaultDecode
instance encodeDriverRegistrationStatusReq :: Encode DriverRegistrationStatusReq where encode = defaultEncode

derive instance genericDriverRegistrationStatusResp :: Generic DriverRegistrationStatusResp _
instance showDriverRegistrationStatusResp :: Show DriverRegistrationStatusResp where show = genericShow
instance standardEncodeDriverRegistrationStatusResp :: StandardEncode DriverRegistrationStatusResp where standardEncode (DriverRegistrationStatusResp req) = standardEncode req
instance decodeDriverRegistrationStatusResp :: Decode DriverRegistrationStatusResp where decode = defaultDecode
instance encodeDriverRegistrationStatusResp :: Encode DriverRegistrationStatusResp where encode = defaultEncode


-- ReferDriver API request, response types
data ReferDriverReq = ReferDriverReq { value :: String}


instance makeReferDriverReq :: RestEndpoint ReferDriverReq where
    makeRequest reqBody headers = defaultMakeRequestWithoutLogs POST (EP.referDriver "") headers reqBody Nothing
    encodeRequest req = defaultEncode req

derive instance genericReferDriverReq :: Generic ReferDriverReq _
instance showReferDriverReq :: Show ReferDriverReq where show = genericShow
instance standardEncodeReferDriverReq :: StandardEncode ReferDriverReq where standardEncode (ReferDriverReq req) = standardEncode req
instance decodeReferDriverReq :: Decode ReferDriverReq where decode = defaultDecode
instance encodeReferDriverReq :: Encode ReferDriverReq where encode = defaultEncode


-- DriverProfileStats API request, response types

data DriverProfileStatsReq = DriverProfileStatsReq String

newtype DriverProfileStatsResp = DriverProfileStatsResp
    {
      totalRidesOfDay :: Int
    , totalEarningsOfDay :: Int
    , bonusEarning :: Int
    , coinBalance :: Int
    , totalEarningsOfDayPerKm :: Maybe Int
    , totalValidRidesOfDay :: Maybe Int
    }

instance makeGetDriverProfileStatsReq :: RestEndpoint DriverProfileStatsReq where
    makeRequest reqBody@(DriverProfileStatsReq date) headers = defaultMakeRequestWithoutLogs GET (EP.getstatsInfo date) headers reqBody Nothing
    encodeRequest req = defaultEncode req

derive instance genericDriverProfileStatsReq :: Generic DriverProfileStatsReq _
instance showDriverProfileStatsReq :: Show DriverProfileStatsReq where show = genericShow
instance standardEncodeDriverProfileStatsReq :: StandardEncode DriverProfileStatsReq where standardEncode (DriverProfileStatsReq req) = standardEncode req
instance decodeDriverProfileStatsReq :: Decode DriverProfileStatsReq where decode = defaultDecode
instance encodeDriverProfileStatsReq :: Encode DriverProfileStatsReq where encode = defaultEncode

derive instance genericgenericDriverProfileStatsResp :: Generic DriverProfileStatsResp _
instance showGetDriverProfileStatsResp :: Show DriverProfileStatsResp where show = genericShow
instance standardEncodeDriverProfileStatsResp :: StandardEncode DriverProfileStatsResp where standardEncode (DriverProfileStatsResp req) = standardEncode req
instance decodeDriverProfileStatsResp :: Decode DriverProfileStatsResp where decode = defaultDecode
instance encodeDriverProfileStatsResp :: Encode DriverProfileStatsResp where encode = defaultEncode

data DriverArrivedRequest = DriverArrivedRequest String DriverArrivedReq

newtype DriverArrivedReq  = DriverArrivedReq {
    lat :: Number
  , lon :: Number
  }


derive instance genericDriverArrivedRequest :: Generic DriverArrivedRequest _
instance standardEncodeDriverArrivedRequest :: StandardEncode DriverArrivedRequest where standardEncode (DriverArrivedRequest rideId req) = standardEncode req
instance showDriverArrivedRequest :: Show DriverArrivedRequest where show = genericShow
instance decodeDriverArrivedRequest :: Decode DriverArrivedRequest where decode = defaultDecode
instance encodeDriverArrivedRequest :: Encode DriverArrivedRequest where encode = defaultEncode


instance makeDriverArrivedReq :: RestEndpoint DriverArrivedRequest where
    makeRequest reqBody@(DriverArrivedRequest rideId (DriverArrivedReq rqBody)) headers = defaultMakeRequestWithoutLogs POST (EP.driverArrived rideId) headers reqBody Nothing
    encodeRequest req = standardEncode req

derive instance genericDriverArrivedReq :: Generic DriverArrivedReq _
instance showDriverArrivedReq :: Show DriverArrivedReq where show = genericShow
instance standardEncodeDriverArrivedReq :: StandardEncode DriverArrivedReq where standardEncode (DriverArrivedReq req) = standardEncode req
instance decodeDriverArrivedReq :: Decode DriverArrivedReq where decode = defaultDecode
instance encodeDriverArrivedReq :: Encode DriverArrivedReq where encode = defaultEncode


-- flowStatus api --

data FlowStatusReq = FlowStatusReq

newtype FlowStatusRes = FlowStatusRes
  { currentStatus :: FlowStatus
  , oldStatus :: Maybe FlowStatus
  }

data FlowStatus = IDLE {}
                | ACTIVE {}
                | GOT_SEARCH_REQUEST {requestId :: String, validTill :: String}
                | OFFERED_QUOTE  {quoteId :: String , validTill :: String}
                | RIDE_ASSIGNED {  rideId :: String}
                | WAITING_FOR_CUSTOMER {  rideId :: String}
                | ON_RIDE {rideId :: String}

instance makeFlowStatusReq :: RestEndpoint FlowStatusReq where
    makeRequest reqBody headers = defaultMakeRequestWithoutLogs GET (EP.flowStatus "") headers reqBody Nothing
    encodeRequest req = defaultEncode req

derive instance genericFlowStatusReq :: Generic FlowStatusReq _
instance standardEncodeFlowStatusReq :: StandardEncode FlowStatusReq where standardEncode (FlowStatusReq) = standardEncode {}
instance decodeFlowStatusReq :: Decode FlowStatusReq where decode = defaultDecode
instance encodeFlowStatusReq :: Encode FlowStatusReq where encode = defaultEncode

derive instance genericFlowStatusRes :: Generic FlowStatusRes _
derive instance newtypeFlowStatusRes:: Newtype FlowStatusRes _
instance standardEncodeFlowStatusRes :: StandardEncode FlowStatusRes where standardEncode (FlowStatusRes req) = standardEncode req
instance showFlowStatusRes :: Show FlowStatusRes where show = genericShow
instance decodeFlowStatusRes :: Decode FlowStatusRes where decode = defaultDecode
instance encodeFlowStatusRes :: Encode FlowStatusRes where encode = defaultEncode

derive instance genericFlowStatus :: Generic FlowStatus _
instance showFlowStatus :: Show FlowStatus where show = genericShow
instance decodeFlowStatus :: Decode FlowStatus
  where
    decode body = case (runExcept $ (readProp "status" body) >>= decode) of
      Either.Right status -> case status of
        "IDLE"                  -> IDLE <$> decode body
        "ACTIVE"                -> ACTIVE <$> decode body
        "GOT_SEARCH_REQUEST"    -> GOT_SEARCH_REQUEST <$> decode body
        "OFFERED_QUOTE"         -> OFFERED_QUOTE <$> decode body
        "RIDE_ASSIGNED"         -> RIDE_ASSIGNED <$> decode body
        "WAITING_FOR_CUSTOMER"  -> WAITING_FOR_CUSTOMER <$> decode body
        "ON_RIDE"               -> ON_RIDE <$> decode body
        _                       -> fail $ ForeignError "Unknown response"

      Either.Left error -> fail $ ForeignError "Unknown response"

instance encodeFlowStatus :: Encode FlowStatus
  where
    encode (IDLE body) = encode body
    encode (ACTIVE body) = encode body
    encode (GOT_SEARCH_REQUEST body) = encode body
    encode (OFFERED_QUOTE body) = encode body
    encode (RIDE_ASSIGNED body) = encode body
    encode (WAITING_FOR_CUSTOMER body) = encode body
    encode (ON_RIDE body) = encode body

instance standardEncodeFlowStatus :: StandardEncode FlowStatus
  where
    standardEncode (IDLE body) = standardEncode body
    standardEncode (ACTIVE body) = standardEncode body
    standardEncode (GOT_SEARCH_REQUEST body) = standardEncode body
    standardEncode (OFFERED_QUOTE body) = standardEncode body
    standardEncode (RIDE_ASSIGNED body) = standardEncode body
    standardEncode (WAITING_FOR_CUSTOMER body) = standardEncode body
    standardEncode (ON_RIDE body) = standardEncode body
------------------------------------------------------ messageList -----------------------------------------------

data MessageListReq = MessageListReq String String

data GetMessageReq = GetMessageReq String

newtype MessageListRes = MessageListRes (Array MessageAPIEntityResponse)

newtype MessageAPIEntityResponse = MessageAPIEntityResponse
  { type :: MessageType
  , mediaFiles :: Array MediaFileApiResponse
  , title :: String
  , readStatus :: Boolean
  , messageId :: String
  , description :: String
  , reply :: Maybe String
  , label :: Maybe String
  , created_at :: String
  , likeCount :: Int
  , viewCount :: Int
  , likeStatus :: Boolean
  , shareable :: Maybe Boolean
  }

newtype MediaFileApiResponse = MediaFileApiResponse
  { url :: String
  , fileType :: MediaType
  }

data MessageType = Action { contents :: String }
                 | Read { }

data MediaType = Video
               | Audio
               | Image
               | AudioLink
               | VideoLink
               | ImageLink
               | PortraitVideoLink

instance makeMessageListReq :: RestEndpoint MessageListReq where
    makeRequest reqBody@(MessageListReq limit offset) headers = defaultMakeRequestWithoutLogs GET (EP.messageList limit offset) headers reqBody Nothing
    encodeRequest req = defaultEncode req

instance makeGetMessageReq:: RestEndpoint GetMessageReq where
    makeRequest reqBody@(GetMessageReq id) headers = defaultMakeRequestWithoutLogs GET (EP.getMessage id) headers reqBody Nothing
    encodeRequest req = defaultEncode req

derive instance genericMessageListReq :: Generic MessageListReq _
instance showMessageListReq :: Show MessageListReq where show = genericShow
instance standardEncodeMessageListReq :: StandardEncode MessageListReq where standardEncode (MessageListReq _ _) = standardEncode {}
instance decodeMessageListReq :: Decode MessageListReq where decode = defaultDecode
instance encodeMessageListReq :: Encode MessageListReq where encode = defaultEncode

derive instance genericGetMessageReq :: Generic GetMessageReq _
instance showGetMessageReq :: Show GetMessageReq where show = genericShow
instance standardEncodeGetMessageReq :: StandardEncode GetMessageReq where standardEncode (GetMessageReq _) = standardEncode {}
instance decodeGetMessageReq :: Decode GetMessageReq where decode = defaultDecode
instance encodeGetMessageReq :: Encode GetMessageReq where encode = defaultEncode

derive instance genericMessageListRes :: Generic MessageListRes _
derive instance newtypeMessageListRes :: Newtype MessageListRes _
instance showMessageListRes :: Show MessageListRes where show = genericShow
instance standardEncodeMessageListRes :: StandardEncode MessageListRes where standardEncode (MessageListRes res) = standardEncode res
instance decodeMessageListRes :: Decode MessageListRes where decode = defaultDecode
instance encodeMessageListRes :: Encode MessageListRes where encode = defaultEncode

derive instance genericMessageAPIEntityResponse :: Generic MessageAPIEntityResponse _
derive instance newtypeMessageAPIEntityResponse :: Newtype MessageAPIEntityResponse _
instance showMessageAPIEntityResponse :: Show MessageAPIEntityResponse where show = genericShow
instance standardEncodeMessageAPIEntityResponse :: StandardEncode MessageAPIEntityResponse where standardEncode (MessageAPIEntityResponse res) = standardEncode res
instance decodeMessageAPIEntityResponse :: Decode MessageAPIEntityResponse where decode = defaultDecode
instance encodeMessageAPIEntityResponse :: Encode MessageAPIEntityResponse where encode = defaultEncode

derive instance genericMediaFileApiResponse :: Generic MediaFileApiResponse _
derive instance newtypeMediaFileApiResponse :: Newtype MediaFileApiResponse _
instance showMediaFileApiResponse :: Show MediaFileApiResponse where show = genericShow
instance standardEncodeMediaFileApiResponse :: StandardEncode MediaFileApiResponse where standardEncode (MediaFileApiResponse res) = standardEncode res
instance decodeMediaFileApiResponse :: Decode MediaFileApiResponse where decode = defaultDecode
instance encodeMediaFileApiResponse :: Encode MediaFileApiResponse where encode = defaultEncode

derive instance genericMessageType :: Generic MessageType _
instance showMessageType :: Show MessageType where show = genericShow
instance decodeMessageType :: Decode MessageType
  where
    decode body = case (runExcept $ (readProp "tag" body) >>= decode) of
                    Either.Right tag -> case tag of
                                  "Action" -> (Action <$> decode body)
                                  "Read"   -> (Read <$> decode body)
                                  _        -> (fail $ ForeignError "Unknown response")
                    Either.Left err  -> (fail $ ForeignError "Unknown response")
instance encodeMessageType :: Encode MessageType
  where
    encode (Action body) = encode body
    encode (Read body) = encode body
instance standardEncodeMessageType :: StandardEncode MessageType
  where
    standardEncode (Action body) = standardEncode body
    standardEncode (Read body) = standardEncode body

derive instance genericMediaType :: Generic MediaType _
instance showMediaType :: Show MediaType where show = genericShow
instance decodeMediaType :: Decode MediaType where decode = defaultEnumDecode
instance encodeMediaType :: Encode MediaType where encode = defaultEnumEncode
instance eqMediaType :: Eq MediaType where eq = genericEq
instance standardEncodeMediaType :: StandardEncode MediaType
  where
    standardEncode _ = standardEncode {}

------------------------------------------------------ messageSeen -----------------------------------------------

data MessageSeenReq = MessageSeenReq String


instance makeMessageSeenReq :: RestEndpoint MessageSeenReq where
    makeRequest reqBody@(MessageSeenReq messageId) headers = defaultMakeRequestWithoutLogs PUT (EP.messageSeen messageId) headers reqBody Nothing
    encodeRequest req = defaultEncode req

derive instance genericMessageSeenReq :: Generic MessageSeenReq _
instance showMessageSeenReq :: Show MessageSeenReq where show = genericShow
instance standardEncodeMessageSeenReq :: StandardEncode MessageSeenReq where standardEncode (MessageSeenReq _) = standardEncode {}
instance decodeMessageSeenReq :: Decode MessageSeenReq where decode = defaultDecode
instance encodeMessageSeenReq :: Encode MessageSeenReq where encode = defaultEncode


------------------------------------------------------ likeMessage -----------------------------------------------

data LikeMessageReq = LikeMessageReq String


instance makeLikeMessageReq :: RestEndpoint LikeMessageReq where
    makeRequest reqBody@(LikeMessageReq messageId) headers = defaultMakeRequestWithoutLogs PUT (EP.likeMessage messageId) headers reqBody Nothing
    encodeRequest req = defaultEncode req

derive instance genericLikeMessageReq :: Generic LikeMessageReq _
instance showLikeMessageReq :: Show LikeMessageReq where show = genericShow
instance standardEncodeLikeMessageReq :: StandardEncode LikeMessageReq where standardEncode (LikeMessageReq _) = standardEncode {}
instance decodeLikeMessageReq :: Decode LikeMessageReq where decode = defaultDecode
instance encodeLikeMessageReq :: Encode LikeMessageReq where encode = defaultEncode


------------------------------------------------------ messageReply -----------------------------------------------

data MessageResponseReq = MessageResponseReq String MessageReplyReq

newtype MessageReplyReq = MessageReplyReq {
    reply :: String
  }


instance makeMessageResponseReq :: RestEndpoint MessageResponseReq where
    makeRequest reqBody@(MessageResponseReq messageId (MessageReplyReq req)) headers = defaultMakeRequestWithoutLogs PUT (EP.messageResponse messageId) headers reqBody Nothing
    encodeRequest req = standardEncode req

derive instance genericMessageResponseReq :: Generic MessageResponseReq _
instance showMessageResponseReq :: Show MessageResponseReq where show = genericShow
instance standardEncodeMessageResponseReq :: StandardEncode MessageResponseReq where standardEncode (MessageResponseReq messageId req) = standardEncode req
instance decodeMessageResponseReq :: Decode MessageResponseReq where decode = defaultDecode
instance encodeMessageResponseReq :: Encode MessageResponseReq where encode = defaultEncode

derive instance genericMessageReplyReq :: Generic MessageReplyReq _
derive instance newtypeMessageReplyReq :: Newtype MessageReplyReq _
instance showMessageReplyReq :: Show MessageReplyReq where show = genericShow
instance standardEncodeMessageReplyReq :: StandardEncode MessageReplyReq where standardEncode (MessageReplyReq reply) = standardEncode reply
instance decodeMessageReplyReq :: Decode MessageReplyReq where decode = defaultDecode
instance encodeMessageReplyReq :: Encode MessageReplyReq where encode = defaultEncode

--------------------------------------------------- linkReferral ----------------------------------------------------

newtype LinkReferralCodeReq = LinkReferralCodeReq {
    referralLinkPassword :: String
  , referralCode :: String
}


instance makeLinkReferralCodeReq :: RestEndpoint LinkReferralCodeReq where
    makeRequest reqBody@(LinkReferralCodeReq date) headers = defaultMakeRequestWithoutLogs POST (EP.linkReferralCode "") headers reqBody Nothing
    encodeRequest req = defaultEncode req

derive instance genericLinkReferralCodeReq :: Generic LinkReferralCodeReq _
instance showLinkReferralCodeReq :: Show LinkReferralCodeReq where show = genericShow
instance standardLinkReferralCodeReq :: StandardEncode LinkReferralCodeReq where standardEncode (LinkReferralCodeReq req) = standardEncode req
instance decodeLinkReferralCodeReq :: Decode LinkReferralCodeReq where decode = defaultDecode
instance encodeLinkReferralCodeReq :: Encode LinkReferralCodeReq where encode = defaultEncode



--------------------------------------------------- getPerformance ----------------------------------------------------

newtype GetPerformanceReq = GetPerformanceReq {}

newtype GetPerformanceRes = GetPerformanceRes {
  referrals :: {
    totalActivatedCustomers :: Int,
    totalReferredCustomers :: Int,
    totalReferredDrivers :: Maybe Int,
    isPayoutEnabled :: Boolean,
    eligiblePayoutAmount :: Int,
    lastPayoutAt :: Maybe String,
    payoutAmountPaid :: Int,
    payoutVpa :: Maybe String
  }
}

instance makeGetPerformanceReq :: RestEndpoint GetPerformanceReq where
    makeRequest reqBody@(GetPerformanceReq date) headers = defaultMakeRequestWithoutLogs GET (EP.getPerformance "") headers reqBody Nothing
    encodeRequest req = defaultEncode req

derive instance genericGetPerformanceReq :: Generic GetPerformanceReq _
instance showGetPerformanceReq :: Show GetPerformanceReq where show = genericShow
instance standardGetPerformanceReq :: StandardEncode GetPerformanceReq where standardEncode (GetPerformanceReq req) = standardEncode req
instance decodeGetPerformanceReq :: Decode GetPerformanceReq where decode = defaultDecode
instance encodeGetPerformanceReq :: Encode GetPerformanceReq where encode = defaultEncode

derive instance genericGetPerformanceRes :: Generic GetPerformanceRes _
instance showGetGetPerformanceRes :: Show GetPerformanceRes where show = genericShow
instance standardEncodeGetPerformanceRes :: StandardEncode GetPerformanceRes where standardEncode (GetPerformanceRes req) = standardEncode req
instance decodeGetPerformanceRes :: Decode GetPerformanceRes where decode = defaultDecode
instance encodeGetPerformanceRes :: Encode GetPerformanceRes where encode = defaultEncode

----------------------------------- driverAlternateNumber ----------------------------------------

newtype DriverAlternateNumberReq = DriverAlternateNumberReq
  {
    mobileCountryCode :: String,
    alternateNumber :: String
  }

newtype DriverAlternateNumberResp =  DriverAlternateNumberResp  {
  attempts :: Int
}


instance makeDriverAlternateNumberReq :: RestEndpoint DriverAlternateNumberReq where
  makeRequest reqBody headers = defaultMakeRequestWithoutLogs POST (EP.driverAlternateNumber "") headers reqBody Nothing
  encodeRequest req = standardEncode req


derive instance genericDriverAlternateNumberReq :: Generic DriverAlternateNumberReq _
derive instance newtypeDriverAlternateNumberReq :: Newtype DriverAlternateNumberReq _
instance standardEncodeDriverAlternateNumberReq :: StandardEncode DriverAlternateNumberReq where standardEncode (DriverAlternateNumberReq reqBody) = standardEncode reqBody
instance showDriverAlternateNumberReq :: Show DriverAlternateNumberReq where show = genericShow
instance decodeDriverAlternateNumberReq :: Decode DriverAlternateNumberReq where decode = defaultDecode
instance encodeDriverAlternateNumberReq :: Encode DriverAlternateNumberReq where encode = defaultEncode

derive instance genericDriverAlternateNumberResp :: Generic DriverAlternateNumberResp _
derive instance newtypeDriverAlternateNumberResp :: Newtype DriverAlternateNumberResp _
instance standardEncodeDriverAlternateNumberResp :: StandardEncode DriverAlternateNumberResp where standardEncode (DriverAlternateNumberResp id) = standardEncode id
instance showDriverAlternateNumberResp :: Show DriverAlternateNumberResp where show = genericShow
instance decodeDriverAlternateNumberResp :: Decode DriverAlternateNumberResp where decode = defaultDecode
instance encodeDriverAlternateNumberResp :: Encode DriverAlternateNumberResp where encode = defaultEncode



--------------------------------  driverAlternateNumberOtp --------------------------------------
newtype DriverAlternateNumberOtpReq = DriverAlternateNumberOtpReq
 {
   otp :: String
 }



instance makeDriverAlternateNumberOtpReq :: RestEndpoint DriverAlternateNumberOtpReq where
  makeRequest reqBody headers = defaultMakeRequestWithoutLogs POST (EP.verifyAlternateNumberOTP "") headers reqBody Nothing
  encodeRequest req = standardEncode req


derive instance genericDriverAlternateNumberOtpReq :: Generic DriverAlternateNumberOtpReq _
derive instance newtypeDriverAlternateNumberOtpReq :: Newtype DriverAlternateNumberOtpReq _
instance standardEncodeDriverAlternateNumberOtpReq :: StandardEncode DriverAlternateNumberOtpReq where standardEncode (DriverAlternateNumberOtpReq reqBody) = standardEncode reqBody
instance showDriverAlternateNumberOtpReq :: Show DriverAlternateNumberOtpReq where show = genericShow
instance decodeDriverAlternateNumberOtpReq :: Decode DriverAlternateNumberOtpReq where decode = defaultDecode
instance encodeDriverAlternateNumberOtpReq :: Encode DriverAlternateNumberOtpReq where encode = defaultEncode




------------------------------  AlternateNumberResendOTP -------------------------

data AlternateNumberResendOTPRequest = AlternateNumberResendOTPRequest
 {
    alternateNumber :: String,
    mobileCountryCode :: String
 }
newtype AlternateNumberResendOTPResp = AlternateNumberResendOTPResp
 {  attemptsLeft :: Int,
    auth :: String

 }

instance makeAlternateNumberResendOTPReq :: RestEndpoint AlternateNumberResendOTPRequest where
    makeRequest reqBody headers = defaultMakeRequestWithoutLogs POST (EP.alternateNumberResendOTP "") headers reqBody Nothing
    encodeRequest req = standardEncode req

derive instance genericAlternateNumberResendOTPResp :: Generic AlternateNumberResendOTPResp _
derive instance newtypeAlternateNumberResendOTPResp :: Newtype AlternateNumberResendOTPResp _
instance standardEncodeAlternateNumberResendOTPResp :: StandardEncode AlternateNumberResendOTPResp where standardEncode (AlternateNumberResendOTPResp req) = standardEncode req
instance showAlternateNumberResendOTPResp :: Show AlternateNumberResendOTPResp where show = genericShow
instance decodeRAlternateNumberResendOTPResp :: Decode AlternateNumberResendOTPResp where decode = defaultDecode
instance encodeAlternateNumberResendOTPResp :: Encode AlternateNumberResendOTPResp where encode = defaultEncode

derive instance genericAlternateNumberResendOTPRequest :: Generic AlternateNumberResendOTPRequest _
instance decodeAlternateNumberResendOTPRequest :: Decode AlternateNumberResendOTPRequest where decode = defaultDecode
instance standardEncodeAlternateNumberResendOTPRequest :: StandardEncode AlternateNumberResendOTPRequest where standardEncode (AlternateNumberResendOTPRequest token) = standardEncode token
instance encodeAlternateNumberResendOTPRequest :: Encode AlternateNumberResendOTPRequest where encode = defaultEncode


------------------------------------------ RemoveAlternateNumber ----------------------------------------

data RemoveAlternateNumberRequest = RemoveAlternateNumberRequest {}



instance makeRemoveAlternateNumberReq :: RestEndpoint RemoveAlternateNumberRequest where
    makeRequest reqBody headers = defaultMakeRequestWithoutLogs DELETE (EP.removeAlternateNumber "") headers reqBody Nothing
    encodeRequest req = standardEncode req


derive instance genericRemoveAlternateNumberRequest :: Generic RemoveAlternateNumberRequest _
instance decodeRemoveAlternateNumberRequest :: Decode RemoveAlternateNumberRequest where decode = defaultDecode
instance standardEncodeRemoveAlternateNumberRequest :: StandardEncode RemoveAlternateNumberRequest where standardEncode (RemoveAlternateNumberRequest token) = standardEncode token
instance encodeRemoveAlternateNumberRequest :: Encode RemoveAlternateNumberRequest where encode = defaultEncode

--------------------------------------------------- getCategories ----------------------------------------------------
data GetCategoriesReq = GetCategoriesReq String

newtype GetCategoriesRes = GetCategoriesRes { categories :: Array Category }

newtype Category = Category
  { label    :: String
  , logoUrl  :: String
  , category :: String
  , isRideRequired :: Boolean
  , issueCategoryId :: String
  , maxAllowedRideAge :: Maybe Int
  , allowedRideStatuses :: Maybe (Array String)
  , categoryType :: String
  }

instance makeGetCategoriesReq :: RestEndpoint GetCategoriesReq where
    makeRequest reqBody@(GetCategoriesReq language) headers = defaultMakeRequestWithoutLogs GET (EP.getCategories language) headers reqBody Nothing
    encodeRequest req = defaultEncode req

derive instance genericGetCategoriesReq :: Generic GetCategoriesReq _
instance showGetCategoriesReq     :: Show GetCategoriesReq where show     = genericShow
instance standardGetCategoriesReq :: StandardEncode GetCategoriesReq where standardEncode (GetCategoriesReq req) = standardEncode req
instance decodeGetCategoriesReq   :: Decode GetCategoriesReq where decode = defaultDecode
instance encodeGetCategoriesReq   :: Encode GetCategoriesReq where encode = defaultEncode

derive instance genericCategory :: Generic Category _
instance showCategory     :: Show Category where show     = genericShow
instance standardCategory :: StandardEncode Category where standardEncode (Category req) = standardEncode req
instance decodeCategory   :: Decode Category where decode = defaultDecode
instance encodeCategory   :: Encode Category where encode = defaultEncode

derive instance genericGetCategoriesRes :: Generic GetCategoriesRes _
instance showGetGetCategoriesRes        :: Show GetCategoriesRes where show     = genericShow
instance standardEncodeGetCategoriesRes :: StandardEncode GetCategoriesRes where standardEncode (GetCategoriesRes res) = standardEncode res
instance decodeGetCategoriesRes         :: Decode GetCategoriesRes where decode = defaultDecode
instance encodeGetCategoriesRes         :: Encode GetCategoriesRes where encode = defaultEncode

--------------------------------------------------- getOptions ----------------------------------------------------
data GetOptionsReq = GetOptionsReq String String

newtype GetOptionsRes = GetOptionsRes { options :: Array Option }

newtype Option = Option
  { label  :: String
  , option :: String
  , issueOptionId :: String
  }

instance makeGetOptionsReq :: RestEndpoint GetOptionsReq where
    makeRequest reqBody@(GetOptionsReq categoryId language) headers = defaultMakeRequestWithoutLogs GET (EP.getOptions categoryId language) headers reqBody Nothing
    encodeRequest req = defaultEncode req

derive instance genericGetOptionsReq :: Generic GetOptionsReq _
instance showGetOptionsReq     :: Show GetOptionsReq where show     = genericShow
instance standardGetOptionsReq :: StandardEncode GetOptionsReq where standardEncode (GetOptionsReq _ _) = standardEncode {}
instance decodeGetOptionsReq   :: Decode GetOptionsReq where decode = defaultDecode
instance encodeGetOptionsReq   :: Encode GetOptionsReq where encode = defaultEncode

derive instance genericOption :: Generic Option _
instance showOption     :: Show Option where show     = genericShow
instance standardOption :: StandardEncode Option where standardEncode (Option _) = standardEncode {}
instance decodeOption   :: Decode Option where decode = defaultDecode
instance encodeOption   :: Encode Option where encode = defaultEncode

derive instance genericGetOptionsRes :: Generic GetOptionsRes _
instance showGetGetOptionsRes        :: Show GetOptionsRes where show     = genericShow
instance standardEncodeGetOptionsRes :: StandardEncode GetOptionsRes where standardEncode (GetOptionsRes req) = standardEncode req
instance decodeGetOptionsRes         :: Decode GetOptionsRes where decode = defaultDecode
instance encodeGetOptionsRes         :: Encode GetOptionsRes where encode = defaultEncode

--------------------------------------------------- IssueReport ----------------------------------------------------
newtype PostIssueReq = PostIssueReq
  { optionId :: Maybe String
  , rideId :: Maybe String
  , categoryId :: String
  , mediaFiles :: Array String
  , description :: String
  , chats :: Array ChatDetail
  }

newtype ChatDetail = ChatDetail
  { timestamp :: String,
    content :: Maybe String,
    id :: String,
    label :: Maybe String,
    chatType :: String,
    sender :: String
  }

newtype PostIssueRes = PostIssueRes { issueReportId :: String }

instance makePostIssueReq :: RestEndpoint PostIssueReq where
    makeRequest reqBody@(PostIssueReq issueDetails) headers = defaultMakeRequestWithoutLogs POST (EP.postIssue "") headers reqBody Nothing
    encodeRequest req = defaultEncode req

derive instance genericPostIssueReq :: Generic PostIssueReq _
instance showPostIssueReq     :: Show PostIssueReq where show     = genericShow
instance standardPostIssueReq :: StandardEncode PostIssueReq where standardEncode (PostIssueReq req) = standardEncode {}
instance decodePostIssueReq   :: Decode PostIssueReq where decode = defaultDecode
instance encodePostIssueReq   :: Encode PostIssueReq where encode = defaultEncode

derive instance genericPostIssueRes :: Generic PostIssueRes _
instance showGetPostIssueRes        :: Show PostIssueRes where show     = genericShow
instance standardEncodePostIssueRes :: StandardEncode PostIssueRes where standardEncode (PostIssueRes res) = standardEncode res
instance decodePostIssueRes         :: Decode PostIssueRes where decode = defaultDecode
instance encodePostIssueRes         :: Encode PostIssueRes where encode = defaultEncode

derive instance genericChatDetail :: Generic ChatDetail _
instance showChatDetail     :: Show ChatDetail where show     = genericShow
instance standardChatDetail :: StandardEncode ChatDetail where standardEncode (ChatDetail chtDetail) = standardEncode chtDetail
instance decodeChatDetail   :: Decode ChatDetail where decode = defaultDecode
instance encodeChatDetail   :: Encode ChatDetail where encode = defaultEncode

--------------------------------------------------- IssueInfo ----------------------------------------------------
newtype IssueInfoReq = IssueInfoReq String

newtype IssueInfoRes = IssueInfoRes
  { mediaFiles    :: Array { url :: String, _type :: String }
  , description   :: String
  , issueReportId :: String
  , categoryId    :: Maybe String
  }

instance makeIssueInfoReq :: RestEndpoint IssueInfoReq where
    makeRequest reqBody@(IssueInfoReq issueId) headers = defaultMakeRequestWithoutLogs GET (EP.issueInfo issueId) headers reqBody Nothing
    encodeRequest req = defaultEncode req

derive instance genericIssueInfoReq :: Generic IssueInfoReq _
instance showIssueInfoReq     :: Show IssueInfoReq where show     = genericShow
instance standardIssueInfoReq :: StandardEncode IssueInfoReq where standardEncode (IssueInfoReq _) = standardEncode {}
instance decodeIssueInfoReq   :: Decode IssueInfoReq where decode = defaultDecode
instance encodeIssueInfoReq   :: Encode IssueInfoReq where encode = defaultEncode

derive instance genericIssueInfoRes :: Generic IssueInfoRes _
instance showGetIssueInfoRes        :: Show IssueInfoRes where show     = genericShow
instance standardEncodeIssueInfoRes :: StandardEncode IssueInfoRes where standardEncode (IssueInfoRes res) = standardEncode res
instance decodeIssueInfoRes         :: Decode IssueInfoRes where decode = defaultDecode
instance encodeIssueInfoRes         :: Encode IssueInfoRes where encode = defaultEncode

--------------------------------------------------- CallCustomer ----------------------------------------------------
data CallCustomerReq = CallCustomerReq String

newtype CallCustomerRes = CallCustomerRes {
  callId :: String
}

instance makeCallCustomerReq :: RestEndpoint CallCustomerReq where
  makeRequest reqBody@(CallCustomerReq rideId) headers = defaultMakeRequestWithoutLogs POST (EP.callDriverToCustomer rideId) headers reqBody Nothing
  encodeRequest req = standardEncode req

derive instance genericCallCustomerReq :: Generic CallCustomerReq _
instance standardEncodeCallCustomerReq :: StandardEncode CallCustomerReq where standardEncode (CallCustomerReq body) = standardEncode body
instance showCallCustomerReq :: Show CallCustomerReq where show = genericShow
instance decodeCallCustomerReq :: Decode CallCustomerReq where decode = defaultDecode
instance encodeCallCustomerReq  :: Encode CallCustomerReq where encode = defaultEncode

derive instance genericCallCustomerRes :: Generic CallCustomerRes _
derive instance newtypeCallCustomerRes :: Newtype CallCustomerRes _
instance standardEncodeCallCustomerRes :: StandardEncode CallCustomerRes where standardEncode (CallCustomerRes body) = standardEncode body
instance showCallCustomerRes :: Show CallCustomerRes where show = genericShow
instance decodeCallCustomerRes :: Decode CallCustomerRes where decode = defaultDecode
instance encodeCallCustomerRes  :: Encode CallCustomerRes where encode = defaultEncode

-------------------------------------------- FetchIssueList -------------------------------------------


data FetchIssueListReq = FetchIssueListReq

newtype FetchIssueListResp = FetchIssueListResp
 {
   issues :: Array IssueReportDriverListItem
 }

newtype IssueReportDriverListItem = IssueReportDriverListItem
  {
    issueReportId :: String,
    status :: String,
    category :: String,
    createdAt :: String,
    issueReportShortId :: Maybe String,
    optionLabel :: Maybe String,
    rideId :: Maybe String
  }

instance makeFetchIssueListReq :: RestEndpoint FetchIssueListReq where
    makeRequest reqBody headers = defaultMakeRequestWithoutLogs GET (EP.fetchIssueList "") headers reqBody Nothing
    encodeRequest req = standardEncode req

derive instance genericFetchIssueListResp :: Generic FetchIssueListResp _
derive instance newtypeFetchIssueListResp :: Newtype FetchIssueListResp _
instance showFetchIssueListResp :: Show FetchIssueListResp where show = genericShow
instance standardEncodeFetchIssueListResp :: StandardEncode FetchIssueListResp where standardEncode (FetchIssueListResp req) = standardEncode req
instance decodeFetchIssueListResp :: Decode FetchIssueListResp where decode = defaultDecode
instance encodeFetchIssueListResp :: Encode FetchIssueListResp where encode = defaultEncode

derive instance genericIssueReportDriverListItem :: Generic IssueReportDriverListItem _
derive instance newtypeIssueReportDriverListItem :: Newtype IssueReportDriverListItem _
instance showIssueReportDriverListItem :: Show IssueReportDriverListItem where show = genericShow
instance standardEncodeIssueReportDriverListItem :: StandardEncode IssueReportDriverListItem where standardEncode (IssueReportDriverListItem req) = standardEncode req
instance decodeIssueReportDriverListItem :: Decode IssueReportDriverListItem where decode = defaultDecode
instance encodeIssueReportDriverListItem :: Encode IssueReportDriverListItem where encode = defaultEncode

derive instance genericFetchIssueListReq :: Generic FetchIssueListReq _
instance decodeFetchIssueListReq :: Decode FetchIssueListReq where decode = defaultDecode
instance standardEncodeRemoveAlternateNumberReq :: StandardEncode FetchIssueListReq where standardEncode (FetchIssueListReq) = standardEncode {}
instance encodeFetchIssueListReq :: Encode FetchIssueListReq where encode = defaultEncode





------------------------------------------ deleteIssue --------------------------------------

newtype DeleteIssueReq = DeleteIssueReq String




instance makeDeleteIssueReq :: RestEndpoint DeleteIssueReq where
  makeRequest reqBody@(DeleteIssueReq issueId) headers = defaultMakeRequestWithoutLogs DELETE (EP.deleteIssue issueId) headers reqBody Nothing
  encodeRequest req = standardEncode req


derive instance genericDeleteIssueReq :: Generic DeleteIssueReq _
derive instance newtypeDeleteIssueReq :: Newtype DeleteIssueReq _
instance standardEncodeDeleteIssueReq :: StandardEncode DeleteIssueReq where standardEncode (DeleteIssueReq reqBody) = standardEncode reqBody
instance showDeleteIssueReq :: Show DeleteIssueReq where show = genericShow
instance decodeDeleteIssueReq :: Decode DeleteIssueReq where decode = defaultDecode
instance encodeDeleteIssueReq :: Encode DeleteIssueReq where encode = defaultEncode




--------------------------------------------------- rideOtp ----------------------------------------------------

newtype OTPRideReq = OTPRideReq
    {
      specialZoneOtpCode :: String,
      point :: Point
    }

data OTPRideRequest = OTPRideRequest OTPRideReq

instance makeOTPRideReq :: RestEndpoint OTPRideRequest where
    makeRequest reqBody@(OTPRideRequest (OTPRideReq rqBody)) headers = defaultMakeRequestWithoutLogs POST (EP.otpRide "") headers reqBody Nothing
    encodeRequest req = standardEncode req

derive instance genricOTPRideReq :: Generic OTPRideReq _
instance showOTPRideReq :: Show OTPRideReq where show = genericShow
instance standardEncodeOTPRideReq :: StandardEncode OTPRideReq where standardEncode (OTPRideReq req) = standardEncode req
instance decodeOTPRideReq :: Decode OTPRideReq where decode = defaultDecode
instance encodeOTPRideReq :: Encode OTPRideReq where encode = defaultEncode


derive instance genericOTPRideRequest :: Generic OTPRideRequest _
instance standardEncodeOTPRideRequest :: StandardEncode OTPRideRequest where standardEncode (OTPRideRequest req) = standardEncode req
instance showOTPRideRequest :: Show OTPRideRequest where show = genericShow
instance decodeOTPRideRequest :: Decode OTPRideRequest where decode = defaultDecode
instance encodeOTPRideRequest :: Encode OTPRideRequest where encode = defaultEncode

----------------------------------------------------------------------- onCall api -------------------------------------------------------------------

newtype OnCallReq = OnCallReq
  {
     rideId :: String,
     exophoneNumber :: String
  }


instance makeOnCallReq :: RestEndpoint OnCallReq where
 makeRequest reqBody headers = defaultMakeRequestWithoutLogs POST (EP.onCall "") headers reqBody Nothing
 encodeRequest req = standardEncode req

derive instance genericOnCallReq :: Generic OnCallReq _
derive instance newtypeOnCallReq:: Newtype OnCallReq _
instance standardEncodeOnCallReq :: StandardEncode OnCallReq where standardEncode (OnCallReq req) = standardEncode req
instance showOnCallReq :: Show OnCallReq where show = genericShow
instance decodeOnCallReq :: Decode OnCallReq where decode = defaultDecode
instance encodeOnCallReq :: Encode OnCallReq where encode = defaultEncode

------------------------------------------------------ leaderBoard -----------------------------------------------

data LeaderBoardReq = DailyRequest String
                    | WeeklyRequest String String
                    | MonthlyRequest Int

newtype LeaderBoardRes = LeaderBoardRes {
    lastUpdatedAt :: Maybe String
  , totalEligibleDrivers :: Maybe Int
  , driverList :: Array DriversInfo
}

newtype DriversInfo = DriversInfo
  { name :: String
  , totalRides :: Int
  , rank :: Int
  , isCurrentDriver :: Boolean
  , totalDistance :: Int
  , gender :: String
  }

instance makeLeaderBoardReq :: RestEndpoint LeaderBoardReq where
    makeRequest reqBody@(DailyRequest date) headers = defaultMakeRequestWithoutLogs GET (EP.leaderBoardDaily date) headers reqBody Nothing
    makeRequest reqBody@(WeeklyRequest fromDate toDate) headers = defaultMakeRequestWithoutLogs GET (EP.leaderBoardWeekly fromDate toDate) headers reqBody Nothing
    makeRequest reqBody@(MonthlyRequest month) headers = defaultMakeRequestWithoutLogs GET (EP.leaderBoardMonthly month) headers reqBody Nothing
    encodeRequest req = defaultEncode req

derive instance genericLeaderBoardReq :: Generic LeaderBoardReq _
instance showLeaderBoardReq :: Show LeaderBoardReq where show = genericShow
instance standardEncodeLeaderBoardReq :: StandardEncode LeaderBoardReq
  where
    standardEncode (DailyRequest _) = standardEncode {}
    standardEncode (WeeklyRequest _ _) = standardEncode {}
    standardEncode (MonthlyRequest _) = standardEncode {}
instance decodeLeaderBoardReq :: Decode LeaderBoardReq where decode = defaultDecode
instance encodeLeaderBoardReq :: Encode LeaderBoardReq where encode = defaultEncode

derive instance genericLeaderBoardRes :: Generic LeaderBoardRes _
derive instance newtypeLeaderBoardRes :: Newtype LeaderBoardRes _
instance showLeaderBoardRes :: Show LeaderBoardRes where show = genericShow
instance standardEncodeLeaderBoardRes :: StandardEncode LeaderBoardRes where standardEncode (LeaderBoardRes res) = standardEncode res
instance decodeLeaderBoardRes :: Decode LeaderBoardRes where decode = defaultDecode
instance encodeLeaderBoardRes :: Encode LeaderBoardRes where encode = defaultEncode

derive instance genericDriversInfo :: Generic DriversInfo _
derive instance newtypeDriversInfo :: Newtype DriversInfo _
instance showDriversInfo :: Show DriversInfo where show = genericShow
instance standardEncodeDriversInfo :: StandardEncode DriversInfo where standardEncode (DriversInfo res) = standardEncode res
instance decodeDriversInfo :: Decode DriversInfo where decode = defaultDecode
instance encodeDriversInfo :: Encode DriversInfo where encode = defaultEncode

----------------------------------------------------------------------- voipCall api -------------------------------------------------------------------

newtype VoipCallReq = VoipCallReq
  {
     callId :: String,
     callStatus :: String,
     rideId :: String,
     errorCode :: Maybe Int,
     userType :: String,
     networkType :: String,
     networkQuality :: String,
     merchantId :: String,
     merchantOperatingCity :: String
  }

instance makeVoipCallReq :: RestEndpoint VoipCallReq where
 makeRequest reqBody headers = defaultMakeRequestWithoutLogs POST (EP.voipCall "") headers reqBody Nothing
 encodeRequest req = standardEncode req

derive instance genericVoipCallReq :: Generic VoipCallReq _
derive instance newtypeVoipCallReq:: Newtype VoipCallReq _
instance standardEncodeVoipCallReq :: StandardEncode VoipCallReq where standardEncode (VoipCallReq req) = standardEncode req
instance showVoipCallReq :: Show VoipCallReq where show = genericShow
instance decodeVoipCallReq :: Decode VoipCallReq where decode = defaultDecode
instance encodeVoipCallReq :: Encode VoipCallReq where encode = defaultEncode

------------------------------------------ currentDateAndTime --------------------------------------

newtype CurrentDateAndTimeReq = CurrentDateAndTimeReq String
newtype CurrentDateAndTimeRes =  CurrentDateAndTimeRes
  {
    timestamp :: Maybe Number
  }


instance makeCurrentDateAndTimeReq :: RestEndpoint CurrentDateAndTimeReq where
  makeRequest reqBody headers = defaultMakeRequestWithoutLogs GET (EP.currentDateAndTime "") headers reqBody Nothing
  encodeRequest req = defaultEncode req

derive instance genericCurrentDateAndTimeReq :: Generic CurrentDateAndTimeReq _
derive instance newtypeCurrentDateAndTimeReq :: Newtype CurrentDateAndTimeReq _
instance standardEncodeCurrentDateAndTimeReq :: StandardEncode CurrentDateAndTimeReq where standardEncode (CurrentDateAndTimeReq reqBody) = standardEncode reqBody
instance showDCurrentDateAndTimeReq :: Show CurrentDateAndTimeReq where show = genericShow
instance decodeCurrentDateAndTimeReq :: Decode CurrentDateAndTimeReq where decode = defaultDecode
instance encodeCurrentDateAndTimeReq :: Encode CurrentDateAndTimeReq where encode = defaultEncode

derive instance genericCurrentDateAndTimeRes :: Generic CurrentDateAndTimeRes _
derive instance newtypeCurrentDateAndTimeRes :: Newtype CurrentDateAndTimeRes _
instance standardEncodeCurrentDateAndTimeRes :: StandardEncode CurrentDateAndTimeRes where standardEncode (CurrentDateAndTimeRes res) = standardEncode res
instance showCurrentDateAndTimeRes :: Show CurrentDateAndTimeRes where show = genericShow
instance decodeCurrentDateAndTimeRes :: Decode CurrentDateAndTimeRes  where decode = defaultDecode
instance encodeCurrentDateAndTimeRes :: Encode CurrentDateAndTimeRes where encode = defaultEncode

------------------------------------------------------------------------autoComplete-------------------------------------------------------------------------------

newtype AutoCompleteReq = AutoCompleteReq {
  components :: String,
  sessionToken :: Maybe String,
  location :: String,
  radius :: Int,
  input :: String,
  language :: String,
  strictbounds :: Maybe Boolean,
  origin :: LatLong
}

newtype AutoCompleteResp = AutoCompleteResp {
 predictions:: Array Prediction
}

newtype Prediction = Prediction {
 description :: String,
 placeId :: Maybe String,
 distance :: Maybe Int
}

instance makeAutoCompleteReq :: RestEndpoint AutoCompleteReq where
  makeRequest reqBody headers = defaultMakeRequestWithoutLogs POST (EP.autoComplete "") headers reqBody Nothing
  encodeRequest req = standardEncode req

derive instance genericAutoCompleteReq :: Generic AutoCompleteReq _
derive instance newtypeAutoCompleteReq :: Newtype AutoCompleteReq _
instance standardEncodeAutoCompleteReq :: StandardEncode AutoCompleteReq where standardEncode (AutoCompleteReq payload) = standardEncode payload
instance showAutoCompleteReq :: Show AutoCompleteReq where show = genericShow
instance decodeAutoCompleteReq :: Decode AutoCompleteReq where decode = defaultDecode
instance encodeAutoCompleteReq :: Encode AutoCompleteReq where encode = defaultEncode

derive instance genericAutoCompleteResp :: Generic AutoCompleteResp _
derive instance newtypeAutoCompleteResp :: Newtype AutoCompleteResp _
instance standardEncodeAutoCompleteResp :: StandardEncode AutoCompleteResp where standardEncode (AutoCompleteResp id) = standardEncode id
instance showAutoCompleteResp :: Show AutoCompleteResp where show = genericShow
instance decodeAutoCompleteResp :: Decode AutoCompleteResp where decode = defaultDecode
instance encodeAutoCompleteResp :: Encode AutoCompleteResp where encode = defaultEncode

derive instance genericPrediction :: Generic Prediction _
derive instance newtypePrediction :: Newtype Prediction _
instance standardEncodePrediction :: StandardEncode Prediction where standardEncode (Prediction id) = standardEncode id
instance showPrediction :: Show Prediction where show = genericShow
instance decodePrediction :: Decode Prediction where decode = defaultDecode
instance encodePrediction :: Encode Prediction where encode = defaultEncode

--------------------------------------------------------------getPlaceName-----------------------------------------
newtype GetPlaceNameReq = GetPlaceNameReq {
  sessionToken :: Maybe String,
  language :: Maybe String,
  getBy :: GetPlaceNameBy
}

newtype GetPlaceNameBy = GetPlaceNameBy {
  tag :: String,
  contents :: Contents
}
data Contents = PlaceId String | LatLongType LatLonBody

derive instance genericContents :: Generic Contents _
instance showContents :: Show Contents where show = genericShow
instance decodeContents :: Decode Contents where decode = defaultDecode
instance encodeContents :: Encode Contents where encode = defaultEncode
instance standardEncodeContents :: StandardEncode Contents
  where
    standardEncode (LatLongType body) = standardEncode body
    standardEncode (PlaceId param) = standardEncode param

type PlaceId = String

newtype LatLonBody = LatLonBody
  { lat :: Number
  , lon :: Number
  }

derive instance genericLatLonBody :: Generic LatLonBody _
derive instance newtypeLatLonBody :: Newtype LatLonBody _
instance standardEncodeLatLonBody :: StandardEncode LatLonBody where standardEncode (LatLonBody payload) = standardEncode payload
instance showLatLonBody :: Show LatLonBody where show = genericShow
instance decodeLatLonBody :: Decode LatLonBody where decode = defaultDecode
instance encodeLatLonBody :: Encode LatLonBody where encode = defaultEncode

newtype PlaceName = PlaceName {
 formattedAddress :: String,
 location :: LatLong,
 plusCode :: Maybe String,
 addressComponents :: Array AddressComponents
}

newtype AddressComponents =  AddressComponents {
  longName :: String ,
  shortName :: String,
  types :: Array String
}

derive instance genericAddressComponents :: Generic AddressComponents _
derive instance newtypeAddressComponents :: Newtype AddressComponents _
instance standardEncodeAddressComponents :: StandardEncode AddressComponents where standardEncode (AddressComponents payload) = standardEncode payload
instance showAddressComponents :: Show AddressComponents where show = genericShow
instance decodeAddressComponents :: Decode AddressComponents where decode = defaultDecode
instance encodeAddressComponents :: Encode AddressComponents where encode = defaultEncode

newtype GetPlaceNameResp = GetPlaceNameResp (Array PlaceName)

instance makeGetPlaceNameReq :: RestEndpoint GetPlaceNameReq where
 makeRequest reqBody@(GetPlaceNameReq payload) headers = defaultMakeRequestWithoutLogs POST (EP.getPlaceName "") headers reqBody Nothing
 encodeRequest req = standardEncode req

derive instance genericGetPlaceNameReq :: Generic GetPlaceNameReq _
derive instance newtypeGetPlaceNameReq :: Newtype GetPlaceNameReq _
instance standardEncodeGetPlaceNameReq :: StandardEncode GetPlaceNameReq where standardEncode (GetPlaceNameReq payload) = standardEncode payload
instance showGetPlaceNameReq :: Show GetPlaceNameReq where show = genericShow
instance decodeGetPlaceNameReq :: Decode GetPlaceNameReq where decode = defaultDecode
instance encodeGetPlaceNameReq :: Encode GetPlaceNameReq where encode = defaultEncode

derive instance genericGetPlaceNameBy :: Generic GetPlaceNameBy _
derive instance newtypeGetPlaceNameBy :: Newtype GetPlaceNameBy _
instance standardEncodeGetPlaceNameBy :: StandardEncode GetPlaceNameBy where standardEncode (GetPlaceNameBy body) = standardEncode body
instance showGetPlaceNameBy :: Show GetPlaceNameBy where show = genericShow
instance decodeGetPlaceNameBy :: Decode GetPlaceNameBy where decode = defaultDecode
instance encodeGetPlaceNameBy :: Encode GetPlaceNameBy where encode = defaultEncode

derive instance genericGetPlaceNameResp :: Generic GetPlaceNameResp _
derive instance newtypeGetPlaceNameResp :: Newtype GetPlaceNameResp _
instance standardEncodeGetPlaceNameResp :: StandardEncode GetPlaceNameResp where standardEncode (GetPlaceNameResp body) = standardEncode body
instance showGetPlaceNameResp :: Show GetPlaceNameResp where show = genericShow
instance decodeGetPlaceNameResp :: Decode GetPlaceNameResp where decode = defaultDecode
instance encodeGetPlaceNameResp :: Encode GetPlaceNameResp where encode = defaultEncode

derive instance genericPlaceName :: Generic PlaceName _
derive instance newtypePlaceName :: Newtype PlaceName _
instance standardEncodePlaceName :: StandardEncode PlaceName where standardEncode (PlaceName body) = standardEncode body  
instance showPlaceName :: Show PlaceName where show = genericShow
instance decodePlaceName :: Decode PlaceName where decode = defaultDecode
instance encodePlaceName :: Encode PlaceName where encode = defaultEncode

------------------------------------------ Multiple RCs --------------------------------------

data GetAllRcDataReq = GetAllRcDataReq

newtype GetAllRcDataResp = GetAllRcDataResp (Array GetAllRcDataRecords)

newtype GetAllRcDataRecords = GetAllRcDataRecords
    { rcActive  :: Boolean
    , rcDetails :: VehicleDetails
    }

newtype VehicleDetails = VehicleDetails
    { certificateNumber :: String
    , vehicleModel :: Maybe String
    , vehicleColor :: Maybe String
    , vehicleVariant :: Maybe String
    }

instance makeGetAllRcDataReq :: RestEndpoint GetAllRcDataReq where
    makeRequest reqBody headers = defaultMakeRequestWithoutLogs GET (EP.getAllRcData "") headers reqBody Nothing
    encodeRequest req = defaultEncode req

derive instance genericGetAllRcDataReq :: Generic GetAllRcDataReq _
instance decodeGetAllRcDataReq :: Decode GetAllRcDataReq where decode = defaultDecode
instance standardEncodeGetAllRcDataReq :: StandardEncode GetAllRcDataReq where standardEncode (GetAllRcDataReq) = standardEncode {}
instance encodeGetAllRcDataReq :: Encode GetAllRcDataReq where encode = defaultEncode

derive instance genericGetAllRcDataResp :: Generic GetAllRcDataResp _
derive instance newtypeGetAllRcDataResp :: Newtype GetAllRcDataResp _
instance showGetAllRcDataResp :: Show GetAllRcDataResp where show = genericShow
instance standardEncodeGetAllRcDataResp :: StandardEncode GetAllRcDataResp where standardEncode (GetAllRcDataResp req) = standardEncode req
instance decodeGetAllRcDataResp :: Decode GetAllRcDataResp where decode = defaultDecode
instance encodeGetAllRcDataResp :: Encode GetAllRcDataResp where encode = defaultEncode

derive instance genericGetAllRcDataRes1 :: Generic GetAllRcDataRecords _
derive instance newtypeGetAllRcDataRes1 :: Newtype GetAllRcDataRecords _
instance showGetAllRcDataRes1 :: Show GetAllRcDataRecords where show = genericShow
instance standardEncodeGetAllRcDataRes1 :: StandardEncode GetAllRcDataRecords where standardEncode (GetAllRcDataRecords req) = standardEncode req
instance decodeGetAllRcDataRes1 :: Decode GetAllRcDataRecords where decode = defaultDecode
instance encodeGetAllRcDataRes1 :: Encode GetAllRcDataRecords where encode = defaultEncode

derive instance genericVehicleDetails :: Generic VehicleDetails _
derive instance newtypeVehicleDetails :: Newtype VehicleDetails _
instance standardEncodeVehicleDetails :: StandardEncode VehicleDetails where standardEncode (VehicleDetails req) = standardEncode req
instance showVehicleDetails :: Show VehicleDetails where show = genericShow
instance decodeVehicleDetails :: Decode VehicleDetails where decode = defaultDecode
instance encodeVehicleDetails :: Encode VehicleDetails where encode = defaultEncode


newtype MakeRcActiveOrInactiveReq = MakeRcActiveOrInactiveReq {
  isActivate :: Boolean,
  rcNo :: String
}


instance makeRcActiveOrInactiveReq :: RestEndpoint MakeRcActiveOrInactiveReq where
  makeRequest reqBody headers = defaultMakeRequestWithoutLogs POST (EP.makeRcActiveOrInactive "") headers reqBody Nothing
  encodeRequest req = standardEncode req

derive instance genericMakeRcActiveOrInactiveReq :: Generic MakeRcActiveOrInactiveReq _
derive instance newtypeMakeRcActiveOrInactiveReq :: Newtype MakeRcActiveOrInactiveReq _
instance standardEncodeMakeRcActiveOrInactiveReq :: StandardEncode MakeRcActiveOrInactiveReq where standardEncode (MakeRcActiveOrInactiveReq body) = standardEncode body
instance showMakeRcActiveOrInactiveReq :: Show MakeRcActiveOrInactiveReq where show = genericShow
instance decodeMakeRcActiveOrInactiveReq:: Decode MakeRcActiveOrInactiveReq where decode = defaultDecode
instance encodeMakeRcActiveOrInactiveReq  ::Encode MakeRcActiveOrInactiveReq where encode = defaultEncode


newtype DeleteRcReq = DeleteRcReq {
  rcNo :: String
}


instance deleteRcReq :: RestEndpoint DeleteRcReq where
  makeRequest reqBody headers = defaultMakeRequestWithoutLogs POST (EP.deleteRc "") headers reqBody Nothing
  encodeRequest req = standardEncode req

derive instance genericDeleteRcReq :: Generic DeleteRcReq _
derive instance newtypeDeleteRcReq :: Newtype DeleteRcReq _
instance standardEncodeDeleteRcReq :: StandardEncode DeleteRcReq where standardEncode (DeleteRcReq body) = standardEncode body
instance showDeleteRcReq :: Show DeleteRcReq where show = genericShow
instance decodeDeleteRcReq:: Decode DeleteRcReq where decode = defaultDecode
instance encodeDeleteRcReq  ::Encode DeleteRcReq where encode = defaultEncode



data CallDriverToDriverReq = CallDriverToDriverReq String

newtype CallDriverToDriverResp = CallDriverToDriverResp
    {
     callId :: String
    }

instance makeGetCallDriverToDriverReq :: RestEndpoint CallDriverToDriverReq where
    makeRequest reqBody@(CallDriverToDriverReq rcNo) headers = defaultMakeRequestWithoutLogs GET (EP.callDriverToDriver rcNo) headers reqBody Nothing
    encodeRequest req = defaultEncode req

derive instance genericCallDriverToDriverReq :: Generic CallDriverToDriverReq _
instance showCallDriverToDriverReq :: Show CallDriverToDriverReq where show = genericShow
instance standardEncodeCallDriverToDriverReq :: StandardEncode CallDriverToDriverReq where standardEncode (CallDriverToDriverReq req) = standardEncode req
instance decodeCallDriverToDriverReq :: Decode CallDriverToDriverReq where decode = defaultDecode
instance encodeCallDriverToDriverReq :: Encode CallDriverToDriverReq where encode = defaultEncode

derive instance genericgenericCallDriverToDriverResp :: Generic CallDriverToDriverResp _
instance showGetCallDriverToDriverResp :: Show CallDriverToDriverResp where show = genericShow
instance standardEncodeCallDriverToDriverResp :: StandardEncode CallDriverToDriverResp where standardEncode (CallDriverToDriverResp req) = standardEncode req
instance decodeCallDriverToDriverResp :: Decode CallDriverToDriverResp where decode = defaultDecode
instance encodeCallDriverToDriverResp :: Encode CallDriverToDriverResp where encode = defaultEncode


------------------------------------------ driverProfileSummary --------------------------------------
data DriverProfileSummaryReq = DriverProfileSummaryReq

newtype DriverProfileSummaryRes
  = DriverProfileSummaryRes
  { id :: String
  , firstName :: String
  , middleName :: Maybe String
  , lastName :: Maybe String
  , totalRidesAssigned :: Int
  , mobileNumber :: Maybe String
  , linkedVehicle :: Maybe Vehicle
  , totalDistanceTravelled :: Int
  , rating :: Maybe Number
  , totalUsersRated :: Int
  , language :: Maybe String
  , alternateNumber :: Maybe String
  , gender :: Maybe String
  , driverSummary :: DriverSummary
  , missedOpp :: DriverMissedOpp
  , feedbackBadges :: DriverBadges
  , languagesSpoken :: Maybe (Array String)
  , hometown :: Maybe String
  , cancellationRateInWindow :: Maybe Int
  , cancelledRidesCountInWindow :: Maybe Int
  , assignedRidesCountInWindow :: Maybe Int
  , windowSize :: Maybe Int
  }

newtype DriverSummary
  = DriverSummary
  { totalEarnings :: Int
  , bonusEarned :: Int
  , totalCompletedTrips :: Int
  , lateNightTrips :: Int
  , lastRegistered :: String
  }

newtype DriverMissedOpp
  = DriverMissedOpp
  { cancellationRate :: Int
  , ridesCancelled :: Int
  , totalRides :: Int
  , missedEarnings :: Int
  }

newtype DriverBadges
  = DriverBadges
  { driverBadges :: Array Badges
  }

newtype Badges
  = Badges
  { badgeName :: String
  , badgeCount :: Int
  }

instance makeDriverProfileSummaryReq :: RestEndpoint DriverProfileSummaryReq where
  makeRequest reqBody headers = defaultMakeRequestWithoutLogs GET (EP.profileSummary "") headers reqBody Nothing
  encodeRequest req = defaultEncode req

derive instance genericDriverProfileSummaryReq :: Generic DriverProfileSummaryReq _
instance standardEncodeDriverProfileSummaryReq :: StandardEncode DriverProfileSummaryReq where standardEncode reqBody = standardEncode {}
instance showDDriverProfileSummaryReq :: Show DriverProfileSummaryReq where show = genericShow
instance decodeDriverProfileSummaryReq :: Decode DriverProfileSummaryReq where decode = defaultDecode
instance encodeDriverProfileSummaryReq :: Encode DriverProfileSummaryReq where encode = defaultEncode

derive instance genericDriverProfileSummaryRes :: Generic DriverProfileSummaryRes _
derive instance newtypeDriverProfileSummaryRes :: Newtype DriverProfileSummaryRes _
instance standardEncodeDriverProfileSummaryRes :: StandardEncode DriverProfileSummaryRes where standardEncode (DriverProfileSummaryRes reqBody) = standardEncode reqBody
instance showDDriverProfileSummaryRes :: Show DriverProfileSummaryRes where show = genericShow
instance decodeDriverProfileSummaryRes :: Decode DriverProfileSummaryRes where decode = defaultDecode
instance encodeDriverProfileSummaryRes :: Encode DriverProfileSummaryRes where encode = defaultEncode

derive instance genericDriverSummary :: Generic DriverSummary _
derive instance newtypeDriverSummary :: Newtype DriverSummary _
instance standardEncodeDriverSummary :: StandardEncode DriverSummary where standardEncode (DriverSummary reqBody) = standardEncode reqBody
instance showDDriverSummary :: Show DriverSummary where show = genericShow
instance decodeDriverSummary :: Decode DriverSummary where decode = defaultDecode
instance encodeDriverSummary :: Encode DriverSummary where encode = defaultEncode

derive instance genericDriverMissedOpp :: Generic DriverMissedOpp _
derive instance newtypeDriverMissedOpp :: Newtype DriverMissedOpp _
instance standardEncodeDriverMissedOpp :: StandardEncode DriverMissedOpp where standardEncode (DriverMissedOpp reqBody) = standardEncode reqBody
instance showDDriverMissedOpp :: Show DriverMissedOpp where show = genericShow
instance decodeDriverMissedOpp :: Decode DriverMissedOpp where decode = defaultDecode
instance encodeDriverMissedOpp :: Encode DriverMissedOpp where encode = defaultEncode

derive instance genericDriverBadges :: Generic DriverBadges _
derive instance newtypeDriverBadges :: Newtype DriverBadges _
instance standardEncodeDriverBadges :: StandardEncode DriverBadges where standardEncode (DriverBadges reqBody) = standardEncode reqBody
instance showDDriverBadges :: Show DriverBadges where show = genericShow
instance decodeDriverBadges :: Decode DriverBadges where decode = defaultDecode
instance encodeDriverBadges :: Encode DriverBadges where encode = defaultEncode

derive instance genericBadges :: Generic Badges _
derive instance newtypeBadges :: Newtype Badges _
instance standardEncodeBadges :: StandardEncode Badges where standardEncode (Badges reqBody) = standardEncode reqBody
instance showDBadges :: Show Badges where show = genericShow
instance decodeBadges :: Decode Badges where decode = defaultDecode
instance encodeBadges :: Encode Badges where encode = defaultEncode

-- order status api
data CreateOrderReq = CreateOrderReq String

newtype CreateOrderRes = CreateOrderRes
  {
    sdk_payload :: PaymentPagePayload,
    id :: String,
    order_id :: String,
    payment_links :: PaymentLinks,
    sdk_payload_json :: Maybe Foreign
  }


newtype PaymentLinks = PaymentLinks
  {
    web :: Maybe String,
    iframe :: Maybe String,
    mobile :: Maybe String
  }

instance makeCreateOrderReq :: RestEndpoint CreateOrderReq where
 makeRequest reqBody@(CreateOrderReq estimateId) headers = defaultMakeRequestWithoutLogs POST (EP.createOrder estimateId) headers reqBody Nothing
 encodeRequest req = standardEncode req

derive instance genericCreateOrderReq :: Generic CreateOrderReq _
instance standardEncodeCreateOrderReq :: StandardEncode CreateOrderReq where standardEncode (CreateOrderReq dummy) = standardEncode dummy
instance showCreateOrderReq :: Show CreateOrderReq where show = genericShow
instance decodeCreateOrderReq :: Decode CreateOrderReq where decode = defaultDecode
instance encodeCreateOrderReq :: Encode CreateOrderReq where encode = defaultEncode

derive instance genericPaymentLinks :: Generic PaymentLinks _
derive instance newtypePaymentLinks :: Newtype PaymentLinks _
instance standardEncodePaymentLinks :: StandardEncode PaymentLinks where standardEncode (PaymentLinks id) = standardEncode id
instance showPaymentLinks :: Show PaymentLinks where show = genericShow
instance decodePaymentLinks :: Decode PaymentLinks where decode = defaultDecode
instance encodePaymentLinks :: Encode PaymentLinks where encode = defaultEncode

derive instance genericCreateOrderRes :: Generic CreateOrderRes _
derive instance newtypeCreateOrderRes :: Newtype CreateOrderRes _
instance standardEncodeCreateOrderRes :: StandardEncode CreateOrderRes where standardEncode (CreateOrderRes res) = standardEncode res
instance showCreateOrderRes :: Show CreateOrderRes where 
  show (CreateOrderRes { id, order_id, payment_links, sdk_payload }) =
    show {id, order_id, payment_links, sdk_payload}
instance decodeCreateOrderRes :: Decode CreateOrderRes where decode = defaultDecode
instance encodeCreateOrderRes :: Encode CreateOrderRes where encode = defaultEncode

-- order status

data OrderStatusReq = OrderStatusReq String

newtype OrderStatusRes = OrderStatusRes
  {
    status :: PP.APIPaymentStatus
  }

instance makeOrderStatusReq :: RestEndpoint OrderStatusReq where
 makeRequest reqBody@(OrderStatusReq orderId) headers = defaultMakeRequestWithoutLogs GET (EP.orderStatus orderId) headers reqBody Nothing
 encodeRequest req = standardEncode req

derive instance genericOrderStatusReq :: Generic OrderStatusReq _
instance standardEncodeOrderStatusReq :: StandardEncode OrderStatusReq where standardEncode (OrderStatusReq dummy) = standardEncode dummy
instance showOrderStatusReq :: Show OrderStatusReq where show = genericShow
instance decodeOrderStatusReq :: Decode OrderStatusReq where decode = defaultDecode
instance encodeOrderStatusReq :: Encode OrderStatusReq where encode = defaultEncode

derive instance genericOrderStatusRes :: Generic OrderStatusRes _
derive instance newtypeOrderStatusRes :: Newtype OrderStatusRes _
instance standardEncodeOrderStatusRes :: StandardEncode OrderStatusRes where standardEncode (OrderStatusRes res) = standardEncode res
instance showOrderStatusRes :: Show OrderStatusRes where show = genericShow
instance decodeOrderStatusRes :: Decode OrderStatusRes where decode = defaultDecode
instance encodeOrderStatusRes :: Encode OrderStatusRes where encode = defaultEncode

-- payment history

data GetPaymentHistoryReq = GetPaymentHistoryReq String String (Maybe String)

newtype GetPaymentHistoryResp = GetPaymentHistoryResp (Array PaymentDetailsEntity)

newtype PaymentDetailsEntity = PaymentDetailsEntity {
    date :: String
  , totalRides :: Int
  , totalEarnings :: Int
  , charges :: Int
  , chargesBreakup :: Array PaymentBreakUp
  , txnInfo :: Array TxnInfo
  , invoiceId :: String
  , status :: DriverFeeStatus
}

newtype PaymentBreakUp = PaymentBreakUp {
    component :: String
  , amount :: Number
}

newtype TxnInfo = TxnInfo {
    id :: String
  , status :: PP.APIPaymentStatus
}

data DriverFeeStatus = ONGOING | PAYMENT_PENDING | PAYMENT_OVERDUE | CLEARED | EXEMPTED | COLLECTED_CASH | INACTIVE_DRIVERFEE

instance makeGetPaymentHistoryReq :: RestEndpoint GetPaymentHistoryReq where
 makeRequest reqBody@(GetPaymentHistoryReq from to status) headers = defaultMakeRequestWithoutLogs GET (EP.paymentHistory from to status) headers reqBody Nothing
 encodeRequest req = standardEncode req

derive instance genericGetPaymentHistoryReq :: Generic GetPaymentHistoryReq _
instance standardEncodeGetPaymentHistoryReq :: StandardEncode GetPaymentHistoryReq where standardEncode res = standardEncode {}
instance showGetPaymentHistoryReq :: Show GetPaymentHistoryReq where show = genericShow
instance decodeGetPaymentHistoryReq :: Decode GetPaymentHistoryReq where decode = defaultDecode
instance encodeGetPaymentHistoryReq :: Encode GetPaymentHistoryReq where encode = defaultEncode

derive instance genericGetPaymentHistoryResp :: Generic GetPaymentHistoryResp _
derive instance newtypeGetPaymentHistoryResp:: Newtype GetPaymentHistoryResp _
instance standardEncodeGetPaymentHistoryResp :: StandardEncode GetPaymentHistoryResp where standardEncode (GetPaymentHistoryResp res) = standardEncode res
instance showGetPaymentHistoryResp :: Show GetPaymentHistoryResp where show = genericShow
instance decodeGetPaymentHistoryResp :: Decode GetPaymentHistoryResp where decode = defaultDecode
instance encodeGetPaymentHistoryResp :: Encode GetPaymentHistoryResp where encode = defaultEncode

derive instance genericDriverFeeStatus :: Generic DriverFeeStatus _
instance showDriverFeeStatus :: Show DriverFeeStatus where show = genericShow
instance decodeDriverFeeStatus :: Decode DriverFeeStatus 
  where 
    decode status = 
      case (runExcept $ decode status) of
        Either.Right val -> case val of
                      "INACTIVE" -> defaultEnumDecode $ encode "INACTIVE_DRIVERFEE"
                      _ -> defaultEnumDecode status
        Either.Left err -> (fail $ ForeignError "Unknown response")
instance encodeDriverFeeStatus :: Encode DriverFeeStatus where encode = defaultEnumEncode
instance eqDriverFeeStatus :: Eq DriverFeeStatus where eq = genericEq
instance standardEncodeDriverFeeStatus :: StandardEncode DriverFeeStatus where standardEncode _ = standardEncode {}


derive instance genericPaymentDetailsEntity :: Generic PaymentDetailsEntity _
derive instance newtypePaymentDetailsEntity:: Newtype PaymentDetailsEntity _
instance standardEncodePaymentDetailsEntity :: StandardEncode PaymentDetailsEntity where standardEncode (PaymentDetailsEntity res) = standardEncode res
instance showPaymentDetailsEntity :: Show PaymentDetailsEntity where show = genericShow
instance decodePaymentDetailsEntity :: Decode PaymentDetailsEntity where decode = defaultDecode
instance encodePaymentDetailsEntity :: Encode PaymentDetailsEntity where encode = defaultEncode

derive instance genericPaymentBreakUp :: Generic PaymentBreakUp _
derive instance newtypePaymentBreakUp:: Newtype PaymentBreakUp _
instance standardEncodePaymentBreakUp :: StandardEncode PaymentBreakUp where standardEncode (PaymentBreakUp res) = standardEncode res
instance showPaymentBreakUp :: Show PaymentBreakUp where show = genericShow
instance decodePaymentBreakUp :: Decode PaymentBreakUp where decode = defaultDecode
instance encodePaymentBreakUp :: Encode PaymentBreakUp where encode = defaultEncode

derive instance genericTxnInfo :: Generic TxnInfo _
derive instance newtypeTxnInfo:: Newtype TxnInfo _
instance standardEncodeTxnInfo :: StandardEncode TxnInfo where standardEncode (TxnInfo res) = standardEncode res
instance showTxnInfo :: Show TxnInfo where show = genericShow
instance decodeTxnInfo :: Decode TxnInfo where decode = defaultDecode
instance encodeTxnInfo :: Encode TxnInfo where encode = defaultEncode
newtype GenerateAadhaarOTPReq = GenerateAadhaarOTPReq {
  aadhaarNumber :: String,
  consent :: String
}

newtype GenerateAadhaarOTPResp = GenerateAadhaarOTPResp {
  message :: String,
  requestId :: String,
  transactionId :: String,
  statusCode :: String
}

instance makeGenerateAadhaarOTPReq :: RestEndpoint GenerateAadhaarOTPReq where
    makeRequest reqBody headers = defaultMakeRequestWithoutLogs POST (EP.triggerAadhaarOTP "") headers reqBody Nothing
    encodeRequest req = standardEncode req

derive instance genericGenerateAadhaarOTPReq :: Generic GenerateAadhaarOTPReq _
derive instance newtypeGenerateAadhaarOTPReq :: Newtype GenerateAadhaarOTPReq _
instance standardEncodeGenerateAadhaarOTPReq :: StandardEncode GenerateAadhaarOTPReq where standardEncode (GenerateAadhaarOTPReq body) = standardEncode body
instance showGenerateAadhaarOTPReq :: Show GenerateAadhaarOTPReq where show = genericShow
instance decodeGenerateAadhaarOTPReq :: Decode GenerateAadhaarOTPReq  where decode = defaultDecode
instance encodeGenerateAadhaarOTPReq :: Encode GenerateAadhaarOTPReq where encode = defaultEncode

derive instance genericGenerateAadhaarOTPResp :: Generic GenerateAadhaarOTPResp _
derive instance newtypeGenerateAadhaarOTPResp :: Newtype GenerateAadhaarOTPResp _
instance standardEncodeGenerateAadhaarOTPResp :: StandardEncode GenerateAadhaarOTPResp where standardEncode (GenerateAadhaarOTPResp body) = standardEncode body
instance showGenerateAadhaarOTPResp :: Show GenerateAadhaarOTPResp where show = genericShow
instance decodeGenerateAadhaarOTPResp :: Decode GenerateAadhaarOTPResp  where decode = defaultDecode
instance encodeGenerateAadhaarOTPResp :: Encode GenerateAadhaarOTPResp where encode = defaultEncode

newtype VerifyAadhaarOTPReq = VerifyAadhaarOTPReq {
  otp :: Int,
  shareCode :: String
}

newtype VerifyAadhaarOTPResp = VerifyAadhaarOTPResp {
    message :: String
  , date_of_birth :: String
  , name :: String
  , share_code :: String
  , gender :: String
  , request_id :: String
  , transactionId :: String
  , image :: String
  , code :: Int
}

instance makeVerifyAadhaarOTPReq :: RestEndpoint VerifyAadhaarOTPReq where
    makeRequest reqBody headers = defaultMakeRequestWithoutLogs POST (EP.verifyAadhaarOTP "") headers reqBody Nothing
    encodeRequest req = standardEncode req

derive instance genericVerifyAadhaarOTPReq :: Generic VerifyAadhaarOTPReq _
derive instance newtypeVerifyAadhaarOTPReq :: Newtype VerifyAadhaarOTPReq _
instance standardEncodeVerifyAadhaarOTPReq :: StandardEncode VerifyAadhaarOTPReq where standardEncode (VerifyAadhaarOTPReq body) = standardEncode body
instance showVerifyAadhaarOTPReq :: Show VerifyAadhaarOTPReq where show = genericShow
instance decodeVerifyAadhaarOTPReq :: Decode VerifyAadhaarOTPReq  where decode = defaultDecode
instance encodeVerifyAadhaarOTPReq :: Encode VerifyAadhaarOTPReq where encode = defaultEncode

derive instance genericVerifyAadhaarOTPResp :: Generic VerifyAadhaarOTPResp _
derive instance newtypeVerifyAadhaarOTPResp :: Newtype VerifyAadhaarOTPResp _
instance standardEncodeVerifyAadhaarOTPResp :: StandardEncode VerifyAadhaarOTPResp where standardEncode (VerifyAadhaarOTPResp body) = standardEncode body
instance showVerifyAadhaarOTPResp :: Show VerifyAadhaarOTPResp where show = genericShow
instance decodeVerifyAadhaarOTPResp :: Decode VerifyAadhaarOTPResp  where decode = defaultDecode
instance encodeVerifyAadhaarOTPResp :: Encode VerifyAadhaarOTPResp where encode = defaultEncode

-----------------------------UnVerifiedData-----------------

newtype UnVerifiedDataReq = UnVerifiedDataReq {
  driverName :: String,
  driverGender :: String,
  driverDob ::  String
}


instance makeUnVerifiedDataReq :: RestEndpoint UnVerifiedDataReq where
    makeRequest reqBody headers = defaultMakeRequestWithoutLogs POST (EP.unVerifiedAadhaarData "") headers reqBody Nothing
    encodeRequest req = standardEncode req

derive instance genericUnVerifiedDataReq :: Generic UnVerifiedDataReq _
derive instance newtypeUnVerifiedDataReq :: Newtype UnVerifiedDataReq _
instance standardEncodeUnVerifiedDataReq :: StandardEncode UnVerifiedDataReq where standardEncode (UnVerifiedDataReq body) = standardEncode body
instance showUnVerifiedDataReq :: Show UnVerifiedDataReq where show = genericShow
instance decodeUnVerifiedDataReq :: Decode UnVerifiedDataReq  where decode = defaultDecode
instance encodeUnVerifiedDataReq :: Encode UnVerifiedDataReq where encode = defaultEncode


-------------------------------------------------- getUiPlans -------------------------------------

data UiPlansReq = UiPlansReq String

newtype UiPlansResp = UiPlansResp {
  list :: Array PlanEntity,
  subscriptionStartTime :: String,
  isLocalized :: Maybe Boolean
}

newtype PlanEntity = PlanEntity {
  id :: String,
  name :: String,
  description :: String,
  freeRideCount :: Int,
  frequency :: String, -- DAILY | WEEKLY | MONTHLY | PER_RIDE
  offers :: Array OfferEntity,
  planFareBreakup :: Array PaymentBreakUp,
  totalPlanCreditLimit :: Number,
  currentDues :: Number,
  autopayDues :: Number,
  dues :: Array DriverDuesEntity,
  bankErrors :: Array BankError,
  dueBoothCharges :: Maybe Number,
  coinEntity :: Maybe CoinEntity
}


newtype CoinEntity = CoinEntity { 
  coinDiscountUpto :: Number
  }

newtype DriverDuesEntity = DriverDuesEntity {
    autoPayStage :: Maybe AutopayPaymentStage, -- AutopayPaymentStage NOTIFICATION_SCHEDULED | NOTIFICATION_ATTEMPTING | EXECUTION_SCHEDULED | EXECUTION_ATTEMPTING | EXECUTION_SUCCESS
    paymentStatus :: Maybe InvoiceStatus, --InvoiceStatus ACTIVE_INVOICE (Pending) | SUCCESS | FAILED | EXPIRED | INACTIVE
    totalEarnings :: Number,
    totalRides :: Int,
    planAmount :: Number,
    isSplit :: Boolean,
    offerAndPlanDetails :: Maybe String,
    createdAt :: Maybe String,
    feeType :: FeeType,
    executionAt :: Maybe String,
    rideTakenOn :: String,
    driverFeeAmount :: Number,
    specialZoneRideCount :: Maybe Int,
    totalSpecialZoneCharges :: Maybe Number,
    maxRidesEligibleForCharge :: Maybe Int,
    isCoinCleared :: Boolean,
    coinDiscountAmount :: Maybe Number
}

newtype OfferEntity = OfferEntity {
  offerId :: String,
  title :: Maybe String,
  description :: Maybe String,
  tnc :: Maybe String,
  gradient :: Maybe (Array String)
}

newtype PromotionPopupConfig = PromotionPopupConfig {
  title :: String,
  description :: String,
  imageUrl :: String,
  buttonText :: String,
  heading :: String
}

derive instance genericPromotionPopupConfig :: Generic PromotionPopupConfig _
derive instance newtypePromotionPopupConfig :: Newtype PromotionPopupConfig _
instance standardEncodePromotionPopupConfig :: StandardEncode PromotionPopupConfig where standardEncode (PromotionPopupConfig res) = standardEncode res
instance showPromotionPopupConfig :: Show PromotionPopupConfig where show = genericShow
instance decodePromotionPopupConfig :: Decode PromotionPopupConfig where decode = defaultDecode
instance encodePromotionPopupConfig :: Encode PromotionPopupConfig where encode = defaultEncode 

instance makeUiPlansReq :: RestEndpoint UiPlansReq where
 makeRequest reqBody@(UiPlansReq vehicleVariant) headers = defaultMakeRequestWithoutLogs GET (EP.getUiPlans vehicleVariant) headers reqBody Nothing
 encodeRequest req = standardEncode req

derive instance genericUiPlansReq :: Generic UiPlansReq _
instance standardEncodeUiPlansReq :: StandardEncode UiPlansReq where standardEncode (UiPlansReq dummy ) = standardEncode dummy
instance showUiPlansReq :: Show UiPlansReq where show = genericShow
instance decodeUiPlansReq :: Decode UiPlansReq where decode = defaultDecode
instance encodeUiPlansReq :: Encode UiPlansReq where encode = defaultEncode

derive instance genericDriverDuesEntity :: Generic DriverDuesEntity _
derive instance newtypeDriverDuesEntity :: Newtype DriverDuesEntity _
instance standardEncodeDriverDuesEntity :: StandardEncode DriverDuesEntity where standardEncode (DriverDuesEntity body) = standardEncode body
instance showDriverDuesEntity :: Show DriverDuesEntity where show = genericShow
instance decodeDriverDuesEntity :: Decode DriverDuesEntity  where decode = defaultDecode
instance encodeDriverDuesEntity :: Encode DriverDuesEntity where encode = defaultEncode


derive instance genericUiPlansResp :: Generic UiPlansResp _
derive instance newtypeUiPlansResp :: Newtype UiPlansResp _
instance standardEncodeUiPlansResp :: StandardEncode UiPlansResp where standardEncode (UiPlansResp res) = standardEncode res
instance showUiPlansResp :: Show UiPlansResp where show = genericShow
instance decodeUiPlansResp :: Decode UiPlansResp where decode = defaultDecode
instance encodeUiPlansResp :: Encode UiPlansResp where encode = defaultEncode 

derive instance genericPlanEntity :: Generic PlanEntity _
derive instance newtypePlanEntity :: Newtype PlanEntity _
instance standardEncodePlanEntity :: StandardEncode PlanEntity where standardEncode (PlanEntity res) = standardEncode res
instance showPlanEntity :: Show PlanEntity where show = genericShow
instance decodePlanEntity :: Decode PlanEntity where decode = defaultDecode
instance encodePlanEntity :: Encode PlanEntity where encode = defaultEncode

derive instance genericOfferEntity :: Generic OfferEntity _
derive instance newtypeOfferEntity :: Newtype OfferEntity _
instance standardEncodeOfferEntity :: StandardEncode OfferEntity where standardEncode (OfferEntity res) = standardEncode res
instance showOfferEntity :: Show OfferEntity where show = genericShow
instance decodeOfferEntity :: Decode OfferEntity where decode = defaultDecode
instance encodeOfferEntity :: Encode OfferEntity where encode = defaultEncode 

derive instance genericCoinEntity :: Generic CoinEntity _
derive instance newtypeCoinEntity :: Newtype CoinEntity _
instance standardEncodeCoinEntity :: StandardEncode CoinEntity where standardEncode (CoinEntity res) = standardEncode res
instance showCoinEntity :: Show CoinEntity where show = genericShow
instance decodeCoinEntity :: Decode CoinEntity where decode = defaultDecode
instance encodeCoinEntity :: Encode CoinEntity where encode = defaultEncode 


-------------------------------------------------- SubscribePlan ------------------------------

newtype SubscribePlanReq = SubscribePlanReq String

newtype SubscribePlanResp = SubscribePlanResp {
  orderResp :: CreateOrderRes,
  orderId :: String
}

instance makeSubscribePlanReq :: RestEndpoint SubscribePlanReq where
    makeRequest reqBody@(SubscribePlanReq planId) headers = defaultMakeRequestWithoutLogs POST (EP.subscribePlan planId) headers reqBody Nothing
    encodeRequest req = standardEncode req

derive instance genericSubscribePlanReq :: Generic SubscribePlanReq _
derive instance newtypeSubscribePlanReq :: Newtype SubscribePlanReq _
instance standardEncodeSubscribePlanReq :: StandardEncode SubscribePlanReq where standardEncode (SubscribePlanReq body) = standardEncode body
instance showSubscribePlanReq :: Show SubscribePlanReq where show = genericShow
instance decodeSubscribePlanReq :: Decode SubscribePlanReq  where decode = defaultDecode
instance encodeSubscribePlanReq :: Encode SubscribePlanReq where encode = defaultEncode

derive instance genericSubscribePlanResp :: Generic SubscribePlanResp _
derive instance newtypeSubscribePlanResp :: Newtype SubscribePlanResp _
instance standardEncodeSubscribePlanResp :: StandardEncode SubscribePlanResp where standardEncode (SubscribePlanResp res) = standardEncode res
instance showSubscribePlanResp :: Show SubscribePlanResp where show = genericShow
instance decodeSubscribePlanResp :: Decode SubscribePlanResp where decode = defaultDecode
instance encodeSubscribePlanResp :: Encode SubscribePlanResp where encode = defaultEncode


--------------------------------------- PaymentDues -----------------------

data PaymentDuesReq = PaymentDuesReq String

newtype PaymentDuesResp = PaymentDuesResp {
  maxAmount :: Int,
  currentDue :: Int,
  isLimitReached :: Boolean,
  dues :: DuesEntity
}

newtype DuesEntity = DuesEntity {
  id :: String,
  date :: String,
  amount :: Int
}


instance makePaymentDuesReq :: RestEndpoint PaymentDuesReq where
 makeRequest reqBody@(PaymentDuesReq dummy) headers = defaultMakeRequestWithoutLogs GET (EP.paymentDues dummy) headers reqBody Nothing
 encodeRequest req = standardEncode req

derive instance genericPaymentDuesReq :: Generic PaymentDuesReq _
instance standardEncodePaymentDuesReq :: StandardEncode PaymentDuesReq where standardEncode (PaymentDuesReq dummy ) = standardEncode dummy
instance showPaymentDuesReq :: Show PaymentDuesReq where show = genericShow
instance decodePaymentDuesReq :: Decode PaymentDuesReq where decode = defaultDecode
instance encodePaymentDuesReq :: Encode PaymentDuesReq where encode = defaultEncode

derive instance genericPaymentDuesResp :: Generic PaymentDuesResp _
derive instance newtypePaymentDuesResp:: Newtype PaymentDuesResp _
instance standardEncodePaymentDuesResp :: StandardEncode PaymentDuesResp where standardEncode (PaymentDuesResp res) = standardEncode res
instance showPaymentDuesResp :: Show PaymentDuesResp where show = genericShow
instance decodePaymentDuesResp :: Decode PaymentDuesResp where decode = defaultDecode
instance encodePaymentDuesResp :: Encode PaymentDuesResp where encode = defaultEncode

derive instance genericDuesEntity :: Generic DuesEntity _
derive instance newtypeDuesEntity:: Newtype DuesEntity _
instance standardEncodeDuesEntity :: StandardEncode DuesEntity where standardEncode (DuesEntity res) = standardEncode res
instance showDuesEntity :: Show DuesEntity where show = genericShow
instance decodeDuesEntity :: Decode DuesEntity where decode = defaultDecode
instance encodeDuesEntity :: Encode DuesEntity where encode = defaultEncode

--------------------------------------------- ResumeMandate --------------------------------------------------

data ResumeMandateReq = ResumeMandateReq String


instance makeResumeMandateReq :: RestEndpoint ResumeMandateReq where
 makeRequest reqBody@(ResumeMandateReq driverId) headers = defaultMakeRequestWithoutLogs PUT (EP.resumeMandate driverId) headers reqBody Nothing
 encodeRequest req = standardEncode req

derive instance genericResumeMandateReq :: Generic ResumeMandateReq _
instance standardEncodeResumeMandateReq :: StandardEncode ResumeMandateReq where standardEncode (ResumeMandateReq dummy) = standardEncode dummy
instance showResumeMandateReq :: Show ResumeMandateReq where show = genericShow
instance decodeResumeMandateReq :: Decode ResumeMandateReq where decode = defaultDecode
instance encodeResumeMandateReq :: Encode ResumeMandateReq where encode = defaultEncode



--------------------------------------------- SuspendMandate --------------------------------------------------

data SuspendMandateReq = SuspendMandateReq String


instance makeSuspendMandateReq :: RestEndpoint SuspendMandateReq where
 makeRequest reqBody@(SuspendMandateReq id) headers = defaultMakeRequestWithoutLogs PUT (EP.suspendMandate id) headers reqBody Nothing
 encodeRequest req = standardEncode req

derive instance genericSuspendMandateReq :: Generic SuspendMandateReq _
instance standardEncodeSuspendMandateReq :: StandardEncode SuspendMandateReq where standardEncode (SuspendMandateReq dummy) = standardEncode dummy
instance showSuspendMandateReq :: Show SuspendMandateReq where show = genericShow
instance decodeSuspendMandateReq :: Decode SuspendMandateReq where decode = defaultDecode
instance encodeSuspendMandateReq :: Encode SuspendMandateReq where encode = defaultEncode


------------------------------------------ SelectPlan ------------------------------------------------------

newtype SelectPlanReq = SelectPlanReq String


instance makeSelectPlanReq :: RestEndpoint SelectPlanReq where
    makeRequest reqBody@(SelectPlanReq id) headers = defaultMakeRequestWithoutLogs PUT (EP.selectPlan id) headers reqBody Nothing
    encodeRequest req = standardEncode req

derive instance genericSelectPlanReq :: Generic SelectPlanReq _
derive instance newtypeSelectPlanReq :: Newtype SelectPlanReq _
instance standardEncodeSelectPlanReq :: StandardEncode SelectPlanReq where standardEncode (SelectPlanReq body) = standardEncode body
instance showSelectPlanReq :: Show SelectPlanReq where show = genericShow
instance decodeSelectPlanReq :: Decode SelectPlanReq  where decode = defaultDecode
instance encodeSelectPlanReq :: Encode SelectPlanReq where encode = defaultEncode

---------------------------------------------- CurrentPlanAPI ---------------------------------------------------
data GetCurrentPlanReq = GetCurrentPlanReq String

newtype GetCurrentPlanResp = GetCurrentPlanResp {
  currentPlanDetails :: Maybe PlanEntity,
  mandateDetails :: Maybe MandateData,
  autoPayStatus :: Maybe String,
  orderId :: Maybe String,
  isLocalized :: Maybe Boolean,
  lastPaymentType :: Maybe LastPaymentType,
  askForPlanSwitchByCity :: Maybe Boolean,
  askForPlanSwitchByVehicle :: Maybe Boolean
}

newtype MandateData = MandateData {
  status :: String, --CREATED | ACTIVE | FAILURE | PAUSED | EXPIRED | REVOKED
  startDate :: String,
  endDate :: String,
  mandateId :: String,
  payerVpa :: Maybe String,
  frequency :: String,
  maxAmount :: Number
}

newtype BankError = BankError {
  message :: String,
  code :: String,
  amount :: Number
}

instance makeGetCurrentPlanReq :: RestEndpoint GetCurrentPlanReq where
 makeRequest reqBody@(GetCurrentPlanReq driverId) headers = defaultMakeRequestWithoutLogs GET (EP.getCurrentPlan driverId) headers reqBody Nothing
 encodeRequest req = standardEncode req

derive instance genericGetCurrentPlanReq :: Generic GetCurrentPlanReq _
instance standardEncodeGetCurrentPlanReq :: StandardEncode GetCurrentPlanReq where standardEncode (GetCurrentPlanReq dummy ) = standardEncode dummy
instance showGetCurrentPlanReq :: Show GetCurrentPlanReq where show = genericShow
instance decodeGetCurrentPlanReq :: Decode GetCurrentPlanReq where decode = defaultDecode
instance encodeGetCurrentPlanReq :: Encode GetCurrentPlanReq where encode = defaultEncode

derive instance genericGetCurrentPlanResp :: Generic GetCurrentPlanResp _
derive instance newtypeGetCurrentPlanResp :: Newtype GetCurrentPlanResp _
instance standardEncodeGetCurrentPlanResp :: StandardEncode GetCurrentPlanResp where standardEncode (GetCurrentPlanResp res) = standardEncode res
instance showGetCurrentPlanResp :: Show GetCurrentPlanResp where show = genericShow
instance decodeGetCurrentPlanResp :: Decode GetCurrentPlanResp where decode = defaultDecode
instance encodeGetCurrentPlanResp :: Encode GetCurrentPlanResp where encode = defaultEncode 

derive instance genericMandateData :: Generic MandateData _
derive instance newtypeMandateData:: Newtype MandateData _
instance standardEncodeMandateData :: StandardEncode MandateData where standardEncode (MandateData res) = standardEncode res
instance showMandateData :: Show MandateData where show = genericShow
instance decodeMandateData :: Decode MandateData where decode = defaultDecode
instance encodeMandateData :: Encode MandateData where encode = defaultEncode

derive instance genericBankError :: Generic BankError _
derive instance newtypeBankError:: Newtype BankError _
instance standardEncodeBankError :: StandardEncode BankError where standardEncode (BankError res) = standardEncode res
instance showBankError :: Show BankError where show = genericShow
instance decodeBankError :: Decode BankError where decode = defaultDecode
instance encodeBankError :: Encode BankError where encode = defaultEncode

---------------------------------------------- KioskLocations ---------------------------------------------------

data KioskLocationReq = KioskLocationReq String

newtype KioskLocationResp = KioskLocationResp (Array KioskLocationRes)

type KioskLocationRes = {
  longitude :: Number,
  address :: String,
  contact :: Maybe String,
  latitude :: Number,
  landmark :: String
}

instance makeKioskLocationReq :: RestEndpoint KioskLocationReq where
    makeRequest reqBody headers = defaultMakeRequestWithoutLogs GET (EP.getKioskLocations "" ) headers reqBody Nothing
    encodeRequest req = defaultEncode req

derive instance genericKioskLocationReq :: Generic KioskLocationReq _
instance showKioskLocationReq :: Show KioskLocationReq where show = genericShow
instance standardEncodeKioskLocationReq :: StandardEncode KioskLocationReq where standardEncode (KioskLocationReq req) = standardEncode req
instance decodeKioskLocationReq :: Decode KioskLocationReq where decode = defaultDecode
instance encodeKioskLocationReq :: Encode KioskLocationReq where encode = defaultEncode

derive instance genericKioskLocationResp :: Generic KioskLocationResp _
derive instance newtypeKioskLocationResp :: Newtype KioskLocationResp _
instance standardEncodeKioskLocationResp :: StandardEncode KioskLocationResp where standardEncode (KioskLocationResp req) = standardEncode req
instance showKioskLocationResp :: Show KioskLocationResp where show = genericShow
instance decodeKioskLocationResp :: Decode KioskLocationResp where decode = defaultDecode
instance encodeKioskLocationResp :: Encode KioskLocationResp where encode = defaultEncode
------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Post Driver Feedback 


newtype PostRideFeedbackReq = PostRideFeedbackReq {
  rideId :: String,
  feedbackDetails:: String,
  ratingValue :: Int
}


instance makePostRideFeedbackReq :: RestEndpoint PostRideFeedbackReq where
 makeRequest reqBody headers = defaultMakeRequestWithoutLogs POST (EP.postRideFeedback "") headers reqBody Nothing
 encodeRequest req = standardEncode req


derive instance genericPostRideFeedbackReq :: Generic PostRideFeedbackReq _
derive instance newtypePostRideFeedbackReq :: Newtype PostRideFeedbackReq _
instance standardEncodePostRideFeedbackReq :: StandardEncode PostRideFeedbackReq where standardEncode (PostRideFeedbackReq reqBody) = standardEncode reqBody
instance showPostRideFeedbackReq :: Show PostRideFeedbackReq where show = genericShow
instance decodePostRideFeedbackReq :: Decode PostRideFeedbackReq where decode = defaultDecode
instance encodePostRideFeedbackReq :: Encode PostRideFeedbackReq where encode = defaultEncode


---------------------------------------------------------------------------------------------------------------


newtype GenerateReferralCodeReq = GenerateReferralCodeReq {}

newtype GenerateReferralCodeRes = GenerateReferralCodeRes {
  referralCode :: String
}

instance makeGenerateReferralCodeReq :: RestEndpoint GenerateReferralCodeReq where
    makeRequest reqBody@(GenerateReferralCodeReq date) headers = defaultMakeRequestWithoutLogs POST (EP.generateReferralCode "") headers reqBody Nothing
    encodeRequest req = defaultEncode req

derive instance genericGenerateReferralCodeReq :: Generic GenerateReferralCodeReq _
instance showGenerateReferralCodeReq :: Show GenerateReferralCodeReq where show = genericShow
instance standardGenerateReferralCodeReq :: StandardEncode GenerateReferralCodeReq where standardEncode (GenerateReferralCodeReq req) = standardEncode req
instance decodeGenerateReferralCodeReq :: Decode GenerateReferralCodeReq where decode = defaultDecode
instance encodeGenerateReferralCodeReq :: Encode GenerateReferralCodeReq where encode = defaultEncode

derive instance genericGenerateReferralCodeRes :: Generic GenerateReferralCodeRes _
derive instance newtypeGenerateReferralCodeRes :: Newtype GenerateReferralCodeRes _
instance showGenerateReferralCodeRes :: Show GenerateReferralCodeRes where show = genericShow
instance standardEncodeGenerateReferralCodeRes :: StandardEncode GenerateReferralCodeRes where standardEncode (GenerateReferralCodeRes req) = standardEncode req
instance decodeGenerateReferralCodeRes :: Decode GenerateReferralCodeRes where decode = defaultDecode
instance encodeGenerateReferralCodeRes :: Encode GenerateReferralCodeRes where encode = defaultEncode


---------------------------------------------- NY-PaymentHistory ---------------------------------------------------


data FeeType = AUTOPAY_REGISTRATION | MANUAL_PAYMENT | AUTOPAY_PAYMENT

derive instance genericFeeType :: Generic FeeType _
instance showFeeType :: Show FeeType where show = genericShow
instance decodeFeeType :: Decode FeeType where 
  decode body = case unsafeFromForeign body of
                  "MANDATE_REGISTRATION"        -> except $ Right AUTOPAY_REGISTRATION 
                  "RECURRING_INVOICE"           -> except $ Right MANUAL_PAYMENT 
                  "RECURRING_EXECUTION_INVOICE" -> except $ Right AUTOPAY_PAYMENT 
                  _                             -> fail $ ForeignError "Unknown response"
instance encodeFeeType :: Encode FeeType where 
  encode _ = encode {}
instance eqFeeType :: Eq FeeType where eq = genericEq
instance standardEncodeFeeType :: StandardEncode FeeType where standardEncode _ = standardEncode {}

data LastPaymentType = AUTOPAY_REGISTRATION_TYPE | CLEAR_DUE

derive instance genericLastPaymentType :: Generic LastPaymentType _
instance showLastPaymentType :: Show LastPaymentType where show = genericShow
instance decodeLastPaymentType :: Decode LastPaymentType where 
  decode body = case unsafeFromForeign body of
                  "AUTOPAY_REGISTRATION"        -> except $ Right AUTOPAY_REGISTRATION_TYPE 
                  "CLEAR_DUE"           -> except $ Right CLEAR_DUE 
                  _                             -> fail $ ForeignError "Unknown response"
instance encodeLastPaymentType :: Encode LastPaymentType where 
  encode _ = encode {}
instance eqLastPaymentType :: Eq LastPaymentType where eq = genericEq
instance standardEncodeLastPaymentType :: StandardEncode LastPaymentType where standardEncode _ = standardEncode {}

--------------------------------------------------------------------------------------------------------------------------------------------------------

data HistoryEntityV2Req = HistoryEntityV2Req String String String

newtype HistoryEntityV2Resp = HistoryEntityV2Resp {
    autoPayInvoices :: Array AutoPayInvoiceHistory,
    manualPayInvoices :: Array ManualInvoiceHistory
}

newtype AutoPayInvoiceHistory = AutoPayInvoiceHistory {
  invoiceId :: String,
  amount :: Number,
  executionAt :: String,
  autoPayStage :: Maybe AutopayPaymentStage, 
  rideTakenOn :: String,
  isCoinCleared :: Boolean,
  coinDiscountAmount :: Maybe Number
}

newtype ManualInvoiceHistory = ManualInvoiceHistory {
  invoiceId :: String,
  createdAt :: String,
  rideDays :: Int,
  amount :: Number,
  feeType :: FeeType,
  paymentStatus :: InvoiceStatus,
  rideTakenOn :: Maybe String,
  isCoinCleared :: Boolean,
  coinDiscountAmount :: Maybe Number
}

instance makeHistoryEntityV2Req :: RestEndpoint HistoryEntityV2Req where
    makeRequest reqBody@(HistoryEntityV2Req limit offset historyType) headers = defaultMakeRequestWithoutLogs GET (EP.paymentHistoryListV2 limit offset historyType) headers reqBody Nothing
    encodeRequest req = standardEncode req


derive instance genericHistoryEntityV2Req :: Generic HistoryEntityV2Req _
instance standardEncodeHistoryEntityV2Req :: StandardEncode HistoryEntityV2Req where standardEncode _ = standardEncode {}
instance showHistoryEntityV2Req :: Show HistoryEntityV2Req where show = genericShow
instance decodeHistoryEntityV2Req :: Decode HistoryEntityV2Req where decode = defaultDecode
instance encodeHistoryEntityV2Req :: Encode HistoryEntityV2Req where encode = defaultEncode

derive instance genericHistoryEntityV2Resp :: Generic HistoryEntityV2Resp _
derive instance newtypeHistoryEntityV2Resp :: Newtype HistoryEntityV2Resp _
instance standardEncodeHistoryEntityV2Resp :: StandardEncode HistoryEntityV2Resp where standardEncode (HistoryEntityV2Resp resp) = standardEncode resp
instance showHistoryEntityV2Resp :: Show HistoryEntityV2Resp where show = genericShow
instance decodeHistoryEntityV2Resp :: Decode HistoryEntityV2Resp where decode = defaultDecode
instance encodeHistoryEntityV2Resp :: Encode HistoryEntityV2Resp where encode = defaultEncode

derive instance genericAutoPayInvoiceHistory :: Generic AutoPayInvoiceHistory _
derive instance newtypeAutoPayInvoiceHistory :: Newtype AutoPayInvoiceHistory _
instance standardEncodeAutoPayInvoiceHistory :: StandardEncode AutoPayInvoiceHistory where standardEncode (AutoPayInvoiceHistory resp) = standardEncode resp
instance showAutoPayInvoiceHistory :: Show AutoPayInvoiceHistory where show = genericShow
instance decodeAutoPayInvoiceHistory :: Decode AutoPayInvoiceHistory where decode = defaultDecode
instance encodeAutoPayInvoiceHistory :: Encode AutoPayInvoiceHistory where encode = defaultEncode

derive instance genericManualInvoiceHistory :: Generic ManualInvoiceHistory _
derive instance newtypeManualInvoiceHistory :: Newtype ManualInvoiceHistory _
instance standardEncodeManualInvoiceHistory :: StandardEncode ManualInvoiceHistory where standardEncode (ManualInvoiceHistory resp) = standardEncode resp
instance showManualInvoiceHistory :: Show ManualInvoiceHistory where show = genericShow
instance decodeManualInvoiceHistory :: Decode ManualInvoiceHistory where decode = defaultDecode
instance encodeManualInvoiceHistory :: Encode ManualInvoiceHistory where encode = defaultEncode

------------------------------------------------------------------------------------------------------------------------

data HistoryEntryDetailsEntityV2Req = HistoryEntryDetailsEntityV2Req String

newtype HistoryEntryDetailsEntityV2Resp = HistoryEntryDetailsEntityV2Resp {
    invoiceId :: String,
    amount :: Number,
    createdAt :: Maybe String,
    executionAt :: Maybe String,
    feeType :: FeeType ,
    driverFeeInfo :: Array DriverFeeInfoEntity
}

newtype DriverFeeInfoEntity = DriverFeeInfoEntity {
    autoPayStage :: Maybe AutopayPaymentStage,
    paymentStatus :: Maybe InvoiceStatus,
    totalEarnings :: Number,
    totalRides :: Int,
    planAmount :: Number,
    isSplit :: Boolean,
    offerAndPlanDetails :: Maybe String,
    rideTakenOn :: String,
    driverFeeAmount :: Number,
    gst :: Maybe Number,
    gstPercentage :: Maybe Number,
    specialZoneRideCount :: Maybe Int,
    totalSpecialZoneCharges :: Maybe Number,
    maxRidesEligibleForCharge :: Maybe Int,
    isCoinCleared :: Boolean,
    coinDiscountAmount :: Maybe Number
}

instance makeHistoryEntryDetailsEntityV2Req :: RestEndpoint HistoryEntryDetailsEntityV2Req where
    makeRequest reqBody@(HistoryEntryDetailsEntityV2Req id) headers = defaultMakeRequestWithoutLogs GET (EP.paymentEntityDetails id) headers reqBody Nothing
    encodeRequest req = standardEncode req


derive instance genericHistoryEntryDetailsEntityV2Req :: Generic HistoryEntryDetailsEntityV2Req _
instance showHistoryEntryDetailsEntityV2Req :: Show HistoryEntryDetailsEntityV2Req where show = genericShow
instance standardEncodeHistoryEntryDetailsEntityV2Req :: StandardEncode HistoryEntryDetailsEntityV2Req where standardEncode _ = standardEncode {}
instance decodeHistoryEntryDetailsEntityV2Req :: Decode HistoryEntryDetailsEntityV2Req where decode = defaultDecode
instance encodeHistoryEntryDetailsEntityV2Req :: Encode HistoryEntryDetailsEntityV2Req where encode = defaultEncode

derive instance genericHistoryEntryDetailsEntityV2Resp :: Generic HistoryEntryDetailsEntityV2Resp _
derive instance newtypeHistoryEntryDetailsEntityV2Resp :: Newtype HistoryEntryDetailsEntityV2Resp _
instance standardEncodeHistoryEntryDetailsEntityV2Resp :: StandardEncode HistoryEntryDetailsEntityV2Resp where standardEncode (HistoryEntryDetailsEntityV2Resp req) = standardEncode req
instance showHistoryEntryDetailsEntityV2Resp :: Show HistoryEntryDetailsEntityV2Resp where show = genericShow
instance decodeHistoryEntryDetailsEntityV2Resp :: Decode HistoryEntryDetailsEntityV2Resp where decode = defaultDecode
instance encodeHistoryEntryDetailsEntityV2Resp :: Encode HistoryEntryDetailsEntityV2Resp where encode = defaultEncode

derive instance genericDriverFeeInfoEntity :: Generic DriverFeeInfoEntity _
derive instance newtypeDriverFeeInfoEntity :: Newtype DriverFeeInfoEntity _
instance standardEncodeDriverFeeInfoEntity :: StandardEncode DriverFeeInfoEntity where standardEncode (DriverFeeInfoEntity req) = standardEncode req
instance showDriverFeeInfoEntity :: Show DriverFeeInfoEntity where show = genericShow
instance decodeDriverFeeInfoEntity :: Decode DriverFeeInfoEntity where decode = defaultDecode
instance encodeDriverFeeInfoEntity :: Encode DriverFeeInfoEntity where encode = defaultEncode

data AutopayPaymentStage =  NOTIFICATION_SCHEDULED | NOTIFICATION_ATTEMPTING | EXECUTION_SCHEDULED | EXECUTION_ATTEMPTING | EXECUTION_SUCCESS | EXECUTION_FAILED | NOTIFICATION_FAILED
data InvoiceStatus =  ACTIVE_INVOICE | SUCCESS | FAILED | EXPIRED | INACTIVE | CLEARED_BY_YATRI_COINS

derive instance genericAutopayPaymentStage :: Generic AutopayPaymentStage _
instance showAutopayPaymentStage :: Show AutopayPaymentStage where show = genericShow
instance decodeAutopayPaymentStage :: Decode AutopayPaymentStage
  where decode body = case unsafeFromForeign body of
                  "NOTIFICATION_SCHEDULED"        -> except $ Right NOTIFICATION_SCHEDULED 
                  "NOTIFICATION_ATTEMPTING"           -> except $ Right NOTIFICATION_ATTEMPTING 
                  "EXECUTION_SCHEDULED" -> except $ Right EXECUTION_SCHEDULED 
                  "EXECUTION_ATTEMPTING" -> except $ Right EXECUTION_ATTEMPTING 
                  "EXECUTION_SUCCESS" -> except $ Right EXECUTION_SUCCESS 
                  "EXECUTION_FAILED" -> except $ Right EXECUTION_FAILED 
                  "NOTIFICATION_FAILED" -> except $ Right NOTIFICATION_FAILED 
                  _   ->  except $ Right NOTIFICATION_SCHEDULED
instance encodeAutopayPaymentStage :: Encode AutopayPaymentStage where encode = defaultEnumEncode
instance eqAutopayPaymentStage :: Eq AutopayPaymentStage where eq = genericEq
instance standardEncodeAutopayPaymentStage :: StandardEncode AutopayPaymentStage
  where
    standardEncode _ = standardEncode {}

derive instance genericInvoiceStatus :: Generic InvoiceStatus _
instance showInvoiceStatus :: Show InvoiceStatus where show = genericShow
instance decodeInvoiceStatus :: Decode InvoiceStatus where decode = defaultEnumDecode
instance encodeInvoiceStatus :: Encode InvoiceStatus where encode = defaultEnumEncode
instance eqInvoiceStatus :: Eq InvoiceStatus where eq = genericEq
instance standardEncodeInvoiceStatus :: StandardEncode InvoiceStatus
  where
    standardEncode _ = standardEncode {}
-----------------------------------------------------------------------------------------------------------------------------------------------

newtype ClearDuesReq = ClearDuesReq String

newtype ClearDuesResp = ClearDuesResp {
  orderResp :: CreateOrderRes,
  orderId :: String
}

instance makeClearDuesReq :: RestEndpoint ClearDuesReq where
    makeRequest reqBody@(ClearDuesReq id) headers = defaultMakeRequestWithoutLogs GET (EP.cleardues id) headers reqBody Nothing
    encodeRequest req = standardEncode req


derive instance genericClearDuesReq :: Generic ClearDuesReq _
instance showClearDuesReq :: Show ClearDuesReq where show = genericShow
instance standardEncodeClearDuesReq :: StandardEncode ClearDuesReq where standardEncode _ = standardEncode {}
instance decodeClearDuesReq :: Decode ClearDuesReq where decode = defaultDecode
instance encodeClearDuesReq :: Encode ClearDuesReq where encode = defaultEncode

derive instance genericClearDuesResp :: Generic ClearDuesResp _
derive instance newtypeClearDuesResp :: Newtype ClearDuesResp _
instance standardEncodeClearDuesResp :: StandardEncode ClearDuesResp where standardEncode (ClearDuesResp req) = standardEncode req
instance showClearDuesResp :: Show ClearDuesResp where show = genericShow
instance decodeClearDuesResp :: Decode ClearDuesResp where decode = defaultDecode
instance encodeClearDuesResp :: Encode ClearDuesResp where encode = defaultEncode

----------------------------------------------------------- driverGoTo ------------------------------------------------------------------
data ActivateDriverGoToReq = ActivateDriverGoToReq String String



instance makeActivateDriverGoToReq :: RestEndpoint ActivateDriverGoToReq where
  makeRequest reqBody@(ActivateDriverGoToReq homeLocationId currentLocation) headers = defaultMakeRequestWithoutLogs POST (EP.activateDriverGoTo homeLocationId currentLocation) headers reqBody Nothing
  encodeRequest req = standardEncode req

derive instance genericActivateDriverGoToReq :: Generic ActivateDriverGoToReq _
instance standardActivateDriverGoToReq :: StandardEncode ActivateDriverGoToReq where standardEncode (ActivateDriverGoToReq req currentLocation) = standardEncode req
instance showActivateDriverGoToReq :: Show ActivateDriverGoToReq where show = genericShow
instance decodeActivateDriverGoToReq :: Decode ActivateDriverGoToReq where decode = defaultDecode
instance encodeActivateDriverGoToReq :: Encode ActivateDriverGoToReq where encode = defaultEncode


data DeactivateDriverGoToReq = DeactivateDriverGoToReq


instance makeDeactivateDriverGoToReq :: RestEndpoint DeactivateDriverGoToReq where
  makeRequest reqBody headers = defaultMakeRequestWithoutLogs POST (EP.deactivateDriverGoTo "") headers reqBody Nothing
  encodeRequest req = standardEncode req

derive instance genericDeactivateDriverGoToReq :: Generic DeactivateDriverGoToReq _
instance showDeactivateDriverGoToReq :: Show DeactivateDriverGoToReq where show = genericShow
instance decodeDeactivateDriverGoToReq :: Decode DeactivateDriverGoToReq where decode = defaultDecode
instance encodeDeactivateDriverGoToReq :: Encode DeactivateDriverGoToReq where encode = defaultEncode
instance standardDeactivateDriverGoToReq :: StandardEncode DeactivateDriverGoToReq where standardEncode body = standardEncode {}




newtype AddHomeLocationReq = AddHomeLocationReq
  { position :: LatLong,
    address :: String,
    tag :: String
  }


instance makeAddHomeLocationReq :: RestEndpoint AddHomeLocationReq where
  makeRequest reqBody headers = defaultMakeRequestWithoutLogs POST (EP.addDriverHomeLocation "") headers reqBody Nothing
  encodeRequest req = standardEncode req

derive instance genericAddHomeLocationReq :: Generic AddHomeLocationReq _
derive instance newtypeAddHomeLocationReq :: Newtype AddHomeLocationReq _
instance standardEncodeAddHomeLocationReq :: StandardEncode AddHomeLocationReq where standardEncode (AddHomeLocationReq payload) = standardEncode payload
instance showAddHomeLocationReq :: Show AddHomeLocationReq where show = genericShow
instance decodeAddHomeLocationReq :: Decode AddHomeLocationReq where decode = defaultDecode
instance encodeAddHomeLocationReq :: Encode AddHomeLocationReq where encode = defaultEncode



data GetHomeLocationReq = GetHomeLocationReq
newtype GetHomeLocationsRes = GetHomeLocationsRes { 
  locations :: Array DriverHomeLocationAPIEntity
}

newtype DriverHomeLocationAPIEntity = DriverHomeLocationAPIEntity
  { id :: String,
    lat :: Number,
    lon :: Number,
    address :: String,
    tag :: String
  }


instance makeGetHomeLocationReq :: RestEndpoint GetHomeLocationReq where
  makeRequest reqBody headers = defaultMakeRequestWithoutLogs GET (EP.getDriverHomeLocation "") headers reqBody Nothing
  encodeRequest req = standardEncode req

derive instance genericGetHomeLocationReq :: Generic GetHomeLocationReq _
instance showGetHomeLocationReq :: Show GetHomeLocationReq where show = genericShow
instance decodeGetHomeLocationReq :: Decode GetHomeLocationReq where decode = defaultDecode
instance encodeGetHomeLocationReq :: StandardEncode GetHomeLocationReq where standardEncode req = standardEncode {}

derive instance genericGetHomeLocationsRes :: Generic GetHomeLocationsRes _
derive instance newtypeGetHomeLocationsRes :: Newtype GetHomeLocationsRes _
instance standardGetHomeLocationsRes :: StandardEncode GetHomeLocationsRes where standardEncode (GetHomeLocationsRes id) = standardEncode id
instance showGetHomeLocationsRes :: Show GetHomeLocationsRes where show = genericShow
instance decodeGetHomeLocationsRes :: Decode GetHomeLocationsRes where decode = defaultDecode
instance encodeGetHomeLocationsRes :: Encode GetHomeLocationsRes where encode = defaultEncode

derive instance genericDriverHomeLocationAPIEntity :: Generic DriverHomeLocationAPIEntity _
derive instance newtypeDriverHomeLocationAPIEntity :: Newtype DriverHomeLocationAPIEntity _
instance standardDriverHomeLocationAPIEntity :: StandardEncode DriverHomeLocationAPIEntity where standardEncode (DriverHomeLocationAPIEntity id) = standardEncode id
instance showDriverHomeLocationAPIEntity :: Show DriverHomeLocationAPIEntity where show = genericShow
instance decodeDriverHomeLocationAPIEntity :: Decode DriverHomeLocationAPIEntity where decode = defaultDecode
instance encodeDriverHomeLocationAPIEntity :: Encode DriverHomeLocationAPIEntity where encode = defaultEncode

newtype DeleteDriverHomeLocationReq = DeleteDriverHomeLocationReq String


instance makeDeleteDriverHomeLocationReq :: RestEndpoint DeleteDriverHomeLocationReq where
  makeRequest reqBody@(DeleteDriverHomeLocationReq id) headers = defaultMakeRequestWithoutLogs DELETE (EP.deleteDriverHomeLocation id) headers reqBody Nothing
  encodeRequest req = standardEncode req


derive instance genericDeleteDriverHomeLocationReq :: Generic DeleteDriverHomeLocationReq _
derive instance newtypeDeleteDriverHomeLocationReq :: Newtype DeleteDriverHomeLocationReq _
instance standardDeleteDriverHomeLocationReq :: StandardEncode DeleteDriverHomeLocationReq where standardEncode (DeleteDriverHomeLocationReq id) = standardEncode id
instance showDeleteDriverHomeLocationReq :: Show DeleteDriverHomeLocationReq where show = genericShow
instance decodeDeleteDriverHomeLocationReq :: Decode DeleteDriverHomeLocationReq where decode = defaultDecode
instance encodeDeleteDriverHomeLocationReq :: Encode DeleteDriverHomeLocationReq where encode = defaultEncode


newtype UpdateHomeLocationReq = UpdateHomeLocationReq {
  qParam :: String,
  body :: AddHomeLocationReq
  }


instance makeUpdateHomeLocationReq :: RestEndpoint UpdateHomeLocationReq where
  makeRequest (UpdateHomeLocationReq req) headers = defaultMakeRequestWithoutLogs POST (EP.updateDriverHomeLocation req.qParam) headers req.body Nothing
  encodeRequest (UpdateHomeLocationReq req) = standardEncode req.body

derive instance genericUpdateHomeLocationReq :: Generic UpdateHomeLocationReq _
derive instance newtypeUpdateHomeLocationReq :: Newtype UpdateHomeLocationReq _
instance standardUpdateHomeLocationReq :: StandardEncode UpdateHomeLocationReq where standardEncode (UpdateHomeLocationReq id) = standardEncode id
instance showUpdateHomeLocationReq :: Show UpdateHomeLocationReq where show = genericShow
instance decodeUpdateHomeLocationReq :: Decode UpdateHomeLocationReq where decode = defaultDecode
instance encodeUpdateHomeLocationReq :: Encode UpdateHomeLocationReq where encode = defaultEncode



newtype RideRouteReq = RideRouteReq String

newtype RideRouteResp = RideRouteResp {
  points :: (Array LatLong),
  duration :: Maybe Int,
  distance :: Maybe Int

}

instance makeRideRouteReq :: RestEndpoint RideRouteReq where
  makeRequest reqBody@(RideRouteReq id) headers = defaultMakeRequestWithoutLogs POST (EP.rideRoute id) headers reqBody Nothing
  encodeRequest req = standardEncode req


derive instance genericRideRouteReq :: Generic RideRouteReq _
derive instance newtypeRideRouteReq :: Newtype RideRouteReq _
instance standardRideRouteReq :: StandardEncode RideRouteReq where standardEncode (RideRouteReq id) = standardEncode id
instance showRideRouteReq :: Show RideRouteReq where show = genericShow
instance decodeRideRouteReq :: Decode RideRouteReq where decode = defaultDecode
instance encodeRideRouteReq :: Encode RideRouteReq where encode = defaultEncode

derive instance genericRideRouteResp :: Generic RideRouteResp _
derive instance newtypeRideRouteResp :: Newtype RideRouteResp _
instance standardRideRouteResp :: StandardEncode RideRouteResp where standardEncode (RideRouteResp id) = standardEncode id
instance showRideRouteResp :: Show RideRouteResp where show = genericShow
instance decodeRideRouteResp :: Decode RideRouteResp where decode = defaultDecode
instance encodeRideRouteResp :: Encode RideRouteResp where encode = defaultEncode

----------------------------- Merchant Operating City -----------------------------------------------------
newtype GetCityRes = GetCityRes (Array CityRes)

newtype CityRes = CityRes {
  name :: String ,
  code :: String
}

data GetCityReq = GetCityReq String

instance makeGetCityReq :: RestEndpoint GetCityReq where
  makeRequest reqBody@(GetCityReq id) headers = defaultMakeRequestWithoutLogs GET (EP.getMerchantIdList id) headers reqBody Nothing
  encodeRequest req = standardEncode req

derive instance genericGetCityReq :: Generic GetCityReq _
instance showGetCityReq :: Show GetCityReq where show = genericShow
instance decodeGetCityReq :: Decode GetCityReq where decode = defaultDecode
instance encodeGetCityReq :: StandardEncode GetCityReq where standardEncode req = standardEncode {}

derive instance genericCityRes :: Generic CityRes _
derive instance newtypeCityRes :: Newtype CityRes _
instance standardCityRes :: StandardEncode CityRes where standardEncode (CityRes id) = standardEncode id
instance showCityRes :: Show CityRes where show = genericShow
instance decodeCityRes :: Decode CityRes where decode = defaultDecode
instance encodeCityRes :: Encode CityRes where encode = defaultEncode

derive instance genericGetCityRes :: Generic GetCityRes _
derive instance newtypeGetCityRes :: Newtype GetCityRes _
instance standardGetCityRes :: StandardEncode GetCityRes where standardEncode (GetCityRes id) = standardEncode id
instance showGetCityRes :: Show GetCityRes where show = genericShow
instance decodeGetCityRes :: Decode GetCityRes where decode = defaultDecode
instance encodeGetCityRes :: Encode GetCityRes where encode = defaultEncode

---------------------------------------------- DriverCoins ---------------------------------------------------
  
data CoinTransactionReq = CoinTransactionReq String

newtype CoinTransactionRes = CoinTransactionRes
  {
    coinBalance :: Int,
    coinEarned :: Int,
    coinUsed :: Int,
    coinExpired :: Int,
    todayCoinSummary :: Int,
    coinsEarnedPreviousDay :: Int,
    expiringCoins :: Int,
    expiringDays :: Int,
    coinTransactionHistory :: Array CoinTransactionHistoryItem
  }

newtype CoinTransactionHistoryItem = CoinTransactionHistoryItem
  {
    coins :: Int,
    eventFunction :: DriverCoinsFunctionType,
    createdAt :: String,
    bulkUploadTitle :: Maybe BulkCoinTitleTranslations
  }

newtype BulkCoinTitleTranslations = BulkCoinTitleTranslations
  { en :: String,
    bn :: String,
    hi :: String,
    ml :: String,
    ta :: String,
    te :: String,
    kn :: String,
    fr :: String
  }

data DriverCoinsFunctionType
  = OneOrTwoStarRating
  | RideCompleted
  | FiveStarRating
  | BookingCancellation
  | CustomerReferral
  | DriverReferral
  | TwoRidesCompleted
  | FiveRidesCompleted
  | TenRidesCompleted
  | EightPlusRidesInOneDay
  | PurpleRideCompleted
  | LeaderBoardTopFiveHundred
  | TrainingCompleted
  | BulkUploadFunction
  | BulkUploadFunctionV2
  | RidesCompleted Int
  | MetroRideCompleted MetroRideType (Maybe Int)

data MetroRideType
  = ToMetro
  | FromMetro
  | None

instance makeCoinTransactionReq :: RestEndpoint CoinTransactionReq where
    makeRequest reqBody@(CoinTransactionReq date) headers = defaultMakeRequestWithoutLogs GET (EP.getCoinTransactions date) headers reqBody Nothing
    encodeRequest = standardEncode

derive instance genericCoinTransactionReq :: Generic CoinTransactionReq _
instance showCoinTransactionReq :: Show CoinTransactionReq where show = genericShow
instance standardEncodeCoinTransactionReq :: StandardEncode CoinTransactionReq where standardEncode (CoinTransactionReq req) = standardEncode req
instance decodeCoinTransactionReq :: Decode CoinTransactionReq where decode = defaultDecode
instance encodeCoinTransactionReq :: Encode CoinTransactionReq where encode = defaultEncode

derive instance genericCoinTransactionRes :: Generic CoinTransactionRes _
derive instance newtypeCoinTransactionRes :: Newtype CoinTransactionRes _
instance standardEncodeCoinTransactionRes :: StandardEncode CoinTransactionRes where standardEncode (CoinTransactionRes resp) = standardEncode resp
instance showCoinTransactionRes :: Show CoinTransactionRes where show = genericShow
instance decodeCoinTransactionRes :: Decode CoinTransactionRes where decode = defaultDecode
instance encodeCoinTransactionRes :: Encode CoinTransactionRes where encode = defaultEncode

derive instance genericDriverCoinsFunctionType :: Generic DriverCoinsFunctionType _
instance showDriverCoinsFunctionType :: Show DriverCoinsFunctionType where show = genericShow
instance decodeDriverCoinsFunctionType :: Decode DriverCoinsFunctionType where 
  decode body = 
    case (typeOf body == "object") of
      true -> defaultDecode body
      false -> defaultDecode $ unsafeToForeign {tag : body}

instance encodeDriverCoinsFunctionType :: Encode DriverCoinsFunctionType where encode = defaultEncode
instance eqDriverCoinsFunctionType :: Eq DriverCoinsFunctionType where eq = genericEq
instance standardEncodeDriverCoinsFunctionType :: StandardEncode DriverCoinsFunctionType
  where
    standardEncode _ = standardEncode {}

derive instance genericMetroRideType :: Generic MetroRideType _
instance showMetroRideType :: Show MetroRideType where show = genericShow
instance decodeMetroRideType :: Decode MetroRideType where decode = defaultEnumDecode
instance encodeMetroRideType :: Encode MetroRideType where encode = defaultEnumEncode
instance eqMetroRideType :: Eq MetroRideType where eq = genericEq

derive instance genericCoinTransactionHistoryItem :: Generic CoinTransactionHistoryItem _
derive instance newtypeCoinTransactionHistoryItem :: Newtype CoinTransactionHistoryItem _
instance standardEncodeCoinTransactionHistoryItem :: StandardEncode CoinTransactionHistoryItem where standardEncode (CoinTransactionHistoryItem resp) = standardEncode resp
instance showCoinTransactionHistoryItem :: Show CoinTransactionHistoryItem where show = genericShow
instance decodeCoinTransactionHistoryItem :: Decode CoinTransactionHistoryItem where decode = defaultDecode
instance encodeCoinTransactionHistoryItem :: Encode CoinTransactionHistoryItem where encode = defaultEncode

derive instance genericBulkCoinTitleTranslations :: Generic BulkCoinTitleTranslations _
derive instance newtypeBulkCoinTitleTranslations :: Newtype BulkCoinTitleTranslations _
instance standardEncodeBulkCoinTitleTranslations :: StandardEncode BulkCoinTitleTranslations where standardEncode (BulkCoinTitleTranslations resp) = standardEncode resp
instance showBulkCoinTitleTranslations :: Show BulkCoinTitleTranslations where show = genericShow
instance decodeBulkCoinTitleTranslations :: Decode BulkCoinTitleTranslations where decode = defaultDecode
instance encodeBulkCoinTitleTranslations :: Encode BulkCoinTitleTranslations where encode = defaultEncode

data CoinsUsageReq = CoinsUsageReq String String

newtype CoinsUsageRes = CoinsUsageRes
  {
    coinBalance :: Int,
    totalCoinConvertedToCash :: Number,
    coinConvertedToCashUsedForLatestDues :: Maybe Int,
    coinConvertedTocashLeft :: Number,
    coinConversionRate :: Number,
    coinUsageHistory :: Array CoinUsageHistoryItem
  }

newtype CoinUsageHistoryItem = CoinUsageHistoryItem
  {
    numCoins :: Int,
    title :: String,
    createdAt :: String,
    cash :: Number
  }

instance makeCoinsUsageReq :: RestEndpoint CoinsUsageReq where
    makeRequest reqBody@(CoinsUsageReq limit offset) headers = defaultMakeRequestWithoutLogs GET (EP.getCoinUsageHistory limit offset) headers reqBody Nothing
    encodeRequest req = defaultEncode req

derive instance genericCoinsUsageReq :: Generic CoinsUsageReq _
instance showCoinsUsageReq :: Show CoinsUsageReq where show = genericShow
instance standardEncodeCoinsUsageReq :: StandardEncode CoinsUsageReq where standardEncode _ = standardEncode {}
instance decodeCoinsUsageReq :: Decode CoinsUsageReq where decode = defaultDecode
instance encodeCoinsUsageReq :: Encode CoinsUsageReq where encode = defaultEncode

derive instance genericCoinsUsageRes :: Generic CoinsUsageRes _
derive instance newtypeCoinsUsageRes :: Newtype CoinsUsageRes _
instance standardEncodeCoinsUsageRes :: StandardEncode CoinsUsageRes where standardEncode (CoinsUsageRes resp) = standardEncode resp
instance showCoinsUsageRes :: Show CoinsUsageRes where show = genericShow
instance decodeCoinsUsageRes :: Decode CoinsUsageRes where decode = defaultDecode
instance encodeCoinsUsageRes :: Encode CoinsUsageRes where encode = defaultEncode

derive instance genericCoinUsageHistoryItem :: Generic CoinUsageHistoryItem _
derive instance newtypeCoinUsageHistoryItem :: Newtype CoinUsageHistoryItem _
instance standardEncodeCoinUsageHistoryItem :: StandardEncode CoinUsageHistoryItem where standardEncode (CoinUsageHistoryItem resp) = standardEncode resp
instance showCoinUsageHistoryItem :: Show CoinUsageHistoryItem where show = genericShow
instance decodeCoinUsageHistoryItem :: Decode CoinUsageHistoryItem where decode = defaultDecode
instance encodeCoinUsageHistoryItem :: Encode CoinUsageHistoryItem where encode = defaultEncode

newtype ConvertCoinToCashReq = ConvertCoinToCashReq
  {
    coins :: Int
  }


instance makeConvertCoinToCashReq :: RestEndpoint ConvertCoinToCashReq where
 makeRequest reqBody headers = defaultMakeRequestWithoutLogs POST (EP.convertCoinToCash "") headers reqBody Nothing
 encodeRequest req = standardEncode req

derive instance genericConvertCoinToCashReq :: Generic ConvertCoinToCashReq _
derive instance newtypeConvertCoinToCashReq :: Newtype ConvertCoinToCashReq _
instance standardEncodeConvertCoinToCashReq :: StandardEncode ConvertCoinToCashReq where standardEncode (ConvertCoinToCashReq reqBody) = standardEncode reqBody
instance showConvertCoinToCashReq :: Show ConvertCoinToCashReq where show = genericShow
instance decodeConvertCoinToCashReq :: Decode ConvertCoinToCashReq where decode = defaultDecode
instance encodeConvertCoinToCashReq :: Encode ConvertCoinToCashReq where encode = defaultEncode




----------------------------- Referred Drivers -----------------------------------------------------

data ReferredDriversReq = ReferredDriversReq String

newtype ReferredDriversResp = ReferredDriversResp
 {
   value :: Int
 }

instance makeReferredDriversReq :: RestEndpoint ReferredDriversReq where
    makeRequest reqBody headers = defaultMakeRequestWithoutLogs GET (EP.referredDrivers "") headers reqBody Nothing
    encodeRequest req = standardEncode req

derive instance genericReferredDriversResp :: Generic ReferredDriversResp _
derive instance newtypeReferredDriversResp :: Newtype ReferredDriversResp _
instance standardReferredDriversResp :: StandardEncode ReferredDriversResp where standardEncode (ReferredDriversResp id) = standardEncode id
instance showReferredDriversResp :: Show ReferredDriversResp where show = genericShow
instance decodeReferredDriversResp :: Decode ReferredDriversResp where decode = defaultDecode
instance encodeReferredDriversResp :: Encode ReferredDriversResp where encode = defaultEncode

derive instance genericReferredDriversReq :: Generic ReferredDriversReq _
instance showReferredDriversReq :: Show ReferredDriversReq where show = genericShow
instance decodeReferredDriversReq :: Decode ReferredDriversReq where decode = defaultDecode
instance encodeRReferredDriversReq :: Encode ReferredDriversReq where encode = defaultEncode
instance standardReferredDriversReq :: StandardEncode ReferredDriversReq where standardEncode body = standardEncode {}


newtype DetectCityReq = DetectCityReq {
  lat :: Number,
  lon :: Number,
  merchantId :: String
}

newtype DetectCityResp = DetectCityResp {
  city :: Maybe String,
  status :: ApiSuccessResult
}

instance makeDetectCityReq :: RestEndpoint DetectCityReq where
  makeRequest reqBody@(DetectCityReq req) headers = defaultMakeRequestWithoutLogs POST (EP.detectCity "") headers reqBody Nothing
  encodeRequest req = standardEncode req

derive instance genericDetectCityReq :: Generic DetectCityReq _
derive instance newtypeDetectCityReq :: Newtype DetectCityReq _
instance standardDetectCityReq :: StandardEncode DetectCityReq where standardEncode (DetectCityReq id) = standardEncode id
instance showDetectCityReq :: Show DetectCityReq where show = genericShow
instance decodeDetectCityReq :: Decode DetectCityReq where decode = defaultDecode
instance encodeDetectCityReq :: Encode DetectCityReq where encode = defaultEncode

derive instance genericDetectCityResp :: Generic DetectCityResp _
instance standardDetectCityResp :: StandardEncode DetectCityResp where standardEncode (DetectCityResp body) = standardEncode body
instance showDetectCityResp :: Show DetectCityResp where show = genericShow
instance decodeDetectCityResp :: Decode DetectCityResp where decode = defaultDecode
instance encodeDetectCityResp  :: Encode DetectCityResp where encode = defaultEncode

----------------------------- SDK Events -----------------------------------------------------
newtype SDKEventsReq = SDKEventsReq {
  event :: String,
  events :: Array EventsPayload
}

newtype EventsPayload = EventsPayload {
  eventName :: String,
  module :: Maybe String,
  clientType :: String,
  sessionId :: String,
  payload :: Maybe String,
  source :: String,
  userId :: String,
  timestamp :: String,
  vehicleType :: Maybe String,
  cityId :: Maybe String
}

derive instance genericEventsPayload :: Generic EventsPayload _
instance standardEncodeEventsPayload :: StandardEncode EventsPayload where standardEncode (EventsPayload res) = standardEncode res
instance showEventsPayload :: Show EventsPayload where show = genericShow
instance decodeEventsPayload :: Decode EventsPayload where decode = defaultDecode
instance encodeEventsPayload  :: Encode EventsPayload where encode = defaultEncode


instance makeSDKEventsReq :: RestEndpoint SDKEventsReq where
 makeRequest reqBody headers = defaultMakeRequestWithoutLogs POST (EP.pushSDKEvents "") headers reqBody Nothing
 encodeRequest req = standardEncode req

derive instance genericSDKEventsReq :: Generic SDKEventsReq _
derive instance newtypeSDKEventsReq :: Newtype SDKEventsReq _
instance standardEncodeSDKEventsReq :: StandardEncode SDKEventsReq where standardEncode (SDKEventsReq reqBody) = standardEncode reqBody
instance showSDKEventsReq :: Show SDKEventsReq where show = genericShow
instance decodeSDKEventsReq :: Decode SDKEventsReq where decode = defaultDecode
instance encodeSDKEventsReq :: Encode SDKEventsReq where encode = defaultEncode



data UploadOdometerImageReq = UploadOdometerImageReq String OdometerImage

data OdometerImage = OdometerImage String 
  

data UploadOdometerImageResp =  UploadOdometerImageResp {
   fileId :: String 
  }


instance makeUploadOdometerImageReq :: RestEndpoint UploadOdometerImageReq where
  makeRequest reqBody@(UploadOdometerImageReq rideId (OdometerImage rqBody)) headers = defaultMakeRequestWithoutLogs POST (EP.uploadOdometerImage rideId) headers reqBody Nothing
  encodeRequest req = standardEncode req

derive instance genericOdometerImage :: Generic OdometerImage _
instance standardEncodeOdometerImage :: StandardEncode OdometerImage where standardEncode (OdometerImage id) = standardEncode id
instance showOdometerImage :: Show OdometerImage where show = genericShow
instance decodeOdometerImage :: Decode OdometerImage where decode = defaultDecode
instance encodeOdometerImage :: Encode OdometerImage where encode = defaultEncode


derive instance genericUploadOdometerImageReq :: Generic UploadOdometerImageReq _
instance standardEncodeUploadOdometerImageReq :: StandardEncode UploadOdometerImageReq where standardEncode (UploadOdometerImageReq rideId req) = standardEncode req           
instance showUploadOdometerImageReq :: Show UploadOdometerImageReq where show = genericShow
instance decodeUploadOdometerImageReq :: Decode UploadOdometerImageReq where decode = defaultDecode   
instance encodeUploadOdometerImageReq :: Encode UploadOdometerImageReq where encode = defaultEncode



derive instance genericUploadOdometerImageResp :: Generic UploadOdometerImageResp _     
instance standardUploadOdometerImageResp :: StandardEncode UploadOdometerImageResp where standardEncode (UploadOdometerImageResp body) = standardEncode body      
instance showUploadOdometerImageResp :: Show UploadOdometerImageResp where show = genericShow           
instance decodeUploadOdometerImageResp :: Decode UploadOdometerImageResp where decode = defaultDecode
instance encodeUploadOdometerImageResp :: Encode UploadOdometerImageResp where encode = defaultEncode     

------------------------------------------ Get all lms modules ------------------------------------------------------------------------
 
data GetAllModuleReq = GetAllModuleReq String

newtype LmsGetModuleRes = LmsGetModuleRes
  { completed :: Array LmsModuleRes,
    remaining :: Array LmsModuleRes
  }

newtype LmsModuleRes = LmsModuleRes
  { category :: LmsCategory,
    completedAt :: Maybe String,
    description :: String,
    duration :: Int,
    moduleCompletionStatus ::ModuleCompletionStatus,
    moduleId :: String,
    name :: String,
    noOfVideos :: Int,
    rank :: Int,
    thumbnailImage :: String,
    variant :: Maybe String,
    noOfVideosCompleted :: Int,
    moduleCompletionCriteria :: ModuleCompletionCriteria,
    languagesAvailableForQuiz :: Array String,
    languagesAvailableForVideos :: Array String
  }

data LmsBonus = BONUS_COIN Int | BONUS_BADGE String

data LmsReward = COIN Int | BADGE String

data LmsCategory =  Safety | Financial | Training

data ModuleCompletionStatus = MODULE_NOT_YET_STARTED | MODULE_ONGOING | MODULE_COMPLETED

data ModuleCompletionCriteria = ONLY_VIDEOS | VIDEOS_AND_QUIZ Int

instance makeGetAllModuleReq :: RestEndpoint GetAllModuleReq where
    makeRequest reqBody@(GetAllModuleReq language) headers = defaultMakeRequestWithoutLogs GET (EP.getAllLmsModules language) headers reqBody Nothing
    encodeRequest req = defaultEncode req

derive instance genericGetAllModuleReq :: Generic GetAllModuleReq _
instance showGetAllModuleReq :: Show GetAllModuleReq where show = genericShow
instance standardGetAllModuleReq :: StandardEncode GetAllModuleReq where standardEncode (GetAllModuleReq req) = standardEncode req
instance decodeGetAllModuleReq :: Decode GetAllModuleReq where decode = defaultDecode
instance encodeGetAllModuleReq :: Encode GetAllModuleReq where encode = defaultEncode

derive instance genericLmsGetModuleRes :: Generic LmsGetModuleRes _
instance standardLmsGetModuleRes :: StandardEncode LmsGetModuleRes where standardEncode (LmsGetModuleRes body) = standardEncode body
instance showLmsGetModuleRes :: Show LmsGetModuleRes where show = genericShow
instance decodeLmsGetModuleRes :: Decode LmsGetModuleRes where decode = defaultDecode
instance encodeLmsGetModuleRes  :: Encode LmsGetModuleRes where encode = defaultEncode

derive instance genericLmsModuleRes :: Generic LmsModuleRes _
instance standardLmsModuleRes :: StandardEncode LmsModuleRes where standardEncode (LmsModuleRes body) = standardEncode body
instance showLmsModuleRes :: Show LmsModuleRes where show = genericShow
instance decodeLmsModuleRes :: Decode LmsModuleRes where decode = defaultDecode
instance encodeLmsModuleRes  :: Encode LmsModuleRes where encode = defaultEncode
derive instance newtypeLmsModuleRes :: Newtype LmsModuleRes _

derive instance genericLmsCategory :: Generic LmsCategory _
instance showLmsCategory :: Show LmsCategory where show = genericShow
instance decodeLmsCategory :: Decode LmsCategory where decode = defaultEnumDecode
instance encodeLmsCategory :: Encode LmsCategory where encode = defaultEnumEncode
instance eqLmsCategory :: Eq LmsCategory where eq = genericEq
instance standardEncodeLmsCategory :: StandardEncode LmsCategory where standardEncode _ = standardEncode {}

derive instance genericModuleCompletionStatus :: Generic ModuleCompletionStatus _
instance showModuleCompletionStatus :: Show ModuleCompletionStatus where show = genericShow
instance decodeModuleCompletionStatus :: Decode ModuleCompletionStatus where decode = defaultEnumDecode
instance encodeModuleCompletionStatus :: Encode ModuleCompletionStatus where encode = defaultEnumEncode
instance eqModuleCompletionStatus :: Eq ModuleCompletionStatus where eq = genericEq
instance standardEncodeModuleCompletionStatus :: StandardEncode ModuleCompletionStatus where standardEncode _ = standardEncode {}

derive instance genericModuleCompletionCriteria :: Generic ModuleCompletionCriteria _
instance showModuleCompletionCriteria :: Show ModuleCompletionCriteria where show = genericShow
instance decodeModuleCompletionCriteria :: Decode ModuleCompletionCriteria where decode = defaultDecode
instance encodeModuleCompletionCriteria :: Encode ModuleCompletionCriteria where encode = defaultEncode
instance standardEncodeModuleCompletionCriteria :: StandardEncode ModuleCompletionCriteria 
  where 
    standardEncode (ONLY_VIDEOS) = standardEncode {}
    standardEncode (VIDEOS_AND_QUIZ _) = standardEncode {}

derive instance genericLmsReward :: Generic LmsReward _
instance showLmsReward :: Show LmsReward where show = genericShow
instance decodeLmsReward :: Decode LmsReward where decode = defaultDecode
instance encodeLmsReward :: Encode LmsReward where encode = defaultEncode
instance standardEncodeLmsReward :: StandardEncode LmsReward 
  where 
    standardEncode (COIN _) = standardEncode {}
    standardEncode (BADGE _) = standardEncode {}

derive instance genericLmsBonus :: Generic LmsBonus _
instance showLmsBonus :: Show LmsBonus where show = genericShow
instance decodeLmsBonus :: Decode LmsBonus where decode = defaultDecode
instance encodeLmsBonus :: Encode LmsBonus where encode = defaultEncode
instance standardEncodeLmsBonus :: StandardEncode LmsBonus
  where
    standardEncode (BONUS_COIN _) = standardEncode {}
    standardEncode (BONUS_BADGE _) = standardEncode {}

---------------------------------------------------------- Get All Lms Videos --------------------------------------------------------------------------
data GetAllVideosReq = GetAllVideosReq String String

newtype LmsGetVideosRes = LmsGetVideosRes
 { completed :: Array LmsVideoRes
 , pending :: Array LmsVideoRes
 , quizStatus :: LmsEntityCompletionStatus
 , quizEnabled :: Boolean
 , selectedModuleInfo :: LmsTranslatedModuleInfoRes
 }

newtype LmsVideoRes = LmsVideoRes
 {  videoId :: String
  , moduleId :: String
  , language :: String
  , url :: String
  , shareLink :: Maybe String
  , ytVideoId :: String
  , duration :: Int
  , completedWatchCount :: Int
  , viewCount :: Int
  , thumbnailImage :: String
  , title :: String
  , description :: Maybe String
  , thresholdEnabled :: Boolean
  , startThresholdInPercentage :: Maybe Int
  , completedThresholdInPercentage :: Maybe Int
  , videoCompletionStatus :: LmsEntityCompletionStatus
  , attemptNumber :: Int
  , rank :: Int
  , sideButtonConfig :: Maybe (Array (Array CTA.ReelButtonConfig))
  , bottomButtonConfig :: Maybe (Array (Array CTA.ReelButtonConfig))
 }

data LmsEntityCompletionStatus = ENTITY_COMPLETED | ENTITY_INCOMPLETE | ENTITY_NOT_STARTED

newtype LmsTranslatedModuleInfoRes = LmsTranslatedModuleInfoRes
  { category :: LmsCategory,
    description :: String,
    duration :: Int,
    languagesAvailableForQuiz :: Array String,
    languagesAvailableForVideos :: Array String,
    moduleCompletionCriteria :: ModuleCompletionCriteria,
    moduleId :: String,
    name :: String,
    noOfVideos :: Int,
    rank :: Int,
    thumbnailImage :: String,
    variant :: Maybe String
  }

instance makeGetAllVideosReq :: RestEndpoint GetAllVideosReq where
    makeRequest reqBody@(GetAllVideosReq moduleId language) headers = defaultMakeRequestWithoutLogs GET (EP.getAllLmsVideos moduleId language) headers reqBody Nothing
    encodeRequest req = defaultEncode req

derive instance genericGetAllVideosReq :: Generic GetAllVideosReq _
instance showGetAllVideosReq :: Show GetAllVideosReq where show = genericShow
instance standardGetAllVideosReq :: StandardEncode GetAllVideosReq where standardEncode req = standardEncode req
instance decodeGetAllVideosReq :: Decode GetAllVideosReq where decode = defaultDecode
instance encodeGetAllVideosReq :: Encode GetAllVideosReq where encode = defaultEncode

derive instance genericLmsGetVideosRes :: Generic LmsGetVideosRes _
instance standardLmsGetVideosRes :: StandardEncode LmsGetVideosRes where standardEncode (LmsGetVideosRes body) = standardEncode body
instance showLmsGetVideosRes :: Show LmsGetVideosRes where show = genericShow
instance decodeLmsGetVideosRes :: Decode LmsGetVideosRes where decode = defaultDecode
instance encodeLmsGetVideosRes  :: Encode LmsGetVideosRes where encode = defaultEncode

derive instance genericLmsVideoRes :: Generic LmsVideoRes _
instance standardLmsVideoRes :: StandardEncode LmsVideoRes where standardEncode (LmsVideoRes body) = standardEncode body
instance showLmsVideoRes :: Show LmsVideoRes where show = genericShow
instance decodeLmsVideoRes :: Decode LmsVideoRes where decode = defaultDecode
instance encodeLmsVideoRes  :: Encode LmsVideoRes where encode = defaultEncode


derive instance genericLmsEntityCompletionStatus :: Generic LmsEntityCompletionStatus _
instance showLmsEntityCompletionStatus :: Show LmsEntityCompletionStatus where show = genericShow
instance decodeLmsEntityCompletionStatus :: Decode LmsEntityCompletionStatus where decode = defaultEnumDecode
instance encodeLmsEntityCompletionStatus :: Encode LmsEntityCompletionStatus where encode = defaultEnumEncode
instance eqLmsEntityCompletionStatus :: Eq LmsEntityCompletionStatus where eq = genericEq
instance standardEncodeLmsEntityCompletionStatus :: StandardEncode LmsEntityCompletionStatus where standardEncode _ = standardEncode {}

derive instance genericeLmsTranslatedModuleInfoRes :: Generic LmsTranslatedModuleInfoRes _
derive instance newtypeLmsTranslatedModuleInfoRes :: Newtype LmsTranslatedModuleInfoRes _
instance showLmsTranslatedModuleInfoRes :: Show LmsTranslatedModuleInfoRes where show = genericShow
instance decodeLmsTranslatedModuleInfoRes :: Decode LmsTranslatedModuleInfoRes where decode = defaultDecode
instance encodeLmsTranslatedModuleInfoRes :: Encode LmsTranslatedModuleInfoRes where encode = defaultEncode
instance standardEncodeLmsTranslatedModuleInfoRes :: StandardEncode LmsTranslatedModuleInfoRes where standardEncode (LmsTranslatedModuleInfoRes body) = standardEncode body

---------------------------------------------------------- Get All Lms Questions --------------------------------------------------------------------------

data GetAllQuestionsReq = GetAllQuestionsReq String String

-- newtype LmsGetQuizRes = LmsGetQuizRes (Array LmsQuestionRes)
newtype LmsGetQuizRes = LmsGetQuizRes {
  questions :: Array LmsQuestionRes,
  selectedModuleInfo :: LmsTranslatedModuleInfoRes
}

newtype LmsQuestionRes = LmsQuestionRes
 {  questionId :: String
  , moduleId :: String
  , language :: String
  , question :: QuizQuestion
  , options :: QuizOptions
  , previousHistory :: Maybe LmsQuizHistory
  }

data QuizQuestion = TextQuestion String | ImageQuestion String String Int Int

data QuizOptions = SingleSelect Options | MultiSelect Options

newtype Options = Options
  { options :: Array OptionEntity
  }

newtype OptionEntity = OptionEntity
  { isCorrect :: Boolean,
    option :: SingleOption,
    optionId :: String
  }

data SingleOption = TextOption String | SingleLineImage String Int Int | TwoColumnImage String Int Int | TwoColumnOption String

newtype LmsQuizHistory = LmsQuizHistory
  { attemptNumber ::Int,
    selectedOptions :: Array String,
    status :: LmsQuestionStatus
  }

data LmsQuestionStatus = CORRECT | INCORRECT

instance makeGetAllQuestionsReq :: RestEndpoint GetAllQuestionsReq where
    makeRequest reqBody@(GetAllQuestionsReq  moduleId language) headers = defaultMakeRequestWithoutLogs GET (EP.getAllLmsQuestions moduleId language) headers reqBody Nothing
    encodeRequest req = defaultEncode req

derive instance genericGetAllQuestionsReq :: Generic GetAllQuestionsReq _
instance showGetAllQuestionsReq :: Show GetAllQuestionsReq where show = genericShow
instance standardGetAllQuestionsReq :: StandardEncode GetAllQuestionsReq where standardEncode req = standardEncode req
instance decodeGetAllQuestionsReq :: Decode GetAllQuestionsReq where decode = defaultDecode
instance encodeGetAllQuestionsReq :: Encode GetAllQuestionsReq where encode = defaultEncode

derive instance genericLmsGetQuizRes :: Generic LmsGetQuizRes _
instance showLmsGetQuizRes :: Show LmsGetQuizRes where show = genericShow
instance standardLmsGetQuizRes :: StandardEncode LmsGetQuizRes where standardEncode (LmsGetQuizRes req) = standardEncode req
instance decodeLmsGetQuizRes :: Decode LmsGetQuizRes where decode = defaultDecode
instance encodeLmsGetQuizRes :: Encode LmsGetQuizRes where encode = defaultEncode

derive instance genericLmsQuestionRes :: Generic LmsQuestionRes _
instance showLmsQuestionRes :: Show LmsQuestionRes where show = genericShow
instance standardLmsQuestionRes :: StandardEncode LmsQuestionRes where standardEncode (LmsQuestionRes req) = standardEncode req
instance decodeLmsQuestionRes :: Decode LmsQuestionRes where decode = defaultDecode
instance encodeLmsQuestionRes :: Encode LmsQuestionRes where encode = defaultEncode

derive instance genericQuizQuestion :: Generic QuizQuestion _
instance showQuizQuestion :: Show QuizQuestion where show = genericShow
instance decodeQuizQuestion :: Decode QuizQuestion where decode = defaultDecode
instance encodeQuizQuestion :: Encode QuizQuestion where encode = defaultEncode
instance standardEncodeQuizQuestion :: StandardEncode QuizQuestion 
  where 
    standardEncode (TextQuestion _) = standardEncode {}
    standardEncode (ImageQuestion _ _ _ _) = standardEncode {}

derive instance genericQuizOptions :: Generic QuizOptions _
instance showQuizOptions :: Show QuizOptions where show = genericShow
instance decodeQuizOptions :: Decode QuizOptions where decode = defaultDecode
instance encodeQuizOptions :: Encode QuizOptions where encode = defaultEncode
instance standardEncodeQuizOptions :: StandardEncode QuizOptions 
  where 
    standardEncode (SingleSelect _) = standardEncode {}
    standardEncode (MultiSelect _) = standardEncode {}

derive instance genericOptions :: Generic Options _
instance showOptions :: Show Options where show = genericShow
instance decodeOptions :: Decode Options where decode = defaultDecode
instance encodeOptions :: Encode Options where encode = defaultEncode
instance eqOptions :: Eq Options where eq = genericEq
instance standardEncodeOptions :: StandardEncode Options where standardEncode _ = standardEncode {}

derive instance genericOptionEntity :: Generic OptionEntity _
instance showOptionEntity :: Show OptionEntity where show = genericShow
instance decodeOptionEntity :: Decode OptionEntity where decode = defaultDecode
instance encodeOptionEntity :: Encode OptionEntity where encode = defaultEncode
instance eqOptionEntity :: Eq OptionEntity where eq = genericEq
instance standardEncodeOptionEntity :: StandardEncode OptionEntity where standardEncode _ = standardEncode {}

derive instance genericSingleOption :: Generic SingleOption _
instance showSingleOption :: Show SingleOption where show = genericShow
instance decodeSingleOption :: Decode SingleOption where decode = defaultDecode
instance encodeSingleOption :: Encode SingleOption where encode = defaultEncode
instance eqSingleOption :: Eq SingleOption where eq = genericEq
instance standardEncodeSingleOption :: StandardEncode SingleOption 
  where 
    standardEncode (TextOption _) = standardEncode {}
    standardEncode (SingleLineImage _ _ _) = standardEncode {}
    standardEncode (TwoColumnImage _ _ _) = standardEncode {}
    standardEncode (TwoColumnOption _) = standardEncode {}

derive instance genericLmsQuizHistory :: Generic LmsQuizHistory _
instance showLmsQuizHistory :: Show LmsQuizHistory where show = genericShow
instance standardLmsQuizHistory :: StandardEncode LmsQuizHistory where standardEncode (LmsQuizHistory req) = standardEncode req
instance decodeLmsQuizHistory :: Decode LmsQuizHistory where decode = defaultDecode
instance eqLmsQuizHistory :: Eq LmsQuizHistory where eq = genericEq
instance encodeLmsQuizHistory :: Encode LmsQuizHistory where encode = defaultEncode

derive instance genericLmsQuestionStatus :: Generic LmsQuestionStatus _
instance showLmsQuestionStatus :: Show LmsQuestionStatus where show = genericShow
instance decodeLmsQuestionStatus :: Decode LmsQuestionStatus where decode = defaultEnumDecode
instance encodeLmsQuestionStatus :: Encode LmsQuestionStatus where encode = defaultEnumEncode
instance eqLmsQuestionStatus :: Eq LmsQuestionStatus where eq = genericEq
instance standardEncodeLmsQuestionStatus :: StandardEncode LmsQuestionStatus where standardEncode _ = standardEncode {}


---------------------------------------------------------- Lms Video marking as completed or started  And Question confirmation  --------------------------------------------------------------------------

newtype StartVideoUpdateAPIReq = StartVideoUpdateAPIReq
 { moduleId :: String,
   videoId :: String,
   language :: String
 }

newtype CompletedVideoUpdateAPIReq = CompletedVideoUpdateAPIReq
 { moduleId :: String,
   videoId :: String,
   language :: String
 }

newtype StartVideoUpdateRes = StartVideoUpdateRes ApiSuccessResult
newtype CompletedVideoUpdateRes = CompletedVideoUpdateRes ApiSuccessResult

instance makeStartVideoUpdateAPIReq :: RestEndpoint StartVideoUpdateAPIReq where
    makeRequest reqBody headers = defaultMakeRequestWithoutLogs POST (EP.markVideoAsStarted "") headers reqBody Nothing
    encodeRequest req = defaultEncode req

instance makeCompletedVideoUpdateAPIReq :: RestEndpoint CompletedVideoUpdateAPIReq where
    makeRequest reqBody headers = defaultMakeRequestWithoutLogs POST (EP.markVideoAsCompleted "") headers reqBody Nothing
    encodeRequest req = defaultEncode req

derive instance genericStartVideoUpdateAPIReq :: Generic StartVideoUpdateAPIReq _
instance showStartVideoUpdateAPIReq :: Show StartVideoUpdateAPIReq where show = genericShow
instance standardStartVideoUpdateAPIReq :: StandardEncode StartVideoUpdateAPIReq where standardEncode (StartVideoUpdateAPIReq req) = standardEncode req
instance decodeStartVideoUpdateAPIReq :: Decode StartVideoUpdateAPIReq where decode = defaultDecode
instance encodeStartVideoUpdateAPIReq :: Encode StartVideoUpdateAPIReq where encode = defaultEncode

derive instance genericCompletedVideoUpdateAPIReq :: Generic CompletedVideoUpdateAPIReq _
instance showCompletedVideoUpdateAPIReq :: Show CompletedVideoUpdateAPIReq where show = genericShow
instance standardCompletedVideoUpdateAPIReq :: StandardEncode CompletedVideoUpdateAPIReq where standardEncode (CompletedVideoUpdateAPIReq req) = standardEncode req
instance decodeCompletedVideoUpdateAPIReq :: Decode CompletedVideoUpdateAPIReq where decode = defaultDecode
instance encodeCompletedVideoUpdateAPIReq :: Encode CompletedVideoUpdateAPIReq where encode = defaultEncode


newtype QuestionConfirmReq = QuestionConfirmReq
 {  questionId :: String
  , moduleId :: String
  , language :: String
  , selectedOption :: SelectedOption
 }

data SelectedOption = SingleSelectedOption String | MultiSelectedOption (Array String)

newtype QuestionConfirmRes = QuestionConfirmRes
  { validation :: QuestionValidation
  , validationRes :: SelectedOptionValidation
  }

data QuestionValidation = CORRECT_ANSWER | INCORRECT_ANSWER

data SelectedOptionValidation = SingleSelectedOptionValidation ValidationResult | MultiSelectedOptionValidation (Array ValidationResult)

newtype ValidationResult = ValidationResult
 { id :: String,
   isCorrect :: Boolean
 }

instance makeQuestionConfirmReq :: RestEndpoint QuestionConfirmReq where
    makeRequest reqBody headers = defaultMakeRequestWithoutLogs POST (EP.confirmQuestion "") headers reqBody Nothing
    encodeRequest req = defaultEncode req

derive instance genericQuestionConfirmReq :: Generic QuestionConfirmReq _
instance showQuestionConfirmReq :: Show QuestionConfirmReq where show = genericShow
instance standardQuestionConfirmReq :: StandardEncode QuestionConfirmReq where standardEncode (QuestionConfirmReq req) = standardEncode req
instance decodeQuestionConfirmReq :: Decode QuestionConfirmReq where decode = defaultDecode
instance encodeQuestionConfirmReq :: Encode QuestionConfirmReq where encode = defaultEncode

derive instance genericQuestionConfirmRes :: Generic QuestionConfirmRes _
instance showQuestionConfirmRes :: Show QuestionConfirmRes where show = genericShow
instance standardQuestionConfirmRes :: StandardEncode QuestionConfirmRes where standardEncode (QuestionConfirmRes req) = standardEncode req
instance decodeQuestionConfirmRes :: Decode QuestionConfirmRes where decode = defaultDecode
instance encodeQuestionConfirmRes :: Encode QuestionConfirmRes where encode = defaultEncode

derive instance genericSelectedOption :: Generic SelectedOption _
instance showSelectedOption :: Show SelectedOption where show = genericShow
instance decodeSelectedOption :: Decode SelectedOption where decode = defaultDecode
instance encodeSelectedOption :: Encode SelectedOption where encode = defaultEncode
instance eqSelectedOption :: Eq SelectedOption where eq = genericEq
instance standardEncodeSelectedOption :: StandardEncode SelectedOption 
  where 
    standardEncode (SingleSelectedOption _) = standardEncode {}
    standardEncode (MultiSelectedOption _) = standardEncode {}

derive instance genericQuestionValidation :: Generic QuestionValidation _
instance showQuestionValidation :: Show QuestionValidation where show = genericShow
instance decodeQuestionValidation :: Decode QuestionValidation where decode = defaultEnumDecode
instance encodeQuestionValidation :: Encode QuestionValidation where encode = defaultEnumEncode
instance eqQuestionValidation :: Eq QuestionValidation where eq = genericEq
instance standardEncodeQuestionValidation :: StandardEncode QuestionValidation where standardEncode _ = standardEncode {}

derive instance genericSelectedOptionValidation :: Generic SelectedOptionValidation _
instance showSelectedOptionValidation :: Show SelectedOptionValidation where show = genericShow
instance decodeSelectedOptionValidation :: Decode SelectedOptionValidation where decode = defaultDecode
instance encodeSelectedOptionValidation :: Encode SelectedOptionValidation where encode = defaultEncode
instance eqSelectedOptionValidation :: Eq SelectedOptionValidation where eq = genericEq
instance standardEncodeSelectedOptionValidation :: StandardEncode SelectedOptionValidation 
  where 
    standardEncode (SingleSelectedOptionValidation _) = standardEncode {}
    standardEncode (MultiSelectedOptionValidation _) = standardEncode {}

derive instance genericValidationResult :: Generic ValidationResult _
instance showValidationResult :: Show ValidationResult where show = genericShow
instance standardValidationResult :: StandardEncode ValidationResult where standardEncode (ValidationResult req) = standardEncode req
instance encodeValidationResult :: Encode ValidationResult where encode = defaultEncode
instance decodeValidationResult :: Decode ValidationResult where decode = defaultDecode
instance eqValidationResult :: Eq ValidationResult where eq = genericEq

---------------------------------------------------- api for getting reels data ---------------------------------------------------------------------------

data GetAllReelsVideosReq = GetAllReelsVideosReq String String

newtype ReelsResp = ReelsResp
  { reels :: Array ReelsData
  }

newtype ReelsData = ReelsData
  { bottomButtonConfig :: Array ReelRowButtonConfig,
    carouselBigImageUrl :: Maybe String,
    carouselSmallImageUrl :: Maybe String,
    carouselTextColor :: Maybe String,
    carouselTextString :: Maybe String,
    description :: Maybe String,
    id :: String,
    language :: String,
    rank :: Int,
    reelKey :: String,
    shareLink :: Maybe String,
    sideButtonConfig :: Array ReelRowButtonConfig,
    thresholdConfig :: Maybe ReelVideoThresholdConfig,
    thumbnailImageUrl :: Maybe String,
    title :: Maybe String,
    videoUrl :: String,
    createdAt :: String,
    updatedAt :: String
  }

newtype ReelButtonConfig = ReelButtonConfig
  { actions :: Maybe (Array String),
    activeIndex :: Maybe String,
    activeIndexHeight :: Maybe Int,
    activeIndexWidth :: Maybe Int,
    buttonColor :: Maybe String,
    cornerRadius :: Maybe Int,
    inActiveIndex :: Maybe String,
    inActiveIndexHeight :: Maybe Int,
    inActiveIndexWidth :: Maybe Int,
    prefixImage :: Maybe String,
    prefixImageHeight :: Maybe Int,
    prefixImageWidth :: Maybe Int,
    shareLink :: Maybe String,
    shareText :: Maybe String,
    suffixImage :: Maybe String,
    suffixImageHeight :: Maybe Int,
    suffixImageWidth :: Maybe Int,
    text :: Maybe String,
    textColor :: Maybe String,
    textSize :: Maybe Int
  }

newtype ReelRowButtonConfig = ReelRowButtonConfig
  { row :: Array ReelButtonConfig
  }

newtype ReelVideoThresholdConfig = ReelVideoThresholdConfig
  { endThreshold :: Maybe Int,
    isEndThresholdEnabled :: Maybe Boolean,
    isStartThresholdEnabled :: Maybe Boolean,
    isThresholdEnabled :: Maybe Boolean,
    sendCallbackAfterEverySecondEnabled :: Maybe Boolean,
    startThreshold :: Maybe Int
  }

instance makeGetAllReelsVideosReq :: RestEndpoint GetAllReelsVideosReq where
    makeRequest reqBody@(GetAllReelsVideosReq  reelsKey language) headers = defaultMakeRequestWithoutLogs GET (EP.getReelsData reelsKey language) headers reqBody Nothing
    encodeRequest req = defaultEncode req

derive instance genericGetAllReelsVideosReq :: Generic GetAllReelsVideosReq _
instance showGetAllReelsVideosReq :: Show GetAllReelsVideosReq where show = genericShow
instance standardGetAllReelsVideosReq :: StandardEncode GetAllReelsVideosReq where standardEncode req = standardEncode req
instance decodeGetAllReelsVideosReq :: Decode GetAllReelsVideosReq where decode = defaultDecode
instance encodeGetAllReelsVideosReq :: Encode GetAllReelsVideosReq where encode = defaultEncode

derive instance genericReelsResp :: Generic ReelsResp _
instance showReelsResp :: Show ReelsResp where show = genericShow
instance standardReelsResp :: StandardEncode ReelsResp where standardEncode (ReelsResp res) = standardEncode res
instance decodeReelsResp :: Decode ReelsResp where decode = defaultDecode
instance encodeReelsResp :: Encode ReelsResp where encode = defaultEncode

derive instance genericReelsData :: Generic ReelsData _
instance showReelsData :: Show ReelsData where show = genericShow
instance standardReelsData :: StandardEncode ReelsData where standardEncode (ReelsData res) = standardEncode res
instance decodeReelsData :: Decode ReelsData where decode = defaultDecode
instance encodeReelsData :: Encode ReelsData where encode = defaultEncode

derive instance genericReelButtonConfig :: Generic ReelButtonConfig _
instance showReelButtonConfig :: Show ReelButtonConfig where show = genericShow
instance standardReelButtonConfig :: StandardEncode ReelButtonConfig where standardEncode (ReelButtonConfig res) = standardEncode res
instance decodeReelButtonConfig :: Decode ReelButtonConfig where decode = defaultDecode
instance encodeReelButtonConfig :: Encode ReelButtonConfig where encode = defaultEncode

derive instance genericReelRowButtonConfig :: Generic ReelRowButtonConfig _
instance showReelRowButtonConfig :: Show ReelRowButtonConfig where show = genericShow
instance standardReelRowButtonConfig :: StandardEncode ReelRowButtonConfig where standardEncode (ReelRowButtonConfig res) = standardEncode res
instance decodeReelRowButtonConfig :: Decode ReelRowButtonConfig where decode = defaultDecode
instance encodeReelRowButtonConfig :: Encode ReelRowButtonConfig where encode = defaultEncode

derive instance genericReelVideoThresholdConfig :: Generic ReelVideoThresholdConfig _
instance showReelVideoThresholdConfig :: Show ReelVideoThresholdConfig where show = genericShow
instance standardReelVideoThresholdConfig :: StandardEncode ReelVideoThresholdConfig where standardEncode (ReelVideoThresholdConfig res) = standardEncode res
instance decodeReelVideoThresholdConfig :: Decode ReelVideoThresholdConfig where decode = defaultDecode
instance encodeReelVideoThresholdConfig :: Encode ReelVideoThresholdConfig where encode = defaultEncode

---------------------------------------------------- Dummy Ride Request ---------------------------------------------------------------------------

data DummyRideRequestReq = DummyRideRequestReq String


instance makeDummyRideRequestReq :: RestEndpoint DummyRideRequestReq where
    makeRequest reqBody headers = defaultMakeRequestWithoutLogs GET (EP.dummyRideRequest "") headers reqBody Nothing
    encodeRequest req = standardEncode req


derive instance genericDummyRideRequestReq :: Generic DummyRideRequestReq _
instance showDummyRideRequestReq :: Show DummyRideRequestReq where show = genericShow
instance decodeDummyRideRequestReq :: Decode DummyRideRequestReq where decode = defaultDecode
instance encodeRDummyRideRequestReq :: Encode DummyRideRequestReq where encode = defaultEncode
instance standardDummyRideRequestReq :: StandardEncode DummyRideRequestReq where standardEncode body = standardEncode {}

--------------------------------------------------------- specialLocation list --------------------------------------------------------------------------
data SpecialLocationFullReq = SpecialLocationFullReq

data SpecialLocationFullRes = SpecialLocationFullRes (Array SpecialLocationFull)

newtype SpecialLocationFull = SpecialLocationFull
  { locationName :: String
  , category :: String
  , merchantOperatingCityId :: Maybe String
  , gatesInfo :: Array GateInfoFull
  , gates :: Array GatesInfo
  , geoJson :: Maybe String    
  }

newtype GatesInfo = GatesInfo {
  name :: String,
  point :: LatLong,
  address :: Maybe String
}

newtype GateInfoFull = GateInfoFull {
  address :: Maybe String,
  canQueueUpOnGate :: Maybe Boolean,
  defaultDriverExtra :: Maybe Int,
  geoJson :: Maybe String,
  name :: String,
  point :: LatLong
}

instance makeSpecialLocationFullReq :: RestEndpoint SpecialLocationFullReq where
  makeRequest reqBody headers = defaultMakeRequestWithoutLogs GET (EP.specialLocationList "") headers reqBody Nothing
  encodeRequest req = standardEncode req

derive instance genericSpecialLocationFullReq :: Generic SpecialLocationFullReq _
instance standardEncodeSpecialLocationFullReq :: StandardEncode SpecialLocationFullReq where standardEncode (SpecialLocationFullReq) = standardEncode {}
instance showSpecialLocationFullReq :: Show SpecialLocationFullReq where show = genericShow
instance decodeSpecialLocationFullReq :: Decode SpecialLocationFullReq where decode = defaultDecode
instance encodeSpecialLocationFullReq  :: Encode SpecialLocationFullReq where encode = defaultEncode

derive instance genericSpecialLocationFullRes :: Generic SpecialLocationFullRes _
instance standardEncodeSpecialLocationFullRes :: StandardEncode SpecialLocationFullRes where standardEncode (SpecialLocationFullRes res) = standardEncode res
instance showSpecialLocationFullRes :: Show SpecialLocationFullRes where show = genericShow
instance decodeSpecialLocationFullRes :: Decode SpecialLocationFullRes where decode = defaultDecode
instance encodeSpecialLocationFullRes  :: Encode SpecialLocationFullRes where encode = defaultEncode

derive instance genericSpecialLocationFull :: Generic SpecialLocationFull _
derive instance newtypeSpecialLocationFull :: Newtype SpecialLocationFull _
instance standardEncodeSpecialLocationFull :: StandardEncode SpecialLocationFull where standardEncode (SpecialLocationFull req) = standardEncode req
instance showSpecialLocationFull :: Show SpecialLocationFull where show = genericShow
instance decodeSpecialLocationFull :: Decode SpecialLocationFull where decode = defaultDecode
instance encodeSpecialLocationFull :: Encode SpecialLocationFull where encode = defaultEncode

derive instance genericGatesInfo :: Generic GatesInfo _
derive instance newtypeGatesInfo:: Newtype GatesInfo _
instance standardEncodeGatesInfo :: StandardEncode GatesInfo where standardEncode (GatesInfo req) = standardEncode req
instance showGatesInfo :: Show GatesInfo where show = genericShow
instance decodeGatesInfo :: Decode GatesInfo where decode = defaultDecode
instance encodeGatesInfo :: Encode GatesInfo where encode = defaultEncode

derive instance genericGateInfoFull :: Generic GateInfoFull _
derive instance newtypeGateInfoFull:: Newtype GateInfoFull _
instance standardEncodeGateInfoFull :: StandardEncode GateInfoFull where standardEncode (GateInfoFull req) = standardEncode req
instance showGateInfoFull :: Show GateInfoFull where show = genericShow
instance decodeGateInfoFull :: Decode GateInfoFull where decode = defaultDecode
instance encodeGateInfoFull :: Encode GateInfoFull where encode = defaultEncode


data OnboardingDocsReq = OnboardingDocsReq Boolean Boolean

newtype OnboardingDocsRes = OnboardingDocsRes {
  autos :: Maybe (Array OnboardingDoc),
  cabs :: Maybe (Array OnboardingDoc),
  bikes :: Maybe (Array OnboardingDoc),
  ambulances :: Maybe (Array OnboardingDoc),
  trucks :: Maybe (Array OnboardingDoc),
  bus :: Maybe (Array OnboardingDoc)
}

newtype OnboardingDoc = OnboardingDoc {
  documentType :: String,
  title :: String,
  description :: Maybe String,
  isMandatory :: Boolean,
  isDisabled :: Boolean,
  disableWarning :: Maybe String,
  isHidden :: Boolean,
  dependencyDocumentType :: Array String,
  rcNumberPrefixList :: Array String
}

instance makeOnboardingDocsReq :: RestEndpoint OnboardingDocsReq where
  makeRequest reqBody@(OnboardingDocsReq makeAadhaarSelfieMandatory onlyVehicle) headers = defaultMakeRequestWithoutLogs GET (EP.onBoardingConfigs makeAadhaarSelfieMandatory onlyVehicle) headers reqBody Nothing
  encodeRequest req = defaultEncode req

derive instance genericOnboardingDocsReq :: Generic OnboardingDocsReq _
instance standardEncodeOnboardingDocsReq :: StandardEncode OnboardingDocsReq where standardEncode req = standardEncode req
instance showOnboardingDocsReq :: Show OnboardingDocsReq where show = genericShow
instance decodeOnboardingDocsReq :: Decode OnboardingDocsReq where decode = defaultDecode
instance encodeOnboardingDocsReq  :: Encode OnboardingDocsReq where encode = defaultEncode

derive instance genericOnboardingDocsRes :: Generic OnboardingDocsRes _
instance standardEncodeOnboardingDocsRes :: StandardEncode OnboardingDocsRes where standardEncode (OnboardingDocsRes res) = standardEncode res
instance showOnboardingDocsRes :: Show OnboardingDocsRes where show = genericShow
instance decodeOnboardingDocsRes :: Decode OnboardingDocsRes where decode = defaultDecode
instance encodeOnboardingDocsRes  :: Encode OnboardingDocsRes where encode = defaultEncode

derive instance genericOnboardingDoc :: Generic OnboardingDoc _
instance standardEncodeOnboardingDoc :: StandardEncode OnboardingDoc where standardEncode (OnboardingDoc res) = standardEncode res
instance showOnboardingDoc :: Show OnboardingDoc where show = genericShow
instance decodeOnboardingDoc :: Decode OnboardingDoc where decode = defaultDecode
instance encodeOnboardingDoc  :: Encode OnboardingDoc where encode = defaultEncode

data ServiceTierType
  = COMFY
  | ECO
  | PREMIUM
  | SUV_TIER
  | AUTO_RICKSHAW
  | HATCHBACK_TIER
  | SEDAN_TIER
  | TAXI
  | TAXI_PLUS
  | RENTALS
  | LOCAL
  | INTERCITY
  | BIKE_TIER
  | SUV_PLUS_TIER
  | DELIVERY_BIKE
  | AMBULANCE_TAXI_TIER
  | AMBULANCE_TAXI_OXY_TIER
  | AMBULANCE_AC_TIER
  | AMBULANCE_AC_OXY_TIER
  | AMBULANCE_VENTILATOR
  | EV_AUTO_RICKSHAW
  | HERITAGE_CAB_TIER

data AirConditionedRestrictionType
  = ToggleAllowed
  | ToggleNotAllowed
  | NoRestriction

data DriverVehicleServiceTierReq = DriverVehicleServiceTierReq

data UpdateDriverVehicleServiceTierReq = UpdateDriverVehicleServiceTierReq {
  tiers :: Array DriverVehicleServiceTier,
  airConditioned :: Maybe AirConditionedTier
}

data UpdateDriverVehicleServiceTierResp = UpdateDriverVehicleServiceTierResp String

newtype DriverVehicleServiceTierResponse = DriverVehicleServiceTierResponse {
  tiers :: Array DriverVehicleServiceTier,
  airConditioned :: Maybe AirConditionedTier,
  canSwitchToInterCity :: Maybe Boolean,
  canSwitchToIntraCity :: Maybe Boolean,
  canSwitchToRental :: Maybe Boolean
}

newtype AirConditionedTier = AirConditionedTier {
  isWorking :: Boolean,
  restrictionMessage :: Maybe String,
  usageRestrictionType :: AirConditionedRestrictionType
}

newtype DriverVehicleServiceTier = DriverVehicleServiceTier {
  airConditioned :: Maybe Number,
  driverRating :: Maybe Number,
  isDefault :: Boolean,
  isSelected :: Boolean,
  longDescription :: Maybe String,
  luggageCapacity :: Maybe Int,
  name :: String,
  seatingCapacity :: Maybe Int,
  serviceTierType :: ServiceTierType,
  shortDescription :: Maybe String,
  vehicleRating :: Maybe Number,
  isUsageRestricted :: Maybe Boolean,
  priority :: Maybe Int
}

derive instance genericServiceTierType :: Generic ServiceTierType _
instance showServiceTierType :: Show ServiceTierType where show = genericShow
instance decodeServiceTierType :: Decode ServiceTierType
  where decode body = case unsafeFromForeign body of
                  "COMFY"        -> except $ Right COMFY
                  "ECO"          -> except $ Right ECO
                  "PREMIUM"      -> except $ Right PREMIUM
                  "SUV"          -> except $ Right SUV_TIER
                  "AUTO_RICKSHAW"-> except $ Right AUTO_RICKSHAW
                  "HATCHBACK"    -> except $ Right HATCHBACK_TIER
                  "SEDAN"        -> except $ Right SEDAN_TIER
                  "TAXI"         -> except $ Right TAXI
                  "TAXI_PLUS"    -> except $ Right TAXI_PLUS
                  "RENTALS"      -> except $ Right RENTALS
                  "INTERCITY"    -> except $ Right INTERCITY
                  "LOCAL"        -> except $ Right LOCAL
                  "BIKE"         -> except $ Right BIKE_TIER
                  "SUV_PLUS"     -> except $ Right SUV_PLUS_TIER
                  "DELIVERY_BIKE" -> except $ Right DELIVERY_BIKE
                  "AMBULANCE_TAXI" -> except $ Right AMBULANCE_TAXI_TIER
                  "AMBULANCE_TAXI_OXY" -> except $ Right AMBULANCE_TAXI_OXY_TIER
                  "AMBULANCE_AC" -> except $ Right AMBULANCE_AC_TIER
                  "AMBULANCE_AC_OXY" -> except $ Right AMBULANCE_AC_OXY_TIER
                  "AMBULANCE_VENTILATOR" -> except $ Right AMBULANCE_VENTILATOR
                  "EV_AUTO_RICKSHAW" -> except $ Right EV_AUTO_RICKSHAW
                  "HERITAGE_CAB"  -> except $ Right HERITAGE_CAB_TIER
                  _              -> except $ Right COMFY
instance encodeServiceTierType :: Encode ServiceTierType where encode = defaultEnumEncode
instance eqServiceTierType :: Eq ServiceTierType where eq = genericEq
instance standardEncodeServiceTierType :: StandardEncode ServiceTierType
  where
    standardEncode COMFY = standardEncode "COMFY"
    standardEncode ECO = standardEncode "ECO"
    standardEncode PREMIUM = standardEncode "PREMIUM"
    standardEncode SUV_TIER = standardEncode "SUV"
    standardEncode AUTO_RICKSHAW = standardEncode "AUTO_RICKSHAW"
    standardEncode HATCHBACK_TIER = standardEncode "HATCHBACK"
    standardEncode SEDAN_TIER = standardEncode "SEDAN"
    standardEncode TAXI = standardEncode "TAXI"
    standardEncode TAXI_PLUS = standardEncode "TAXI_PLUS"
    standardEncode BIKE_TIER = standardEncode "BIKE"
    standardEncode DELIVERY_BIKE = standardEncode "DELIVERY_BIKE"
    standardEncode RENTALS = standardEncode "RENTALS"
    standardEncode LOCAL = standardEncode "LOCAL"
    standardEncode INTERCITY = standardEncode "INTERCITY"
    standardEncode SUV_PLUS_TIER = standardEncode "SUV_PLUS"
    standardEncode AMBULANCE_TAXI_TIER = standardEncode "AMBULANCE_TAXI"
    standardEncode AMBULANCE_TAXI_OXY_TIER = standardEncode "AMBULANCE_TAXI_OXY"
    standardEncode AMBULANCE_AC_TIER = standardEncode "AMBULANCE_AC"
    standardEncode AMBULANCE_AC_OXY_TIER = standardEncode "AMBULANCE_AC_OXY"
    standardEncode AMBULANCE_VENTILATOR = standardEncode "AMBULANCE_VENTILATOR"
    standardEncode EV_AUTO_RICKSHAW = standardEncode "EV_AUTO_RICKSHAW"
    standardEncode HERITAGE_CAB_TIER = standardEncode "HERITAGE_CAB"

derive instance genericAirConditionedRestrictionType :: Generic AirConditionedRestrictionType _
instance showAirConditionedRestrictionType :: Show AirConditionedRestrictionType where show = genericShow
instance decodeAirConditionedRestrictionType :: Decode AirConditionedRestrictionType
  where decode body = case unsafeFromForeign body of
                  "ToggleAllowed"        -> except $ Right ToggleAllowed
                  "ToggleNotAllowed"     -> except $ Right ToggleNotAllowed
                  "NoRestriction"        -> except $ Right NoRestriction
                  _                      -> except $ Right NoRestriction
instance encodeAirConditionedRestrictionType :: Encode AirConditionedRestrictionType where encode = defaultEnumEncode
instance eqAirConditionedRestrictionType :: Eq AirConditionedRestrictionType where eq = genericEq
instance standardEncodeAirConditionedRestrictionType :: StandardEncode AirConditionedRestrictionType
  where
    standardEncode ToggleAllowed = standardEncode "ToggleAllowed"
    standardEncode ToggleNotAllowed = standardEncode "ToggleNotAllowed"
    standardEncode NoRestriction = standardEncode "NoRestriction"

derive instance genericDriverVehicleServiceTierReq :: Generic DriverVehicleServiceTierReq _
instance standardEncodeDriverVehicleServiceTierReq :: StandardEncode DriverVehicleServiceTierReq where standardEncode (DriverVehicleServiceTierReq) = standardEncode {}
instance showDriverVehicleServiceTierReq :: Show DriverVehicleServiceTierReq where show = genericShow
instance decodeDriverVehicleServiceTierReq :: Decode DriverVehicleServiceTierReq where decode = defaultDecode
instance encodeDriverVehicleServiceTierReq  :: Encode DriverVehicleServiceTierReq where encode = defaultEncode

derive instance genericAirConditionedTier :: Generic AirConditionedTier _
instance standardEncodeAirConditionedTier :: StandardEncode AirConditionedTier where standardEncode (AirConditionedTier res) = standardEncode res
instance showAirConditionedTier :: Show AirConditionedTier where show = genericShow
instance decodeAirConditionedTier :: Decode AirConditionedTier where decode = defaultDecode
instance encodeAirConditionedTier  :: Encode AirConditionedTier where encode = defaultEncode

derive instance genericDriverVehicleServiceTierResponse :: Generic DriverVehicleServiceTierResponse _
instance standardEncodeDriverVehicleServiceTierResponse :: StandardEncode DriverVehicleServiceTierResponse where standardEncode (DriverVehicleServiceTierResponse res) = standardEncode res
instance showDriverVehicleServiceTierResponse :: Show DriverVehicleServiceTierResponse where show = genericShow
instance decodeDriverVehicleServiceTierResponse :: Decode DriverVehicleServiceTierResponse where decode = defaultDecode
instance encodeDriverVehicleServiceTierResponse  :: Encode DriverVehicleServiceTierResponse where encode = defaultEncode

derive instance genericUpdateDriverVehicleServiceTierReq :: Generic UpdateDriverVehicleServiceTierReq _
instance standardEncodeUpdateDriverVehicleServiceTierReq :: StandardEncode UpdateDriverVehicleServiceTierReq where standardEncode (UpdateDriverVehicleServiceTierReq res) = standardEncode res
instance showUpdateDriverVehicleServiceTierReq :: Show UpdateDriverVehicleServiceTierReq where show = genericShow
instance decodeUpdateDriverVehicleServiceTierReq :: Decode UpdateDriverVehicleServiceTierReq where decode = defaultDecode
instance encodeUpdateDriverVehicleServiceTierReq  :: Encode UpdateDriverVehicleServiceTierReq where encode = defaultEncode

derive instance genericUpdateDriverVehicleServiceTierResp :: Generic UpdateDriverVehicleServiceTierResp _
instance standardEncodeUpdateDriverVehicleServiceTierResp :: StandardEncode UpdateDriverVehicleServiceTierResp where standardEncode (UpdateDriverVehicleServiceTierResp res) = standardEncode res
instance showUpdateDriverVehicleServiceTierResp :: Show UpdateDriverVehicleServiceTierResp where show = genericShow
instance decodeUpdateDriverVehicleServiceTierResp :: Decode UpdateDriverVehicleServiceTierResp where decode = defaultDecode
instance encodeUpdateDriverVehicleServiceTierResp  :: Encode UpdateDriverVehicleServiceTierResp where encode = defaultEncode

instance makeUpdateDriverVehicleServiceTierReq :: RestEndpoint UpdateDriverVehicleServiceTierReq where
 makeRequest reqBody headers = defaultMakeRequestWithoutLogs POST (EP.updateDriverVehicleServiceTier "") headers reqBody Nothing
 encodeRequest req = standardEncode req

derive instance genericDriverVehicleServiceTier :: Generic DriverVehicleServiceTier _
instance standardEncodeDriverVehicleServiceTier :: StandardEncode DriverVehicleServiceTier where standardEncode (DriverVehicleServiceTier res) = standardEncode res
instance showDriverVehicleServiceTier :: Show DriverVehicleServiceTier where show = genericShow
instance decodeDriverVehicleServiceTier :: Decode DriverVehicleServiceTier where decode = defaultDecode
instance encodeDriverVehicleServiceTier  :: Encode DriverVehicleServiceTier where encode = defaultEncode

instance makeDriverVehicleServiceTierReq :: RestEndpoint DriverVehicleServiceTierReq where
  makeRequest reqBody headers = defaultMakeRequestWithoutLogs GET (EP.driverVehicleServiceTier "") headers reqBody Nothing
  encodeRequest req = standardEncode req
  
data RideStatusPastDaysReq = RideStatusPastDaysReq

newtype RideStatusPastDaysRes = RideStatusPastDaysRes {
  rideCountPopupValue :: Boolean
}

instance makeRideStatusPastDaysReq :: RestEndpoint RideStatusPastDaysReq where
    makeRequest reqBody headers = defaultMakeRequestWithoutLogs GET (EP.getRideStatusPastDays "") headers reqBody Nothing
    encodeRequest req = standardEncode req

derive instance genericRideStatusPastDaysReq :: Generic RideStatusPastDaysReq _
instance showRideStatusPastDaysReq :: Show RideStatusPastDaysReq where show = genericShow
instance decodeRideStatusPastDaysReq :: Decode RideStatusPastDaysReq where decode = defaultDecode
instance encodeRideStatusPastDaysReq :: Encode RideStatusPastDaysReq where encode = defaultEncode
instance standardEncodeRideStatusPastDaysReq :: StandardEncode RideStatusPastDaysReq where standardEncode body = standardEncode {}

derive instance genericRideStatusPastDaysRes :: Generic RideStatusPastDaysRes _
derive instance newtypeRideStatusPastDaysRes :: Newtype RideStatusPastDaysRes _
instance standardEncodeRideStatusPastDaysRes :: StandardEncode RideStatusPastDaysRes where standardEncode (RideStatusPastDaysRes res) = standardEncode res
instance showRideStatusPastDaysRes :: Show RideStatusPastDaysRes where show = genericShow
instance decodeRideStatusPastDaysRes :: Decode RideStatusPastDaysRes where decode = defaultDecode
instance encodeRideStatusPastDaysRes :: Encode RideStatusPastDaysRes where encode = defaultEncode

newtype UpdateAirConditionUpdateRequest = UpdateAirConditionUpdateRequest {
  isAirConditioned :: Boolean
}


instance makeUpdateAirConditionUpdateRequest :: RestEndpoint UpdateAirConditionUpdateRequest where
  makeRequest reqBody headers = defaultMakeRequestWithoutLogs POST (EP.updateAirConditioned "") headers reqBody Nothing
  encodeRequest req = standardEncode req

derive instance genericUpdateAirConditionUpdateRequest :: Generic UpdateAirConditionUpdateRequest _
instance standardEncodeUpdateAirConditionUpdateRequest :: StandardEncode UpdateAirConditionUpdateRequest where standardEncode (UpdateAirConditionUpdateRequest res) = standardEncode res
instance showUpdateAirConditionUpdateRequest :: Show UpdateAirConditionUpdateRequest where show = genericShow
instance decodeUpdateAirConditionUpdateRequest :: Decode UpdateAirConditionUpdateRequest where decode = defaultDecode
instance encodeUpdateAirConditionUpdateRequest  :: Encode UpdateAirConditionUpdateRequest where encode = defaultEncode


--------------------------------------------------------- Get Driver Rate Card --------------------------------------------------------------------------

data GetDriverRateCardReq = GetDriverRateCardReq (Maybe String) (Maybe Int)

newtype GetDriverRateCardRes = GetDriverRateCardRes (Array RateCardRespItem)

newtype RateCardRespItem = RateCardRespItem {
    perKmRate :: CTA.Price,
    perMinuteRate :: Maybe CTA.Price,
    rateCardItems :: Array CTA.EstimateFares,
    serviceTierType :: ServiceTierType,
    totalFare :: CTA.Price,
    farePolicyHour :: FarePolicyHour
  }

instance makeGetDriverRateCardReq :: RestEndpoint GetDriverRateCardReq where
  makeRequest reqBody@(GetDriverRateCardReq vehicleServiceTier dist) headers = defaultMakeRequestWithoutLogs GET (EP.getDriverRateCard vehicleServiceTier dist) headers reqBody Nothing
  encodeRequest req = standardEncode req

derive instance genericGetDriverRateCardReq :: Generic GetDriverRateCardReq _
instance standardEncodeGetDriverRateCardReq :: StandardEncode GetDriverRateCardReq where standardEncode (GetDriverRateCardReq _ _ ) = standardEncode {}
instance showGetDriverRateCardReq :: Show GetDriverRateCardReq where show = genericShow
instance decodeGetDriverRateCardReq :: Decode GetDriverRateCardReq where decode = defaultDecode
instance encodeGetDriverRateCardReq  :: Encode GetDriverRateCardReq where encode = defaultEncode

derive instance genericRateCardRespItem :: Generic RateCardRespItem _
instance standardEncodeRateCardRespItem :: StandardEncode RateCardRespItem where standardEncode (RateCardRespItem _) = standardEncode {}
instance showRateCardRespItem :: Show RateCardRespItem where show = genericShow
instance decodeRateCardRespItem :: Decode RateCardRespItem where decode = defaultDecode
instance encodeRateCardRespItem  :: Encode RateCardRespItem where encode = defaultEncode

derive instance genericGetDriverRateCardRes :: Generic GetDriverRateCardRes _
instance standardEncodeGetDriverRateCardRes :: StandardEncode GetDriverRateCardRes where standardEncode (GetDriverRateCardRes res) = standardEncode res
instance showGetDriverRateCardRes :: Show GetDriverRateCardRes where show = genericShow
instance decodeGetDriverRateCardRes :: Decode GetDriverRateCardRes where decode = defaultDecode
instance encodeGetDriverRateCardRes  :: Encode GetDriverRateCardRes where encode = defaultEncode

data FarePolicyHour
  = Peak
  | NonPeak
  | Night

derive instance genericFarePolicyHour :: Generic FarePolicyHour _
instance showFarePolicyHour :: Show FarePolicyHour where show = genericShow
instance decodeFarePolicyHour :: Decode FarePolicyHour
  where decode body = case unsafeFromForeign body of
                  "Peak"       -> except $ Right Peak
                  "NonPeak"    -> except $ Right NonPeak
                  "Night"      -> except $ Right Night
                  _            -> except $ Right NonPeak
instance encodeFarePolicyHour :: Encode FarePolicyHour where encode = defaultEnumEncode
instance eqFarePolicyHour :: Eq FarePolicyHour where eq = genericEq
instance standardEncodeFarePolicyHour :: StandardEncode FarePolicyHour
  where
    standardEncode Peak = standardEncode "Peak"
    standardEncode NonPeak = standardEncode "NonPeak"
    standardEncode Night = standardEncode "Night"

--------------------------------- GET REFERRAL EARNINGS ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

data ReferralEarningsReq = ReferralEarningsReq String String

newtype ReferralEarningsResp = ReferralEarningsResp
    {
      dailyEarnings :: Array DailyEarnings,
      orderId :: Maybe String,
      orderStatus :: Maybe PP.APIPaymentStatus,
      referralRewardAmountPerRide :: Int,
      totalReferralCount :: Int,
      vpaId :: Maybe String
    }

newtype DailyEarnings = DailyEarnings
  {
    activatedItems :: Int,
    earningDate :: String,
    earnings :: Int,
    payoutOrderId :: Maybe String,
    payoutOrderStatus :: Maybe String,
    referrals :: Int,
    status :: CRST.PayoutStatus
  }

instance makeReferralEarningsReq :: RestEndpoint ReferralEarningsReq where
    makeRequest reqBody@(ReferralEarningsReq fromDate toDate) headers = defaultMakeRequestWithoutLogs GET (EP.getReferralEarnings fromDate toDate) headers reqBody Nothing
    encodeRequest req = defaultEncode req

derive instance genericReferralEarningsReq :: Generic ReferralEarningsReq _
instance showReferralEarningsReq :: Show ReferralEarningsReq where show = genericShow
instance standardEncodeReferralEarningsReq :: StandardEncode ReferralEarningsReq where standardEncode _ = standardEncode {}
instance decodeReferralEarningsReq :: Decode ReferralEarningsReq where decode = defaultDecode
instance encodeReferralEarningsReq :: Encode ReferralEarningsReq where encode = defaultEncode

derive instance genericReferralEarningsResp :: Generic ReferralEarningsResp _
derive instance newtypeReferralEarningsResp :: Newtype ReferralEarningsResp _
instance standardEncodeReferralEarningsResp :: StandardEncode ReferralEarningsResp where standardEncode (ReferralEarningsResp req) = standardEncode req
instance showReferralEarningsResp :: Show ReferralEarningsResp where show = genericShow
instance decodeReferralEarningsResp :: Decode ReferralEarningsResp where decode = defaultDecode
instance encodeReferralEarningsResp :: Encode ReferralEarningsResp where encode = defaultEncode

derive instance genericDailyEarnings :: Generic DailyEarnings _
derive instance newtypeDailyEarnings :: Newtype DailyEarnings _
instance standardEncodeDailyEarnings :: StandardEncode DailyEarnings where standardEncode (DailyEarnings req) = standardEncode req
instance showDailyEarnings :: Show DailyEarnings where show = genericShow
instance decodeDailyEarnings :: Decode DailyEarnings where decode = defaultDecode
instance encodeDailyEarnings :: Encode DailyEarnings where encode = defaultEncode

--------------------------------- DELETE VPA REQUEST ---------------------------------------------------------------------------------------------------------------------------

data DeleteVPAReq = DeleteVPAReq String

newtype DeleteVPARes = DeleteVPARes ApiSuccessResult

instance makeDeleteVPAReq :: RestEndpoint DeleteVPAReq where
    makeRequest reqBody@(DeleteVPAReq vpaId) headers = defaultMakeRequestWithoutLogs POST (EP.deleteVPA vpaId) headers reqBody Nothing
    encodeRequest req = defaultEncode req

derive instance genericDeleteVPAReq :: Generic DeleteVPAReq _
instance showDeleteVPAReq :: Show DeleteVPAReq where show = genericShow
instance standardEncodeDeleteVPAReq :: StandardEncode DeleteVPAReq where standardEncode _ = standardEncode {}
instance decodeDeleteVPAReq :: Decode DeleteVPAReq where decode = defaultDecode
instance encodeDeleteVPAReq :: Encode DeleteVPAReq where encode = defaultEncode

derive instance genericDeleteVPARes :: Generic DeleteVPARes _
instance showDeleteVPARes :: Show DeleteVPARes where show = genericShow
instance standardEncodeDeleteVPARes :: StandardEncode DeleteVPARes where standardEncode _ = standardEncode {}
instance decodeDeleteVPARes :: Decode DeleteVPARes where decode = defaultDecode
instance encodeDeleteVPARes :: Encode DeleteVPARes where encode = defaultEncode

--------------------------------- VERIFY VPA REQUEST ---------------------------------------------------------------------------------------------------------------------------

data VerifyVpaReq = VerifyVpaReq String

newtype VerifyVpaRes = VerifyVpaRes ApiSuccessResult

instance makeVerifyVpaReq :: RestEndpoint VerifyVpaReq where
    makeRequest reqBody@(VerifyVpaReq dummy) headers = defaultMakeRequestWithoutLogs POST (EP.verifyUPI dummy) headers reqBody Nothing
    encodeRequest req = defaultEncode req

derive instance genericVerifyVpaReq :: Generic VerifyVpaReq _
instance showVerifyVpaReq :: Show VerifyVpaReq where show = genericShow
instance standardEncodeVerifyVpaReq :: StandardEncode VerifyVpaReq where standardEncode _ = standardEncode {}
instance decodeVerifyVpaReq :: Decode VerifyVpaReq where decode = defaultDecode
instance encodeVerifyVpaReq :: Encode VerifyVpaReq where encode = defaultEncode

derive instance genericVerifyVpaRes :: Generic VerifyVpaRes _
instance showVerifyVpaRes :: Show VerifyVpaRes where show = genericShow
instance standardEncodeVerifyVpaRes :: StandardEncode VerifyVpaRes where standardEncode _ = standardEncode {}
instance decodeVerifyVpaRes :: Decode VerifyVpaRes where decode = defaultDecode
instance encodeVerifyVpaRes :: Encode VerifyVpaRes where encode = defaultEncode

--------------------------------- GET PAYOUT REGISTER ---------------------------------------------------------------------------------------------------------------------------

data PayoutRegisterReq = PayoutRegisterReq String

newtype PayoutRegisterRes = PayoutRegisterRes 
    {
      orderResp :: CreateOrderRes,
      orderId :: String
    }

instance makePayoutRegisterReq :: RestEndpoint PayoutRegisterReq where
 makeRequest reqBody@(PayoutRegisterReq val) headers = defaultMakeRequestWithoutLogs GET (EP.registerPayout val) headers reqBody Nothing
 encodeRequest req = standardEncode req

derive instance genericPayoutRegisterReq :: Generic PayoutRegisterReq _
instance standardEncodePayoutRegisterReq :: StandardEncode PayoutRegisterReq where standardEncode (PayoutRegisterReq dummy) = standardEncode dummy
instance showPayoutRegisterReq :: Show PayoutRegisterReq where show = genericShow
instance decodePayoutRegisterReq :: Decode PayoutRegisterReq where decode = defaultDecode
instance encodePayoutRegisterReq :: Encode PayoutRegisterReq where encode = defaultEncode

derive instance genericPayoutRegisterRes :: Generic PayoutRegisterRes _
instance standardEncodePayoutRegisterRes :: StandardEncode PayoutRegisterRes where standardEncode (PayoutRegisterRes dummy) = standardEncode dummy
instance showPayoutRegisterRes :: Show PayoutRegisterRes where show = genericShow
instance decodePayoutRegisterRes :: Decode PayoutRegisterRes where decode = defaultDecode
instance encodePayoutRegisterRes :: Encode PayoutRegisterRes where encode = defaultEncode

--------------------------------- SdkToken API ---------------------------------------------------------------------------------------------------------------------------

data ServiceName = Gullak | HyperVerge

data GetSdkTokenReq = GetSdkTokenReq String ServiceName

newtype GetSdkTokenResp = GetSdkTokenResp {
  token :: String,
  expiry :: Maybe String
}

instance makeGetSdkTokenReq  :: RestEndpoint GetSdkTokenReq where
    makeRequest reqBody@(GetSdkTokenReq exp svc) headers = defaultMakeRequestWithoutLogs GET (EP.getSdkToken exp (show svc)) headers reqBody Nothing
    encodeRequest req = defaultEncode req

derive instance genericGetSdkTokenReq :: Generic GetSdkTokenReq _
instance showGetSdkTokenReq :: Show GetSdkTokenReq where show = genericShow
instance standardEncodeGetSdkTokenReq :: StandardEncode GetSdkTokenReq where standardEncode _ = standardEncode{}
instance decodeGetSdkTokenReq :: Decode GetSdkTokenReq where decode = defaultDecode
instance encodeGetSdkTokenReq :: Encode GetSdkTokenReq where encode = defaultEncode

derive instance genericServiceName :: Generic ServiceName _
instance showServiceName :: Show ServiceName where show = genericShow
instance decodeServiceName :: Decode ServiceName where decode = defaultEnumDecode
instance encodeServiceName :: Encode ServiceName where encode = defaultEnumEncode
instance standardEncodeServiceName :: StandardEncode ServiceName where standardEncode _ = standardEncode {}


derive instance genericGetSdkTokenResp :: Generic GetSdkTokenResp _
derive instance newtypeGetSdkTokenResp :: Newtype GetSdkTokenResp _
instance standardEncodeGetSdkTokenResp :: StandardEncode GetSdkTokenResp where standardEncode (GetSdkTokenResp rsp) = standardEncode rsp
instance showGetSdkTokenResp :: Show GetSdkTokenResp where show = genericShow
instance decodeGetSdkTokenResp :: Decode GetSdkTokenResp where decode = defaultDecode
instance encodeGetSdkTokenResp :: Encode GetSdkTokenResp where encode = defaultEncode

------------------------------------------------------ Onboarding Live selfie, aadhaar, and PAN APIs --------------------------------------------------------

data GetLiveSelfieReq = GetLiveSelfieReq String

newtype GetLiveSelfieResp = GetLiveSelfieResp {image :: String}

data ValidationStatus = APPROVED | DECLINED | AUTO_APPROVED | AUTO_DECLINED | NEEDS_REVIEW

data VerifiedBy = FRONTEND_SDK | DASHBOARD

newtype PanCardReq = PanCardReq
  { consent :: Boolean,
    consentTimestamp :: String,
    dateOfBirth :: Maybe String,
    nameOnCard :: Maybe String,
    imageId1 :: Maybe String,
    imageId2 :: Maybe String,
    panNumber :: String,
    validationStatus :: ValidationStatus, 
    verifiedBy :: VerifiedBy, 
    transactionId :: Maybe String,
    nameOnGovtDB :: Maybe String
  }

newtype DriverPANResp = DriverPANResp ApiSuccessResult

newtype AadhaarCardReq = AadhaarCardReq
  { aadhaarBackImageId :: Maybe String,
    aadhaarFrontImageId :: Maybe String,
    address :: Maybe String,
    consent :: Boolean,
    consentTimestamp :: String,
    dateOfBirth :: Maybe String,
    maskedAadhaarNumber :: Maybe String,
    nameOnCard :: Maybe String,
    validationStatus :: ValidationStatus,
    transactionId :: String
  }

newtype DriverAadhaarResp = DriverAadhaarResp ApiSuccessResult

instance makeGetLiveSelfieReq  :: RestEndpoint GetLiveSelfieReq where
    makeRequest reqBody@(GetLiveSelfieReq status) headers = defaultMakeRequestWithoutLogs GET (EP.getLiveSelfie (show status)) headers reqBody Nothing
    encodeRequest req = defaultEncode req

derive instance genericGetLiveSelfieReq :: Generic GetLiveSelfieReq _
instance showGetLiveSelfieReq :: Show GetLiveSelfieReq where show = genericShow
instance standardEncodeGetLiveSelfieReq :: StandardEncode GetLiveSelfieReq where standardEncode _ = standardEncode{}
instance decodeGetLiveSelfieReq :: Decode GetLiveSelfieReq where decode = defaultDecode
instance encodeGetLiveSelfieReq :: Encode GetLiveSelfieReq where encode = defaultEncode

derive instance genericGetLiveSelfieResp :: Generic GetLiveSelfieResp _
derive instance newtypeGetLiveSelfieResp :: Newtype GetLiveSelfieResp _
instance standardEncodeGetLiveSelfieResp :: StandardEncode GetLiveSelfieResp where standardEncode (GetLiveSelfieResp rsp) = standardEncode rsp
instance showGetLiveSelfieResp :: Show GetLiveSelfieResp where show = genericShow
instance decodeGetLiveSelfieResp :: Decode GetLiveSelfieResp where decode = defaultDecode
instance encodeGetLiveSelfieResp :: Encode GetLiveSelfieResp where encode = defaultEncode

derive instance genericValidationStatus :: Generic ValidationStatus _
instance showValidationStatus :: Show ValidationStatus where show = genericShow
instance decodeValidationStatus :: Decode ValidationStatus where decode = defaultEnumDecode
instance encodeValidationStatus :: Encode ValidationStatus where encode = defaultEnumEncode
instance standardEncodeValidationStatus :: StandardEncode ValidationStatus where standardEncode _ = standardEncode {}

derive instance genericVerifiedBy :: Generic VerifiedBy _
instance showVerifiedBy :: Show VerifiedBy where show = genericShow
instance decodeVerifiedBy :: Decode VerifiedBy where decode = defaultEnumDecode
instance encodeVerifiedBy :: Encode VerifiedBy where encode = defaultEnumEncode
instance standardEncodeVerifiedBy :: StandardEncode VerifiedBy where standardEncode _ = standardEncode {}

instance makePanCardReq :: RestEndpoint PanCardReq where
    makeRequest reqBody headers = defaultMakeRequestWithoutLogs POST (EP.registerPAN "") headers reqBody Nothing
    encodeRequest req = defaultEncode req

derive instance genericPanCardReq :: Generic PanCardReq _
instance showPanCardReq :: Show PanCardReq where show = genericShow
instance standardPanCardReq :: StandardEncode PanCardReq where standardEncode (PanCardReq req) = standardEncode req
instance decodePanCardReq :: Decode PanCardReq where decode = defaultDecode
instance encodePanCardReq :: Encode PanCardReq where encode = defaultEncode

derive instance genericDriverPANResp :: Generic DriverPANResp _
derive instance newtypeDriverPANResp :: Newtype DriverPANResp _
instance standardEncodeDriverPANResp :: StandardEncode DriverPANResp where standardEncode (DriverPANResp body) = standardEncode body
instance showDriverPANResp :: Show DriverPANResp where show = genericShow
instance decodeDriverPANResp:: Decode DriverPANResp where decode = defaultDecode
instance encodeDriverPANResp  :: Encode DriverPANResp where encode = defaultEncode

instance makeAadhaarCardReq :: RestEndpoint AadhaarCardReq where
    makeRequest reqBody headers = defaultMakeRequestWithoutLogs POST (EP.registerAadhaar "") headers reqBody Nothing
    encodeRequest req = defaultEncode req

derive instance genericAadhaarCardReq :: Generic AadhaarCardReq _
instance showAadhaarCardReq :: Show AadhaarCardReq where show = genericShow
instance standardAadhaarCardReq :: StandardEncode AadhaarCardReq where standardEncode (AadhaarCardReq req) = standardEncode req
instance decodeAadhaarCardReq :: Decode AadhaarCardReq where decode = defaultDecode
instance encodeAadhaarCardReq :: Encode AadhaarCardReq where encode = defaultEncode

derive instance genericDriverAadhaarResp :: Generic DriverAadhaarResp _
derive instance newtypeDriverAadhaarResp :: Newtype DriverAadhaarResp _
instance standardEncodeDriverAadhaarResp :: StandardEncode DriverAadhaarResp where standardEncode (DriverAadhaarResp body) = standardEncode body
instance showDriverAadhaarResp :: Show DriverAadhaarResp where show = genericShow
instance decodeDriverAadhaarResp:: Decode DriverAadhaarResp where decode = defaultDecode
instance encodeDriverAadhaarResp  :: Encode DriverAadhaarResp where encode = defaultEncode

-----------------------------------------------------------Fetching Driver Profile--------------------------------------------------
data DriverProfileDataReq = DriverProfileDataReq Boolean 

newtype DriverProfileDataRes = DriverProfileDataRes
  {
    aspirations :: Array String,
    drivingSince :: Maybe Int,
    hometown :: Maybe String,
    otherImages :: Array String,
    otherImageIds :: Array String,
    pledges :: Array String,
    profileImage :: Maybe String,
    vehicleTags :: Array String
  }

instance driverProfileDataReq :: RestEndpoint DriverProfileDataReq  where
 makeRequest reqBody@(DriverProfileDataReq isImages) headers = defaultMakeRequestWithoutLogs GET (EP.getDriverProfile isImages) headers reqBody Nothing
 encodeRequest req = standardEncode req

derive instance genericDriverProfileDataReq :: Generic DriverProfileDataReq _
instance standardEncodeDriverProfileDataReq :: StandardEncode DriverProfileDataReq where standardEncode (DriverProfileDataReq _) = standardEncode {}
instance decodeDriverProfileDataReq :: Decode DriverProfileDataReq where decode = defaultDecode
instance encodeDriverProfileDataReq :: Encode DriverProfileDataReq where encode = defaultEncode

derive instance genericDriverProfileDataRes:: Generic DriverProfileDataRes _
derive instance newtypeDriverProfileDataRes :: Newtype DriverProfileDataRes _
instance standardEncodeDriverProfileDataRes :: StandardEncode DriverProfileDataRes where standardEncode (DriverProfileDataRes id) = standardEncode id
instance showDriverProfileDataRes:: Show DriverProfileDataRes where show = genericShow
instance decodeDriverProfileDataRes :: Decode DriverProfileDataRes where decode = defaultDecode
instance encodeDriverProfileDataRes :: Encode DriverProfileDataRes where encode = defaultEncode
------------------------------------------------------ Driver Reached Destination --------------------------------------------------------

data DriverReachedDestinationRequest = DriverReachedDestinationRequest String DriverReachedReq

newtype DriverReachedReq  = DriverReachedReq {
    lat :: Number
  , lon :: Number
  }


derive instance genericDriverReachedDestinationRequest :: Generic DriverReachedDestinationRequest _
instance standardEncodeDriverReachedDestinationRequest :: StandardEncode DriverReachedDestinationRequest where standardEncode (DriverReachedDestinationRequest rideId req) = standardEncode req
instance showDriverReachedDestinationRequest :: Show DriverReachedDestinationRequest where show = genericShow
instance decodeDriverReachedDestinationRequest :: Decode DriverReachedDestinationRequest where decode = defaultDecode
instance encodeDriverReachedDestinationRequest :: Encode DriverReachedDestinationRequest where encode = defaultEncode


instance makeDriverReachedReq :: RestEndpoint DriverReachedDestinationRequest where
    makeRequest reqBody@(DriverReachedDestinationRequest rideId (DriverReachedReq rqBody)) headers = defaultMakeRequestWithoutLogs POST (EP.driverReachedDestination rideId) headers reqBody Nothing
    encodeRequest req = standardEncode req

derive instance genericDriverReachedReq :: Generic DriverReachedReq _
instance showDriverReachedReq :: Show DriverReachedReq where show = genericShow
instance standardEncodeDriverReachedReq :: StandardEncode DriverReachedReq where standardEncode (DriverReachedReq req) = standardEncode req
instance decodeDriverReachedReq :: Decode DriverReachedReq where decode = defaultDecode
instance encodeDriverReachedReq :: Encode DriverReachedReq where encode = defaultEncode


------------------------------------------------------ Driver Reached Destination --------------------------------------------------------

data CoinInfoReq = CoinInfoReq 

newtype CoinInfoRes = CoinInfoRes (Array CoinInfo)

newtype CoinInfo = CoinInfo CoinInfoType

type CoinInfoType = {
  coins :: Int,
  key :: String,
  title :: String,
  description :: String
}

instance makeCoinInfoReq  :: RestEndpoint CoinInfoReq where
    makeRequest reqBody headers = defaultMakeRequestWithoutLogs GET (EP.getCoinInfo "") headers reqBody Nothing
    encodeRequest req = defaultEncode req

derive instance genericCoinInfoReq :: Generic CoinInfoReq _
instance showCoinInfoReq :: Show CoinInfoReq where show = genericShow
instance standardEncodeCoinInfoReq :: StandardEncode CoinInfoReq where standardEncode _ = standardEncode{}
instance decodeCoinInfoReq :: Decode CoinInfoReq where decode = defaultDecode
instance encodeCoinInfoReq :: Encode CoinInfoReq where encode = defaultEncode

derive instance genericCoinInfoRes :: Generic CoinInfoRes _
derive instance newtypeCoinInfoRes :: Newtype CoinInfoRes _
instance standardEncodeCoinInfoRes :: StandardEncode CoinInfoRes where standardEncode (CoinInfoRes rsp) = standardEncode rsp
instance showCoinInfoRes :: Show CoinInfoRes where show = genericShow
instance decodeCoinInfoRes :: Decode CoinInfoRes where decode = defaultDecode
instance encodeCoinInfoRes :: Encode CoinInfoRes where encode = defaultEncode


derive instance genericCoinInfo :: Generic CoinInfo _
instance showCoinInfo :: Show CoinInfo where show = genericShow
instance standardEncodeCoinInfo :: StandardEncode CoinInfo where standardEncode _ = standardEncode{} 
instance decodeCoinInfo :: Decode CoinInfo where decode = defaultDecode
instance encodeCoinInfo :: Encode CoinInfo where encode = defaultEncode

--------------------------------------------------------------- Demand Hotspots API response -------------------------------------------------------------

data DemandHotspotsReq = DemandHotspotsReq String

newtype DemandHotspotsResp = DemandHotspotsResp 
  { createdAt :: String,
    expiryAt :: String,
    hotspotsDetails :: Array HotspotsDetails
  }

newtype HotspotsDetails = HotspotsDetails {
  frequency :: Int, 
  location :: LatLong
}

instance makeDemandHotspotsReq :: RestEndpoint DemandHotspotsReq where
    makeRequest reqBody@(DemandHotspotsReq dummy) headers = defaultMakeRequestWithoutLogs GET (EP.demandHotspots dummy) headers reqBody Nothing
    encodeRequest req = defaultEncode req

derive instance genericDemandHotspotsReq :: Generic DemandHotspotsReq _
instance showDemandHotspotsReq :: Show DemandHotspotsReq where show = genericShow
instance standardEncodeDemandHotspotsReq :: StandardEncode DemandHotspotsReq where standardEncode _ = standardEncode {}
instance decodeDemandHotspotsReq :: Decode DemandHotspotsReq where decode = defaultDecode
instance encodeDemandHotspotsReq :: Encode DemandHotspotsReq where encode = defaultEncode

derive instance genericDemandHotspotsResp :: Generic DemandHotspotsResp _
derive instance newtypeDemandHotspotsResp :: Newtype DemandHotspotsResp _
instance showDemandHotspotsResp :: Show DemandHotspotsResp where show = genericShow
instance standardEncodeDemandHotspotsResp :: StandardEncode DemandHotspotsResp where standardEncode _ = standardEncode {}
instance decodeDemandHotspotsResp :: Decode DemandHotspotsResp where decode = defaultDecode
instance encodeDemandHotspotsResp :: Encode DemandHotspotsResp where encode = defaultEncode

derive instance genericHotspotsDetails :: Generic HotspotsDetails _
derive instance newtypeHotspotsDetails :: Newtype HotspotsDetails _
instance standardEncodeHotspotsDetails :: StandardEncode HotspotsDetails where standardEncode _ = standardEncode {}
instance showHotspotsDetails :: Show HotspotsDetails where show = genericShow
instance decodeHotspotsDetails :: Decode HotspotsDetails where decode = defaultDecode
instance encodeHotspotsDetails :: Encode HotspotsDetails where encode = defaultEncode

data ScheduledBookingListRequest = ScheduledBookingListRequest String String String String String String String

instance makeScheduledBookingListRequest :: RestEndpoint ScheduledBookingListRequest where
    makeRequest reqBody@(ScheduledBookingListRequest limit offset from to  tripCategory lat lon ) headers = defaultMakeRequestWithoutLogs GET (EP.getScheduledBookingList limit offset  from to  tripCategory lat lon) headers reqBody Nothing
    encodeRequest req = defaultEncode req

derive instance genericScheduledBookingListRequest :: Generic ScheduledBookingListRequest _
instance showScheduledBookingListRequest:: Show ScheduledBookingListRequest where show = genericShow
instance standardEncodeScheduledBookingListRequest:: StandardEncode ScheduledBookingListRequest where standardEncode _ = standardEncode {}
instance decodeScheduledBookingListRequest :: Decode ScheduledBookingListRequest where decode = defaultDecode
instance encodeScheduledBookingListRequest:: Encode ScheduledBookingListRequest where encode = defaultEncode

newtype ScheduledBookingListResponse = ScheduledBookingListResponse
    {
      bookings :: Array ScheduleBooking
    }

derive instance genericScheduledBookingListResponse:: Generic ScheduledBookingListResponse _
instance standardEncodeScheduledBookingListResponse :: StandardEncode ScheduledBookingListResponse where standardEncode (ScheduledBookingListResponse res) = standardEncode res
instance showScheduledBookingListResponse:: Show ScheduledBookingListResponse where show = genericShow
instance decodeScheduledBookingListResponse:: Decode ScheduledBookingListResponse where decode = defaultDecode
instance encodeScheduledBookingListResponse  :: Encode ScheduledBookingListResponse where encode = defaultEncode
instance eqScheduledBookingListResponse :: Eq ScheduledBookingListResponse where eq = genericEq

newtype ScheduleBooking = ScheduleBooking  
   {
    bookingDetails :: BookingAPIEntity,
    fareDetails ::  Array RateCardItem
   }

derive instance genericScheduleBooking:: Generic ScheduleBooking _
instance standardEncodeScheduleBooking :: StandardEncode ScheduleBooking where standardEncode (ScheduleBooking res) = standardEncode res
instance showScheduleBooking:: Show ScheduleBooking where show = genericShow
instance decodeScheduleBooking:: Decode ScheduleBooking where decode = defaultDecode
instance encodeScheduleBooking :: Encode ScheduleBooking where encode = defaultEncode
instance eqScheduleBooking :: Eq ScheduleBooking where eq = genericEq

newtype BookingAPIEntity = BookingAPIEntity {
   area :: Area,
   createdAt :: String,
   currency :: Currency,
   disabilityTag :: Maybe String,
   distanceToPickup :: Maybe Int,
   estimatedDistance ::Maybe Int,
   estimatedDuration ::Maybe Int,
   estimatedFare :: Number ,
   fareParams :: FareParameters,
   fromLocation :: Location,
   id :: String,
   isAirConditioned ::Maybe Boolean ,
   isScheduled :: Boolean,
   maxEstimatedDistance :: Maybe Number,
   returnTime :: Maybe String,
   roundTrip :: Maybe Boolean,
   specialZoneOtpCode :: Maybe String,
   startTime :: String ,
   status :: Common.BookingStatus,
   stopLocationId :: Maybe String,
   toLocation :: Maybe Location,
   tollNames :: Maybe (Array String),
   tripCategory :: CTA.TripCategory,
   updatedAt :: String,
  vehicleServiceTier :: ServiceTierType,
  vehicleServiceTierAirConditioned :: Maybe Number ,
  vehicleServiceTierName :: String,
  vehicleServiceTierSeatingCapacity ::  Maybe Int
}
derive instance genericBookingAPIEntity:: Generic BookingAPIEntity _
instance standardBookingAPIEntity:: StandardEncode BookingAPIEntity where standardEncode (BookingAPIEntity res) = standardEncode res
instance showBookingAPIEntity :: Show BookingAPIEntity where show = genericShow
instance decodeBookingAPIEntity :: Decode BookingAPIEntity where decode = defaultDecode
instance encodeBookingAPIEntity :: Encode BookingAPIEntity where encode = defaultEncode
instance eqBookingAPIEntity :: Eq BookingAPIEntity where eq = genericEq

newtype Location =  Location {
  address :: LocationAddress,
  createdAt :: String,
  id  :: String,
  lat :: Number,
  lon :: Number,
  updatedAt ::String

}
derive instance genericLocation:: Generic Location _
instance standardLocation:: StandardEncode Location where standardEncode (Location res) = standardEncode res
instance showLocation :: Show Location where show = genericShow
instance decodeLocation:: Decode Location where decode = defaultDecode
instance encodeLocation :: Encode Location where encode = defaultEncode
instance eqLocation :: Eq Location where eq = genericEq

newtype LocationAddress = LocationAddress {
  area :: Maybe String,
  areaCode :: Maybe String,
  building :: Maybe String,
  city :: Maybe String,
  country ::Maybe String,
  state :: Maybe String,
  street :: Maybe String,
  fullAddress :: Maybe String,
  door :: Maybe String
}
derive instance genericLocationAddress:: Generic LocationAddress _
instance standardLocationAddress:: StandardEncode LocationAddress where standardEncode (LocationAddress res) = standardEncode res
instance showLocationAddress :: Show LocationAddress where show = genericShow
instance decodeLocationAddress:: Decode LocationAddress where decode = defaultDecode
instance encodeLocationAddress :: Encode LocationAddress where encode = defaultEncode
instance eqLocationAddress :: Eq LocationAddress where eq = genericEq


data Area = Default | Drop String | Pickup String

derive instance genericArea :: Generic Area _
instance standardEncodeArea :: StandardEncode Area where standardEncode _ = standardEncode {}
instance showArea :: Show Area where show = genericShow
instance decodeArea :: Decode Area 
  where decode body =
          case (runExcept $ (readProp "tag" body) >>= decode) of
              Right tag -> case tag of
                                "Pickup" -> case runExcept $ (readProp "contents" body) >>= decode of 
                                              Right content -> except $ Right $ Pickup content
                                              Left _  -> except $ Right $ Default
                                "Drop" -> case runExcept $ (readProp "contents" body) >>= decode of 
                                              Right content ->except $ Right $ Drop content
                                              Left _  -> except $ Right $ Default
                                _ -> except $ Right $ Default
              Left err     -> except $ Right $ Default
instance encodeArea  :: Encode Area where encode = defaultEncode
instance eqArea :: Eq Area where eq = genericEq

data Currency  = INR | USD | EUR 

derive instance genericCurrency :: Generic Currency  _
instance showCurrency :: Show Currency where show = genericShow
instance decodeCurrency :: Decode Currency where decode = defaultEnumDecode
instance encodeCurrency :: Encode Currency where encode = defaultEnumEncode
instance eqCurrency :: Eq Currency where eq = genericEq
instance standardEncodeCurrency :: StandardEncode Currency where standardEncode _ = standardEncode {}

newtype FareParameters = FareParameters {
  baseFare :: Number,
  congestionCharge :: Maybe Number,
  currency :: Currency,
  customerCancellationDues :: Maybe Number,
  customerExtraFee :: Maybe Number,
  driverSelectedFare :: Maybe Number,
  fareParametersDetails ::FareParametersDetails,
  govtCharges :: Maybe Number,
  id ::String,
  nightShiftCharge :: Maybe Number,
  nightShiftRateIfApplies :: Maybe Number,
  parkingCharge :: Maybe Number,
  rideExtraTimeFare:: Maybe Number,
  serviceCharge::Maybe Number,
  tollCharges:: Maybe Number,
  updatedAt ::String,
  waitingCharge :: Maybe Number

}
derive instance genericFareParameters:: Generic FareParameters _
instance standardFareParameters :: StandardEncode FareParameters where standardEncode (FareParameters res) = standardEncode res
instance showFareParameters :: Show FareParameters where show = genericShow
instance decodeFareParameters :: Decode FareParameters where decode = defaultDecode
instance encodeFareParameters :: Encode FareParameters where encode = defaultEncode
instance eqFareParameters :: Eq FareParameters where eq = genericEq

newtype FareParametersDetails = FareParametersDetails {
  contents :: Content,
  tag :: Maybe String
}
derive instance genericFareParametersDetails:: Generic FareParametersDetails _
instance standardFareParametersDetails :: StandardEncode FareParametersDetails where standardEncode (FareParametersDetails res) = standardEncode res
instance showFareParametersDetails:: Show FareParametersDetails where show = genericShow
instance decodeFareParametersDetails :: Decode FareParametersDetails where decode = defaultDecode
instance encodeFareParametersDetails:: Encode FareParametersDetails where encode = defaultEncode
instance eqFareParametersDetails :: Eq FareParametersDetails where eq = genericEq


newtype Content  = Content {
  currency :: Currency,
  deadKmFare :: Maybe Int,
  distBasedFare :: Maybe Int,
  distanceUnit :: Maybe String,
  extraDistance :: Maybe Int,
  extraDuration :: Maybe Int,
  timeBasedFare :: Maybe Int
}

derive instance genericContent:: Generic Content _
instance standardContent :: StandardEncode Content where standardEncode (Content res) = standardEncode res
instance showContent:: Show Content where show = genericShow
instance decodeContent :: Decode Content where decode = defaultDecode
instance encodeContent:: Encode Content where encode = defaultEncode
instance eqContent :: Eq Content where eq = genericEq

newtype RateCardItem  = RateCardItem{
  price :: Int,
  priceWithCurrency :: CTA.Price,
  title :: TitleTag
}
derive instance genericRateCardItem:: Generic RateCardItem _
instance standardRateCardItem :: StandardEncode RateCardItem where standardEncode (RateCardItem res) = standardEncode res
instance showRateCardItem:: Show RateCardItem where show = genericShow
instance decodeRateCardItem :: Decode RateCardItem where decode = defaultDecode
instance encodeRateCardItem:: Encode RateCardItem where encode = defaultEncode
instance eqRateCardItem:: Eq RateCardItem where eq = genericEq

data TitleTag = SERVICE_CHARGE 
              | GOVERNMENT_CHARGE 
              | NIGHT_SHIFT_START_TIME_IN_SECONDS 
              | NIGHT_SHIFT_END_TIME_IN_SECONDS 
              | MIN_FARE 
              | PER_HOUR_CHARGE 
              | PER_MINUTE_CHARGE 
              | UNPLANNED_PER_KM_CHARGE 
              | PER_HOUR_DISTANCE_KM 
              | PLANNED_PER_KM_CHARGE 
              | DEAD_KILOMETER_FARE 
              | WAITING_CHARGE_PER_MIN 
              | FREE_WAITING_TIME_IN_MINUTES 
              | NIGHT_SHIFT_CHARGE 
              | CONSTANT_NIGHT_SHIFT_CHARGE
              | WAITING_CHARGE_RATE_PER_MIN

derive instance genericTitleTag :: Generic TitleTag   _
instance showTitleTag :: Show TitleTag  where show = genericShow
instance decodeTitleTag :: Decode TitleTag  
   where decode body = case unsafeFromForeign body of 
                    "SERVICE_CHARGE"                     ->  except $ Right SERVICE_CHARGE
                    "GOVERNMENT_CHARGE"                  ->  except $ Right GOVERNMENT_CHARGE
                    "NIGHT_SHIFT_START_TIME_IN_SECONDS"  ->  except $ Right NIGHT_SHIFT_START_TIME_IN_SECONDS
                    "NIGHT_SHIFT_END_TIME_IN_SECONDS "   ->  except $ Right NIGHT_SHIFT_END_TIME_IN_SECONDS
                    "MIN_FARE"                           ->  except $ Right MIN_FARE
                    "PER_HOUR_CHARGE"                    ->  except $ Right PER_HOUR_CHARGE
                    "PER_MINUTE_CHARGE"                  ->  except $ Right PER_MINUTE_CHARGE
                    "UNPLANNED_PER_KM_CHARGE"            ->  except $ Right UNPLANNED_PER_KM_CHARGE
                    "PER_HOUR_DISTANCE_KM"               ->  except $ Right PER_HOUR_DISTANCE_KM
                    "PLANNED_PER_KM_CHARGE"              ->  except $ Right PLANNED_PER_KM_CHARGE
                    "DEAD_KILOMETER_FARE"                ->  except $ Right DEAD_KILOMETER_FARE
                    "WAITING_CHARGE_PER_MIN"             ->  except $ Right WAITING_CHARGE_PER_MIN
                    "FREE_WAITING_TIME_IN_MINUTES"       ->  except $ Right FREE_WAITING_TIME_IN_MINUTES
                    "NIGHT_SHIFT_CHARGE"                 ->  except $ Right NIGHT_SHIFT_CHARGE
                    "CONSTANT_NIGHT_SHIFT_CHARGE"        ->  except $ Right CONSTANT_NIGHT_SHIFT_CHARGE
                    "WAITING_CHARGE_RATE_PER_MIN"        ->  except $ Right WAITING_CHARGE_RATE_PER_MIN
                    _                                    ->  except $ Right SERVICE_CHARGE

instance encodeTitleTag:: Encode TitleTag  where encode = defaultEnumEncode
instance eqTitleTag :: Eq TitleTag  where eq = genericEq
instance standardTitleTag :: StandardEncode TitleTag   where standardEncode _ = standardEncode {}


data ScheduleBookingAcceptReq = ScheduleBookingAcceptReq String 

newtype ScheduleBookingAcceptRes = ScheduleBookingAcceptRes ApiSuccessResult

instance makeScheduleBookingAcceptReq  :: RestEndpoint ScheduleBookingAcceptReq where
    makeRequest reqBody@(ScheduleBookingAcceptReq bookingId) headers = defaultMakeRequestWithoutLogs POST (EP.scheduleBookingAccept bookingId) headers reqBody Nothing
    encodeRequest req = defaultEncode req

derive instance genericScheduleBookingAcceptReq :: Generic ScheduleBookingAcceptReq _
instance showScheduleBookingAcceptReq :: Show ScheduleBookingAcceptReq where show = genericShow
instance standardEncodeScheduleBookingAcceptReq :: StandardEncode ScheduleBookingAcceptReq where standardEncode (ScheduleBookingAcceptReq bookingId) = standardEncode{}
instance decodeScheduleBookingAcceptReq :: Decode ScheduleBookingAcceptReq where decode = defaultDecode
instance encodeScheduleBookingAcceptReq :: Encode ScheduleBookingAcceptReq where encode = defaultEncode

derive instance genericScheduleBookingAcceptRes :: Generic ScheduleBookingAcceptRes _
derive instance newtypeScheduleBookingAcceptRes :: Newtype ScheduleBookingAcceptRes _
instance standardEncodeScheduleBookingAcceptRes :: StandardEncode ScheduleBookingAcceptRes where standardEncode (ScheduleBookingAcceptRes req) = standardEncode req
instance showScheduleBookingAcceptRes :: Show ScheduleBookingAcceptRes where show = genericShow
instance decodeScheduleBookingAcceptRes :: Decode ScheduleBookingAcceptRes where decode = defaultDecode
instance encodeScheduleBookingAcceptRes :: Encode ScheduleBookingAcceptRes where encode = defaultEncode



------------------------------------------------------ HyperVerge Sdk Calls logging -----------------------------------------------------------------------------

newtype HVSdkCallLogReq = HVSdkCallLogReq
  { callbackResponse :: Maybe String,
    docType :: Maybe String,
    failureReason :: Maybe String,
    hvFlowId :: Maybe String,
    status :: Maybe String,
    txnId :: String
  }

newtype HVSdkCallLogResp = HVSdkCallLogResp ApiSuccessResult

instance makeHVSdkCallLogReq :: RestEndpoint HVSdkCallLogReq where
    makeRequest reqBody headers = defaultMakeRequestWithoutLogs POST (EP.updateHVSdkCallLog "") headers reqBody Nothing
    encodeRequest req = defaultEncode req

derive instance genericHVSdkCallLogReq :: Generic HVSdkCallLogReq _
instance showHVSdkCallLogReq :: Show HVSdkCallLogReq where show = genericShow
instance standardHVSdkCallLogReq :: StandardEncode HVSdkCallLogReq where standardEncode (HVSdkCallLogReq req) = standardEncode req
instance decodeHVSdkCallLogReq :: Decode HVSdkCallLogReq where decode = defaultDecode
instance encodeHVSdkCallLogReq :: Encode HVSdkCallLogReq where encode = defaultEncode

derive instance genericHVSdkCallLogResp :: Generic HVSdkCallLogResp _
derive instance newtypeHVSdkCallLogResp :: Newtype HVSdkCallLogResp _
instance standardEncodeHVSdkCallLogResp :: StandardEncode HVSdkCallLogResp where standardEncode (HVSdkCallLogResp body) = standardEncode body
instance showHVSdkCallLogResp :: Show HVSdkCallLogResp where show = genericShow
instance decodeHVSdkCallLogResp:: Decode HVSdkCallLogResp where decode = defaultDecode
instance encodeHVSdkCallLogResp  :: Encode HVSdkCallLogResp where encode = defaultEncode

-------------- METRO WARRIOR 

newtype UpdateSpecialLocWarriorInfoReq = UpdateSpecialLocWarriorInfoReq {
  isSpecialLocWarrior :: Boolean,
  preferredPrimarySpecialLocId :: Maybe String,
  preferredSecondarySpecialLocIds :: Array String,
  driverId :: String
}

newtype SpecialLocWarriorInfoRes = SpecialLocWarriorInfoRes {
  isSpecialLocWarrior :: Boolean,
  preferredPrimarySpecialLoc :: Maybe SpecialLocationWarrior,
  preferredSecondarySpecialLocIds :: Array String
}

newtype SpecialLocationWarrior = SpecialLocationWarrior
  { id :: String,
    locationName :: String,
    category :: String,
    linkedLocations :: Array SpecialLocation
  }

newtype SpecialLocation = SpecialLocation
  { id :: String,
    locationName :: String,
    category :: String
  }

derive instance genericUpdateSpecialLocWarriorInfoReq :: Generic UpdateSpecialLocWarriorInfoReq _
derive instance newtypeUpdateSpecialLocWarriorInfoReq :: Newtype UpdateSpecialLocWarriorInfoReq _
instance standardEncodeUpdateSpecialLocWarriorInfoReq :: StandardEncode UpdateSpecialLocWarriorInfoReq where standardEncode (UpdateSpecialLocWarriorInfoReq req) = standardEncode req
instance showUpdateSpecialLocWarriorInfoReq :: Show UpdateSpecialLocWarriorInfoReq where show = genericShow
instance decodeUpdateSpecialLocWarriorInfoReq :: Decode UpdateSpecialLocWarriorInfoReq where decode = defaultDecode
instance encodeUpdateSpecialLocWarriorInfoReq :: Encode UpdateSpecialLocWarriorInfoReq where encode = defaultEncode

derive instance genericSpecialLocWarriorInfoRes :: Generic SpecialLocWarriorInfoRes _
derive instance newtypeSpecialLocWarriorInfoRes :: Newtype SpecialLocWarriorInfoRes _
instance standardEncodeSpecialLocWarriorInfoRes :: StandardEncode SpecialLocWarriorInfoRes where standardEncode (SpecialLocWarriorInfoRes req) = standardEncode req
instance showSpecialLocWarriorInfoRes :: Show SpecialLocWarriorInfoRes where show = genericShow
instance decodeSpecialLocWarriorInfoRes :: Decode SpecialLocWarriorInfoRes where decode = defaultDecode
instance encodeSpecialLocWarriorInfoRes :: Encode SpecialLocWarriorInfoRes where encode = defaultEncode

derive instance genericSpecialLocationWarrior :: Generic SpecialLocationWarrior _
derive instance newtypeSpecialLocationWarrior :: Newtype SpecialLocationWarrior _
instance standardEncodeSpecialLocationWarrior :: StandardEncode SpecialLocationWarrior where standardEncode (SpecialLocationWarrior req) = standardEncode req
instance showSpecialLocationWarrior :: Show SpecialLocationWarrior where show = genericShow
instance decodeSpecialLocationWarrior :: Decode SpecialLocationWarrior where decode = defaultDecode
instance encodeSpecialLocationWarrior :: Encode SpecialLocationWarrior where encode = defaultEncode

derive instance genericSpecialLocation :: Generic SpecialLocation _
derive instance newtypeSpecialLocation :: Newtype SpecialLocation _
instance standardEncodeSpecialLocation :: StandardEncode SpecialLocation where standardEncode (SpecialLocation res) = standardEncode res
instance showSpecialLocation :: Show SpecialLocation where show = genericShow
instance decodeSpecialLocation :: Decode SpecialLocation where decode = defaultDecode
instance encodeSpecialLocation :: Encode SpecialLocation where encode = defaultEncode

instance makeUpdateSpecialLocWarriorInfoReq  :: RestEndpoint UpdateSpecialLocWarriorInfoReq where
    makeRequest reqBody@(UpdateSpecialLocWarriorInfoReq req) headers = defaultMakeRequestWithoutLogs POST (EP.updateMetroWarriorInfo req.driverId) headers reqBody Nothing
    encodeRequest req = defaultEncode req

instance makeGetSpecialLocWarriorInfoReq  :: RestEndpoint GetSpecialLocWarriorInfoReq where
    makeRequest reqBody@(GetSpecialLocWarriorInfoReq driverId) headers = defaultMakeRequestWithoutLogs GET (EP.getMetroWarriorInfo driverId) headers reqBody Nothing
    encodeRequest req = defaultEncode req

data GetSpecialLocWarriorInfoReq = GetSpecialLocWarriorInfoReq String

derive instance genericGetSpecialLocWarriorInfoReq :: Generic GetSpecialLocWarriorInfoReq _
instance standardEncodeGetSpecialLocWarriorInfoReq :: StandardEncode GetSpecialLocWarriorInfoReq where standardEncode _ = standardEncode {}
instance showGetSpecialLocWarriorInfoReq :: Show GetSpecialLocWarriorInfoReq where show = genericShow
instance decodeGetSpecialLocWarriorInfoReq :: Decode GetSpecialLocWarriorInfoReq where decode = defaultDecode
instance encodeGetSpecialLocWarriorInfoReq :: Encode GetSpecialLocWarriorInfoReq where encode = defaultEncode

data SpecialLocationListCategoryReq = SpecialLocationListCategoryReq String
instance makeSpecialLocationListCategoryReq :: RestEndpoint SpecialLocationListCategoryReq where
  makeRequest reqBody@(SpecialLocationListCategoryReq category) headers = defaultMakeRequestWithoutLogs GET (EP.specialLocationListCategory category) headers reqBody Nothing
  encodeRequest req = standardEncode req

derive instance genericSpecialLocationListCategoryReq :: Generic SpecialLocationListCategoryReq _
instance standardEncodeSpecialLocationListCategoryReq :: StandardEncode SpecialLocationListCategoryReq where standardEncode _ = standardEncode {}
instance showSpecialLocationListCategoryReq :: Show SpecialLocationListCategoryReq where show = genericShow
instance decodeSpecialLocationListCategoryReq :: Decode SpecialLocationListCategoryReq where decode = defaultDecode
instance encodeSpecialLocationListCategoryReq  :: Encode SpecialLocationListCategoryReq where encode = defaultEncode

newtype SpecialLocationListRes = SpecialLocationListRes (Array SpecialLocation)

derive instance genericSpecialLocationListRes :: Generic SpecialLocationListRes _
derive instance newtypeSpecialLocationListRes :: Newtype SpecialLocationListRes _
instance standardEncodeSpecialLocationListRes :: StandardEncode SpecialLocationListRes where standardEncode (SpecialLocationListRes res) = standardEncode res
instance showSpecialLocationListRes :: Show SpecialLocationListRes where show = genericShow
instance decodeSpecialLocationListRes :: Decode SpecialLocationListRes where decode = defaultDecode
instance encodeSpecialLocationListRes :: Encode SpecialLocationListRes where encode = defaultEncode

newtype Stop = Stop {
  location :: LocationInfo,
  stopInfo :: Maybe StopInformation 
}

newtype StopInformation = StopInformation
  { id :: String,
    rideId :: String,
    stopEndLatLng :: Maybe LatLong,
    stopLocId :: String,
    stopOrder :: Int,
    stopStartLatLng :: LatLong,
    waitingTimeEnd :: Maybe String,
    waitingTimeStart :: String,
    merchantId :: Maybe String,
    merchantOperatingCityId :: Maybe String
  }

derive instance genericStop :: Generic Stop _
derive instance newtypeStop :: Newtype Stop _
instance standardEncodeStop :: StandardEncode Stop where standardEncode (Stop req) = standardEncode req
instance showStop :: Show Stop where show = genericShow
instance decodeStop :: Decode Stop where decode = defaultDecode
instance encodeStop :: Encode Stop where encode = defaultEncode
instance eqStop :: Eq Stop where eq = genericEq

derive instance genericStopInformation :: Generic StopInformation _
derive instance newtypeStopInformation :: Newtype StopInformation _
instance standardEncodeStopInformation :: StandardEncode StopInformation where standardEncode (StopInformation req) = standardEncode req
instance showStopInformation :: Show StopInformation where show = genericShow
instance decodeStopInformation :: Decode StopInformation where decode = defaultDecode
instance encodeStopInformation :: Encode StopInformation where encode = defaultEncode
instance eqStopInformation :: Eq StopInformation where eq = genericEq

data UpdateStopStatusReq = UpdateStopStatusReq String String Boolean LatLong

instance makeUpdateStopStatusReq :: RestEndpoint UpdateStopStatusReq where
    makeRequest reqBody@(UpdateStopStatusReq rideId stopId hasArrived (LatLong rqBody)) headers = defaultMakeRequestWithoutLogs POST ((if hasArrived then EP.arrivedStop else EP.departedStop) rideId stopId) headers reqBody Nothing
    encodeRequest req = standardEncode req

derive instance genericUpdateStopStatusReq :: Generic UpdateStopStatusReq _
instance standardEncodeUpdateStopStatusReq :: StandardEncode UpdateStopStatusReq where standardEncode (UpdateStopStatusReq rideId stopId hasArrived req) = standardEncode req
instance showUpdateStopStatusReq :: Show UpdateStopStatusReq where show = genericShow
instance decodeUpdateStopStatusReq :: Decode UpdateStopStatusReq where decode = defaultDecode
instance encodeUpdateStopStatusReq :: Encode UpdateStopStatusReq where encode = defaultEncode
