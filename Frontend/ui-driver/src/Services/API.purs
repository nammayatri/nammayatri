{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Services.API where

import Common.Types.App (Version(..),APIPaymentStatus(..)) as Common
import Control.Alt ((<|>))
import Control.Monad.Except (runExcept)
import Data.Either as Either
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Foreign (ForeignError(..), fail)
import Foreign.Class (class Decode, class Encode, decode, encode)
import Foreign.Generic (decodeJSON)
import Foreign.Generic.EnumEncoding (genericDecodeEnum, genericEncodeEnum, defaultGenericEnumOptions)
import Foreign.Index (readProp)
import Prelude (class Eq, class Show, bind, show, ($), (<$>), (>>=))
import Presto.Core.Types.API (class RestEndpoint, class StandardEncode, ErrorResponse, Method(..), defaultMakeRequest, standardEncode)
import Presto.Core.Utils.Encoding (defaultDecode, defaultEncode, defaultEnumDecode, defaultEnumEncode)
import Services.EndPoints as EP
import Foreign.Index (readProp)
import Control.Monad.Except (runExcept)
import Data.Either as Either
import Data.Maybe

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
  merchantId :: String
}

newtype TriggerOTPResp = TriggerOTPResp {
  authId :: String,
  attempts :: Int
}

instance makeTriggerOTPReq :: RestEndpoint TriggerOTPReq TriggerOTPResp where
 makeRequest reqBody headers = defaultMakeRequest POST (EP.triggerOTP "") headers reqBody Nothing
 decodeResponse = decodeJSON
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

instance makeVerifyTokenReq :: RestEndpoint VerifyTokenRequest VerifyTokenResp where
 makeRequest reqBody@(VerifyTokenRequest token (VerifyTokenReq rqBody)) headers = defaultMakeRequest POST (EP.verifyToken token) headers reqBody Nothing
 decodeResponse = decodeJSON
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

instance makeResendOTPReq :: RestEndpoint ResendOTPRequest ResendOTPResp where
    makeRequest reqBody@(ResendOTPRequest token) headers = defaultMakeRequest POST (EP.resendOTP token) headers reqBody Nothing
    decodeResponse = decodeJSON
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

instance makeDriverActiveInactiveReq :: RestEndpoint DriverActiveInactiveReq DriverActiveInactiveResp  where
    makeRequest reqBody@(DriverActiveInactiveReq status status_n) headers = defaultMakeRequest POST (EP.driverActiveInactiveSilent status status_n) headers reqBody Nothing
    decodeResponse = decodeJSON
    encodeRequest req = standardEncode req

-- instance makeDriverActiveInactiveSilentReq :: RestEndpoint DriverActiveInactiveReq DriverActiveInactiveResp  where
--     makeRequest reqBody@(DriverActiveInactiveReq status ) headers = defaultMakeRequest POST (EP.driverActiveInactiveSilent status status_n) headers reqBody
--     decodeResponse = decodeJSON
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
    }

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

newtype StartRideResponse = StartRideResponse ApiSuccessResult

instance makeStartRideReq :: RestEndpoint StartRideRequest StartRideResponse where
    makeRequest reqBody@(StartRideRequest rideId (StartRideReq rqBody)) headers = defaultMakeRequest POST (EP.startRide rideId) headers reqBody Nothing
    decodeResponse = decodeJSON
    encodeRequest req = standardEncode req

derive instance genericStartRideResponse :: Generic StartRideResponse _
derive instance newtypeStartRideResponse :: Newtype StartRideResponse _
instance standardEncodeStartRideResponse :: StandardEncode StartRideResponse where standardEncode (StartRideResponse req) = standardEncode req
instance showStartRideResponse :: Show StartRideResponse where show = genericShow
instance decodeStartRideResponse :: Decode StartRideResponse where decode = defaultDecode
instance encodeStartRideResponse :: Encode StartRideResponse where encode = defaultEncode


----------------------------------------------------------------------------------------------------------------------------END RIDE----------------------------------------------------------------------------------------------------------------------------------------------
-- EndRide API request, response types
data EndRideRequest = EndRideRequest String EndRideReq

newtype EndRideReq = EndRideReq
    {
      point :: Point,
      numberOfDeviation :: Maybe Boolean
    }

newtype EndRideResponse = EndRideResponse ApiSuccessResult

instance makeEndRideReq :: RestEndpoint EndRideRequest EndRideResponse where
    makeRequest reqBody@(EndRideRequest rideId (EndRideReq rqBody)) headers = defaultMakeRequest POST (EP.endRide rideId) headers reqBody Nothing
    decodeResponse = decodeJSON
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
--------------------------------------------------------CANCEL RIDE----------------------------------------------------------------------------------------------------------------------------------------------
-- Cancel Ride API request, response types
newtype DriverCancelRideResponse = DriverCancelRideResponse ApiSuccessResult
newtype DriverCancelRideReq = DriverCancelRideReq
    {
      additionalInfo :: String,
      reasonCode :: String
    }

data DriverCancelRideRequest = DriverCancelRideRequest String DriverCancelRideReq

derive instance genericDriverCancelRideReq :: Generic DriverCancelRideReq _
instance showDriverCancelRideReq :: Show DriverCancelRideReq where show = genericShow
instance standardEncodeDriverCancelRideReq :: StandardEncode DriverCancelRideReq where standardEncode (DriverCancelRideReq req) = standardEncode req
instance decodeDriverCancelRideReq :: Decode DriverCancelRideReq where decode = defaultDecode
instance encodeDriverCancelRideReq :: Encode DriverCancelRideReq where encode = defaultEncode

instance makeDriverCancelRideReq :: RestEndpoint DriverCancelRideRequest DriverCancelRideResponse where
    makeRequest reqBody@(DriverCancelRideRequest rideId (DriverCancelRideReq rqBody)) headers = defaultMakeRequest POST (EP.cancelRide rideId) headers reqBody Nothing
    decodeResponse = decodeJSON
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

newtype LogOutRes = LogOutRes ApiSuccessResult

instance makeLogOutReq  :: RestEndpoint LogOutReq LogOutRes where
    makeRequest reqBody headers = defaultMakeRequest POST (EP.logout "") headers reqBody Nothing
    decodeResponse = decodeJSON
    encodeRequest req = defaultEncode req

derive instance genericLogOutReq :: Generic LogOutReq _
instance showLogOutReq :: Show LogOutReq where show = genericShow
instance standardEncodeLogOutReq :: StandardEncode LogOutReq where standardEncode (LogOutReq) = standardEncode{}
instance decodeLogOutReq :: Decode LogOutReq where decode = defaultDecode
instance encodeLogOutReq :: Encode LogOutReq where encode = defaultEncode

derive instance genericLogOutRes :: Generic LogOutRes _
derive instance newtypeLogOutRes :: Newtype LogOutRes _
instance standardEncodeLogOutRes :: StandardEncode LogOutRes where standardEncode (LogOutRes req) = standardEncode req
instance showLogOutRes :: Show LogOutRes where show = genericShow
instance decodeLogOutRes :: Decode LogOutRes where decode = defaultDecode
instance encodeLogOutRes :: Encode LogOutRes where encode = defaultEncode

------------------------------------------------------------GET DRIVER PROFILE----------------------------------------------------------------------------------------------------------------------------------------------

-- GetDriverInfo API request, response types
data GetDriverInfoReq = GetDriverInfoReq { }

newtype GetDriverInfoResp = GetDriverInfoResp
    { id                    :: String
    , rating                :: Maybe Int
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
    , clientVersion         :: Maybe Common.Version
    , bundleVersion         :: Maybe Common.Version
    , gender                :: Maybe String
    , blocked               :: Maybe Boolean
    , numberOfRides         :: Maybe Int
    , paymentPending        :: Boolean
    , subscribed            :: Boolean
    , mediaUrl              :: Maybe String
    , autoPayStatus         :: Maybe String  
    , aadhaarCardPhoto      :: Maybe String
    , freeTrialDaysLeft     :: Maybe Int
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
        createdAt        :: String
    }

instance makeGetDriverInfoReq :: RestEndpoint GetDriverInfoReq GetDriverInfoResp where
    makeRequest reqBody headers = defaultMakeRequest GET (EP.getDriverInfo "") headers reqBody Nothing
    decodeResponse = decodeJSON
    encodeRequest req = defaultEncode req

derive instance genericGetDriverInfoReq :: Generic GetDriverInfoReq _
instance showGetDriverInfoReq :: Show GetDriverInfoReq where show = genericShow
instance standardEncodeGetDriverInfoReq :: StandardEncode GetDriverInfoReq where standardEncode (GetDriverInfoReq req) = standardEncode req
instance decodeGetDriverInfoReq :: Decode GetDriverInfoReq where decode = defaultDecode
instance encodeGetDriverInfoReq :: Encode GetDriverInfoReq where encode = defaultEncode

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

derive instance genericGetDriverInfoResp :: Generic GetDriverInfoResp _
derive instance newtypeGetDriverInfoResp :: Newtype GetDriverInfoResp _
instance standardEncodeGetDriverInfoResp :: StandardEncode GetDriverInfoResp where standardEncode (GetDriverInfoResp req) = standardEncode req
instance showGetDriverInfoResp :: Show GetDriverInfoResp where show = genericShow
instance decodeGetDriverInfoResp :: Decode GetDriverInfoResp where decode = defaultDecode
instance encodeGetDriverInfoResp :: Encode GetDriverInfoResp where encode = defaultEncode
-----------------------------------------------GET RIDES HISTORY---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

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
      vehicleVariant :: String,
      estimatedBaseFare :: Int,
      vehicleColor :: String,
      tripStartTime :: Maybe String,
      id :: String,
      updatedAt :: String,
      riderName :: Maybe String,
      rideRating :: Maybe Int,
      tripEndTime :: Maybe String,
      fromLocation :: LocationInfo,
      toLocation :: LocationInfo,
      estimatedDistance :: Int,
      exoPhone :: String,
      specialLocationTag :: Maybe String,
      requestedVehicleVariant :: Maybe String,
      customerExtraFee :: Maybe Int,
      disabilityTag :: Maybe String
  }

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
        lon :: Number
      }


instance makeGetRidesHistoryReq :: RestEndpoint GetRidesHistoryReq GetRidesHistoryResp where
    makeRequest reqBody@(GetRidesHistoryReq limit offset isActive status day) headers = defaultMakeRequest GET (EP.getRideHistory limit offset isActive status day) headers reqBody Nothing
    decodeResponse = decodeJSON
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

derive instance genericRidesInfo :: Generic RidesInfo _
derive instance newtypeRidesInfo :: Newtype RidesInfo _
instance standardEncodeRidesInfo :: StandardEncode RidesInfo where standardEncode (RidesInfo req) = standardEncode req
instance showRidesInfo :: Show RidesInfo where show = genericShow
instance decodeRidesInfo :: Decode RidesInfo where decode = defaultDecode
instance encodeRidesInfo :: Encode RidesInfo where encode = defaultEncode

data VehicleVariant = SEDAN | SUV | HATCHBACK | AUTO_VARIANT

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

data Status = NEW | INPROGRESS | COMPLETED | CANCELLED | NOTHING

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
 standardEncode (NOTHING) = standardEncode {}
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Request/Offer Ride API request, response types

newtype OfferRideReq = OfferRideReq {
  searchRequestId :: String,
  offeredFare :: Maybe Number
}

newtype OfferRideResp = OfferRideResp {
  result :: String
}

instance makeOfferRideReq :: RestEndpoint OfferRideReq OfferRideResp where
  makeRequest reqBody headers = defaultMakeRequest POST (EP.offerRide "") headers reqBody Nothing
  decodeResponse = decodeJSON
  encodeRequest req = standardEncode req

derive instance genericOfferRideReq :: Generic OfferRideReq _
derive instance newtypeOfferRideReq :: Newtype OfferRideReq _
instance standardEncodeOfferRideReq :: StandardEncode OfferRideReq where standardEncode (OfferRideReq reqBody) = standardEncode reqBody
instance showOfferRideReq :: Show OfferRideReq where show = genericShow
instance decodeOfferRideReq :: Decode OfferRideReq where decode = defaultDecode
instance encodeOfferRideReq :: Encode OfferRideReq where encode = defaultEncode

derive instance genericOfferRideResp :: Generic OfferRideResp _
derive instance newtypeOfferRideResp :: Newtype OfferRideResp _
instance standardEncodeOfferRideResp :: StandardEncode OfferRideResp where standardEncode (OfferRideResp id) = standardEncode id
instance showOfferRideResp :: Show OfferRideResp where show = genericShow
instance decodeOfferRideResp :: Decode OfferRideResp where decode = defaultDecode
instance encodeOfferRideResp :: Encode OfferRideResp where encode = defaultEncode

----------------------------------------------------------------------------------------------------------------------------UPDATE DRIVER PROFILE----------------------------------------------------------------------------------------------------------------------------------------------

-- UpdateDriverInfo API request, response types
data UpdateDriverInfoRequest = UpdateDriverInfoRequest UpdateDriverInfoReq

newtype UpdateDriverInfoReq
  = UpdateDriverInfoReq
  { middleName :: Maybe String
  , firstName :: Maybe String
  , lastName :: Maybe String
  , deviceToken :: Maybe String
  , canDowngradeToSedan :: Maybe Boolean
  , canDowngradeToHatchback :: Maybe Boolean
  , canDowngradeToTaxi :: Maybe Boolean
  , language :: Maybe String
  , clientVersion :: Maybe Common.Version
  , bundleVersion :: Maybe Common.Version
  , gender :: Maybe String
  , languagesSpoken :: Maybe (Array String)
  , hometown :: Maybe String
  , vehicleName :: Maybe String
  , availableUpiApps :: Maybe String
  }

newtype UpdateDriverInfoResp = UpdateDriverInfoResp GetDriverInfoResp

instance makeUpdateDriverInfoReq :: RestEndpoint UpdateDriverInfoRequest UpdateDriverInfoResp where
    makeRequest reqBody@(UpdateDriverInfoRequest (UpdateDriverInfoReq rqBody)) headers = defaultMakeRequest POST (EP.updateDriverInfo "") headers reqBody Nothing
    decodeResponse = decodeJSON
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

instance makeListCancelReasonReq :: RestEndpoint ListCancelReasonReq ListCancelReasonResp where
    makeRequest reqBody headers = defaultMakeRequest GET (EP.listCancelReason "" ) headers reqBody Nothing
    decodeResponse = decodeJSON
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

instance makeRouteReq :: RestEndpoint RouteReq GetRouteResp where
  makeRequest reqBody@(RouteReq rType (GetRouteReq reqB)) headers = defaultMakeRequest POST (EP.getRoute rType) headers reqBody Nothing
  decodeResponse = decodeJSON
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

------------------------------------------------------------OnBoarding Flow---------------------------------------------------------------------------------------------------------------------------------------------

-- registerDriverRC API request, response types
newtype DriverRCReq = DriverRCReq {
  vehicleRegistrationCertNumber :: String,
  operatingCity :: String,
  imageId :: String,
  dateOfRegistration :: Maybe String,
  multipleRC :: Boolean
}

newtype DriverRCResp = DriverRCResp ApiSuccessResult

instance makeDriverRCReq :: RestEndpoint DriverRCReq DriverRCResp where
  makeRequest reqBody headers = defaultMakeRequest POST (EP.registerDriverRC "") headers reqBody Nothing
  decodeResponse = decodeJSON
  encodeRequest req = standardEncode req

derive instance genericDriverRCReq :: Generic DriverRCReq _
derive instance newtypeDriverRCReq :: Newtype DriverRCReq _
instance standardEncodeDriverRCReq :: StandardEncode DriverRCReq where standardEncode (DriverRCReq body) = standardEncode body
instance showDriverRCReq :: Show DriverRCReq where show = genericShow
instance decodeDriverRCReq:: Decode DriverRCReq where decode = defaultDecode
instance encodeDriverRCReq  :: Encode DriverRCReq where encode = defaultEncode

derive instance genericDriverRCResp :: Generic DriverRCResp _
derive instance newtypeDriverRCResp :: Newtype DriverRCResp _
instance standardEncodeDriverRCResp :: StandardEncode DriverRCResp where standardEncode (DriverRCResp body) = standardEncode body
instance showDriverRCResp :: Show DriverRCResp where show = genericShow
instance decodeDriverRCResp:: Decode DriverRCResp where decode = defaultDecode
instance encodeDriverRCResp  :: Encode DriverRCResp where encode = defaultEncode


-- registerDriverDL API request, response types
newtype DriverDLReq = DriverDLReq {
  driverLicenseNumber :: String,
  driverDateOfBirth :: String,
  operatingCity :: String,
  imageId1 :: String,
  imageId2 :: Maybe String,
  dateOfIssue :: Maybe String
}

newtype DriverDLResp = DriverDLResp ApiSuccessResult

instance makeDriverDLReq :: RestEndpoint DriverDLReq DriverDLResp where
  makeRequest reqBody headers = defaultMakeRequest POST (EP.registerDriverDL "") headers reqBody Nothing
  decodeResponse = decodeJSON
  encodeRequest req = standardEncode req

derive instance genericDriverDLReq :: Generic DriverDLReq _
derive instance newtypeDriverDLReq :: Newtype DriverDLReq _
instance standardEncodeDriverDLReq :: StandardEncode DriverDLReq where standardEncode (DriverDLReq body) = standardEncode body
instance showDriverDLReq :: Show DriverDLReq where show = genericShow
instance decodeDriverDLReq:: Decode DriverDLReq where decode = defaultDecode
instance encodeDriverDLReq  :: Encode DriverDLReq where encode = defaultEncode

derive instance genericDriverDLResp :: Generic DriverDLResp _
derive instance newtypeDriverDLResp :: Newtype DriverDLResp _
instance standardEncodeDriverDLResp :: StandardEncode DriverDLResp where standardEncode (DriverDLResp body) = standardEncode body
instance showDriverDLResp :: Show DriverDLResp where show = genericShow
instance decodeDriverDLResp:: Decode DriverDLResp where decode = defaultDecode
instance encodeDriverDLResp  :: Encode DriverDLResp where encode = defaultEncode

-- validateImage API request, response types
newtype ValidateImageReq = ValidateImageReq {
  image :: String,
  imageType :: String
}

newtype ValidateImageRes = ValidateImageRes {
  imageId :: String
}

instance makeValidateImageReq :: RestEndpoint ValidateImageReq ValidateImageRes where
  makeRequest reqBody headers = defaultMakeRequest POST (EP.validateImage "") headers reqBody Nothing
  decodeResponse = decodeJSON
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
data DriverRegistrationStatusReq = DriverRegistrationStatusReq { }

newtype DriverRegistrationStatusResp = DriverRegistrationStatusResp
    { dlVerificationStatus :: String
    , rcVerificationStatus :: String
    , aadhaarVerificationStatus :: String
    }

instance makeDriverRegistrationStatusReq :: RestEndpoint DriverRegistrationStatusReq DriverRegistrationStatusResp where
    makeRequest reqBody headers = defaultMakeRequest GET (EP.driverRegistrationStatus "") headers reqBody Nothing
    decodeResponse = decodeJSON
    encodeRequest req = defaultEncode req

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

newtype ReferDriverResp = ReferDriverResp ApiSuccessResult

instance makeReferDriverReq :: RestEndpoint ReferDriverReq ReferDriverResp where
    makeRequest reqBody headers = defaultMakeRequest POST (EP.referDriver "") headers reqBody Nothing
    decodeResponse = decodeJSON
    encodeRequest req = defaultEncode req

derive instance genericReferDriverReq :: Generic ReferDriverReq _
instance showReferDriverReq :: Show ReferDriverReq where show = genericShow
instance standardEncodeReferDriverReq :: StandardEncode ReferDriverReq where standardEncode (ReferDriverReq req) = standardEncode req
instance decodeReferDriverReq :: Decode ReferDriverReq where decode = defaultDecode
instance encodeReferDriverReq :: Encode ReferDriverReq where encode = defaultEncode

derive instance genericReferDriverResp :: Generic ReferDriverResp _
instance showReferDriverResp :: Show ReferDriverResp where show = genericShow
instance standardEncodeReferDriverResp :: StandardEncode ReferDriverResp where standardEncode (ReferDriverResp req) = standardEncode req
instance decodeReferDriverResp :: Decode ReferDriverResp where decode = defaultDecode
instance encodeReferDriverResp :: Encode ReferDriverResp where encode = defaultEncode

-- DriverProfileStats API request, response types

data DriverProfileStatsReq = DriverProfileStatsReq String

newtype DriverProfileStatsResp = DriverProfileStatsResp
    {
      totalRidesOfDay :: Int
    , totalEarningsOfDay :: Int
    , bonusEarning :: Int
    }

instance makeGetDriverProfileStatsReq :: RestEndpoint DriverProfileStatsReq DriverProfileStatsResp where
    makeRequest reqBody@(DriverProfileStatsReq date) headers = defaultMakeRequest GET (EP.getstatsInfo date) headers reqBody Nothing
    decodeResponse = decodeJSON
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

newtype DriverArrivedRes = DriverArrivedRes ApiSuccessResult

derive instance genericDriverArrivedRequest :: Generic DriverArrivedRequest _
instance standardEncodeDriverArrivedRequest :: StandardEncode DriverArrivedRequest where standardEncode (DriverArrivedRequest rideId req) = standardEncode req
instance showDriverArrivedRequest :: Show DriverArrivedRequest where show = genericShow
instance decodeDriverArrivedRequest :: Decode DriverArrivedRequest where decode = defaultDecode
instance encodeDriverArrivedRequest :: Encode DriverArrivedRequest where encode = defaultEncode


instance makeDriverArrivedReq :: RestEndpoint DriverArrivedRequest DriverArrivedRes where
    makeRequest reqBody@(DriverArrivedRequest rideId (DriverArrivedReq rqBody)) headers = defaultMakeRequest POST (EP.driverArrived rideId) headers reqBody Nothing
    decodeResponse = decodeJSON
    encodeRequest req = standardEncode req

derive instance genericDriverArrivedReq :: Generic DriverArrivedReq _
instance showDriverArrivedReq :: Show DriverArrivedReq where show = genericShow
instance standardEncodeDriverArrivedReq :: StandardEncode DriverArrivedReq where standardEncode (DriverArrivedReq req) = standardEncode req
instance decodeDriverArrivedReq :: Decode DriverArrivedReq where decode = defaultDecode
instance encodeDriverArrivedReq :: Encode DriverArrivedReq where encode = defaultEncode

derive instance genericDriverArrivedRes :: Generic DriverArrivedRes _
instance showGetDriverArrivedRes :: Show DriverArrivedRes where show = genericShow
instance standardEncodeDriverArrivedRes :: StandardEncode DriverArrivedRes where standardEncode (DriverArrivedRes req) = standardEncode req
instance decodeDriverArrivedRes :: Decode DriverArrivedRes where decode = defaultDecode
instance encodeDriverArrivedRes :: Encode DriverArrivedRes where encode = defaultEncode

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

instance makeFlowStatusReq :: RestEndpoint FlowStatusReq FlowStatusRes where
    makeRequest reqBody headers = defaultMakeRequest GET (EP.flowStatus "") headers reqBody Nothing
    decodeResponse = decodeJSON
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

instance makeMessageListReq :: RestEndpoint MessageListReq MessageListRes where
    makeRequest reqBody@(MessageListReq limit offset) headers = defaultMakeRequest GET (EP.messageList limit offset) headers reqBody Nothing
    decodeResponse = decodeJSON
    encodeRequest req = defaultEncode req

derive instance genericMessageListReq :: Generic MessageListReq _
instance showMessageListReq :: Show MessageListReq where show = genericShow
instance standardEncodeMessageListReq :: StandardEncode MessageListReq where standardEncode (MessageListReq _ _) = standardEncode {}
instance decodeMessageListReq :: Decode MessageListReq where decode = defaultDecode
instance encodeMessageListReq :: Encode MessageListReq where encode = defaultEncode

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

newtype MessageSeenRes = MessageSeenRes ApiSuccessResult

instance makeMessageSeenReq :: RestEndpoint MessageSeenReq MessageSeenRes where
    makeRequest reqBody@(MessageSeenReq messageId) headers = defaultMakeRequest PUT (EP.messageSeen messageId) headers reqBody Nothing
    decodeResponse = decodeJSON
    encodeRequest req = defaultEncode req

derive instance genericMessageSeenReq :: Generic MessageSeenReq _
instance showMessageSeenReq :: Show MessageSeenReq where show = genericShow
instance standardEncodeMessageSeenReq :: StandardEncode MessageSeenReq where standardEncode (MessageSeenReq _) = standardEncode {}
instance decodeMessageSeenReq :: Decode MessageSeenReq where decode = defaultDecode
instance encodeMessageSeenReq :: Encode MessageSeenReq where encode = defaultEncode

derive instance genericMessageSeenRes :: Generic MessageSeenRes _
derive instance newtypeMessageSeenRes :: Newtype MessageSeenRes _
instance showMessageSeenRes :: Show MessageSeenRes where show = genericShow
instance standardEncodeMessageSeenRes :: StandardEncode MessageSeenRes where standardEncode (MessageSeenRes res) = standardEncode res
instance decodeMessageSeenRes :: Decode MessageSeenRes where decode = defaultDecode
instance encodeMessageSeenRes :: Encode MessageSeenRes where encode = defaultEncode

------------------------------------------------------ likeMessage -----------------------------------------------

data LikeMessageReq = LikeMessageReq String

newtype LikeMessageRes = LikeMessageRes ApiSuccessResult

instance makeLikeMessageReq :: RestEndpoint LikeMessageReq LikeMessageRes where
    makeRequest reqBody@(LikeMessageReq messageId) headers = defaultMakeRequest PUT (EP.likeMessage messageId) headers reqBody Nothing
    decodeResponse = decodeJSON
    encodeRequest req = defaultEncode req

derive instance genericLikeMessageReq :: Generic LikeMessageReq _
instance showLikeMessageReq :: Show LikeMessageReq where show = genericShow
instance standardEncodeLikeMessageReq :: StandardEncode LikeMessageReq where standardEncode (LikeMessageReq _) = standardEncode {}
instance decodeLikeMessageReq :: Decode LikeMessageReq where decode = defaultDecode
instance encodeLikeMessageReq :: Encode LikeMessageReq where encode = defaultEncode

derive instance genericLikeMessageRes :: Generic LikeMessageRes _
derive instance newtypeLikeMessageRes :: Newtype LikeMessageRes _
instance showLikeMessageRes :: Show LikeMessageRes where show = genericShow
instance standardEncodeLikeMessageRes :: StandardEncode LikeMessageRes where standardEncode (LikeMessageRes res) = standardEncode res
instance decodeLikeMessageRes :: Decode LikeMessageRes where decode = defaultDecode
instance encodeLikeMessageRes :: Encode LikeMessageRes where encode = defaultEncode

------------------------------------------------------ messageReply -----------------------------------------------

data MessageResponseReq = MessageResponseReq String MessageReplyReq

newtype MessageReplyReq = MessageReplyReq {
    reply :: String
  }

newtype MessageResponseRes = MessageResponseRes ApiSuccessResult

instance makeMessageResponseReq :: RestEndpoint MessageResponseReq MessageResponseRes where
    makeRequest reqBody@(MessageResponseReq messageId (MessageReplyReq req)) headers = defaultMakeRequest PUT (EP.messageResponse messageId) headers reqBody Nothing
    decodeResponse = decodeJSON
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

derive instance genericMessageResponseRes :: Generic MessageResponseRes _
derive instance newtypeMessageResponseRes :: Newtype MessageResponseRes _
instance showMessageResponseRes :: Show MessageResponseRes where show = genericShow
instance standardEncodeMessageResponseRes :: StandardEncode MessageResponseRes where standardEncode (MessageResponseRes res) = standardEncode res
instance decodeMessageResponseRes :: Decode MessageResponseRes where decode = defaultDecode
instance encodeMessageResponseRes :: Encode MessageResponseRes where encode = defaultEncode

--------------------------------------------------- linkReferral ----------------------------------------------------

newtype LinkReferralCodeReq = LinkReferralCodeReq {
    referralLinkPassword :: String
  , referralCode :: String
}

newtype LinkReferralCodeRes = LinkReferralCodeRes ApiSuccessResult

instance makeLinkReferralCodeReq :: RestEndpoint LinkReferralCodeReq LinkReferralCodeRes where
    makeRequest reqBody@(LinkReferralCodeReq date) headers = defaultMakeRequest POST (EP.linkReferralCode "") headers reqBody Nothing
    decodeResponse = decodeJSON
    encodeRequest req = defaultEncode req

derive instance genericLinkReferralCodeReq :: Generic LinkReferralCodeReq _
instance showLinkReferralCodeReq :: Show LinkReferralCodeReq where show = genericShow
instance standardLinkReferralCodeReq :: StandardEncode LinkReferralCodeReq where standardEncode (LinkReferralCodeReq req) = standardEncode req
instance decodeLinkReferralCodeReq :: Decode LinkReferralCodeReq where decode = defaultDecode
instance encodeLinkReferralCodeReq :: Encode LinkReferralCodeReq where encode = defaultEncode

derive instance genericLinkReferralCodeRes :: Generic LinkReferralCodeRes _
instance showGetLinkReferralCodeRes :: Show LinkReferralCodeRes where show = genericShow
instance standardEncodeLinkReferralCodeRes :: StandardEncode LinkReferralCodeRes where standardEncode (LinkReferralCodeRes req) = standardEncode req
instance decodeLinkReferralCodeRes :: Decode LinkReferralCodeRes where decode = defaultDecode
instance encodeLinkReferralCodeRes :: Encode LinkReferralCodeRes where encode = defaultEncode


--------------------------------------------------- getPerformance ----------------------------------------------------

newtype GetPerformanceReq = GetPerformanceReq {}

newtype GetPerformanceRes = GetPerformanceRes {
  referrals :: {
    totalActivatedCustomers :: Int,
    totalReferredCustomers :: Int
  }
}

instance makeGetPerformanceReq :: RestEndpoint GetPerformanceReq GetPerformanceRes where
    makeRequest reqBody@(GetPerformanceReq date) headers = defaultMakeRequest GET (EP.getPerformance "") headers reqBody Nothing
    decodeResponse = decodeJSON
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


instance makeDriverAlternateNumberReq :: RestEndpoint DriverAlternateNumberReq DriverAlternateNumberResp where
  makeRequest reqBody headers = defaultMakeRequest POST (EP.driverAlternateNumber "") headers reqBody Nothing
  decodeResponse = decodeJSON
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

newtype DriverAlternateNumberOtpResp =  DriverAlternateNumberOtpResp  ApiSuccessResult


instance makeDriverAlternateNumberOtpReq :: RestEndpoint DriverAlternateNumberOtpReq DriverAlternateNumberOtpResp where
  makeRequest reqBody headers = defaultMakeRequest POST (EP.verifyAlternateNumberOTP "") headers reqBody Nothing
  decodeResponse = decodeJSON
  encodeRequest req = standardEncode req


derive instance genericDriverAlternateNumberOtpReq :: Generic DriverAlternateNumberOtpReq _
derive instance newtypeDriverAlternateNumberOtpReq :: Newtype DriverAlternateNumberOtpReq _
instance standardEncodeDriverAlternateNumberOtpReq :: StandardEncode DriverAlternateNumberOtpReq where standardEncode (DriverAlternateNumberOtpReq reqBody) = standardEncode reqBody
instance showDriverAlternateNumberOtpReq :: Show DriverAlternateNumberOtpReq where show = genericShow
instance decodeDriverAlternateNumberOtpReq :: Decode DriverAlternateNumberOtpReq where decode = defaultDecode
instance encodeDriverAlternateNumberOtpReq :: Encode DriverAlternateNumberOtpReq where encode = defaultEncode

derive instance genericDriverAlternateNumberOtpResp :: Generic DriverAlternateNumberOtpResp _
derive instance newtypeDriverAlternateNumberOtpResp :: Newtype DriverAlternateNumberOtpResp _
instance standardEncodeDriverAlternateNumberOtpResp :: StandardEncode DriverAlternateNumberOtpResp where standardEncode (DriverAlternateNumberOtpResp id) = standardEncode id
instance showDriverAlternateNumberOtpResp :: Show DriverAlternateNumberOtpResp where show = genericShow
instance decodeDriverAlternateNumberOtpResp :: Decode DriverAlternateNumberOtpResp where decode = defaultDecode
instance encodeDriverAlternateNumberOtpResp :: Encode DriverAlternateNumberOtpResp where encode = defaultEncode



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

instance makeAlternateNumberResendOTPReq :: RestEndpoint AlternateNumberResendOTPRequest AlternateNumberResendOTPResp where
    makeRequest reqBody headers = defaultMakeRequest POST (EP.alternateNumberResendOTP "") headers reqBody Nothing
    decodeResponse = decodeJSON
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

newtype RemoveAlternateNumberResp = RemoveAlternateNumberResp ApiSuccessResult


instance makeRemoveAlternateNumberReq :: RestEndpoint RemoveAlternateNumberRequest RemoveAlternateNumberResp where
    makeRequest reqBody headers = defaultMakeRequest DELETE (EP.removeAlternateNumber "") headers reqBody Nothing
    decodeResponse = decodeJSON
    encodeRequest req = standardEncode req

derive instance genericRemoveAlternateNumberResp :: Generic RemoveAlternateNumberResp _
derive instance newtypeRemoveAlternateNumberResp :: Newtype RemoveAlternateNumberResp _
instance standardEncodeRemoveAlternateNumberResp :: StandardEncode RemoveAlternateNumberResp where standardEncode (RemoveAlternateNumberResp req) = standardEncode req
instance showRemoveAlternateNumberResp :: Show RemoveAlternateNumberResp where show = genericShow
instance decodeRemoveAlternateNumberResp :: Decode RemoveAlternateNumberResp where decode = defaultDecode
instance encodeRemoveAlternateNumberResp :: Encode RemoveAlternateNumberResp where encode = defaultEncode

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
  , issueCategoryId :: String
  }

instance makeGetCategoriesReq :: RestEndpoint GetCategoriesReq GetCategoriesRes where
    makeRequest reqBody@(GetCategoriesReq language) headers = defaultMakeRequest GET (EP.getCategories language) headers reqBody Nothing
    decodeResponse    = decodeJSON
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

instance makeGetOptionsReq :: RestEndpoint GetOptionsReq GetOptionsRes where
    makeRequest reqBody@(GetOptionsReq categoryId language) headers = defaultMakeRequest GET (EP.getOptions categoryId language) headers reqBody Nothing
    decodeResponse    = decodeJSON
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
  }

newtype PostIssueRes = PostIssueRes { issueReportId :: String }

instance makePostIssueReq :: RestEndpoint PostIssueReq PostIssueRes where
    makeRequest reqBody@(PostIssueReq issueDetails) headers = defaultMakeRequest POST (EP.postIssue "") headers reqBody Nothing
    decodeResponse    = decodeJSON
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

--------------------------------------------------- IssueInfo ----------------------------------------------------
newtype IssueInfoReq = IssueInfoReq String

newtype IssueInfoRes = IssueInfoRes
  { mediaFiles    :: Array { url :: String, _type :: String }
  , description   :: String
  , issueReportId :: String
  , categoryId    :: Maybe String
  }

instance makeIssueInfoReq :: RestEndpoint IssueInfoReq IssueInfoRes where
    makeRequest reqBody@(IssueInfoReq issueId) headers = defaultMakeRequest GET (EP.issueInfo issueId) headers reqBody Nothing
    decodeResponse    = decodeJSON
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

instance makeCallCustomerReq :: RestEndpoint CallCustomerReq CallCustomerRes where
  makeRequest reqBody@(CallCustomerReq rideId) headers = defaultMakeRequest POST (EP.callDriverToCustomer rideId) headers reqBody Nothing
  decodeResponse = decodeJSON
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
    createdAt :: String

  }

instance makeFetchIssueListReq :: RestEndpoint FetchIssueListReq FetchIssueListResp where
    makeRequest reqBody headers = defaultMakeRequest GET (EP.fetchIssueList "") headers reqBody Nothing
    decodeResponse = decodeJSON
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


newtype DeleteIssueResp =  DeleteIssueResp  ApiSuccessResult


instance makeDeleteIssueReq :: RestEndpoint DeleteIssueReq DeleteIssueResp where
  makeRequest reqBody@(DeleteIssueReq issueId) headers = defaultMakeRequest DELETE (EP.deleteIssue issueId) headers reqBody Nothing
  decodeResponse = decodeJSON
  encodeRequest req = standardEncode req


derive instance genericDeleteIssueReq :: Generic DeleteIssueReq _
derive instance newtypeDeleteIssueReq :: Newtype DeleteIssueReq _
instance standardEncodeDeleteIssueReq :: StandardEncode DeleteIssueReq where standardEncode (DeleteIssueReq reqBody) = standardEncode reqBody
instance showDeleteIssueReq :: Show DeleteIssueReq where show = genericShow
instance decodeDeleteIssueReq :: Decode DeleteIssueReq where decode = defaultDecode
instance encodeDeleteIssueReq :: Encode DeleteIssueReq where encode = defaultEncode

derive instance genericDeleteIssueResp :: Generic DeleteIssueResp _
derive instance newtypeDeleteIssueResp :: Newtype DeleteIssueResp _
instance standardEncodeDeleteIssueResp :: StandardEncode DeleteIssueResp where standardEncode (DeleteIssueResp id) = standardEncode id
instance showDeleteIssueResp :: Show DeleteIssueResp where show = genericShow
instance decodeDeleteIssueResp :: Decode DeleteIssueResp  where decode = defaultDecode
instance encodeDeleteIssueResp :: Encode DeleteIssueResp where encode = defaultEncode



--------------------------------------------------- rideOtp ----------------------------------------------------

newtype OTPRideReq = OTPRideReq
    {
      specialZoneOtpCode :: String,
      point :: LatLong
    }

data OTPRideRequest = OTPRideRequest OTPRideReq

instance makeOTPRideReq :: RestEndpoint OTPRideRequest RidesInfo where
    makeRequest reqBody@(OTPRideRequest (OTPRideReq rqBody)) headers = defaultMakeRequest POST (EP.otpRide "") headers reqBody Nothing
    decodeResponse = decodeJSON
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
     rideId :: String
  }

newtype OnCallRes = OnCallRes
  {
    result :: String
  }

instance makeOnCallReq :: RestEndpoint OnCallReq OnCallRes where
 makeRequest reqBody headers = defaultMakeRequest POST (EP.onCall "") headers reqBody Nothing
 decodeResponse = decodeJSON
 encodeRequest req = standardEncode req

derive instance genericOnCallReq :: Generic OnCallReq _
derive instance newtypeOnCallReq:: Newtype OnCallReq _
instance standardEncodeOnCallReq :: StandardEncode OnCallReq where standardEncode (OnCallReq req) = standardEncode req
instance showOnCallReq :: Show OnCallReq where show = genericShow
instance decodeOnCallReq :: Decode OnCallReq where decode = defaultDecode
instance encodeOnCallReq :: Encode OnCallReq where encode = defaultEncode

derive instance genericOnCallRes :: Generic OnCallRes _
derive instance newtypeOnCallRes :: Newtype OnCallRes _
instance standardEncodeOnCallRes :: StandardEncode OnCallRes where standardEncode (OnCallRes res) = standardEncode res
instance showOnCallRes :: Show OnCallRes where show = genericShow
instance decodeOnCallRes :: Decode OnCallRes where decode = defaultDecode
instance encodeOnCallRes :: Encode OnCallRes where encode = defaultEncode
------------------------------------------------------ leaderBoard -----------------------------------------------

data LeaderBoardReq = DailyRequest String
                    | WeeklyRequest String String

newtype LeaderBoardRes = LeaderBoardRes {
    lastUpdatedAt :: Maybe String
  , driverList :: Array DriversInfo
}

newtype DriversInfo = DriversInfo
  { name :: String
  , totalRides :: Int
  , rank :: Int
  , isCurrentDriver :: Boolean
  , totalDistance :: Int
  }

instance makeLeaderBoardReq :: RestEndpoint LeaderBoardReq LeaderBoardRes where
    makeRequest reqBody@(DailyRequest date) headers = defaultMakeRequest GET (EP.leaderBoardDaily date) headers reqBody Nothing
    makeRequest reqBody@(WeeklyRequest fromDate toDate) headers = defaultMakeRequest GET (EP.leaderBoardWeekly fromDate toDate) headers reqBody Nothing
    decodeResponse = decodeJSON
    encodeRequest req = defaultEncode req

derive instance genericLeaderBoardReq :: Generic LeaderBoardReq _
instance showLeaderBoardReq :: Show LeaderBoardReq where show = genericShow
instance standardEncodeLeaderBoardReq :: StandardEncode LeaderBoardReq
  where
    standardEncode (DailyRequest _) = standardEncode {}
    standardEncode (WeeklyRequest _ _) = standardEncode {}
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

------------------------------------------ currentDateAndTime --------------------------------------

newtype CurrentDateAndTimeReq = CurrentDateAndTimeReq String
newtype CurrentDateAndTimeRes =  CurrentDateAndTimeRes
  {
    timestamp :: Maybe Number
  }


instance makeCurrentDateAndTimeReq :: RestEndpoint CurrentDateAndTimeReq CurrentDateAndTimeRes where
  makeRequest reqBody headers = defaultMakeRequest GET (EP.currentDateAndTime "") headers reqBody Nothing
  decodeResponse = decodeJSON
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
    }

instance makeGetAllRcDataReq :: RestEndpoint GetAllRcDataReq GetAllRcDataResp where
    makeRequest reqBody headers = defaultMakeRequest GET (EP.getAllRcData "") headers reqBody Nothing
    decodeResponse = decodeJSON
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

newtype MakeRcActiveOrInactiveResp = MakeRcActiveOrInactiveResp ApiSuccessResult

instance makeRcActiveOrInactiveReq :: RestEndpoint MakeRcActiveOrInactiveReq MakeRcActiveOrInactiveResp where
  makeRequest reqBody headers = defaultMakeRequest POST (EP.makeRcActiveOrInactive "") headers reqBody Nothing
  decodeResponse = decodeJSON
  encodeRequest req = standardEncode req

derive instance genericMakeRcActiveOrInactiveReq :: Generic MakeRcActiveOrInactiveReq _
derive instance newtypeMakeRcActiveOrInactiveReq :: Newtype MakeRcActiveOrInactiveReq _
instance standardEncodeMakeRcActiveOrInactiveReq :: StandardEncode MakeRcActiveOrInactiveReq where standardEncode (MakeRcActiveOrInactiveReq body) = standardEncode body
instance showMakeRcActiveOrInactiveReq :: Show MakeRcActiveOrInactiveReq where show = genericShow
instance decodeMakeRcActiveOrInactiveReq:: Decode MakeRcActiveOrInactiveReq where decode = defaultDecode
instance encodeMakeRcActiveOrInactiveReq  ::Encode MakeRcActiveOrInactiveReq where encode = defaultEncode

derive instance genericMakeRcActiveOrInactiveResp :: Generic MakeRcActiveOrInactiveResp _
derive instance newtypeMakeRcActiveOrInactiveResp :: Newtype MakeRcActiveOrInactiveResp _
instance standardEncodeMakeRcActiveOrInactiveResp :: StandardEncode MakeRcActiveOrInactiveResp where standardEncode (MakeRcActiveOrInactiveResp body) = standardEncode body
instance showMakeRcActiveOrInactiveResp :: Show MakeRcActiveOrInactiveResp where show = genericShow
instance decodeMakeRcActiveOrInactiveResp:: Decode MakeRcActiveOrInactiveResp where decode = defaultDecode
instance encodeMakeRcActiveOrInactiveResp  ::Encode MakeRcActiveOrInactiveResp where encode = defaultEncode

newtype DeleteRcReq = DeleteRcReq {
  rcNo :: String
}

newtype DeleteRcResp = DeleteRcResp ApiSuccessResult

instance deleteRcReq :: RestEndpoint DeleteRcReq DeleteRcResp where
  makeRequest reqBody headers = defaultMakeRequest POST (EP.deleteRc "") headers reqBody Nothing
  decodeResponse = decodeJSON
  encodeRequest req = standardEncode req

derive instance genericDeleteRcReq :: Generic DeleteRcReq _
derive instance newtypeDeleteRcReq :: Newtype DeleteRcReq _
instance standardEncodeDeleteRcReq :: StandardEncode DeleteRcReq where standardEncode (DeleteRcReq body) = standardEncode body
instance showDeleteRcReq :: Show DeleteRcReq where show = genericShow
instance decodeDeleteRcReq:: Decode DeleteRcReq where decode = defaultDecode
instance encodeDeleteRcReq  ::Encode DeleteRcReq where encode = defaultEncode

derive instance genericDeleteRcResp :: Generic DeleteRcResp _
derive instance newtypeDeleteRcResp :: Newtype DeleteRcResp _
instance standardEncodeDeleteRcResp :: StandardEncode DeleteRcResp where standardEncode (DeleteRcResp body) = standardEncode body
instance showDeleteRcResp :: Show DeleteRcResp where show = genericShow
instance decodeDeleteRcResp:: Decode DeleteRcResp where decode = defaultDecode
instance encodeDeleteRcResp  ::Encode DeleteRcResp where encode = defaultEncode


data CallDriverToDriverReq = CallDriverToDriverReq String

newtype CallDriverToDriverResp = CallDriverToDriverResp
    {
     callId :: String
    }

instance makeGetCallDriverToDriverReq :: RestEndpoint CallDriverToDriverReq CallDriverToDriverResp where
    makeRequest reqBody@(CallDriverToDriverReq rcNo) headers = defaultMakeRequest GET (EP.callDriverToDriver rcNo) headers reqBody Nothing
    decodeResponse = decodeJSON
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

instance makeDriverProfileSummaryReq :: RestEndpoint DriverProfileSummaryReq DriverProfileSummaryRes where
  makeRequest reqBody headers = defaultMakeRequest GET (EP.profileSummary "") headers reqBody Nothing
  decodeResponse = decodeJSON
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
    status :: String ,
    id :: String,
    order_id :: String,
    payment_links :: PaymentLinks
  }

newtype PaymentPagePayload = PaymentPagePayload
  {
    requestId :: Maybe String,
    service :: Maybe String,
    payload :: PayPayload
  }

newtype PayPayload = PayPayload
  {
    action :: Maybe String,
    amount :: String,
    clientAuthToken :: String,
    clientAuthTokenExpiry :: String,
    clientId :: Maybe String,
    currency :: String,
    customerEmail :: Maybe String,
    customerId :: Maybe String,
    customerPhone :: Maybe String,
    description :: Maybe String,
    environment :: Maybe String,
    firstName :: Maybe String,
    lastName :: Maybe String,
    merchantId :: Maybe String,
    options_getUpiDeepLinks :: Maybe Boolean,
    orderId :: Maybe String,
    returnUrl :: Maybe String,
    "options.createMandate" :: Maybe String,
    "mandate.maxAmount" :: Maybe String,
    "mandate.endDate" :: Maybe String,
    "mandate.startDate" :: Maybe String,
    language :: Maybe String
  }

newtype PaymentLinks = PaymentLinks
  {
    web :: Maybe String,
    iframe :: Maybe String,
    mobile :: Maybe String
  }

instance makeCreateOrderReq :: RestEndpoint CreateOrderReq CreateOrderRes where
 makeRequest reqBody@(CreateOrderReq estimateId) headers = defaultMakeRequest POST (EP.createOrder estimateId) headers reqBody Nothing
 decodeResponse = decodeJSON
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
instance showCreateOrderRes :: Show CreateOrderRes where show = genericShow
instance decodeCreateOrderRes :: Decode CreateOrderRes where decode = defaultDecode
instance encodeCreateOrderRes :: Encode CreateOrderRes where encode = defaultEncode

derive instance genericPayPayload :: Generic PayPayload _
derive instance newtypePayPayload :: Newtype PayPayload _
instance standardEncodePayPayload :: StandardEncode PayPayload where standardEncode (PayPayload id) = standardEncode id
instance showPayPayload :: Show PayPayload where show = genericShow
instance decodePayPayload :: Decode PayPayload where decode = defaultDecode
instance encodePayPayload :: Encode PayPayload where encode = defaultEncode

derive instance genericPaymentPagePayload :: Generic PaymentPagePayload _
derive instance newtypePaymentPagePayload :: Newtype PaymentPagePayload _
instance standardEncodePaymentPagePayload :: StandardEncode PaymentPagePayload where standardEncode (PaymentPagePayload id) = standardEncode id
instance showPaymentPagePayload :: Show PaymentPagePayload where show = genericShow
instance decodePaymentPagePayload :: Decode PaymentPagePayload where decode = defaultDecode
instance encodePaymentPagePayload :: Encode PaymentPagePayload where encode = defaultEncode


-- order status

data OrderStatusReq = OrderStatusReq String

newtype OrderStatusRes = OrderStatusRes
  {
    status :: Common.APIPaymentStatus
  }

instance makeOrderStatusReq :: RestEndpoint OrderStatusReq OrderStatusRes where
 makeRequest reqBody@(OrderStatusReq orderId) headers = defaultMakeRequest GET (EP.orderStatus orderId) headers reqBody Nothing
 decodeResponse = decodeJSON
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
  , status :: Common.APIPaymentStatus
}

data DriverFeeStatus = ONGOING | PAYMENT_PENDING | PAYMENT_OVERDUE | CLEARED | EXEMPTED | COLLECTED_CASH | INACTIVE

instance makeGetPaymentHistoryReq :: RestEndpoint GetPaymentHistoryReq GetPaymentHistoryResp where
 makeRequest reqBody@(GetPaymentHistoryReq from to status) headers = defaultMakeRequest GET (EP.paymentHistory from to status) headers reqBody Nothing
 decodeResponse = decodeJSON
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
instance decodeDriverFeeStatus :: Decode DriverFeeStatus where decode = defaultEnumDecode
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

instance makeGenerateAadhaarOTPReq :: RestEndpoint GenerateAadhaarOTPReq GenerateAadhaarOTPResp where
    makeRequest reqBody headers = defaultMakeRequest POST (EP.triggerAadhaarOTP "") headers reqBody Nothing
    decodeResponse = decodeJSON
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

instance makeVerifyAadhaarOTPReq :: RestEndpoint VerifyAadhaarOTPReq VerifyAadhaarOTPResp where
    makeRequest reqBody headers = defaultMakeRequest POST (EP.verifyAadhaarOTP "") headers reqBody Nothing
    decodeResponse = decodeJSON
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


instance makeUnVerifiedDataReq :: RestEndpoint UnVerifiedDataReq ApiSuccessResult where
    makeRequest reqBody headers = defaultMakeRequest POST (EP.unVerifiedAadhaarData "") headers reqBody Nothing
    decodeResponse = decodeJSON
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
  currentDues :: Number
}

newtype OfferEntity = OfferEntity {
  title :: Maybe String,
  description :: Maybe String,
  tnc :: Maybe String
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

instance makeUiPlansReq :: RestEndpoint UiPlansReq UiPlansResp where
 makeRequest reqBody@(UiPlansReq dummy) headers = defaultMakeRequest GET (EP.getUiPlans "") headers reqBody Nothing
 decodeResponse = decodeJSON
 encodeRequest req = standardEncode req

derive instance genericUiPlansReq :: Generic UiPlansReq _
instance standardEncodeUiPlansReq :: StandardEncode UiPlansReq where standardEncode (UiPlansReq dummy ) = standardEncode dummy
instance showUiPlansReq :: Show UiPlansReq where show = genericShow
instance decodeUiPlansReq :: Decode UiPlansReq where decode = defaultDecode
instance encodeUiPlansReq :: Encode UiPlansReq where encode = defaultEncode


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



-------------------------------------------------- SubscribePlan ------------------------------

newtype SubscribePlanReq = SubscribePlanReq String

newtype SubscribePlanResp = SubscribePlanResp {
  orderResp :: CreateOrderRes,
  status :: String,
  orderId :: String
}

instance makeSubscribePlanReq :: RestEndpoint SubscribePlanReq SubscribePlanResp where
    makeRequest reqBody@(SubscribePlanReq planId) headers = defaultMakeRequest POST (EP.subscribePlan planId) headers reqBody Nothing
    decodeResponse = decodeJSON
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


instance makePaymentDuesReq :: RestEndpoint PaymentDuesReq PaymentDuesResp where
 makeRequest reqBody@(PaymentDuesReq dummy ) headers = defaultMakeRequest GET (EP.getUiPlans "") headers reqBody Nothing
 decodeResponse = decodeJSON
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

newtype ResumeMandateResp = ResumeMandateResp ApiSuccessResult

instance makeResumeMandateReq :: RestEndpoint ResumeMandateReq ResumeMandateResp where
 makeRequest reqBody@(ResumeMandateReq driverId) headers = defaultMakeRequest PUT (EP.resumeMandate driverId) headers reqBody Nothing
 decodeResponse = decodeJSON
 encodeRequest req = standardEncode req

derive instance genericResumeMandateReq :: Generic ResumeMandateReq _
instance standardEncodeResumeMandateReq :: StandardEncode ResumeMandateReq where standardEncode (ResumeMandateReq dummy) = standardEncode dummy
instance showResumeMandateReq :: Show ResumeMandateReq where show = genericShow
instance decodeResumeMandateReq :: Decode ResumeMandateReq where decode = defaultDecode
instance encodeResumeMandateReq :: Encode ResumeMandateReq where encode = defaultEncode

derive instance genericResumeMandateResp :: Generic ResumeMandateResp _
derive instance newtypeResumeMandateResp :: Newtype ResumeMandateResp _
instance standardEncodeResumeMandateResp :: StandardEncode ResumeMandateResp where standardEncode (ResumeMandateResp res) = standardEncode res
instance showResumeMandateResp :: Show ResumeMandateResp where show = genericShow
instance decodeResumeMandateResp :: Decode ResumeMandateResp where decode = defaultDecode
instance encodeResumeMandateResp :: Encode ResumeMandateResp where encode = defaultEncode 


--------------------------------------------- SuspendMandate --------------------------------------------------

data SuspendMandateReq = SuspendMandateReq String

newtype SuspendMandateResp = SuspendMandateResp ApiSuccessResult

instance makeSuspendMandateReq :: RestEndpoint SuspendMandateReq SuspendMandateResp where
 makeRequest reqBody@(SuspendMandateReq id) headers = defaultMakeRequest PUT (EP.suspendMandate id) headers reqBody Nothing
 decodeResponse = decodeJSON
 encodeRequest req = standardEncode req

derive instance genericSuspendMandateReq :: Generic SuspendMandateReq _
instance standardEncodeSuspendMandateReq :: StandardEncode SuspendMandateReq where standardEncode (SuspendMandateReq dummy) = standardEncode dummy
instance showSuspendMandateReq :: Show SuspendMandateReq where show = genericShow
instance decodeSuspendMandateReq :: Decode SuspendMandateReq where decode = defaultDecode
instance encodeSuspendMandateReq :: Encode SuspendMandateReq where encode = defaultEncode

derive instance genericSuspendMandateResp :: Generic SuspendMandateResp _
derive instance newtypeSuspendMandateResp :: Newtype SuspendMandateResp _
instance standardEncodeSuspendMandateResp :: StandardEncode SuspendMandateResp where standardEncode (SuspendMandateResp res) = standardEncode res
instance showSuspendMandateResp :: Show SuspendMandateResp where show = genericShow
instance decodeSuspendMandateResp :: Decode SuspendMandateResp where decode = defaultDecode
instance encodeSuspendMandateResp :: Encode SuspendMandateResp where encode = defaultEncode 

------------------------------------------ SelectPlan ------------------------------------------------------

newtype SelectPlanReq = SelectPlanReq String

newtype SelectPlanResp = SelectPlanResp ApiSuccessResult

instance makeSelectPlanReq :: RestEndpoint SelectPlanReq SelectPlanResp where
    makeRequest reqBody@(SelectPlanReq id) headers = defaultMakeRequest PUT (EP.selectPlan id) headers reqBody Nothing
    decodeResponse = decodeJSON
    encodeRequest req = standardEncode req

derive instance genericSelectPlanReq :: Generic SelectPlanReq _
derive instance newtypeSelectPlanReq :: Newtype SelectPlanReq _
instance standardEncodeSelectPlanReq :: StandardEncode SelectPlanReq where standardEncode (SelectPlanReq body) = standardEncode body
instance showSelectPlanReq :: Show SelectPlanReq where show = genericShow
instance decodeSelectPlanReq :: Decode SelectPlanReq  where decode = defaultDecode
instance encodeSelectPlanReq :: Encode SelectPlanReq where encode = defaultEncode

derive instance genericSelectPlanResp :: Generic SelectPlanResp _
derive instance newtypeSelectPlanResp :: Newtype SelectPlanResp _
instance standardEncodeSelectPlanResp :: StandardEncode SelectPlanResp where standardEncode (SelectPlanResp res) = standardEncode res
instance showSelectPlanResp :: Show SelectPlanResp where show = genericShow
instance decodeSelectPlanResp :: Decode SelectPlanResp where decode = defaultDecode
instance encodeSelectPlanResp :: Encode SelectPlanResp where encode = defaultEncode

---------------------------------------------- CurrentPlanAPI ---------------------------------------------------
data GetCurrentPlanReq = GetCurrentPlanReq String

newtype GetCurrentPlanResp = GetCurrentPlanResp {
  currentPlanDetails :: Maybe PlanEntity,
  mandateDetails :: Maybe MandateData,
  autoPayStatus :: Maybe String,
  orderId :: Maybe String,
  isLocalized :: Maybe Boolean
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

instance makeGetCurrentPlanReq :: RestEndpoint GetCurrentPlanReq GetCurrentPlanResp where
 makeRequest reqBody@(GetCurrentPlanReq driverId) headers = defaultMakeRequest GET (EP.getCurrentPlan driverId) headers reqBody Nothing
 decodeResponse = decodeJSON
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

instance makeKioskLocationReq :: RestEndpoint KioskLocationReq KioskLocationResp where
    makeRequest reqBody headers = defaultMakeRequest GET (EP.getKioskLocations "" ) headers reqBody Nothing
    decodeResponse = decodeJSON
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

newtype PostRideFeedbackResp = PostRideFeedbackResp ApiSuccessResult

instance makePostRideFeedbackReq :: RestEndpoint PostRideFeedbackReq PostRideFeedbackResp where
 makeRequest reqBody headers = defaultMakeRequest POST (EP.postRideFeedback "") headers reqBody Nothing
 decodeResponse = decodeJSON
 encodeRequest req = standardEncode req


derive instance genericPostRideFeedbackReq :: Generic PostRideFeedbackReq _
derive instance newtypePostRideFeedbackReq :: Newtype PostRideFeedbackReq _
instance standardEncodePostRideFeedbackReq :: StandardEncode PostRideFeedbackReq where standardEncode (PostRideFeedbackReq reqBody) = standardEncode reqBody
instance showPostRideFeedbackReq :: Show PostRideFeedbackReq where show = genericShow
instance decodePostRideFeedbackReq :: Decode PostRideFeedbackReq where decode = defaultDecode
instance encodePostRideFeedbackReq :: Encode PostRideFeedbackReq where encode = defaultEncode

derive instance genericPostRideFeedbackResp :: Generic PostRideFeedbackResp _
derive instance newtypePostRideFeedbackResp :: Newtype PostRideFeedbackResp _
instance standardEncodePostRideFeedbackResp :: StandardEncode PostRideFeedbackResp where standardEncode (PostRideFeedbackResp resp) = standardEncode resp
instance showPostRideFeedbackResp :: Show PostRideFeedbackResp where show = genericShow
instance decodePostRideFeedbackResp :: Decode PostRideFeedbackResp where decode = defaultDecode
instance encodePostRideFeedbackResp :: Encode PostRideFeedbackResp where encode = defaultEncode

newtype PaymentNudgeConfig = PaymentNudgeConfig {
  title :: String,
  description :: String,
  imageUrl :: String,
  subType :: String
}

derive instance genericPaymentNudgeConfig :: Generic PaymentNudgeConfig _
derive instance newtypePaymentNudgeConfig :: Newtype PaymentNudgeConfig _
instance standardPaymentNudgeConfig :: StandardEncode PaymentNudgeConfig where standardEncode (PaymentNudgeConfig res) = standardEncode res
instance showPaymentNudgeConfig :: Show PaymentNudgeConfig where show = genericShow
instance decodePaymentNudgeConfig :: Decode PaymentNudgeConfig where decode = defaultDecode
instance encodePaymentNudgeConfig :: Encode PaymentNudgeConfig where encode = defaultEncode 