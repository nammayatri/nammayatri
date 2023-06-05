{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Services.APITypes where

import Control.Alt ((<|>))
import Common.Types.App (Version(..))
import Data.Generic.Rep (class Generic)
import Data.Eq.Generic (genericEq)
import Data.Show.Generic (genericShow)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Foreign (ForeignError(..), fail)
import Foreign.Class (class Decode, class Encode, decode, encode)
import Foreign.Generic (decodeJSON)
import Prelude (class Eq, class Show, bind, show, ($), (<$>), (>>=))
import Presto.Core.Types.API (class RestEndpoint, class StandardEncode, ErrorResponse, Method(..), defaultMakeRequest, standardEncode)
import Presto.Core.Utils.Encoding (defaultDecode, defaultEncode, defaultEnumDecode, defaultEnumEncode)
import Foreign.Generic.EnumEncoding (genericDecodeEnum, genericEncodeEnum, defaultGenericEnumOptions)
import Services.EndPoints as EP
import Foreign.Index (readProp)
import Control.Monad.Except (runExcept)
import Data.Either as Either

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
 makeRequest reqBody headers = defaultMakeRequest POST (EP.triggerOTP "") headers reqBody
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
 makeRequest reqBody@(VerifyTokenRequest token (VerifyTokenReq rqBody)) headers = defaultMakeRequest POST (EP.verifyToken token) headers reqBody
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
    makeRequest reqBody@(ResendOTPRequest token) headers = defaultMakeRequest POST (EP.resendOTP token) headers reqBody
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
    makeRequest reqBody@(DriverActiveInactiveReq status status_n) headers = defaultMakeRequest POST (EP.driverActiveInactiveSilent status status_n) headers reqBody
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
    makeRequest reqBody@(StartRideRequest rideId (StartRideReq rqBody)) headers = defaultMakeRequest POST (EP.startRide rideId) headers reqBody
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
      point :: Point
    }

newtype EndRideResponse = EndRideResponse ApiSuccessResult

instance makeEndRideReq :: RestEndpoint EndRideRequest EndRideResponse where
    makeRequest reqBody@(EndRideRequest rideId (EndRideReq rqBody)) headers = defaultMakeRequest POST (EP.endRide rideId) headers reqBody
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
    makeRequest reqBody@(DriverCancelRideRequest rideId (DriverCancelRideReq rqBody)) headers = defaultMakeRequest POST (EP.cancelRide rideId) headers reqBody
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
    makeRequest reqBody headers = defaultMakeRequest POST (EP.logout "") headers reqBody
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
    , clientVersion         :: Maybe Version
    , bundleVersion         :: Maybe Version
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
    makeRequest reqBody headers = defaultMakeRequest GET (EP.getDriverInfo "") headers reqBody
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

data GetRidesHistoryReq = GetRidesHistoryReq String String String String

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
      exoPhone :: String
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
    makeRequest reqBody@(GetRidesHistoryReq limit offset isActive status) headers = defaultMakeRequest GET (EP.getRideHistory limit offset isActive status) headers reqBody
    decodeResponse = decodeJSON
    encodeRequest req = defaultEncode req

derive instance genericGetRidesHistoryReq :: Generic GetRidesHistoryReq _
instance showGetRidesHistoryReq :: Show GetRidesHistoryReq where show = genericShow
instance standardEncodeGetRidesHistoryReq :: StandardEncode GetRidesHistoryReq where standardEncode (GetRidesHistoryReq req rq limit status) = standardEncode req
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
  makeRequest reqBody headers = defaultMakeRequest POST (EP.offerRide "") headers reqBody
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

newtype UpdateDriverInfoReq = UpdateDriverInfoReq
    {   middleName              :: Maybe String
    ,   firstName               :: Maybe String
    ,   lastName                :: Maybe String
    ,   deviceToken             :: Maybe String
    ,   canDowngradeToSedan     :: Maybe Boolean
    ,   canDowngradeToHatchback :: Maybe Boolean
    ,   canDowngradeToTaxi      :: Maybe Boolean
    ,   language                :: Maybe String
    ,   clientVersion           :: Maybe Version
    ,   bundleVersion           :: Maybe Version
    }

newtype UpdateDriverInfoResp = UpdateDriverInfoResp GetDriverInfoResp

instance makeUpdateDriverInfoReq :: RestEndpoint UpdateDriverInfoRequest UpdateDriverInfoResp where
    makeRequest reqBody@(UpdateDriverInfoRequest (UpdateDriverInfoReq rqBody)) headers = defaultMakeRequest POST (EP.updateDriverInfo "") headers reqBody
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
    makeRequest reqBody headers = defaultMakeRequest GET (EP.listCancelReason "" ) headers reqBody
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
  makeRequest reqBody@(RouteReq rType (GetRouteReq reqB)) headers = defaultMakeRequest POST (EP.getRoute rType) headers reqBody
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
  dateOfRegistration :: Maybe String
}

newtype DriverRCResp = DriverRCResp ApiSuccessResult

instance makeDriverRCReq :: RestEndpoint DriverRCReq DriverRCResp where
  makeRequest reqBody headers = defaultMakeRequest POST (EP.registerDriverRC "") headers reqBody
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
  makeRequest reqBody headers = defaultMakeRequest POST (EP.registerDriverDL "") headers reqBody
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
  makeRequest reqBody headers = defaultMakeRequest POST (EP.validateImage "") headers reqBody
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
    }

instance makeDriverRegistrationStatusReq :: RestEndpoint DriverRegistrationStatusReq DriverRegistrationStatusResp where
    makeRequest reqBody headers = defaultMakeRequest GET (EP.driverRegistrationStatus "") headers reqBody
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
    makeRequest reqBody headers = defaultMakeRequest POST (EP.referDriver "") headers reqBody
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
    }

instance makeGetDriverProfileStatsReq :: RestEndpoint DriverProfileStatsReq DriverProfileStatsResp where
    makeRequest reqBody@(DriverProfileStatsReq date) headers = defaultMakeRequest GET (EP.getstatsInfo date) headers reqBody
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
    makeRequest reqBody@(DriverArrivedRequest rideId (DriverArrivedReq rqBody)) headers = defaultMakeRequest POST (EP.driverArrived rideId) headers reqBody
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
    makeRequest reqBody headers = defaultMakeRequest GET (EP.flowStatus "") headers reqBody
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
    makeRequest reqBody@(MessageListReq limit offset) headers = defaultMakeRequest GET (EP.messageList limit offset) headers reqBody
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
    makeRequest reqBody@(MessageSeenReq messageId) headers = defaultMakeRequest PUT (EP.messageSeen messageId) headers reqBody
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

------------------------------------------------------ messageReply -----------------------------------------------

data MessageResponseReq = MessageResponseReq String MessageReplyReq

newtype MessageReplyReq = MessageReplyReq {
    reply :: String
  }

newtype MessageResponseRes = MessageResponseRes ApiSuccessResult

instance makeMessageResponseReq :: RestEndpoint MessageResponseReq MessageResponseRes where
    makeRequest reqBody@(MessageResponseReq messageId (MessageReplyReq req)) headers = defaultMakeRequest PUT (EP.messageResponse messageId) headers reqBody
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
    makeRequest reqBody@(LinkReferralCodeReq date) headers = defaultMakeRequest POST (EP.linkReferralCode "") headers reqBody
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
    makeRequest reqBody@(GetPerformanceReq date) headers = defaultMakeRequest GET (EP.getPerformance "") headers reqBody
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
  makeRequest reqBody headers = defaultMakeRequest POST (EP.driverAlternateNumber "") headers reqBody
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
  makeRequest reqBody headers = defaultMakeRequest POST (EP.verifyAlternateNumberOTP "") headers reqBody
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
    makeRequest reqBody headers = defaultMakeRequest POST (EP.alternateNumberResendOTP "") headers reqBody
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
    makeRequest reqBody headers = defaultMakeRequest DELETE (EP.removeAlternateNumber "") headers reqBody
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
    makeRequest reqBody@(GetCategoriesReq language) headers = defaultMakeRequest GET (EP.getCategories language) headers reqBody
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
    makeRequest reqBody@(GetOptionsReq categoryId language) headers = defaultMakeRequest GET (EP.getOptions categoryId language) headers reqBody
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
    makeRequest reqBody@(PostIssueReq issueDetails) headers = defaultMakeRequest POST (EP.postIssue "") headers reqBody
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
    makeRequest reqBody@(IssueInfoReq issueId) headers = defaultMakeRequest GET (EP.issueInfo issueId) headers reqBody
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
  makeRequest reqBody@(CallCustomerReq rideId) headers = defaultMakeRequest POST (EP.callDriverToCustomer rideId) headers reqBody
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
    makeRequest reqBody headers = defaultMakeRequest GET (EP.fetchIssueList "") headers reqBody
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
  makeRequest reqBody@(DeleteIssueReq issueId) headers = defaultMakeRequest DELETE (EP.deleteIssue issueId) headers reqBody
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
    makeRequest reqBody@(OTPRideRequest (OTPRideReq rqBody)) headers = defaultMakeRequest POST (EP.otpRide "") headers reqBody
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
