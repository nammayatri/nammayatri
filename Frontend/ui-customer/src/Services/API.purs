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
import Foreign.Generic.Class (Options, SumEncoding(..),class GenericDecode, defaultOptions, decodeWithOptions, encodeWithOptions, SumEncoding(..), decodeOpts)
import Control.Alt ((<|>))
import Control.Monad.Except (runExcept,except)
import Common.Types.App (Version(..), FeedbackAnswer)
import Common.Types.App as CTA
import Common.Types.App
import Data.Either (Either(..))
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Foreign (Foreign, ForeignError(..), fail,unsafeFromForeign, readArray)
import Foreign.Class (class Decode, class Encode, decode, encode)
import Foreign.Generic (decodeJSON, genericDecode)
import Prelude (class Show, class Eq, show, ($), (<$>), (>>=), bind, (=<<), pure)
import Foreign (readString, readNumber, readInt)
import Foreign.Generic (class Decode)
import Presto.Core.Types.API (class RestEndpoint, class StandardEncode, ErrorPayload, Method(..), defaultDecodeResponse, defaultMakeRequestWithoutLogs, standardEncode, defaultMakeRequestString)
import Presto.Core.Utils.Encoding (defaultDecode, defaultEncode)
import Types.EndPoint as EP
import Foreign.Index (readProp, readIndex)
import Control.Monad.Except (runExcept, except)
import Data.Either (Either(..))
import Foreign.Generic.EnumEncoding (GenericEnumOptions, genericDecodeEnum, genericEncodeEnum)
import Data.Eq.Generic (genericEq)
import Data.Maybe
import PaymentPage (PaymentPagePayload(..))
import Presto.Core.Utils.Encoding (defaultEnumDecode, defaultEnumEncode)
import Data.Lens ((^.))
import Accessor (_amount)
import DecodeUtil
import Data.Function.Uncurried (runFn3)
import Debug
import Data.Argonaut.Core as AC
import Data.Tuple (Tuple(..))
import Foreign (unsafeToForeign)

newtype ErrorPayloadWrapper = ErrorPayload ErrorPayload

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

data RetryAPIs  = NoRetry | RetryInfo Int Number -- Retry times, Delay --  TODO: need to add overall timeout and handle

derive instance genericRetryAPIs :: Generic RetryAPIs _
instance showRetryAPIs :: Show RetryAPIs where show = genericShow
instance decodeRetryAPIs :: Decode RetryAPIs where decode = defaultDecode
instance encodeRetryAPIs :: Encode RetryAPIs where encode = defaultEncode

-------------------------------------------------- Trigger OTP API Types --------------------------------------------
newtype TriggerOTPResp = TriggerOTPResp {
    authId :: String
  , attempts :: Int
  , isPersonBlocked :: Boolean
}

newtype TriggerOTPReq = TriggerOTPReq {
    mobileNumber :: String
  , mobileCountryCode :: String
  , merchantId :: String
  , otpChannel :: String
  , allowBlockedUserLogin :: Boolean
}

instance makeTriggerOTPReq :: RestEndpoint TriggerOTPReq  where
 makeRequest reqBody headers = defaultMakeRequestWithoutLogs POST (EP.triggerOTP "") headers reqBody Nothing
 encodeRequest req = standardEncode req

derive instance genericTriggerOTPResp :: Generic TriggerOTPResp _
derive instance newtypeTriggerOTPResp :: Newtype TriggerOTPResp _
instance standardEncodeTriggerOTPResp :: StandardEncode TriggerOTPResp where standardEncode (TriggerOTPResp id) = standardEncode id
instance showTriggerOTPResp :: Show TriggerOTPResp where show = genericShow
instance decodeTriggerOTPResp :: Decode TriggerOTPResp where decode = defaultDecode
instance encodeTriggerOTPResp :: Encode TriggerOTPResp where encode = defaultEncode

derive instance genericTriggerOTPReq :: Generic TriggerOTPReq _
derive instance newtypeTriggerOTPReq :: Newtype TriggerOTPReq _
instance standardEncodeTriggerOTPReq :: StandardEncode TriggerOTPReq where standardEncode (TriggerOTPReq reqBody) = standardEncode reqBody
instance showTriggerOTPReq :: Show TriggerOTPReq where show = genericShow
instance decodeTriggerOTPReq :: Decode TriggerOTPReq where decode = defaultDecode
instance encodeTriggerOTPReq :: Encode TriggerOTPReq where encode = defaultEncode

-------------------------------------------------- Trigger Signature OTP API Types --------------------------------------------
data AuthType = OTP | PASSWORD | DIRECT

newtype TriggerSignatureOTPResp = TriggerSignatureOTPResp {
    authId :: String
  , attempts :: Int
  , authType :: Maybe String
  , token :: Maybe String
  , person :: Maybe User
}

newtype TriggerSignatureOTPReq = TriggerSignatureOTPReq String

instance makeTriggerSignatureOTPReq :: RestEndpoint TriggerSignatureOTPReq  where
 makeRequest (TriggerSignatureOTPReq reqBody) headers = defaultMakeRequestString POST (EP.triggerSignatureOTP "") headers reqBody Nothing
 encodeRequest req = standardEncode req

derive instance genericTriggerSignatureOTPResp :: Generic TriggerSignatureOTPResp _
derive instance newtypeTriggerSignatureOTPResp :: Newtype TriggerSignatureOTPResp _
instance standardEncodeTriggerSignatureOTPResp :: StandardEncode TriggerSignatureOTPResp where standardEncode (TriggerSignatureOTPResp id) = standardEncode id
instance showTriggerSignatureOTPResp :: Show TriggerSignatureOTPResp where show = genericShow
instance decodeTriggerSignatureOTPResp :: Decode TriggerSignatureOTPResp where decode = defaultDecode
instance encodeTriggerSignatureOTPResp :: Encode TriggerSignatureOTPResp where encode = defaultEncode

derive instance genericAuthType :: Generic AuthType _
instance showAuthType :: Show AuthType where show = genericShow
instance decodeAuthType :: Decode AuthType where decode = defaultDecode
instance encodeAuthType :: Encode AuthType where encode = defaultEncode
instance standardEncodeAuthType :: StandardEncode AuthType
  where
  standardEncode OTP = standardEncode $ show OTP
  standardEncode PASSWORD = standardEncode $ show PASSWORD
  standardEncode DIRECT = standardEncode $ show DIRECT

derive instance genericTriggerSignatureOTPReq :: Generic TriggerSignatureOTPReq _
derive instance newtypeTriggerSignatureOTPReq :: Newtype TriggerSignatureOTPReq _
instance standardEncodeTriggerSignatureOTPReq :: StandardEncode TriggerSignatureOTPReq where standardEncode (TriggerSignatureOTPReq reqBody) = standardEncode reqBody
instance showTriggerSignatureOTPReq :: Show TriggerSignatureOTPReq where show = genericShow
instance decodeTriggerSignatureOTPReq :: Decode TriggerSignatureOTPReq where decode = defaultDecode
instance encodeTriggerSignatureOTPReq :: Encode TriggerSignatureOTPReq where encode = defaultEncode

-------------------------------------------------- Resend OTP API Types --------------------------------------------


newtype ResendOTPResp = ResendOTPResp {
    authId :: String
  , attempts :: Int
}

data ResendOTPRequest = ResendOTPRequest String

instance makeResendOTPRequest :: RestEndpoint ResendOTPRequest  where
 makeRequest reqBody@(ResendOTPRequest authId) headers = defaultMakeRequestWithoutLogs POST (EP.resendOTP authId) headers reqBody Nothing
 encodeRequest req = standardEncode req

derive instance genericResendOTPResp :: Generic ResendOTPResp _
derive instance newtypeResendOTPResp :: Newtype ResendOTPResp _
instance standardEncodeResendOTPResp :: StandardEncode ResendOTPResp where standardEncode (ResendOTPResp id) = standardEncode id
instance showResendOTPResp :: Show ResendOTPResp where show = genericShow
instance decodeResendOTPResp :: Decode ResendOTPResp where decode = defaultDecode
instance encodeResendOTPResp :: Encode ResendOTPResp where encode = defaultEncode

derive instance genericResendOTPRequest :: Generic ResendOTPRequest _
instance standardEncodeResendOTPRequest :: StandardEncode ResendOTPRequest where standardEncode (ResendOTPRequest authId) = standardEncode authId
instance decodeResendOTPRequest :: Decode ResendOTPRequest where decode = defaultDecode
instance encodeResendOTPRequest :: Encode ResendOTPRequest where encode = defaultEncode

-------------------------------------------------- Verify OTP API Types --------------------------------------------
newtype VerifyTokenResp = VerifyTokenResp {
    token :: String
  , person :: User
}

data WhatsappOptMethods = OPT_IN | OPT_OUT
data VerifyTokenRequest = VerifyTokenRequest String VerifyTokenReq

newtype VerifyTokenReq = VerifyTokenReq { -- Need to check the type for the app
    otp :: String
  , deviceToken :: String
  , whatsappNotificationEnroll :: WhatsappOptMethods
}

newtype User = User {
    maskedDeviceToken :: Maybe String
  , firstName :: Maybe String
  , middleName :: Maybe String
  , id :: String
  , lastName :: Maybe String
  , maskedMobileNumber :: Maybe String
  }

instance makeVerifyTokenReq :: RestEndpoint VerifyTokenRequest  where
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

derive instance genericVerifyTokenResp :: Generic VerifyTokenResp _
derive instance newtypeVerifyTokenResp :: Newtype VerifyTokenResp _
instance standardEncodeVerifyTokenResp :: StandardEncode VerifyTokenResp where standardEncode (VerifyTokenResp id) = standardEncode id
instance showVerifyTokenResp :: Show VerifyTokenResp where show = genericShow
instance decodeVerifyTokenResp :: Decode VerifyTokenResp where decode = defaultDecode
instance encodeVerifyTokenResp :: Encode VerifyTokenResp where encode = defaultEncode

derive instance genericVerifyTokenReq :: Generic VerifyTokenReq _
derive instance newtypeVerifyTokenReq :: Newtype VerifyTokenReq _
instance standardEncodeVerifyTokenReq :: StandardEncode VerifyTokenReq where standardEncode (VerifyTokenReq reqBody) = standardEncode reqBody -- FOR REVIEW
instance showVerifyTokenReq :: Show VerifyTokenReq where show = genericShow
instance decodeVerifyTokenReq :: Decode VerifyTokenReq where decode = defaultDecode
instance encodeVerifyTokenReq :: Encode VerifyTokenReq where encode = defaultEncode

derive instance genericVerifyTokenRequest :: Generic VerifyTokenRequest _
instance standardEncodeVerifyTokenRequest :: StandardEncode VerifyTokenRequest where standardEncode (VerifyTokenRequest token reqBody) = standardEncode reqBody
instance decodeVerifyTokenRequest :: Decode VerifyTokenRequest where decode = defaultDecode
instance encodeVerifyTokenRequest :: Encode VerifyTokenRequest where encode = defaultEncode

derive instance genericUser :: Generic User _
derive instance newtypeUser :: Newtype User _
instance standardEncodeUser :: StandardEncode User where standardEncode (User req) = standardEncode req
instance showUser :: Show User where show = genericShow
instance decodeUser :: Decode User where decode = defaultDecode
instance encodeUser :: Encode User where encode = defaultEncode

-------------------------------------------------- Logout API Types --------------------------------------------
data LogOutReq = LogOutReq

instance makeLogOutReq  :: RestEndpoint LogOutReq  where
 makeRequest reqBody headers = defaultMakeRequestWithoutLogs POST (EP.logout "") headers reqBody Nothing
 encodeRequest req = standardEncode req

derive instance genericLogOutReq :: Generic LogOutReq _
instance showLogOutReq :: Show LogOutReq where show = genericShow
instance standardEncodeLogOutReq :: StandardEncode LogOutReq where standardEncode (LogOutReq) = standardEncode {}
instance decodeLogOutReq :: Decode LogOutReq where decode = defaultDecode
instance encodeLogOutReq :: Encode LogOutReq where encode = defaultEncode


-------------------------------------------------- Search Location API Types --------------------------------------------

newtype SearchLocationReq = SearchLocationReq {
  components :: String,
  location :: String,
  radius :: Int,
  input :: String,
  searchType :: Maybe String,
  language :: String,
  strictbounds :: Maybe Boolean,
  origin :: LatLong,
  sessionToken :: Maybe String,
  autoCompleteType :: Maybe String
}

newtype SearchLocationResp = SearchLocationResp {
 predictions:: Array Prediction
}

newtype Prediction = Prediction {
 description :: String,
 placeId :: Maybe String,
 distance :: Maybe Int,
 types :: Array String
}

instance makeSearchLocationReq :: RestEndpoint SearchLocationReq  where
  makeRequest reqBody headers = defaultMakeRequestWithoutLogs POST (EP.autoComplete "") headers reqBody Nothing
  encodeRequest req = standardEncode req

derive instance genericSearchLocationReq :: Generic SearchLocationReq _
derive instance newtypeSearchLocationReq :: Newtype SearchLocationReq _
instance standardEncodeSearchLocationReq :: StandardEncode SearchLocationReq where standardEncode (SearchLocationReq payload) = standardEncode payload
instance showSearchLocationReq :: Show SearchLocationReq where show = genericShow
instance decodeSearchLocationReq :: Decode SearchLocationReq where decode = defaultDecode
instance encodeSearchLocationReq :: Encode SearchLocationReq where encode = defaultEncode

derive instance genericSearchLocationResp :: Generic SearchLocationResp _
derive instance newtypeSearchLocationResp :: Newtype SearchLocationResp _
instance standardEncodeSearchLocationResp :: StandardEncode SearchLocationResp where standardEncode (SearchLocationResp id) = standardEncode id
instance showSearchLocationResp :: Show SearchLocationResp where show = genericShow
instance decodeSearchLocationResp :: Decode SearchLocationResp where decode = defaultDecode
instance encodeSearchLocationResp :: Encode SearchLocationResp where encode = defaultEncode

derive instance genericPrediction :: Generic Prediction _
derive instance newtypePrediction :: Newtype Prediction _
instance standardEncodePrediction :: StandardEncode Prediction where standardEncode (Prediction id) = standardEncode id
instance showPrediction :: Show Prediction where show = genericShow
instance decodePrediction :: Decode Prediction where decode = defaultDecode
instance encodePrediction :: Encode Prediction where encode = defaultEncode

-------------------------------------------------- Place Details, Get Cooridnates and Place Name API Types --------------------------------------------

data PlaceDetailsReq = PlaceDetailsReq String

-- data GetCoordinatesReq = GetCoordinatesReq String String

newtype PlaceDetailsResp = PlaceDetailsResp {
 status :: String,
 result :: { geometry :: Geometry}
}

-- newtype GetCoordinatesResp = GetCoordinatesResp {
--  status :: String,
--  results :: Array AddressGeometry
-- }

newtype AddressGeometry = AddressGeometry {
  geometry :: Geometry
}

newtype Geometry = Geometry {
 location :: LocationS
}

newtype LocationS = LocationS {
 lat :: Number,
 lng :: Number
}

newtype PlaceName = PlaceName {
 formattedAddress :: String,
 location :: LatLong,
 plusCode :: Maybe String,
 addressComponents :: Array AddressComponents,
 placeId :: Maybe String
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

newtype GetPlaceNameReq = GetPlaceNameReq {
  sessionToken :: Maybe String,
  language :: Maybe String,
  getBy :: GetPlaceNameBy
}


newtype GetPlaceNameBy = GetPlaceNameBy {
  tag :: String,
  contents :: Contents
}

data Contents = PlaceId String | LatLongType LatLong

derive instance genericContents :: Generic Contents _
instance showContents :: Show Contents where show = genericShow
instance decodeContents :: Decode Contents where decode = defaultDecode
instance encodeContents :: Encode Contents where encode = defaultEncode
instance standardEncodeContents :: StandardEncode Contents
  where
    standardEncode (LatLongType body) = standardEncode body
    standardEncode (PlaceId param) = standardEncode param

type PlaceId = String

newtype GetPlaceNameResp = GetPlaceNameResp (Array PlaceName)


instance placeDetailsReq :: RestEndpoint PlaceDetailsReq  where
 makeRequest reqBody@(PlaceDetailsReq id) headers = defaultMakeRequestWithoutLogs GET (EP.placeDetails id) headers reqBody Nothing
 encodeRequest req = standardEncode req

-- instance getCoordinatesReq :: RestEndpoint GetCoordinatesReq  where
--  makeRequest reqBody@(GetCoordinatesReq id language) headers = defaultMakeRequestWithoutLogs GET (EP.getCoordinates id language) headers reqBod
--  encodeRequest req = standardEncode req

instance makeGetPlaceNameReq :: RestEndpoint GetPlaceNameReq  where
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

derive instance genericAddressGeometry :: Generic AddressGeometry _
derive instance newtypeAddressGeometry :: Newtype AddressGeometry _
instance standardEncodeAddressGeometry :: StandardEncode AddressGeometry where standardEncode (AddressGeometry body) = standardEncode body
instance showAddressGeometry :: Show AddressGeometry where show = genericShow
instance decodeAddressGeometry :: Decode AddressGeometry where decode = defaultDecode
instance encodeAddressGeometry :: Encode AddressGeometry where encode = defaultEncode

derive instance genericPlaceDetailsReq :: Generic PlaceDetailsReq _
instance standardEncodePlaceDetailsReq :: StandardEncode PlaceDetailsReq where standardEncode (PlaceDetailsReq id) = standardEncode id
instance showPlaceDetailsReq :: Show PlaceDetailsReq where show = genericShow
instance decodePlaceDetailsReq :: Decode PlaceDetailsReq where decode = defaultDecode
instance encodePlaceDetailsReq :: Encode PlaceDetailsReq where encode = defaultEncode

-- derive instance genericGetCoordinatesReq :: Generic GetCoordinatesReq _
-- instance standardEncodeGetCoordinatesReq :: StandardEncode GetCoordinatesReq where standardEncode (GetCoordinatesReq id language) = standardEncode id
-- instance showGetCoordinatesReq :: Show GetCoordinatesReq where show = genericShow
-- instance decodeGetCoordinatesReq :: Decode GetCoordinatesReq where decode = defaultDecode
-- instance encodeGetCoordinatesReq :: Encode GetCoordinatesReq where encode = defaultEncode

-- derive instance genericGetCoordinatesResp :: Generic GetCoordinatesResp _
-- derive instance newtypeGetCoordinatesResp :: Newtype GetCoordinatesResp _
-- instance standardEncodeGetCoordinatesResp :: StandardEncode GetCoordinatesResp where standardEncode (GetCoordinatesResp reqBody) = standardEncode reqBody
-- instance decodeGetCoordinatesResp :: Decode GetCoordinatesResp where decode = defaultDecode
-- instance encodeGetCoordinatesResp :: Encode GetCoordinatesResp where encode = defaultEncode

derive instance genericPlaceDetailsResp :: Generic PlaceDetailsResp _
derive instance newtypePlaceDetailsResp :: Newtype PlaceDetailsResp _
instance standardEncodePlaceDetailsResp :: StandardEncode PlaceDetailsResp where standardEncode (PlaceDetailsResp reqBody) = standardEncode reqBody
instance decodePlaceDetailsResp :: Decode PlaceDetailsResp where decode = defaultDecode
instance encodePlaceDetailsResp :: Encode PlaceDetailsResp where encode = defaultEncode

derive instance genericLocationS :: Generic LocationS _
derive instance newtypeLocationS :: Newtype LocationS _
instance standardEncodeLocationS :: StandardEncode LocationS where standardEncode (LocationS req) = standardEncode req
instance showLocationS :: Show LocationS where show = genericShow
instance decodeLocationS :: Decode LocationS where decode = defaultDecode
instance encodeLocationS :: Encode LocationS where encode = defaultEncode

derive instance genericGeometry :: Generic Geometry _
derive instance newtypeGeometry :: Newtype Geometry _
instance standardEncodeGeometry :: StandardEncode Geometry where standardEncode (Geometry req) = standardEncode req
instance showGeometry :: Show Geometry where show = genericShow
instance decodeGeometry :: Decode Geometry where decode = defaultDecode
instance encodeGeometry :: Encode Geometry where encode = defaultEncode

-------- For RideSearch -----------

newtype SearchReq = SearchReq {
  contents :: ContentType ,
  fareProductType :: String
}

data ContentType = OneWaySearchRequest OneWaySearchReq | RentalSearchRequest RentalSearchReq | RoundTripSearchRequest RoundTripSearchReq | DeliverySearchRequest OneWaySearchReq | AmbulanceSearchRequest OneWaySearchReq

newtype OneWaySearchReq = OneWaySearchReq {
  origin :: SearchReqLocation,
  destination :: SearchReqLocation,
  isReallocationEnabled :: Maybe Boolean,
  isSourceManuallyMoved :: Maybe Boolean,
  isDestinationManuallyMoved :: Maybe Boolean,
  sessionToken :: Maybe String,
  isSpecialLocation :: Maybe Boolean,
  startTime :: Maybe String,
  quotesUnifiedFlow :: Maybe Boolean,
  rideRequestAndRideOtpUnifiedFlow :: Maybe Boolean
}

newtype SearchReqLocation = SearchReqLocation {
    gps :: LatLong
  , address :: LocationAddress
  }

newtype LatLong = LatLong {
  lat :: Number,
  lon :: Number
}

instance eqLatLong :: Eq LatLong where eq = genericEq
newtype SearchRes = SearchRes {
  searchId :: String,
  searchExpiry :: String,
  routeInfo :: Maybe Route
}

newtype LocationAddress = LocationAddress {
  area :: Maybe String,
  state :: Maybe String,
  country :: Maybe String,
  building :: Maybe String,
  door :: Maybe String,
  street :: Maybe String,
  city :: Maybe String,
  areaCode :: Maybe String,
  ward :: Maybe String,
  placeId :: Maybe String
}

newtype RentalSearchReq = RentalSearchReq {
  origin :: SearchReqLocation,
  stops :: Maybe (Array SearchReqLocation),
  estimatedRentalDistance :: Int,
  estimatedRentalDuration :: Int,
  startTime :: String,
  isReallocationEnabled :: Maybe Boolean,
  quotesUnifiedFlow :: Maybe Boolean,
  rideRequestAndRideOtpUnifiedFlow :: Maybe Boolean
}


newtype RoundTripSearchReq = RoundTripSearchReq {
  origin :: SearchReqLocation,
  stops :: Maybe (Array SearchReqLocation),
  startTime :: String,
  returnTime :: Maybe String,
  roundTrip :: Boolean,
  isReallocationEnabled ::  Maybe Boolean
}

derive instance genericContentType :: Generic ContentType _
instance standardEncodeContentType :: StandardEncode ContentType where
  standardEncode (OneWaySearchRequest body) = standardEncode body
  standardEncode (RentalSearchRequest body) = standardEncode body
  standardEncode (RoundTripSearchRequest body) = standardEncode body
  standardEncode (DeliverySearchRequest body) = standardEncode body
  standardEncode (AmbulanceSearchRequest body) = standardEncode body
instance showContentType :: Show ContentType where show = genericShow
instance decodeContentType :: Decode ContentType
  where
    decode body = (OneWaySearchRequest <$> decode body) <|> (RentalSearchRequest <$> decode body) <|> (RoundTripSearchRequest  <$> decode body) <|> (DeliverySearchRequest <$> decode body) <|> (fail $ ForeignError "Unknown response")
instance encodeContentType  :: Encode ContentType where
  encode (OneWaySearchRequest body) = encode body
  encode (RentalSearchRequest body) = encode body
  encode (RoundTripSearchRequest body) = encode body
  encode (DeliverySearchRequest body) = encode body
  encode (AmbulanceSearchRequest body) = encode body

derive instance genericRoundTripSearchReq :: Generic RoundTripSearchReq _
derive instance newtypeRoundTripSearchReq :: Newtype RoundTripSearchReq _
instance standardEncodeRoundTripSearchReq :: StandardEncode RoundTripSearchReq where standardEncode (RoundTripSearchReq body) = standardEncode body
instance showRoundTripSearchReq :: Show RoundTripSearchReq where show = genericShow
instance decodeRoundTripSearchReq :: Decode RoundTripSearchReq where decode = defaultDecode
instance encodeRoundTripSearchReq  :: Encode RoundTripSearchReq where encode = defaultEncode

derive instance genericRentalSearchReq :: Generic RentalSearchReq _
derive instance newtypeRentalSearchReq :: Newtype RentalSearchReq _
instance standardEncodeRentalSearchReq :: StandardEncode RentalSearchReq where standardEncode (RentalSearchReq body) = standardEncode body
instance showRentalSearchReq :: Show RentalSearchReq where show = genericShow
instance decodeRentalSearchReq :: Decode RentalSearchReq where decode = defaultDecode
instance encodeRentalSearchReq  :: Encode RentalSearchReq where encode = defaultEncode

derive instance genericLocationAddress :: Generic LocationAddress _
derive instance newtypeLocationAddress :: Newtype LocationAddress _
instance standardEncodeLocationAddress :: StandardEncode LocationAddress where standardEncode (LocationAddress body) = standardEncode body
instance showLocationAddress :: Show LocationAddress where show = genericShow
instance decodeLocationAddress :: Decode LocationAddress where decode = defaultDecode
instance encodeLocationAddress  :: Encode LocationAddress where encode = defaultEncode
instance eqLocationAddress :: Eq LocationAddress where eq = genericEq


instance makeSearchReq :: RestEndpoint SearchReq  where
 makeRequest reqBody headers = defaultMakeRequestWithoutLogs POST (EP.searchReq "") headers reqBody Nothing
 encodeRequest req = encode req

derive instance genericSearchReq :: Generic SearchReq _
derive instance newtypeSearchReq :: Newtype SearchReq _
instance standardEncodeSearchReq :: StandardEncode SearchReq where standardEncode (SearchReq body) = standardEncode body
instance showSearchReq :: Show SearchReq where show = genericShow
instance decodeSearchReq :: Decode SearchReq where decode = defaultDecode
instance encodeSearchReq:: Encode SearchReq where encode = defaultEncode

derive instance genericLatLong :: Generic LatLong _
derive instance newtypeLatLong :: Newtype LatLong _
instance standardEncodeLatLong :: StandardEncode LatLong where standardEncode (LatLong body) = standardEncode body
instance showLatLong :: Show LatLong where show = genericShow
instance decodeLatLong :: Decode LatLong where decode = defaultDecode
instance encodeLatLong:: Encode LatLong where encode = defaultEncode

derive instance genericSearchRes :: Generic SearchRes _
derive instance newtypeSearchRes :: Newtype SearchRes _
instance standardEncodeSearchRes :: StandardEncode SearchRes where standardEncode (SearchRes body) = standardEncode body
instance showSearchRes :: Show SearchRes where show = genericShow
instance decodeSearchRes :: Decode SearchRes where decode = defaultDecode
instance encodeSearchRes :: Encode SearchRes where encode = defaultEncode

derive instance genericSearchReqLocation :: Generic SearchReqLocation _
derive instance newtypeSearchReqLocation :: Newtype SearchReqLocation _
instance standardEncodeSearchReqLocation :: StandardEncode SearchReqLocation where standardEncode (SearchReqLocation body) = standardEncode body
instance showSearchReqLocation :: Show SearchReqLocation where show = genericShow
instance decodeSearchReqLocation :: Decode SearchReqLocation where decode = defaultDecode
instance encodeSearchReqLocation :: Encode SearchReqLocation where encode = defaultEncode

derive instance genericOneWaySearchReq :: Generic OneWaySearchReq _
derive instance newtypeOneWaySearchReq :: Newtype OneWaySearchReq _
instance standardEncodeOneWaySearchReq :: StandardEncode OneWaySearchReq where standardEncode (OneWaySearchReq body) = standardEncode body
instance showOneWaySearchReq :: Show OneWaySearchReq where show = genericShow
instance decodeOneWaySearchReq :: Decode OneWaySearchReq where decode = defaultDecode
instance encodeOneWaySearchReq :: Encode OneWaySearchReq where encode = defaultEncode

----- For rideSearch/{searchId}/results

data GetQuotesReq = GetQuotesReq String

newtype GetQuotesRes = GetQuotesRes {
  quotes :: Array OfferRes,
  estimates :: Array EstimateAPIEntity,
  fromLocation :: SearchReqLocationAPIEntity,
  toLocation :: Maybe SearchReqLocationAPIEntity
}

newtype EstimateAPIEntity = EstimateAPIEntity {
  agencyNumber :: String,
  createdAt :: String,
  discount :: Maybe Int,
  estimatedTotalFare :: Int,
  agencyName :: String,
  vehicleVariant :: String,
  estimatedFare :: Int,
  tripTerms :: Array String,
  id :: String,
  agencyCompletedRidesCount :: Maybe Int,
  estimateFareBreakup :: Maybe (Array CTA.EstimateFares),
  totalFareRange :: Maybe FareRange,
  nightShiftRate :: Maybe NightShiftRate,
  specialLocationTag :: Maybe String,
  driversLatLong :: Array LatLong,
  serviceTierName :: Maybe String,
  serviceTierShortDesc :: Maybe String,
  airConditioned :: Maybe Boolean,
  providerName :: Maybe String,
  providerId :: Maybe String,
  isValueAddNP :: Maybe Boolean,
  validTill :: String,
  smartTipSuggestion :: Maybe Int,
  smartTipReason :: Maybe String
}

newtype NightShiftRate = NightShiftRate {
  nightShiftEnd :: Maybe String,
  nightShiftMultiplier :: Maybe Number,
  nightShiftStart :: Maybe String
}

newtype FareRange = FareRange {
  maxFare :: Int,
  minFare :: Int
}

newtype SearchReqLocationAPIEntity = SearchReqLocationAPIEntity {
  lat :: Number,
  lon :: Number
}

data OfferRes
  = Quotes {onDemandCab :: QuoteAPIEntity}
  | Metro {metro :: MetroOffer}
  | Public {publicTransport :: PublicTransportQuote}
  | RentalQuotes {onRentalCab :: QuoteAPIEntity}

newtype QuoteAPIEntity = QuoteAPIEntity {
  agencyNumber :: Maybe String,
  createdAt :: String,
  discount :: Maybe Int,
  estimatedTotalFare :: Int,
  agencyName :: String,
  quoteDetails :: QuoteAPIDetails,
  vehicleVariant :: String,
  estimatedFare :: Int,
  tripTerms :: Array String,
  id :: String,
  agencyCompletedRidesCount :: Maybe Int,
  serviceTierName :: Maybe String,
  serviceTierShortDesc :: Maybe String,
  isAirConditioned :: Maybe Boolean,
  specialLocationTag :: Maybe String,
  isValueAddNP :: Maybe Boolean
}

data QuoteAPIDetails
  = ONE_WAY OneWayQuoteAPIDetails
  | RENTAL RentalQuoteAPIDetails
  | DRIVER_OFFER DriverOfferAPIEntity
  | INTER_CITY IntercityQuoteAPIDetails
  | DELIVERY DriverOfferAPIEntity
  | OneWaySpecialZoneAPIDetails SpecialZoneQuoteAPIDetails
  | AMBULANCE AmbulanceDetailsAPIEntity

newtype OneWayQuoteAPIDetails = OneWayQuoteAPIDetails {
  distanceToNearestDriver :: String
}

newtype IntercityQuoteAPIDetails = IntercityQuoteAPIDetails {
  quoteId :: String,
  perDayMaxHourAllowance :: Maybe Int,
  baseFare :: Maybe PriceAPIEntityDecimals ,
  kmPerPlannedExtraHour :: Maybe DistanceWithUnit ,
  perExtraKmRate :: Maybe PriceAPIEntityDecimals ,
  perExtraMinRate :: Maybe PriceAPIEntityDecimals ,
  perHourCharge :: Maybe PriceAPIEntityDecimals ,
  plannedPerKmRateOneWay :: Maybe PriceAPIEntityDecimals ,
  plannedPerKmRateRoundTrip :: Maybe PriceAPIEntityDecimals,
  tollCharges :: Maybe PriceAPIEntityDecimals ,
  deadKmFare :: Maybe PriceAPIEntityDecimals ,
  nightShiftInfo :: Maybe NightShiftInfoAPIEntity
}
newtype NightShiftInfoAPIEntity = NightShiftInfoAPIEntity {   
    nightShiftCharge :: Number,
    nightShiftChargeWithCurrency :: Maybe PriceAPIEntityDecimals,
    nightShiftStart :: String,
    nightShiftEnd :: String
}

newtype PriceApiEntity = PriceApiEntity {
  amount :: Int,
  currency :: String
}
newtype PriceAPIEntityDecimals = PriceAPIEntityDecimals {
  amount :: Number,
  currency :: String
}

newtype DistanceWithUnit = DistanceWithUnit {
  value :: Int,
  unit :: String
}

newtype RentalQuoteAPIDetails = RentalQuoteAPIDetails {
  baseFare :: Int ,
  includedKmPerHr :: Maybe Int ,
  perExtraKmRate :: Maybe Int ,
  perExtraMinRate :: Maybe Int ,
  perHourCharge :: Maybe Int ,
  plannedPerKmRate :: Maybe Int ,
  nightShiftCharge :: Maybe Int ,
  tollCharges :: Maybe Number ,
  deadKmFare :: Maybe DeadKmFare
}

newtype DeadKmFare = DeadKmFare {
  amount :: Int,
  currency :: String
}
newtype SpecialZoneQuoteAPIDetails = SpecialZoneQuoteAPIDetails {
  quoteId :: String
}

newtype AmbulanceDetailsAPIEntity = AmbulanceDetailsAPIEntity {
  minEstimatedFare :: Int,
  maxEstimatedFare :: Int,
  ambulanceQuoteBreakupList :: Array AmbulanceQuoteBreakupAPIEntity,
  tollCharges :: Maybe Number
}

newtype AmbulanceQuoteBreakupAPIEntity = AmbulanceQuoteBreakupAPIEntity {
  title :: String,
  price :: Int
}

newtype DriverOfferAPIEntity = DriverOfferAPIEntity
  {
    rating :: Maybe Number
  , validTill :: String
  , driverName :: String
  , distanceToPickup :: Maybe Number
  , durationToPickup :: Maybe Int
  }

newtype MetroOffer = MetroOffer {
  createdAt :: String,
  description :: String,
  rides :: Array MetroRide,
  rideSearchId :: String
}

newtype MetroRide = MetroRide {
  schedule :: Array Schedule,
  price :: Int,
  departureStation :: MetroStation,
  arrivalStation :: MetroStation
}

newtype Schedule = Schedule {
  arrivalTime :: String,
  departureTime :: String
}

newtype MetroStation = MetroStation {
  stationCode :: String,
  point :: LatLong,
  name :: String
}

newtype PublicTransportQuote = PublicTransportQuote {
  createdAt :: String,
  fare :: Int,
  arrivalTime :: String,
  id :: String,
  departureStation :: PublicTransportStation,
  description :: String,
  departureTime :: String,
  arrivalStation :: PublicTransportStation
}

newtype PublicTransportStation = PublicTransportStation {
  stationCode :: String,
  lat :: Number,
  lon :: Number,
  name :: String
}

instance makeGetQuotesReq :: RestEndpoint GetQuotesReq  where
 makeRequest reqBody@(GetQuotesReq searchId) headers = defaultMakeRequestWithoutLogs GET (EP.getQuotes searchId) headers reqBody Nothing
 encodeRequest req = standardEncode req

derive instance genericGetQuotesRes :: Generic GetQuotesRes _
derive instance newtypeGetQuotesRes :: Newtype GetQuotesRes _
instance standardEncodeGetQuotesRes :: StandardEncode GetQuotesRes where standardEncode (GetQuotesRes body) = standardEncode body
instance showGetQuotesRes :: Show GetQuotesRes where show = genericShow
instance decodeGetQuotesRes :: Decode GetQuotesRes where decode = defaultDecode
instance encodeGetQuotesRes  :: Encode GetQuotesRes where encode = defaultEncode

derive instance genericGetQuotesReq :: Generic GetQuotesReq _
instance standardEncodeGetQuotesReq :: StandardEncode GetQuotesReq where standardEncode (GetQuotesReq body) = standardEncode body
instance showGetQuotesReq :: Show GetQuotesReq where show = genericShow
instance decodeGetQuotesReq :: Decode GetQuotesReq where decode = defaultDecode
instance encodeGetQuotesReq  :: Encode GetQuotesReq where encode = defaultEncode

derive instance genericEstimateAPIEntity :: Generic EstimateAPIEntity _
derive instance newtypeEstimateAPIEntity :: Newtype EstimateAPIEntity _
instance standardEncodeEstimateAPIEntity :: StandardEncode EstimateAPIEntity where standardEncode (EstimateAPIEntity body) = standardEncode body
instance showEstimateAPIEntity :: Show EstimateAPIEntity where show = genericShow
instance decodeEstimateAPIEntity :: Decode EstimateAPIEntity where decode = defaultDecode
instance encodeEstimateAPIEntity  :: Encode EstimateAPIEntity where encode = defaultEncode

derive instance genericFareRange :: Generic FareRange _
derive instance newtypeFareRange :: Newtype FareRange _
instance standardEncodeFareRange :: StandardEncode FareRange where standardEncode (FareRange body) = standardEncode body
instance showFareRange :: Show FareRange where show = genericShow
instance decodeFareRange :: Decode FareRange where decode = defaultDecode
instance encodeFareRange  :: Encode FareRange where encode = defaultEncode

derive instance genericNightShiftRate :: Generic NightShiftRate _
derive instance newtypeNightShiftRate :: Newtype NightShiftRate _
instance standardEncodeNightShiftRate :: StandardEncode NightShiftRate where standardEncode (NightShiftRate body) = standardEncode body
instance showNightShiftRate :: Show NightShiftRate where show = genericShow
instance decodeNightShiftRate :: Decode NightShiftRate where decode = defaultDecode
instance encodeNightShiftRate  :: Encode NightShiftRate where encode = defaultEncode

derive instance genericSearchReqLocationAPIEntity :: Generic SearchReqLocationAPIEntity _
derive instance newtypeSearchReqLocationAPIEntity :: Newtype SearchReqLocationAPIEntity _
instance standardEncodeSearchReqLocationAPIEntity :: StandardEncode SearchReqLocationAPIEntity where standardEncode (SearchReqLocationAPIEntity body) = standardEncode body
instance showSearchReqLocationAPIEntity :: Show SearchReqLocationAPIEntity where show = genericShow
instance decodeSearchReqLocationAPIEntity :: Decode SearchReqLocationAPIEntity where decode = defaultDecode
instance encodeSearchReqLocationAPIEntity  :: Encode SearchReqLocationAPIEntity where encode = defaultEncode

derive instance genericOneWayQuoteAPIDetails :: Generic OneWayQuoteAPIDetails _
derive instance newtypeOneWayQuoteAPIDetails :: Newtype OneWayQuoteAPIDetails _
instance standardEncodeOneWayQuoteAPIDetails :: StandardEncode OneWayQuoteAPIDetails where standardEncode (OneWayQuoteAPIDetails body) = standardEncode body
instance showOneWayQuoteAPIDetails :: Show OneWayQuoteAPIDetails where show = genericShow
instance decodeOneWayQuoteAPIDetails :: Decode OneWayQuoteAPIDetails where decode = defaultDecode
instance encodeOneWayQuoteAPIDetails  :: Encode OneWayQuoteAPIDetails where encode = defaultEncode

derive instance genericRentalQuoteAPIDetails :: Generic RentalQuoteAPIDetails _
derive instance newtypeRentalQuoteAPIDetails :: Newtype RentalQuoteAPIDetails _
instance standardEncodeRentalQuoteAPIDetails :: StandardEncode RentalQuoteAPIDetails where standardEncode (RentalQuoteAPIDetails body) = standardEncode body
instance showRentalQuoteAPIDetails :: Show RentalQuoteAPIDetails where show = genericShow
instance decodeRentalQuoteAPIDetails :: Decode RentalQuoteAPIDetails where decode = defaultDecode
instance encodeRentalQuoteAPIDetails  :: Encode RentalQuoteAPIDetails where encode = defaultEncode

derive instance genericSpecialZoneQuoteAPIDetails :: Generic SpecialZoneQuoteAPIDetails _
derive instance newtypeSpecialZoneQuoteAPIDetails :: Newtype SpecialZoneQuoteAPIDetails _
instance standardEncodeSpecialZoneQuoteAPIDetails :: StandardEncode SpecialZoneQuoteAPIDetails where standardEncode (SpecialZoneQuoteAPIDetails body) = standardEncode body
instance showSpecialZoneQuoteAPIDetails :: Show SpecialZoneQuoteAPIDetails where show = genericShow
instance decodeSpecialZoneQuoteAPIDetails :: Decode SpecialZoneQuoteAPIDetails where decode = defaultDecode
instance encodeSpecialZoneQuoteAPIDetails  :: Encode SpecialZoneQuoteAPIDetails where encode = defaultEncode

derive instance genericIntercityQuoteAPIDetails :: Generic IntercityQuoteAPIDetails _
derive instance newtypeIntercityQuoteAPIDetails :: Newtype IntercityQuoteAPIDetails _
instance standardEncodeIntercityQuoteAPIDetails :: StandardEncode IntercityQuoteAPIDetails where standardEncode (IntercityQuoteAPIDetails body) = standardEncode body
instance showIntercityQuoteAPIDetails :: Show IntercityQuoteAPIDetails where show = genericShow
instance decodeIntercityQuoteAPIDetails :: Decode IntercityQuoteAPIDetails where decode = defaultDecode
instance encodeIntercityQuoteAPIDetails  :: Encode IntercityQuoteAPIDetails where encode = defaultEncode

derive instance genericAmbulanceDetailsAPIEntity :: Generic AmbulanceDetailsAPIEntity _
derive instance newtypeAmbulanceDetailsAPIEntity :: Newtype AmbulanceDetailsAPIEntity _
instance standardEncodeAmbulanceDetailsAPIEntity :: StandardEncode AmbulanceDetailsAPIEntity where standardEncode (AmbulanceDetailsAPIEntity body) = standardEncode body
instance showAmbulanceDetailsAPIEntity :: Show AmbulanceDetailsAPIEntity where show = genericShow
instance decodeAmbulanceDetailsAPIEntity :: Decode AmbulanceDetailsAPIEntity where decode = defaultDecode
instance encodeAmbulanceDetailsAPIEntity  :: Encode AmbulanceDetailsAPIEntity where encode = defaultEncode

derive instance genericAmbulanceQuoteBreakupAPIEntity :: Generic AmbulanceQuoteBreakupAPIEntity _
derive instance newtypeAmbulanceQuoteBreakupAPIEntity :: Newtype AmbulanceQuoteBreakupAPIEntity _
instance standardEncodeAmbulanceQuoteBreakupAPIEntity :: StandardEncode AmbulanceQuoteBreakupAPIEntity where standardEncode (AmbulanceQuoteBreakupAPIEntity body) = standardEncode body
instance showAmbulanceQuoteBreakupAPIEntity :: Show AmbulanceQuoteBreakupAPIEntity where show = genericShow
instance decodeAmbulanceQuoteBreakupAPIEntity :: Decode AmbulanceQuoteBreakupAPIEntity where decode = defaultDecode
instance encodeAmbulanceQuoteBreakupAPIEntity  :: Encode AmbulanceQuoteBreakupAPIEntity where encode = defaultEncode


derive instance genericOfferRes :: Generic OfferRes _
instance standardEncodeOfferRes :: StandardEncode OfferRes where
  standardEncode (Quotes body) = standardEncode body
  standardEncode (RentalQuotes body) = standardEncode body
  standardEncode (Metro body) = standardEncode body
  standardEncode (Public body) = standardEncode body
instance showOfferRes :: Show OfferRes where show = genericShow
instance decodeOfferRes :: Decode OfferRes
  where
    decode body = (Quotes <$> decode body) <|> (RentalQuotes <$> decode body) <|> (Metro <$> decode body) <|> (Public <$> decode body) <|> (fail $ ForeignError "Unknown response")
instance encodeOfferRes  :: Encode OfferRes where
  encode (Quotes body) = encode body
  encode (RentalQuotes body) = encode body
  encode (Metro body) = encode body
  encode (Public body) = encode body


derive instance genericQuoteAPIEntity :: Generic QuoteAPIEntity _
derive instance newtypeQuoteAPIEntity :: Newtype QuoteAPIEntity _
instance standardEncodeQuoteAPIEntity :: StandardEncode QuoteAPIEntity where standardEncode (QuoteAPIEntity body) = standardEncode body
instance showQuoteAPIEntity :: Show QuoteAPIEntity where show = genericShow
instance decodeQuoteAPIEntity :: Decode QuoteAPIEntity where decode = defaultDecode
instance encodeQuoteAPIEntity  :: Encode QuoteAPIEntity where encode = defaultEncode

derive instance genericQuoteAPIDetails :: Generic QuoteAPIDetails _
-- derive instance newtypeQuoteAPIDetails :: Newtype QuoteAPIDetails _ 
instance standardEncodeQuoteAPIDetails :: StandardEncode QuoteAPIDetails where standardEncode body = standardEncode body
instance showQuoteAPIDetails :: Show QuoteAPIDetails where show = genericShow 
instance genericDecodeQuoteAPIDetails :: GenericDecode QuoteAPIDetails where decodeOpts = genericDecode 
instance decodeQuoteAPIDetails :: Decode QuoteAPIDetails where
      decode body = do 
        let (TaggedObject sumEncoding) = defaultOptions.sumEncoding
        let updatedSumEncoding = TaggedObject sumEncoding{tagFieldName = "fareProductType"}
        let updatedOptions = defaultOptions{sumEncoding = updatedSumEncoding}
        decodeOpts updatedOptions body
  
instance encodeQuoteAPIDetails  :: Encode QuoteAPIDetails where encode = defaultEncode

derive instance genericDriverOfferAPIEntity :: Generic DriverOfferAPIEntity _
derive instance newtypeDriverOfferAPIEntity :: Newtype DriverOfferAPIEntity _
instance standardEncodeDriverOfferAPIEntity :: StandardEncode DriverOfferAPIEntity where standardEncode (DriverOfferAPIEntity body) = standardEncode body
instance showDriverOfferAPIEntity :: Show DriverOfferAPIEntity where show = genericShow
instance decodeDriverOfferAPIEntity :: Decode DriverOfferAPIEntity where decode = defaultDecode
instance encodeDriverOfferAPIEntity  :: Encode DriverOfferAPIEntity where encode = defaultEncode

derive instance genericMetroOffer :: Generic MetroOffer _
derive instance newtypeMetroOffer :: Newtype MetroOffer _
instance standardEncodeMetroOffer :: StandardEncode MetroOffer where standardEncode (MetroOffer body) = standardEncode body
instance showMetroOffer :: Show MetroOffer where show = genericShow
instance decodeMetroOffer :: Decode MetroOffer where decode = defaultDecode
instance encodeMetroOffer  :: Encode MetroOffer where encode = defaultEncode

derive instance genericMetroRide :: Generic MetroRide _
derive instance newtypeMetroRide :: Newtype MetroRide _
instance standardEncodeMetroRide :: StandardEncode MetroRide where standardEncode (MetroRide body) = standardEncode body
instance showMetroRide :: Show MetroRide where show = genericShow
instance decodeMetroRide :: Decode MetroRide where decode = defaultDecode
instance encodeMetroRide  :: Encode MetroRide where encode = defaultEncode

derive instance genericSchedule :: Generic Schedule _
derive instance newtypeSchedule :: Newtype Schedule _
instance standardEncodeSchedule :: StandardEncode Schedule where standardEncode (Schedule body) = standardEncode body
instance showSchedule :: Show Schedule where show = genericShow
instance decodeSchedule :: Decode Schedule where decode = defaultDecode
instance encodeSchedule  :: Encode Schedule where encode = defaultEncode

derive instance genericMetroStation :: Generic MetroStation _
derive instance newtypeMetroStation :: Newtype MetroStation _
instance standardEncodeMetroStation :: StandardEncode MetroStation where standardEncode (MetroStation body) = standardEncode body
instance showMetroStation :: Show MetroStation where show = genericShow
instance decodeMetroStation :: Decode MetroStation where decode = defaultDecode
instance encodeMetroStation  :: Encode MetroStation where encode = defaultEncode

derive instance genericPublicTransportQuote :: Generic PublicTransportQuote _
derive instance newtypePublicTransportQuote :: Newtype PublicTransportQuote _
instance standardEncodePublicTransportQuote :: StandardEncode PublicTransportQuote where standardEncode (PublicTransportQuote body) = standardEncode body
instance showPublicTransportQuote :: Show PublicTransportQuote where show = genericShow
instance decodePublicTransportQuote :: Decode PublicTransportQuote where decode = defaultDecode
instance encodePublicTransportQuote  :: Encode PublicTransportQuote where encode = defaultEncode

derive instance genericPublicTransportStation :: Generic PublicTransportStation _
derive instance newtypePublicTransportStation :: Newtype PublicTransportStation _
instance standardEncodePublicTransportStation :: StandardEncode PublicTransportStation where standardEncode (PublicTransportStation body) = standardEncode body
instance showPublicTransportStation :: Show PublicTransportStation where show = genericShow
instance decodePublicTransportStation :: Decode PublicTransportStation where decode = defaultDecode
instance encodePublicTransportStation  :: Encode PublicTransportStation where encode = defaultEncode


----- For rideSearch/quotes/{quoteId}/confirm

data ConfirmRequest = ConfirmRequest String


newtype ConfirmRes = ConfirmRes {
  bookingId :: String
}

instance makeConfirmRequest :: RestEndpoint ConfirmRequest  where
 makeRequest reqBody@(ConfirmRequest quoteId) headers = defaultMakeRequestWithoutLogs POST (EP.confirmRide quoteId) headers reqBody Nothing
 encodeRequest req = standardEncode req

derive instance genericConfirmRes :: Generic ConfirmRes _
derive instance newtypeConfirmRes :: Newtype ConfirmRes _
instance standardEncodeConfirmRes :: StandardEncode ConfirmRes where standardEncode (ConfirmRes body) = standardEncode body
instance showConfirmRes :: Show ConfirmRes where show = genericShow
instance decodeConfirmRes :: Decode ConfirmRes where decode = defaultDecode
instance encodeConfirmRes  :: Encode ConfirmRes where encode = defaultEncode

derive instance genericConfirmRequest :: Generic ConfirmRequest _
instance standardEncodeConfirmRequest :: StandardEncode ConfirmRequest where standardEncode (ConfirmRequest quoteId) = standardEncode quoteId
instance showConfirmRequest :: Show ConfirmRequest where show = genericShow
instance decodeConfirmRequest :: Decode ConfirmRequest where decode = defaultDecode
instance encodeConfirmRequest  :: Encode ConfirmRequest where encode = defaultEncode


derive instance genericDeadKmFare :: Generic DeadKmFare _
derive instance newtypeDeadKmFare :: Newtype DeadKmFare _
instance standardEncodeDeadKmFare :: StandardEncode DeadKmFare where standardEncode (DeadKmFare body) = standardEncode body
instance showDeadKmFare :: Show DeadKmFare where show = genericShow
instance decodeDeadKmFare :: Decode DeadKmFare where decode = defaultDecode
instance encodeDeadKmFare  :: Encode DeadKmFare where encode = defaultEncode


derive instance genericNightShiftInfoAPIEntity :: Generic NightShiftInfoAPIEntity _
derive instance newtypeNightShiftInfoAPIEntity :: Newtype NightShiftInfoAPIEntity _
instance standardEncodeNightShiftInfoAPIEntity :: StandardEncode NightShiftInfoAPIEntity where standardEncode (NightShiftInfoAPIEntity body) = standardEncode body
instance showNightShiftInfoAPIEntity :: Show NightShiftInfoAPIEntity where show = genericShow
instance decodeNightShiftInfoAPIEntity :: Decode NightShiftInfoAPIEntity where decode = defaultDecode
instance encodeNightShiftInfoAPIEntity  :: Encode NightShiftInfoAPIEntity where encode = defaultEncode


derive instance genericPriceApiEntity :: Generic PriceApiEntity _
derive instance newtypePriceApiEntity :: Newtype PriceApiEntity _
instance standardEncodePriceApiEntity :: StandardEncode PriceApiEntity where standardEncode (PriceApiEntity body) = standardEncode body
instance showPriceApiEntity :: Show PriceApiEntity where show = genericShow
instance decodePriceApiEntity :: Decode PriceApiEntity where decode = defaultDecode
instance encodePriceApiEntity  :: Encode PriceApiEntity where encode = defaultEncode

derive instance genericPriceAPIEntityDecimals :: Generic PriceAPIEntityDecimals _
derive instance newtypePriceAPIEntityDecimals :: Newtype PriceAPIEntityDecimals _
instance standardEncodePriceAPIEntityDecimals :: StandardEncode PriceAPIEntityDecimals where
  standardEncode (PriceAPIEntityDecimals body) = standardEncode body
instance showPriceAPIEntityDecimals :: Show PriceAPIEntityDecimals where
  show = genericShow
instance decodePriceAPIEntityDecimals :: Decode PriceAPIEntityDecimals where
  decode = defaultDecode
instance encodePriceAPIEntityDecimals :: Encode PriceAPIEntityDecimals where
  encode = defaultEncode

derive instance genericDistanceWithUnit :: Generic DistanceWithUnit _
derive instance newtypeDistanceWithUnit :: Newtype DistanceWithUnit _
instance standardEncodeDistanceWithUnit :: StandardEncode DistanceWithUnit where
  standardEncode (DistanceWithUnit body) = standardEncode body
instance showDistanceWithUnit :: Show DistanceWithUnit where
  show = genericShow
instance decodeDistanceWithUnit :: Decode DistanceWithUnit where
  decode = defaultDecode
instance encodeDistanceWithUnit :: Encode DistanceWithUnit where
  encode = defaultEncode

------- rideBooking/{bookingId}

data RideBookingReq = RideBookingReq String

data RideBookingStatusReq = RideBookingStatusReq String


newtype RideBookingRes = RideBookingRes {
  agencyName :: String,
  agencyNumber :: Maybe String,
  bookingDetails :: RideBookingAPIDetails ,
  createdAt :: String,
  discount :: Maybe Int,
  duration :: Maybe Int,
  estimatedDistance :: Maybe Int,
  estimatedDuration :: Maybe Int,
  estimatedFare :: Int,
  estimatedTotalFare :: Int,
  fareBreakup :: Array FareBreakupAPIEntity,
  fromLocation ::  BookingLocationAPIEntity,
  initialPickupLocation :: BookingLocationAPIEntity,
  hasDisability :: Maybe Boolean,
  hasNightIssue :: Maybe Boolean,
  sosStatus :: Maybe CTA.SosStatus,
  serviceTierName :: Maybe String,
  isValueAddNP :: Maybe Boolean,
  providerName :: Maybe String,
  id :: String,
  merchantExoPhone :: String,
  rideEndTime :: Maybe String,
  rideList :: Array RideAPIEntity,
  rideScheduledTime :: Maybe String,
  rideStartTime :: Maybe String,
  specialLocationTag :: Maybe String,
  specialLocationName :: Maybe String,
  status :: String,
  tripTerms :: Array String,
  updatedAt :: String,
  vehicleServiceTierType :: Maybe String,
  tollConfidence :: Maybe CTA.Confidence,
  driversPreviousRideDropLocLat :: Maybe Number,
  driversPreviousRideDropLocLon :: Maybe Number,
  isScheduled :: Boolean,
  estimatedFareBreakup :: Array FareBreakupAPIEntity,
  isAlreadyFav :: Maybe Boolean,
  favCount :: Maybe Int,
  rideDuration :: Maybe Int,
  vehicleServiceTierSeatingCapacity :: Maybe Int,
  vehicleServiceTierAirConditioned :: Maybe Number,
  returnTime :: Maybe String,
  isAirConditioned :: Maybe Boolean
}

newtype RideBookingStatusRes = RideBookingStatusRes {
  id :: String,
  isBookingUpdated :: Boolean,
  bookingStatus :: String,
  rideStatus :: Maybe String,
  driverArrivalTime :: Maybe String,
  sosStatus :: Maybe CTA.SosStatus,
  driversPreviousRideDropLocLat :: Maybe Number,
  driversPreviousRideDropLocLon :: Maybe Number,
  estimatedEndTimeRange :: Maybe EstimatedTimeToDestination
}

newtype EstimatedTimeToDestination = EstimatedTimeToDestination {
  end :: String,
  start :: String
}

derive instance genericEstimatedTimeToDestination :: Generic EstimatedTimeToDestination _
derive instance newtypeEstimatedTimeToDestination :: Newtype EstimatedTimeToDestination _
instance standardEncodeEstimatedTimeToDestination :: StandardEncode EstimatedTimeToDestination where standardEncode (EstimatedTimeToDestination body) = standardEncode body
instance showEstimatedTimeToDestination :: Show EstimatedTimeToDestination where show = genericShow
instance decodeEstimatedTimeToDestination :: Decode EstimatedTimeToDestination where decode = defaultDecode
instance encodeEstimatedTimeToDestination  :: Encode EstimatedTimeToDestination where encode = defaultEncode

newtype FareBreakupAPIEntity = FareBreakupAPIEntity {
  amount :: Number,
  amountWithCurrency :: CTA.Price,
  description :: String
}

newtype RideAPIEntity = RideAPIEntity {
  computedPrice :: Maybe Int,
  status :: String,
  vehicleModel :: String,
  createdAt :: String,
  driverNumber :: Maybe String,
  shortRideId :: String,
  driverRegisteredAt :: Maybe String,
  vehicleNumber :: String,
  rideOtp :: String,
  driverName :: String,
  chargeableRideDistance :: Maybe Int,
  vehicleVariant :: String,
  driverRatings :: Maybe Number,
  vehicleColor :: String,
  id :: String,
  updatedAt :: String,
  rideStartTime :: Maybe String,
  rideEndTime:: Maybe String,
  rideRating :: Maybe Int,
  driverArrivalTime :: Maybe String,
  destinationReachedAt :: Maybe String,
  bppRideId :: String,
  endOtp :: Maybe String,
  startOdometerReading :: Maybe Number,
  endOdometerReading :: Maybe Number,
  tollConfidence :: Maybe CTA.Confidence, 
  allowedEditPickupLocationAttempts :: Maybe Int
}

newtype RideBookingAPIDetails = RideBookingAPIDetails {
  contents :: RideBookingDetails,
  fareProductType :: String
}

data FareProductType =  ONE_WAY_FARE_PRODUCT
                      | INTER_CITY_FARE_PRODUCT
                      | RENTAL_FARE_PRODUCT
                      | DRIVER_OFFER_FARE_PRODUCT
                      | OneWaySpecialZoneAPIDetails_FARE_PRODUCT
                      | DELIVERY_FARE_PRODUCT

newtype RideBookingDetails = RideBookingDetails {
  toLocation :: Maybe BookingLocationAPIEntity,
  estimatedDistance :: Maybe Int,
  otpCode :: Maybe String,
  stopLocation :: Maybe BookingLocationAPIEntity,
  senderDetails :: Maybe PersonDetails,
  requestorPartyRoles :: Maybe (Array String),
  receiverDetails :: Maybe PersonDetails
}

newtype PersonDetails = PersonDetails {
  name :: String,
  phoneNumber :: String
}


derive instance genericPersonDetails :: Generic PersonDetails _
derive instance newtypePersonDetails :: Newtype PersonDetails _
instance standardEncodePersonDetails :: StandardEncode PersonDetails where standardEncode (PersonDetails body) = standardEncode body
instance showPersonDetails :: Show PersonDetails where show = genericShow
instance decodePersonDetails :: Decode PersonDetails where decode = defaultDecode
instance encodePersonDetails  :: Encode PersonDetails where encode = defaultEncode


newtype BookingLocationAPIEntity = BookingLocationAPIEntity {
  area :: Maybe String,
  state :: Maybe String,
  country :: Maybe String,
  building :: Maybe String,
  door :: Maybe String,
  street :: Maybe String,
  lat :: Number,
  city :: Maybe String,
  areaCode :: Maybe String,
  lon :: Number,
  ward :: Maybe String,
  placeId :: Maybe String,
  extras ::  Maybe String,
  instructions :: Maybe String
}

instance makeRideBooking :: RestEndpoint RideBookingReq  where
 makeRequest reqBody@(RideBookingReq bookingId) headers = defaultMakeRequestWithoutLogs POST (EP.ridebooking bookingId) headers reqBody Nothing
 encodeRequest req = standardEncode req

derive instance genericRideBookingReq :: Generic RideBookingReq _
instance standardEncodeRideBookingReq :: StandardEncode RideBookingReq where standardEncode (RideBookingReq body) = standardEncode body
instance showRideBookingReq :: Show RideBookingReq where show = genericShow
instance decodeRideBookingReq :: Decode RideBookingReq where decode = defaultDecode
instance encodeRideBookingReq  :: Encode RideBookingReq where encode = defaultEncode

derive instance genericRideBookingRes :: Generic RideBookingRes _
derive instance newtypeRideBookingRes :: Newtype RideBookingRes _
instance standardEncodeRideBookingRes :: StandardEncode RideBookingRes where standardEncode (RideBookingRes body) = standardEncode body
instance showRideBookingRes :: Show RideBookingRes where show = genericShow
instance decodeRideBookingRes :: Decode RideBookingRes where decode = defaultDecode
instance encodeRideBookingRes  :: Encode RideBookingRes where encode = defaultEncode


instance makeRideBookingStatus :: RestEndpoint RideBookingStatusReq where
 makeRequest reqBody@(RideBookingStatusReq bookingId) headers = defaultMakeRequestWithoutLogs GET (EP.ridebookingStatus bookingId) headers reqBody Nothing
 encodeRequest req = standardEncode req
  
derive instance genericRideBookingStatusReq :: Generic RideBookingStatusReq _
instance standardEncodeRideBookingStatusReq :: StandardEncode RideBookingStatusReq where standardEncode (RideBookingStatusReq body) = standardEncode body
instance showRideBookingStatusReq :: Show RideBookingStatusReq where show = genericShow
instance decodeRideBookingStatusReq :: Decode RideBookingStatusReq where decode = defaultDecode
instance encodeRideBookingStatusReq  :: Encode RideBookingStatusReq where encode = defaultEncode

derive instance genericRideBookingStatusRes :: Generic RideBookingStatusRes _
derive instance newtypeRideBookingStatusRes :: Newtype RideBookingStatusRes _
instance standardEncodeRideBookingStatusRes :: StandardEncode RideBookingStatusRes where standardEncode (RideBookingStatusRes body) = standardEncode body
instance showRideBookingStatusRes :: Show RideBookingStatusRes where show = genericShow
instance decodeRideBookingStatusRes :: Decode RideBookingStatusRes where decode = defaultDecode
instance encodeRideBookingStatusRes  :: Encode RideBookingStatusRes where encode = defaultEncode

derive instance genericFareBreakupAPIEntity :: Generic FareBreakupAPIEntity _
derive instance newtypeFareBreakupAPIEntity :: Newtype FareBreakupAPIEntity _
instance standardEncodeFareBreakupAPIEntity :: StandardEncode FareBreakupAPIEntity where standardEncode (FareBreakupAPIEntity body) = standardEncode body
instance showFareBreakupAPIEntity :: Show FareBreakupAPIEntity where show = genericShow
instance decodeFareBreakupAPIEntity :: Decode FareBreakupAPIEntity where decode = defaultDecode
instance encodeFareBreakupAPIEntity  :: Encode FareBreakupAPIEntity where encode = defaultEncode

derive instance genericRideAPIEntity :: Generic RideAPIEntity _
derive instance newtypeRideAPIEntity :: Newtype RideAPIEntity _
instance standardEncodeRideAPIEntity :: StandardEncode RideAPIEntity where standardEncode (RideAPIEntity body) = standardEncode body
instance showRideAPIEntity :: Show RideAPIEntity where show = genericShow
instance decodeRideAPIEntity :: Decode RideAPIEntity where decode = defaultDecode
instance encodeRideAPIEntity  :: Encode RideAPIEntity where encode = defaultEncode

derive instance genericRideBookingAPIDetails :: Generic RideBookingAPIDetails _
derive instance newtypeRideBookingAPIDetails :: Newtype RideBookingAPIDetails _
instance standardEncodeRideBookingAPIDetails :: StandardEncode RideBookingAPIDetails where standardEncode (RideBookingAPIDetails body) = standardEncode body
instance showRideBookingAPIDetails :: Show RideBookingAPIDetails where show = genericShow
instance decodeRideBookingAPIDetails :: Decode RideBookingAPIDetails where decode = defaultDecode
instance encodeRideBookingAPIDetails  :: Encode RideBookingAPIDetails where encode = defaultEncode

derive instance genericFareProductType :: Generic FareProductType _
instance showFareProductType :: Show FareProductType where show = genericShow
instance decodeFareProductType :: Decode FareProductType
  where
    decode body = case unsafeFromForeign body of
                    "ONE_WAY"                     -> except $ Right ONE_WAY_FARE_PRODUCT
                    "INTER_CITY"                  -> except $ Right INTER_CITY_FARE_PRODUCT
                    "RENTAL"                      -> except $ Right RENTAL_FARE_PRODUCT
                    "DRIVER_OFFER"                -> except $ Right DRIVER_OFFER_FARE_PRODUCT
                    "OneWaySpecialZoneAPIDetails" -> except $ Right OneWaySpecialZoneAPIDetails_FARE_PRODUCT
                    "DELIVERY"                    -> except $ Right DELIVERY_FARE_PRODUCT
                    _                             -> fail $ ForeignError "Unknown response"
instance encodeFareProductType :: Encode FareProductType
  where
    encode ONE_WAY_FARE_PRODUCT = encode "ONE_WAY"
    encode INTER_CITY_FARE_PRODUCT = encode "INTER_CITY"
    encode RENTAL_FARE_PRODUCT = encode "RENTAL"
    encode DRIVER_OFFER_FARE_PRODUCT = encode "DRIVER_OFFER"
    encode OneWaySpecialZoneAPIDetails_FARE_PRODUCT = encode "OneWaySpecialZoneAPIDetails"
    encode DELIVERY_FARE_PRODUCT = encode "DELIVERY"
instance standardEncodeFareProductType :: StandardEncode FareProductType
  where
    standardEncode ONE_WAY_FARE_PRODUCT = standardEncode "ONE_WAY"
    standardEncode INTER_CITY_FARE_PRODUCT = standardEncode "INTER_CITY"
    standardEncode RENTAL_FARE_PRODUCT = standardEncode "RENTAL"
    standardEncode DRIVER_OFFER_FARE_PRODUCT = standardEncode "DRIVER_OFFER"
    standardEncode OneWaySpecialZoneAPIDetails_FARE_PRODUCT = standardEncode "OneWaySpecialZoneAPIDetails"
    standardEncode DELIVERY_FARE_PRODUCT = standardEncode "DELIVERY"

derive instance genericRideBookingDetails :: Generic RideBookingDetails _
derive instance newtypeRideBookingDetails :: Newtype RideBookingDetails _
instance standardEncodeRideBookingDetails :: StandardEncode RideBookingDetails where standardEncode (RideBookingDetails body) = standardEncode body
instance showRideBookingDetails :: Show RideBookingDetails where show = genericShow
instance decodeRideBookingDetails :: Decode RideBookingDetails where decode = defaultDecode
instance encodeRideBookingDetails  :: Encode RideBookingDetails where encode = defaultEncode

derive instance genericBookingLocationAPIEntity :: Generic BookingLocationAPIEntity _
derive instance newtypeBookingLocationAPIEntity :: Newtype BookingLocationAPIEntity _
instance standardEncodeBookingLocationAPIEntity :: StandardEncode BookingLocationAPIEntity where standardEncode (BookingLocationAPIEntity body) = standardEncode body
instance showBookingLocationAPIEntity :: Show BookingLocationAPIEntity where show = genericShow
instance decodeBookingLocationAPIEntity :: Decode BookingLocationAPIEntity where decode = defaultDecode
instance encodeBookingLocationAPIEntity  :: Encode BookingLocationAPIEntity where encode = defaultEncode


-------------------------------------------------- Select Estimate Id  API Types -----------------------------------------------------------------

data SelectEstimateReq = SelectEstimateReq String DEstimateSelect

newtype DEstimateSelect = DEstimateSelect
  {
    customerExtraFee :: Maybe Int,
    autoAssignEnabled :: Boolean,
    autoAssignEnabledV2 :: Boolean,
    otherSelectedEstimates :: Array String,
    isAdvancedBookingEnabled :: Boolean,
    deliveryDetails :: Maybe DeliveryDetails
  }

data DeliveryDetails = DeliveryDetails
  {
    senderDetails :: PersonLocationAndInstruction,
    receiverDetails :: PersonLocationAndInstruction,
    initiatedAs :: InitiatedAs
  }

derive instance genericDeliveryDetails :: Generic DeliveryDetails _
instance standardEncodeDeliveryDetails :: StandardEncode DeliveryDetails where standardEncode (DeliveryDetails body) = standardEncode body 
instance showDeliveryDetails :: Show DeliveryDetails where show = genericShow
instance decodeDeliveryDetails :: Decode DeliveryDetails where decode = defaultDecode
instance encodeDeliveryDetails  :: Encode DeliveryDetails where encode = defaultEncode

data PersonLocationAndInstruction = PersonLocationAndInstruction
  {
    name :: String,
    phoneNumber :: String,
    address :: InstructionAndAddress
  }

derive instance genericPersonLocationAndInstruction :: Generic PersonLocationAndInstruction _
-- derive instance newtypePersonLocationAndInstruction :: Newtype PersonLocationAndInstruction _
instance standardEncodePersonLocationAndInstruction :: StandardEncode PersonLocationAndInstruction where standardEncode (PersonLocationAndInstruction body) = standardEncode body
instance showPersonLocationAndInstruction :: Show PersonLocationAndInstruction where show = genericShow
instance decodePersonLocationAndInstruction :: Decode PersonLocationAndInstruction where decode = defaultDecode
instance encodePersonLocationAndInstruction  :: Encode PersonLocationAndInstruction where encode = defaultEncode

data InstructionAndAddress = InstructionAndAddress
  {
    instructions :: Maybe String,
    extras :: String
  }

derive instance genericInstructionAndAddress :: Generic InstructionAndAddress _
-- derive instance newtypeInstructionAndAddress :: Newtype InstructionAndAddress _
instance standardEncodeInstructionAndAddress :: StandardEncode InstructionAndAddress where standardEncode (InstructionAndAddress body) = standardEncode body
instance showInstructionAndAddress :: Show InstructionAndAddress where show = genericShow
instance decodeInstructionAndAddress :: Decode InstructionAndAddress where decode = defaultDecode
instance encodeInstructionAndAddress  :: Encode InstructionAndAddress where encode = defaultEncode

data InitiatedAs =  Sender | Receiver | SomeoneElse

derive instance genericInitiatedAs :: Generic InitiatedAs _
instance showInitiatedAs :: Show InitiatedAs where show = genericShow
instance decodeInitiatedAs :: Decode InitiatedAs 
  where 
    decode body = case unsafeFromForeign body of
                    "Sender" -> except $ Right Sender
                    "Receiver" -> except $ Right Receiver
                    "SomeoneElse" -> except $ Right SomeoneElse
                    _ -> fail $ ForeignError "Unknown response"
instance encodeInitiatedAs :: Encode InitiatedAs 
  where
    encode Sender = encode "Sender"
    encode Receiver = encode "Receiver"
    encode SomeoneElse = encode "SomeoneElse"
instance eqInitiatedAs :: Eq InitiatedAs where eq = genericEq
instance standardEncodeInitiatedAs :: StandardEncode InitiatedAs
  where
    standardEncode Sender = standardEncode "Sender"
    standardEncode Receiver = standardEncode "Receiver"
    standardEncode SomeoneElse = standardEncode "SomeoneElse"


instance makeSelectEstimateReq :: RestEndpoint SelectEstimateReq  where
 makeRequest reqBody@(SelectEstimateReq estimateId (DEstimateSelect rqBody)) headers = defaultMakeRequestWithoutLogs POST (EP.selectEstimate estimateId) headers reqBody Nothing
 encodeRequest req = standardEncode req


derive instance genericDEstimateSelect :: Generic DEstimateSelect _
derive instance newtypeDEstimateSelect :: Newtype DEstimateSelect _
instance standardEncodeDEstimateSelect :: StandardEncode DEstimateSelect where standardEncode (DEstimateSelect reqBody) = standardEncode reqBody
instance showDEstimateSelect :: Show DEstimateSelect where show = genericShow
instance decodeDEstimateSelect :: Decode DEstimateSelect where decode = defaultDecode
instance encodeDEstimateSelect :: Encode DEstimateSelect where encode = defaultEncode

derive instance genericSelectEstimateReq :: Generic SelectEstimateReq _
instance standardEncodeSelectEstimateReq :: StandardEncode SelectEstimateReq where standardEncode (SelectEstimateReq estimateId body) = standardEncode body
instance showSelectEstimateReq :: Show SelectEstimateReq where show = genericShow
instance decodeSelectEstimateReq :: Decode SelectEstimateReq where decode = defaultDecode
instance encodeSelectEstimateReq  :: Encode SelectEstimateReq where encode = defaultEncode

-------------------------------------------------------------------------------Estimated Quotes API Types ------------------------------------------------------------
data SelectListReq = SelectListReq String

newtype SelectListRes = SelectListRes {
  selectedQuotes :: Maybe SelectedQuotes,
  bookingId :: Maybe String
}

newtype SelectedQuotes = SelectedQuotes {
  selectedQuotes :: Array QuoteAPIEntity
}

derive instance genericSelectedQuotes:: Generic SelectedQuotes _
derive instance newtypeSelectedQuotes :: Newtype SelectedQuotes _
instance standardSelectedQuotes :: StandardEncode SelectedQuotes where standardEncode (SelectedQuotes body) = standardEncode body
instance showSelectedQuotes :: Show SelectedQuotes where show = genericShow
instance decodeSelectedQuotes :: Decode SelectedQuotes where decode = defaultDecode
instance encodeSelectedQuotes :: Encode SelectedQuotes where encode = defaultEncode

instance makeSelectListReq :: RestEndpoint SelectListReq  where
 makeRequest reqBody@(SelectListReq estimateId) headers = defaultMakeRequestWithoutLogs GET (EP.selectList estimateId) headers reqBody Nothing
 encodeRequest req = standardEncode req

derive instance genericSelectListReq :: Generic SelectListReq _
instance standardEncodeSelectListReq :: StandardEncode SelectListReq where standardEncode (SelectListReq body) = standardEncode body
instance showSelectListReq :: Show SelectListReq where show = genericShow
instance decodeSelectListReq :: Decode SelectListReq where decode = defaultDecode
instance encodeSelectListReq  :: Encode SelectListReq where encode = defaultEncode

derive instance genericSelectListRes :: Generic SelectListRes _
derive instance newtypeSelectListRes :: Newtype SelectListRes _
instance standardEncodeSelectListRes :: StandardEncode SelectListRes where standardEncode (SelectListRes body) = standardEncode body
instance showSelectListRes :: Show SelectListRes where show = genericShow
instance decodeSelectListRes :: Decode SelectListRes where decode = defaultDecode
instance encodeSelectListRes  :: Encode SelectListRes where encode = defaultEncode


--- rideBooking/{bookingId}/cancel

data CancelRequest = CancelRequest CancelReq String

newtype CancelReq = CancelReq {
  additionalInfo :: Maybe String,
  reasonCode :: String,
  reasonStage :: String
}


instance makeCancelReq :: RestEndpoint CancelRequest  where
 makeRequest reqBody@(CancelRequest rqBody bookingId) headers = defaultMakeRequestWithoutLogs POST (EP.cancelRide bookingId) headers reqBody Nothing
 encodeRequest req = standardEncode req


derive instance genericCancelReq :: Generic CancelReq _
derive instance newtypeCancelReq :: Newtype CancelReq _
instance standardEncodeCancelReq :: StandardEncode CancelReq where standardEncode (CancelReq body) = standardEncode body
instance showCancelReq :: Show CancelReq where show = genericShow
instance decodeCancelReq :: Decode CancelReq where decode = defaultDecode
instance encodeCancelReq  :: Encode CancelReq where encode = defaultEncode

derive instance genericCancelRequest :: Generic CancelRequest _
instance standardEncodeCancelRequest :: StandardEncode CancelRequest where standardEncode (CancelRequest body boookingId) = standardEncode body
instance showCancelRequest :: Show CancelRequest where show = genericShow
instance decodeCancelRequest :: Decode CancelRequest where decode = defaultDecode
instance encodeCancelRequest  :: Encode CancelRequest where encode = defaultEncode
-------------------------------------------------------------------------------RideBookingList API Types ------------------------------------------------------------

data RideBookingListReq = RideBookingListReq String String String (Maybe String) (Maybe String)

newtype RideBookingListRes = RideBookingListRes {
  list :: Array RideBookingRes
}

derive instance genericRideBookingListReq:: Generic RideBookingListReq _
instance standardRideBookingListReq :: StandardEncode RideBookingListReq where standardEncode (RideBookingListReq offset limit onlyActive _ _) = standardEncode {}
instance showRideBookingListReq :: Show RideBookingListReq where show = genericShow
instance decodeRideBookingListReq :: Decode RideBookingListReq where decode = defaultDecode
instance encodeRideBookingListReq :: Encode RideBookingListReq where encode = defaultEncode

derive instance genericRideBookingListRes:: Generic RideBookingListRes _
derive instance newtypeRideBookingListRes :: Newtype RideBookingListRes _
instance standardRideBookingListRes :: StandardEncode RideBookingListRes where standardEncode (RideBookingListRes body) = standardEncode body
instance showRideBookingListRes :: Show RideBookingListRes where show = genericShow
instance decodeRideBookingListRes :: Decode RideBookingListRes where decode = defaultDecode
instance encodeRideBookingListRes :: Encode RideBookingListRes where encode = defaultEncode


instance makeRideBookingListReq :: RestEndpoint RideBookingListReq  where
 makeRequest reqBody@(RideBookingListReq limit offset onlyActive status clientId ) headers = defaultMakeRequestWithoutLogs GET (EP.rideBookingList limit offset onlyActive status clientId) headers reqBody Nothing
 encodeRequest req = standardEncode req




------------------------------------------------------------------------ CallDriver API TYPES -----------------------------------------------------------------------------------------
data CallDriverReq = CallDriverReq String

newtype CallDriverRes = CallDriverRes {
  callId :: String
}

instance makeCallDriverReq :: RestEndpoint CallDriverReq  where
  makeRequest reqBody@(CallDriverReq rideId) headers = defaultMakeRequestWithoutLogs POST (EP.callCustomerToDriver rideId) headers reqBody Nothing
  encodeRequest req = standardEncode req

derive instance genericCallDriverReq :: Generic CallDriverReq _
instance standardEncodeCallDriverReq :: StandardEncode CallDriverReq where standardEncode (CallDriverReq body) = standardEncode body
instance showCallDriverReq :: Show CallDriverReq where show = genericShow
instance decodeCallDriverReq :: Decode CallDriverReq where decode = defaultDecode
instance encodeCallDriverReq  :: Encode CallDriverReq where encode = defaultEncode

derive instance genericCallDriverRes :: Generic CallDriverRes _
derive instance newtypeCallDriverRes :: Newtype CallDriverRes _
instance standardEncodeCallDriverRes :: StandardEncode CallDriverRes where standardEncode (CallDriverRes body) = standardEncode body
instance showCallDriverRes :: Show CallDriverRes where show = genericShow
instance decodeCallDriverRes :: Decode CallDriverRes where decode = defaultDecode
instance encodeCallDriverRes  :: Encode CallDriverRes where encode = defaultEncode


newtype FeedbackReq = FeedbackReq
  { rating :: Int
  , rideId :: String
  , feedbackDetails :: String
  , wasRideSafe :: Maybe Boolean
  , wasOfferedAssistance :: Maybe Boolean
  , mbAudio :: Maybe String
  , shouldFavDriver :: Maybe Boolean
  }


instance makeFeedBackReq :: RestEndpoint FeedbackReq  where
  makeRequest reqBody headers = defaultMakeRequestWithoutLogs POST (EP.feedback "") headers reqBody Nothing
  encodeRequest req = standardEncode req

derive instance genericFeedbackReq :: Generic FeedbackReq _
instance standardEncodeFeedbackReq :: StandardEncode FeedbackReq where standardEncode (FeedbackReq body) = standardEncode body
instance showFeedbackReq :: Show FeedbackReq where show = genericShow
instance decodeFeedbackReq :: Decode FeedbackReq where decode = defaultDecode
instance encodeFeedbackReq  :: Encode FeedbackReq where encode = defaultEncode



data GetProfileReq = GetProfileReq

newtype GetProfileRes = GetProfileRes
  {
    middleName ::Maybe String
  , lastName :: Maybe String
  , maskedDeviceToken :: Maybe String
  , firstName :: Maybe String
  , id :: String
  , maskedMobileNumber :: Maybe String
  , email :: Maybe String
  , isBlocked :: Maybe Boolean
  , hasTakenRide :: Boolean
  , hasTakenValidCabRide :: Maybe Boolean
  , hasTakenValidAutoRide :: Maybe Boolean
  , hasTakenValidBikeRide :: Maybe Boolean
  , hasTakenValidRide :: Maybe Boolean
  , referralCode :: Maybe String
  , language :: Maybe String
  , gender :: Maybe String
  , bundleVersion :: Maybe Version
  , clientVersion :: Maybe Version
  , disability :: Maybe String
  , hasDisability :: Maybe Boolean
  , hasCompletedSafetySetup :: Maybe Boolean
  , hasCompletedMockSafetyDrill :: Maybe Boolean
  , followsRide :: Maybe Boolean
  , isSafetyCenterDisabled :: Maybe Boolean
  , customerReferralCode :: Maybe String
  , deviceId :: Maybe String
  , androidId :: Maybe String
  , aadhaarVerified :: Maybe Boolean
  , cancellationRate :: Maybe Number
  }


newtype UpdateProfileReq = UpdateProfileReq
  {
    middleName :: Maybe String
  , lastName :: Maybe String
  , deviceToken :: Maybe String
  , firstName :: Maybe String
  , email :: Maybe String
  , referralCode :: Maybe String
  , language :: Maybe String
  , gender :: Maybe String
  , bundleVersion :: Maybe Version
  , clientVersion :: Maybe Version
  , disability :: Maybe Disability
  , hasDisability :: Maybe Boolean
  , deviceId :: Maybe String
  , androidId :: Maybe String
  }



data Disability = Disability 
  {
    id :: String
  , tag :: String
  , description :: String
  }

derive instance genericDisability :: Generic Disability _
instance standardEncodeDisability :: StandardEncode Disability where standardEncode (Disability body) = standardEncode body
instance showDisability :: Show Disability where show = genericShow
instance decodeDisability :: Decode Disability where decode = defaultDecode
instance encodeDisability  :: Encode Disability where encode = defaultEncode

instance makeGetProfileReq :: RestEndpoint GetProfileReq  where
  makeRequest reqBody headers = defaultMakeRequestWithoutLogs GET (EP.profile "") headers reqBody Nothing
  encodeRequest req = standardEncode req

instance makeUpdateProfileReq :: RestEndpoint UpdateProfileReq  where
  makeRequest reqBody headers = defaultMakeRequestWithoutLogs POST (EP.profile "") headers reqBody Nothing
  encodeRequest req = standardEncode req

derive instance genericGetProfileRes :: Generic GetProfileRes _
derive instance newtypeGetProfileRes :: Newtype GetProfileRes _
instance standardEncodeGetProfileRes :: StandardEncode GetProfileRes where standardEncode (GetProfileRes body) = standardEncode body
instance showGetProfileRes :: Show GetProfileRes where show = genericShow
instance decodeGetProfileRes :: Decode GetProfileRes where decode = defaultDecode
instance encodeGetProfileRes  :: Encode GetProfileRes where encode = defaultEncode

derive instance genericGetProfileReq :: Generic GetProfileReq _
instance standardEncodeGetProfileReq :: StandardEncode GetProfileReq where standardEncode (GetProfileReq) = standardEncode {}
instance showGetProfileReq :: Show GetProfileReq where show = genericShow
instance decodeGetProfileReq :: Decode GetProfileReq where decode = defaultDecode
instance encodeGetProfileReq  :: Encode GetProfileReq where encode = defaultEncode

derive instance genericUpdateProfileReq :: Generic UpdateProfileReq _
derive instance newtypeUpdateProfileReq :: Newtype UpdateProfileReq _
instance standardEncodeUpdateProfileReq :: StandardEncode UpdateProfileReq where standardEncode (UpdateProfileReq body) = standardEncode body
instance showUpdateProfileReq :: Show UpdateProfileReq where show = genericShow
instance decodeUpdateProfileReq :: Decode UpdateProfileReq where decode = defaultDecode
instance encodeUpdateProfileReq  :: Encode UpdateProfileReq where encode = defaultEncode


data RouteReq = RouteReq String GetRouteReq

newtype GetRouteReq = GetRouteReq {
  waypoints :: Array LatLong
, mode :: Maybe String
, calcPoints :: Boolean
, rideId :: Maybe String
}

newtype GetRouteResp = GetRouteResp (Array Route)

newtype Route = Route
  {
    points :: Snapped
  , pointsForRentals :: Maybe Snapped
  , boundingBox :: Maybe (Array Number)
  , snappedWaypoints :: Snapped
  , duration :: Int
  , distance :: Int
  }

newtype BoundingLatLong = BoundingLatLong (Array Number)

newtype Snapped = Snapped (Array LatLong)

instance makeRouteReq :: RestEndpoint RouteReq  where
  makeRequest reqBody@(RouteReq rType (GetRouteReq reqB)) headers = defaultMakeRequestWithoutLogs POST (EP.getRoute rType) headers reqBody Nothing
  encodeRequest req = standardEncode req

derive instance genericRouteReq :: Generic RouteReq _
instance standardEncodeRouteReq :: StandardEncode RouteReq where standardEncode (RouteReq rType body) = standardEncode body
instance showRouteReq :: Show RouteReq where show = genericShow
instance decodeRouteReq :: Decode RouteReq where decode = defaultDecode
instance encodeRouteReq  :: Encode RouteReq where encode = defaultEncode

derive instance genericGetRouteReq :: Generic GetRouteReq _
derive instance newtypeGetRouteReq :: Newtype GetRouteReq _
instance standardEncodeGetRouteReq :: StandardEncode GetRouteReq where standardEncode (GetRouteReq body) = standardEncode body
instance showGetRouteReq :: Show GetRouteReq where show = genericShow
instance decodeGetRouteReq :: Decode GetRouteReq where decode = defaultDecode
instance encodeGetRouteReq  :: Encode GetRouteReq where encode = defaultEncode

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

derive instance genericBoundingLatLong :: Generic BoundingLatLong _
derive instance newtypeBoundingLatLong :: Newtype BoundingLatLong _
instance standardEncodeBoundingLatLong :: StandardEncode BoundingLatLong where standardEncode (BoundingLatLong body) = standardEncode body
instance showBoundingLatLong :: Show BoundingLatLong where show = genericShow
instance decodeBoundingLatLong :: Decode BoundingLatLong where decode = defaultDecode
instance encodeBoundingLatLong  :: Encode BoundingLatLong where encode = defaultEncode

newtype EmergContactsReq = EmergContactsReq {
  defaultEmergencyNumbers :: Array ContactDetails
}

newtype ContactDetails = ContactDetails {
  mobileNumber :: String,
  name :: String,
  mobileCountryCode :: String,
  priority :: Maybe Int,
  enableForFollowing :: Maybe Boolean,
  enableForShareRide :: Maybe Boolean,
  shareTripWithEmergencyContactOption:: Maybe RideShareOptions,
  onRide :: Maybe Boolean,
  contactPersonId :: Maybe String,
  notifiedViaFCM :: Maybe Boolean
}


instance makeEmergContactsReq :: RestEndpoint EmergContactsReq  where
  makeRequest reqBody headers = defaultMakeRequestWithoutLogs POST (EP.emergencyContacts "") headers reqBody Nothing
  encodeRequest req = encode req

derive instance genericEmergContactsReq :: Generic EmergContactsReq _
derive instance newtypeEmergContactsReq :: Newtype EmergContactsReq _
instance standardEncodeEmergContactsReq :: StandardEncode EmergContactsReq where standardEncode (EmergContactsReq body) = standardEncode body
instance showEmergContactsReq :: Show EmergContactsReq where show = genericShow
instance decodeEmergContactsReq :: Decode EmergContactsReq where decode = defaultDecode
instance encodeEmergContactsReq  :: Encode EmergContactsReq where encode = defaultEncode

derive instance genericContactDetails :: Generic ContactDetails _
derive instance newtypeContactDetails :: Newtype ContactDetails _
instance standardEncodeContactDetails :: StandardEncode ContactDetails where standardEncode (ContactDetails body) = standardEncode body
instance showContactDetails :: Show ContactDetails where show = genericShow
instance decodeContactDetails :: Decode ContactDetails where decode = defaultDecode
instance encodeContactDetails  :: Encode ContactDetails where encode = defaultEncode



data GetEmergContactsReq = GetEmergContactsReq

newtype GetEmergContactsResp = GetEmergContactsResp {
  defaultEmergencyNumbers :: Array ContactDetails
}

instance makeGetEmergContactsReq :: RestEndpoint GetEmergContactsReq  where
  makeRequest reqBody headers = defaultMakeRequestWithoutLogs GET (EP.emergencyContacts "") headers reqBody Nothing
  encodeRequest req = standardEncode req

derive instance genericGetEmergContactsReq :: Generic GetEmergContactsReq _
instance standardEncodeGetEmergContactsReq :: StandardEncode GetEmergContactsReq where standardEncode (GetEmergContactsReq) = standardEncode {}
instance showGetEmergContactsReq :: Show GetEmergContactsReq where show = genericShow
instance decodeGetEmergContactsReq :: Decode GetEmergContactsReq where decode = defaultDecode
instance encodeGetEmergContactsReq  :: Encode GetEmergContactsReq where encode = defaultEncode

derive instance genericGetEmergContactsResp :: Generic GetEmergContactsResp _
derive instance newtypeGetEmergContactsResp :: Newtype GetEmergContactsResp _
instance standardEncodeGetEmergContactsResp :: StandardEncode GetEmergContactsResp where standardEncode (GetEmergContactsResp body) = standardEncode body
instance showGetEmergContactsResp :: Show GetEmergContactsResp where show = genericShow
instance decodeGetEmergContactsResp :: Decode GetEmergContactsResp where decode = defaultDecode
instance encodeGetEmergContactsResp  :: Encode GetEmergContactsResp where encode = defaultEncode

data GetDriverLocationReq = GetDriverLocationReq String

newtype GetDriverLocationResp = GetDriverLocationResp LatLong

instance makeGetDriverLocationReq :: RestEndpoint GetDriverLocationReq  where
  makeRequest reqBody@(GetDriverLocationReq rideId) headers = defaultMakeRequestWithoutLogs POST (EP.getCurrentLocation rideId) headers reqBody Nothing
  encodeRequest req = standardEncode req

derive instance genericGetDriverLocationResp :: Generic GetDriverLocationResp _
derive instance newtypeGetDriverLocationResp :: Newtype GetDriverLocationResp _
instance standardEncodeGetDriverLocationResp :: StandardEncode GetDriverLocationResp where standardEncode (GetDriverLocationResp body) = standardEncode body
instance showGetDriverLocationResp :: Show GetDriverLocationResp where show = genericShow
instance decodeGetDriverLocationResp :: Decode GetDriverLocationResp where decode = defaultDecode
instance encodeGetDriverLocationResp  :: Encode GetDriverLocationResp where encode = defaultEncode

derive instance genericGetDriverLocationReq :: Generic GetDriverLocationReq _
instance standardEncodeGetDriverLocationReq :: StandardEncode GetDriverLocationReq where standardEncode (GetDriverLocationReq rideId) = standardEncode rideId
instance showGetDriverLocationReq :: Show GetDriverLocationReq where show = genericShow
instance decodeGetDriverLocationReq :: Decode GetDriverLocationReq where decode = defaultDecode
instance encodeGetDriverLocationReq  :: Encode GetDriverLocationReq where encode = defaultEncode

newtype SavedReqLocationAPIEntity = SavedReqLocationAPIEntity
  { area :: Maybe String
  , state :: Maybe String
  , tag :: String
  , country :: Maybe String
  , building :: Maybe String
  , door :: Maybe String
  , street :: Maybe String
  , lat :: Number
  , city :: Maybe String
  , areaCode :: Maybe String
  , lon :: Number
  , placeId :: Maybe String
  , ward :: Maybe String
  , locationName :: Maybe String
}

data AddLocationResp = AddLocationResp

instance makeSavedReqLocationAPIEntity :: RestEndpoint SavedReqLocationAPIEntity  where
 makeRequest reqBody headers = defaultMakeRequestWithoutLogs POST (EP.addLocation "") headers reqBody Nothing
 encodeRequest req = standardEncode req

derive instance genericSavedReqLocationAPIEntity :: Generic SavedReqLocationAPIEntity _
derive instance newtypeSavedReqLocationAPIEntity :: Newtype SavedReqLocationAPIEntity _
instance standardEncodeSavedReqLocationAPIEntity :: StandardEncode SavedReqLocationAPIEntity where standardEncode (SavedReqLocationAPIEntity id) = standardEncode id
instance showSavedReqLocationAPIEntity:: Show SavedReqLocationAPIEntity where show = genericShow
instance decodeSavedReqLocationAPIEntity :: Decode SavedReqLocationAPIEntity where decode = defaultDecode
instance encodeSavedReqLocationAPIEntity :: Encode SavedReqLocationAPIEntity where encode = defaultEncode

derive instance genericAddLocationResp :: Generic AddLocationResp _
instance standardEncodeAddLocationResp :: StandardEncode AddLocationResp where standardEncode (AddLocationResp) = standardEncode {}
instance decodeAddLocationResp :: Decode AddLocationResp where decode = defaultDecode
instance encodeAddLocationResp :: Encode AddLocationResp where encode = defaultEncode

data SavedLocationReq = SavedLocationReq

newtype SavedLocationsListRes = SavedLocationsListRes
  {
    list :: Array SavedReqLocationAPIEntity
  }

instance savedLocationReq :: RestEndpoint SavedLocationReq  where
 makeRequest reqBody headers = defaultMakeRequestWithoutLogs GET (EP.savedLocation "") headers reqBody Nothing
 encodeRequest req = standardEncode req

derive instance genericSavedLocationReq :: Generic SavedLocationReq _
instance standardEncodeSavedLocationReq :: StandardEncode SavedLocationReq where standardEncode (SavedLocationReq) = standardEncode {}
instance decodeSavedLocationReq :: Decode SavedLocationReq where decode = defaultDecode
instance encodeSavedLocationReq :: Encode SavedLocationReq where encode = defaultEncode

derive instance genericSavedLocationsListRes:: Generic SavedLocationsListRes _
derive instance newtypeSavedLocationsListRes :: Newtype SavedLocationsListRes _
instance standardEncodeSavedLocationsListRes :: StandardEncode SavedLocationsListRes where standardEncode (SavedLocationsListRes id) = standardEncode id
instance showSavedLocationsListRes:: Show SavedLocationsListRes where show = genericShow
instance decodeSavedLocationsListRes :: Decode SavedLocationsListRes where decode = defaultDecode
instance encodeSSavedLocationsListRes :: Encode SavedLocationsListRes where encode = defaultEncode

data DeleteSavedLocationReq = DeleteSavedLocationReq String

data DeleteSavedLocationRes = DeleteSavedLocationRes

instance deleteSavedLocationReq :: RestEndpoint DeleteSavedLocationReq  where
 makeRequest reqBody@(DeleteSavedLocationReq tag) headers = defaultMakeRequestWithoutLogs DELETE (EP.deleteLocation tag) headers reqBody Nothing
 encodeRequest req = standardEncode req


derive instance genericDeleteSavedLocationReq :: Generic DeleteSavedLocationReq _
instance standardEncodeDeleteSavedLocationReq :: StandardEncode DeleteSavedLocationReq where standardEncode (DeleteSavedLocationReq tag) = standardEncode tag
instance decodeDeleteSavedLocationReq :: Decode DeleteSavedLocationReq where decode = defaultDecode
instance encodeDeleteSavedLocationReq :: Encode DeleteSavedLocationReq where encode = defaultEncode

derive instance genericDeleteSavedLocationRes :: Generic DeleteSavedLocationRes _
instance standardEncodeDeleteSavedLocationRes :: StandardEncode DeleteSavedLocationRes where standardEncode (DeleteSavedLocationRes) = standardEncode {}
instance decodeDeleteSavedLocationRes :: Decode DeleteSavedLocationRes where decode = defaultDecode
instance encodeDeleteSavedLocationRes :: Encode DeleteSavedLocationRes where encode = defaultEncode


newtype SendIssueReq = SendIssueReq
  {
    contactEmail :: Maybe String
  , rideBookingId :: Maybe String
  , issue :: Issue
  , nightSafety :: Maybe Boolean
  }

newtype Issue = Issue
  {
    reason :: String
  , description :: String
  }


instance makeSendIssueReq :: RestEndpoint SendIssueReq  where
    makeRequest reqBody headers = defaultMakeRequestWithoutLogs POST (EP.sendIssue "") headers reqBody Nothing
    encodeRequest req = defaultEncode req

derive instance genericSendIssueReq :: Generic SendIssueReq _
derive instance newtypeSendIssueReq :: Newtype SendIssueReq _
instance standardEncodeSendIssueReq :: StandardEncode SendIssueReq where standardEncode (SendIssueReq req) = standardEncode req
instance showSendIssueReq :: Show SendIssueReq where show = genericShow
instance decodeSendIssueReq :: Decode SendIssueReq where decode = defaultDecode
instance encodeSendIssueReq :: Encode SendIssueReq where encode = defaultEncode

derive instance genericIssue :: Generic Issue _
derive instance newtypeIssue:: Newtype Issue _
instance standardEncodeIssue :: StandardEncode Issue where standardEncode (Issue req) = standardEncode req
instance showIssue :: Show Issue where show = genericShow
instance decodeIssue :: Decode Issue where decode = defaultDecode
instance encodeIssue :: Encode Issue where encode = defaultEncode


newtype DestinationServiceabilityReq = DestinationServiceabilityReq
  { location  :: LatLong
  }

data ServiceabilityType = ORIGIN | DESTINATION

derive instance genericServiceabilityType :: Generic ServiceabilityType _
instance showServiceabilityType :: Show ServiceabilityType where show = genericShow

newtype ServiceabilityReq = ServiceabilityReq
  { location  :: LatLong
  }

newtype ServiceabilityRes = ServiceabilityRes
  { serviceable :: Boolean,
    -- geoJson :: Maybe String, :: to be deprecated
    specialLocation :: Maybe SpecialLocation,
    city :: Maybe String ,
    hotSpotInfo :: Array HotSpotInfo,
    currentCity :: Maybe String
  }

newtype ServiceabilityResDestination = ServiceabilityResDestination
  { serviceable :: Boolean,
    -- geoJson :: Maybe String, :: to be deprecated
    specialLocation :: Maybe SpecialLocation,
    city :: Maybe String,
    hotSpotInfo :: Array HotSpotInfo
  }
  
newtype SpecialLocation = SpecialLocation
  {
    category :: String,
    -- gates :: Array GatesInfo, :: to be deprecated
    gatesInfo :: Array GateInfoFull,
    locationName :: String,
    geoJson :: Maybe String
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
  id :: String,
  name :: String,
  point :: LatLong
}

newtype HotSpotInfo = HotSpotInfo {
  centroidLatLong :: LatLong,
  geoHash :: String
}


data ServiceabilityRequest = ServiceabilityRequest String ServiceabilityReq 

instance makeServiceabilityReq :: RestEndpoint ServiceabilityRequest  where 
    makeRequest reqBody@(ServiceabilityRequest serviceabilityType rqBody) headers = defaultMakeRequestWithoutLogs POST (EP.locServiceability serviceabilityType) headers reqBody Nothing
    encodeRequest (ServiceabilityRequest serviceabilityType req) = defaultEncode req


derive instance genericServiceabilityRequest :: Generic ServiceabilityRequest _
instance standardEncodeServiceabilityRequest :: StandardEncode ServiceabilityRequest where standardEncode (ServiceabilityRequest serviceabilityType req) = standardEncode req
instance decodeServiceabilityRequest :: Decode ServiceabilityRequest where decode = defaultDecode
instance encodeServiceabilityRequest :: Encode ServiceabilityRequest where encode = defaultEncode

derive instance genericServiceabilityReq :: Generic ServiceabilityReq _
derive instance newtypeServiceabilityReq:: Newtype ServiceabilityReq _
instance standardEncodeServiceabilityReq :: StandardEncode ServiceabilityReq where standardEncode (ServiceabilityReq req) = standardEncode req
instance showServiceabilityReq :: Show ServiceabilityReq where show = genericShow
instance decodeServiceabilityReq :: Decode ServiceabilityReq where decode = defaultDecode
instance encodeServiceabilityReq :: Encode ServiceabilityReq where encode = defaultEncode

derive instance genericServiceabilityRes :: Generic ServiceabilityRes _
derive instance newtypeServiceabilityRes:: Newtype ServiceabilityRes _
instance standardEncodeServiceabilityRes :: StandardEncode ServiceabilityRes where standardEncode (ServiceabilityRes req) = standardEncode req
instance showServiceabilityRes :: Show ServiceabilityRes where show = genericShow
instance decodeServiceabilityRes :: Decode ServiceabilityRes where decode = defaultDecode
instance encodeServiceabilityRes :: Encode ServiceabilityRes where encode = defaultEncode

derive instance genericSpecialLocation :: Generic SpecialLocation _
derive instance newtypeSpecialLocation:: Newtype SpecialLocation _
instance standardEncodeSpecialLocation :: StandardEncode SpecialLocation where standardEncode (SpecialLocation req) = standardEncode req
instance showSpecialLocation :: Show SpecialLocation where show = genericShow
instance decodeSpecialLocation :: Decode SpecialLocation where decode = defaultDecode
instance encodeSpecialLocation :: Encode SpecialLocation where encode = defaultEncode

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

derive instance genericHotSpotInfo :: Generic HotSpotInfo _
derive instance newtypeHotSpotInfo:: Newtype HotSpotInfo _
instance standardEncodeHotSpotInfo :: StandardEncode HotSpotInfo where standardEncode (HotSpotInfo req) = standardEncode req
instance showHotSpotInfo :: Show HotSpotInfo where show = genericShow
instance decodeHotSpotInfo :: Decode HotSpotInfo where decode = defaultDecode
instance encodeHotSpotInfo :: Encode HotSpotInfo where encode = defaultEncode

----------------------------------------------------------------------- flowStatus api -------------------------------------------------------------------

data FlowStatusReq = FlowStatusReq

newtype FlowStatusRes = FlowStatusRes
  { currentStatus :: FlowStatus
  , oldStatus :: Maybe FlowStatus
  , isValueAddNP :: Maybe Boolean
  }

data FlowStatus = IDLE {}
                | SEARCHING { requestId :: String , validTill :: String }
                | GOT_ESTIMATE { requestId :: String , validTill :: String }
                | WAITING_FOR_DRIVER_OFFERS { validTill :: String , estimateId :: String, otherSelectedEstimates :: Maybe (Array String), tripCategory :: Maybe TripCategory}
                | DRIVER_OFFERED_QUOTE { validTill :: String , estimateId :: String}
                | WAITING_FOR_DRIVER_ASSIGNMENT { bookingId :: String , validTill :: String, fareProductType :: Maybe String, tripCategory :: Maybe TripCategory }
                | RIDE_ASSIGNED { rideId :: String }
                | PENDING_RATING { rideId :: String }

instance makeFlowStatusReq :: RestEndpoint FlowStatusReq  where
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
                    Right status -> case status of
                                      "IDLE"                          -> (IDLE <$> decode body)
                                      "SEARCHING"                     -> (SEARCHING <$> decode body)
                                      "GOT_ESTIMATE"                  -> (GOT_ESTIMATE <$> decode body)
                                      "WAITING_FOR_DRIVER_OFFERS"     -> (WAITING_FOR_DRIVER_OFFERS <$> decode body)
                                      "DRIVER_OFFERED_QUOTE"          -> (DRIVER_OFFERED_QUOTE <$> decode body)
                                      "WAITING_FOR_DRIVER_ASSIGNMENT" -> (WAITING_FOR_DRIVER_ASSIGNMENT <$> decode body)
                                      "RIDE_ASSIGNED"                 -> (RIDE_ASSIGNED <$> decode body)
                                      "PENDING_RATING"                -> (PENDING_RATING <$> decode body)
                                      _                               -> (fail $ ForeignError "Unknown response")
                    Left err     -> (fail $ ForeignError "Unknown response")
instance encodeFlowStatus :: Encode FlowStatus
  where
    encode (IDLE body) = encode body
    encode (SEARCHING body) = encode body
    encode (GOT_ESTIMATE body) = encode body
    encode (WAITING_FOR_DRIVER_OFFERS body) = encode body
    encode (DRIVER_OFFERED_QUOTE body) = encode body
    encode (WAITING_FOR_DRIVER_ASSIGNMENT body) = encode body
    encode (RIDE_ASSIGNED body) = encode body
    encode (PENDING_RATING body) = encode body
instance standardEncodeFlowStatus :: StandardEncode FlowStatus
  where
    standardEncode (IDLE body) = standardEncode body
    standardEncode (SEARCHING body) = standardEncode body
    standardEncode (GOT_ESTIMATE body) = standardEncode body
    standardEncode (WAITING_FOR_DRIVER_OFFERS body) = standardEncode body
    standardEncode (DRIVER_OFFERED_QUOTE body) = standardEncode body
    standardEncode (WAITING_FOR_DRIVER_ASSIGNMENT body) = standardEncode body
    standardEncode (RIDE_ASSIGNED body) = standardEncode body
    standardEncode (PENDING_RATING body) = standardEncode body

----------------------------------------------------------------------- notifyFlowEvent api -------------------------------------------------------------------

newtype NotifyFlowEventReq = NotifyFlowEventReq
  { event :: String }


instance makeNotifyFlowEventReq :: RestEndpoint NotifyFlowEventReq  where
 makeRequest reqBody headers = defaultMakeRequestWithoutLogs POST (EP.notifyFlowEvent "") headers reqBody Nothing
 encodeRequest req = standardEncode req

derive instance genericNotifyFlowEventReq :: Generic NotifyFlowEventReq _
derive instance newtypeNotifyFlowEventReq :: Newtype NotifyFlowEventReq _
instance standardEncodeNotifyFlowEventReq :: StandardEncode NotifyFlowEventReq where standardEncode (NotifyFlowEventReq req) = standardEncode req
instance showNotifyFlowEventReq :: Show NotifyFlowEventReq where show = genericShow
instance decodeNotifyFlowEventReq :: Decode NotifyFlowEventReq where decode = defaultDecode
instance encodeNotifyFlowEventReq :: Encode NotifyFlowEventReq where encode = defaultEncode



----------------------------------------------------------------------- RequestCallback api -------------------------------------------------------------------

data RequestCallbackReq = RequestCallbackReq


instance makeRequestCallbackReq :: RestEndpoint RequestCallbackReq  where
    makeRequest reqBody@(RequestCallbackReq) headers = defaultMakeRequestWithoutLogs POST (EP.callbackRequest "") headers reqBody Nothing
    encodeRequest req = defaultEncode req

derive instance genericRequestCallbackReq :: Generic RequestCallbackReq _
instance standardEncodeRequestCallbackReq :: StandardEncode RequestCallbackReq where standardEncode (RequestCallbackReq ) = standardEncode {}
instance decodeRequestCallbackReq :: Decode RequestCallbackReq where decode = defaultDecode
instance encodeRequestCallbackReq :: Encode RequestCallbackReq where encode = defaultEncode


----------------------------------------------------------------------- cancelEstimate api -------------------------------------------------------------------

data CancelEstimateReq = CancelEstimateReq String


instance makeCancelEstimateReq :: RestEndpoint CancelEstimateReq  where
 makeRequest reqBody@(CancelEstimateReq estimateId) headers = defaultMakeRequestWithoutLogs POST (EP.cancelEstimate estimateId) headers reqBody Nothing
 encodeRequest req = standardEncode req


derive instance genericCancelEstimateReq :: Generic CancelEstimateReq _
instance standardEncodeCancelEstimateReq :: StandardEncode CancelEstimateReq where standardEncode (CancelEstimateReq body) = standardEncode body
instance showCancelEstimateReq :: Show CancelEstimateReq where show = genericShow
instance decodeCancelEstimateReq :: Decode CancelEstimateReq where decode = defaultDecode
instance encodeCancelEstimateReq  :: Encode CancelEstimateReq where encode = defaultEncode

----------------------------------------------------------------------- userCreateSos api -------------------------------------------------------------------

newtype UserSosReq = UserSosReq
  {
     flow :: UserSosFlow,
     rideId :: String,
     isRideEnded :: Boolean,
     notifyAllContacts :: Boolean,
     customerLocation :: Maybe LatLong,
     sendPNOnPostRideSOS :: Maybe Boolean
  }

newtype UserSosFlow = UserSosFlow
  {
    tag :: String,
    contents :: String
  }

newtype UserSosRes = UserSosRes
  {
    sosId :: String
  }

instance makeUserSosReq :: RestEndpoint UserSosReq  where
 makeRequest reqBody headers = defaultMakeRequestWithoutLogs POST (EP.userSos "") headers reqBody Nothing
 encodeRequest req = standardEncode req

derive instance genericUserSosReq :: Generic UserSosReq _
derive instance newtypeUserSosReq:: Newtype UserSosReq _
instance standardEncodeUserSosReq :: StandardEncode UserSosReq where standardEncode (UserSosReq req) = standardEncode req
instance showUserSosReq :: Show UserSosReq where show = genericShow
instance decodeUserSosReq :: Decode UserSosReq where decode = defaultDecode
instance encodeUserSosReq :: Encode UserSosReq where encode = defaultEncode

derive instance genericUserSosRes :: Generic UserSosRes _
derive instance newtypeUserSosRes :: Newtype UserSosRes _
instance standardEncodeUserSosRes :: StandardEncode UserSosRes where standardEncode (UserSosRes res) = standardEncode res
instance showUserSosRes :: Show UserSosRes where show = genericShow
instance decodeUserSosRes :: Decode UserSosRes where decode = defaultDecode
instance encodeUserSosRes :: Encode UserSosRes where encode = defaultEncode

derive instance genericUserSosFlow :: Generic UserSosFlow _
derive instance newtypeUserSosFlow :: Newtype UserSosFlow _
instance standardEncodeUserSosFlow :: StandardEncode UserSosFlow where standardEncode (UserSosFlow res) = standardEncode res
instance showUserSosFlow :: Show UserSosFlow where show = genericShow
instance decodeUserSosFlow :: Decode UserSosFlow where decode = defaultDecode
instance encodeUserSosFlow :: Encode UserSosFlow where encode = defaultEncode


----------------------------------------------------------------------- userUpdateSos api -------------------------------------------------------------------

data UserSosStatusReq = UserSosStatusReq String SosStatus

newtype SosStatus = SosStatus
  {
    status :: String,
    comment :: String
  }


instance makeUserSosStatusReq :: RestEndpoint UserSosStatusReq  where
 makeRequest reqBody@(UserSosStatusReq sosId (SosStatus rqBody)) headers = defaultMakeRequestWithoutLogs POST (EP.userSosStatus sosId) headers reqBody Nothing
 encodeRequest req = standardEncode req

derive instance genericUserSosStatusReq :: Generic UserSosStatusReq _
instance standardEncodeUserSosStatusReq :: StandardEncode UserSosStatusReq where standardEncode (UserSosStatusReq sosId req) = standardEncode req
instance showUserSosStatusReq :: Show UserSosStatusReq where show = genericShow
instance decodeUserSosStatusReq :: Decode UserSosStatusReq where decode = defaultDecode
instance encodeUserSosStatusReq :: Encode UserSosStatusReq where encode = defaultEncode


derive instance genericSosStatus :: Generic SosStatus _
derive instance newtypeSosStaus :: Newtype SosStatus _
instance standardEncodeSosStatus :: StandardEncode SosStatus where standardEncode (SosStatus res) = standardEncode res
instance showSosStatus :: Show SosStatus where show = genericShow
instance decodeSosStatus:: Decode SosStatus where decode = defaultDecode
instance encodeSosStatus :: Encode SosStatus where encode = defaultEncode

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

----------------------------------------------------------------------- onCall api -------------------------------------------------------------------

newtype OnCallReq = OnCallReq
  {
     rideId :: String,
     callType :: String,
     exophoneNumber :: String
  }


instance makeOnCallReq :: RestEndpoint OnCallReq  where
 makeRequest reqBody headers = defaultMakeRequestWithoutLogs POST (EP.onCall "") headers reqBody Nothing
 encodeRequest req = standardEncode req

derive instance genericOnCallReq :: Generic OnCallReq _
derive instance newtypeOnCallReq:: Newtype OnCallReq _
instance standardEncodeOnCallReq :: StandardEncode OnCallReq where standardEncode (OnCallReq req) = standardEncode req
instance showOnCallReq :: Show OnCallReq where show = genericShow
instance decodeOnCallReq :: Decode OnCallReq where decode = defaultDecode
instance encodeOnCallReq :: Encode OnCallReq where encode = defaultEncode


newtype RideFeedbackReq = RideFeedbackReq
  {
    rideId :: String,
    feedback :: Array FeedbackAnswer
  }


data RideFeedbackRes = RideFeedbackRes

instance makeRideFeedbackReq :: RestEndpoint RideFeedbackReq  where
  makeRequest reqBody headers = defaultMakeRequestWithoutLogs POST (EP.bookingFeedback "") headers reqBody Nothing
  encodeRequest req = standardEncode req

derive instance genericRideFeedbackReq :: Generic RideFeedbackReq _
instance standardEncodeRideFeedbackReq :: StandardEncode RideFeedbackReq where standardEncode (RideFeedbackReq body) = standardEncode body
instance showRideFeedbackReq :: Show RideFeedbackReq where show = genericShow
instance decodeRideFeedbackReq :: Decode RideFeedbackReq where decode = defaultDecode
instance encodeRideFeedbackReq  :: Encode RideFeedbackReq where encode = defaultEncode

derive instance genericRideFeedbackRes :: Generic RideFeedbackRes _
instance standardEncodeRideFeedbackRes :: StandardEncode RideFeedbackRes where standardEncode (RideFeedbackRes) = standardEncode {}
instance decodeRideFeedbackRes :: Decode RideFeedbackRes where decode = defaultDecode
instance encodeRideFeedbackRes  :: Encode RideFeedbackRes where encode = defaultEncode

data GetDisabilityListReq = GetDisabilityListReq

newtype GetDisabilityListResp = GetDisabilityListResp (Array Disability)

instance makeGetDisabilityListReq :: RestEndpoint GetDisabilityListReq  where
  makeRequest reqBody headers = defaultMakeRequestWithoutLogs GET (EP.disabilityList "") headers reqBody Nothing
  encodeRequest req = standardEncode req

derive instance genericGetDisabilityListReq :: Generic GetDisabilityListReq _
instance standardEncodeGetDisabilityListReq :: StandardEncode GetDisabilityListReq where standardEncode _ = standardEncode {}
instance showGetDisabilityListReq :: Show GetDisabilityListReq where show = genericShow
instance decodeGetDisabilityListReq :: Decode GetDisabilityListReq where decode = defaultDecode
instance encodeGetDisabilityListReq  :: Encode GetDisabilityListReq where encode = defaultEncode

derive instance genericGetDisabilityListResp :: Generic GetDisabilityListResp _
derive instance newtypeGetDisabilityListResp :: Newtype GetDisabilityListResp _
instance standardEncodeGetDisabilityListResp :: StandardEncode GetDisabilityListResp where standardEncode _ = standardEncode {}
instance showGetDisabilityListResp :: Show GetDisabilityListResp where show = genericShow
instance decodeGetDisabilityListResp :: Decode GetDisabilityListResp where decode = defaultDecode
instance encodeGetDisabilityListResp  :: Encode GetDisabilityListResp where encode = defaultEncode

data PersonStatsReq = PersonStatsReq

newtype PersonStatsRes = PersonStatsRes
  {
    userCategory :: String
  , lastRideTaken :: Maybe String
  , frequencyCategory :: String
  , commonAppUseCase :: String
  , offPeakRidesRate :: Maybe Number
  , email :: Maybe String
  , weekdayEveningPeakRidesRate :: Maybe Number
  , weekendPeakRideRate :: Maybe Number
  , latestSearch :: Maybe String
  , userCancellationRate :: Maybe Number
  , weekdayRidesRate :: Maybe Number
  , weekdayMorningPeakRidesRate :: Maybe Number
  , lifetimeRides :: Int
  , signupDate :: String
  , isBlocked :: Boolean
  , isChurnedUser :: Boolean
  , latestSearchFrom :: Maybe LatLong
  , isWhatsAppOptInStatus :: Boolean
  , emergencyContactsNum :: Int
  , weekendRidesRate :: Maybe Number
  , favoriteLocationsNum :: Int
  , overalCancellationRate :: Maybe Number
  , riderId :: String
  , status :: Maybe String
  }

instance personStatsReq :: RestEndpoint PersonStatsReq  where
 makeRequest reqBody headers = defaultMakeRequestWithoutLogs GET (EP.personStats "") headers reqBody Nothing
 encodeRequest req = standardEncode req

derive instance genericPersonStatsReq :: Generic PersonStatsReq _
instance standardEncodePersonStatsReq :: StandardEncode PersonStatsReq where standardEncode (PersonStatsReq) = standardEncode {}
instance decodePersonStatsReq :: Decode PersonStatsReq where decode = defaultDecode
instance encodePersonStatsReq :: Encode PersonStatsReq where encode = defaultEncode

derive instance genericPersonStatsRes:: Generic PersonStatsRes _
derive instance newtypePersonStatsRes :: Newtype PersonStatsRes _
instance standardEncodePersonStatsRes :: StandardEncode PersonStatsRes where standardEncode (PersonStatsRes id) = standardEncode id
instance showPersonStatsRes:: Show PersonStatsRes where show = genericShow
instance decodePersonStatsRes :: Decode PersonStatsRes where decode = defaultDecode
instance encodeSPersonStatsRes :: Encode PersonStatsRes where encode = defaultEncode

-- =========================================== Zoo Booking API's ================================================================

data BookingStatus = Pending | Failed | Booked | Cancelled

derive instance genericBookingStatus :: Generic BookingStatus _
instance standardEncodeBookingStatus :: StandardEncode BookingStatus where standardEncode _ = standardEncode {}
instance showBookingStatus :: Show BookingStatus where show = genericShow
instance decodeBookingStatus :: Decode BookingStatus where decode = defaultDecode
instance encodeBookingStatus  :: Encode BookingStatus where encode = defaultEncode
instance eqBookingStatus :: Eq BookingStatus where eq = genericEq

data GetAllBookingsReq = GetAllBookingsReq String

newtype GetAllBookingsRes = GetAllBookingsRes (Array TicketBookingAPIEntity)

newtype TicketBookingAPIEntity = TicketBookingAPIEntity
  { ticketShortId :: String,
    ticketPlaceId ::  String,
    ticketPlaceName :: String,
    personId ::  String,
    amount :: Number,
    visitDate :: String,
    status :: String
  }

instance getAllBookingsReq :: RestEndpoint GetAllBookingsReq  where
 makeRequest reqBody@(GetAllBookingsReq status) headers = defaultMakeRequestWithoutLogs GET (EP.getAllBookings (show status)) headers reqBody Nothing
 encodeRequest req = standardEncode req


derive instance genericGetAllBookingsReq :: Generic GetAllBookingsReq _
instance standardEncodeGetAllBookingsReq :: StandardEncode GetAllBookingsReq where standardEncode (GetAllBookingsReq _) = standardEncode {}
instance decodeGetAllBookingsReq :: Decode GetAllBookingsReq where decode = defaultDecode
instance encodeGetAllBookingsReq :: Encode GetAllBookingsReq where encode = defaultEncode
instance showGetAllBookingsReq:: Show GetAllBookingsReq where show = genericShow


derive instance genericGetAllBookingsRes:: Generic GetAllBookingsRes _
derive instance newtypeGetAllBookingsRes :: Newtype GetAllBookingsRes _
instance standardEncodeGetAllBookingsRes :: StandardEncode GetAllBookingsRes where standardEncode _ = standardEncode {}
instance showGetAllBookingsRes:: Show GetAllBookingsRes where show = genericShow
instance decodeGetAllBookingsRes :: Decode GetAllBookingsRes where decode = defaultDecode
instance encodeGetAllBookingsRes :: Encode GetAllBookingsRes where encode = defaultEncode

derive instance genericTicketBookingAPIEntity:: Generic TicketBookingAPIEntity _
derive instance newtypeTicketBookingAPIEntity :: Newtype TicketBookingAPIEntity _
instance standardEncodeTicketBookingAPIEntity :: StandardEncode TicketBookingAPIEntity where standardEncode _ = standardEncode {}
instance showTicketBookingAPIEntity:: Show TicketBookingAPIEntity where show = genericShow
instance decodeTicketBookingAPIEntity :: Decode TicketBookingAPIEntity where decode = defaultDecode
instance encodeTicketBookingAPIEntity :: Encode TicketBookingAPIEntity where encode = defaultEncode

newtype TicketBookingServiceDetails = TicketBookingServiceDetails
  { amount :: Number,
    status :: String,
    verificationCount :: Int,
    expiryDate :: Maybe String,
    ticketServiceName :: String,
    categories :: Array TicketBookingCategoryDetails,
    ticketServiceShortId :: String,
    slot :: Maybe String
  }

newtype TicketBookingCategoryDetails = TicketBookingCategoryDetails
  { amount :: Number,
    bookedSeats :: Int,
    name :: String,
    peopleCategories :: Array TicketBookingPeopleCategoryDetails
  }

newtype TicketBookingPeopleCategoryDetails = TicketBookingPeopleCategoryDetails
  { name :: String,
    numberOfUnits ::Int,
    pricePerUnit :: Number
  }

derive instance genericTicketBookingServiceDetails :: Generic TicketBookingServiceDetails _
derive instance newtypeTicketBookingServiceDetails  :: Newtype TicketBookingServiceDetails  _
instance standardEncodeTicketBookingServiceDetails :: StandardEncode TicketBookingServiceDetails where standardEncode _ = standardEncode {}
instance decodeTicketBookingServiceDetails :: Decode TicketBookingServiceDetails where decode = defaultDecode
instance encodeTicketBookingServiceDetails :: Encode TicketBookingServiceDetails where encode = defaultEncode
instance showTicketBookingServiceDetails :: Show TicketBookingServiceDetails where show = genericShow

derive instance genericTicketBookingCategoryDetails :: Generic TicketBookingCategoryDetails _
derive instance newtypeTicketBookingCategoryDetails  :: Newtype TicketBookingCategoryDetails  _
instance standardEncodeTicketBookingCategoryDetails :: StandardEncode TicketBookingCategoryDetails where standardEncode _ = standardEncode {}
instance decodeTicketBookingCategoryDetails :: Decode TicketBookingCategoryDetails where decode = defaultDecode
instance encodeTicketBookingCategoryDetails :: Encode TicketBookingCategoryDetails where encode = defaultEncode
instance showTicketBookingCategoryDetails :: Show TicketBookingCategoryDetails where show = genericShow

derive instance genericTicketBookingPeopleCategoryDetails :: Generic TicketBookingPeopleCategoryDetails _
derive instance newtypeTicketBookingPeopleCategoryDetails  :: Newtype TicketBookingPeopleCategoryDetails  _
instance standardEncodeTicketBookingPeopleCategoryDetails :: StandardEncode TicketBookingPeopleCategoryDetails where standardEncode _ = standardEncode {}
instance decodeTicketBookingPeopleCategoryDetails :: Decode TicketBookingPeopleCategoryDetails where decode = defaultDecode
instance encodeTicketBookingPeopleCategoryDetails :: Encode TicketBookingPeopleCategoryDetails where encode = defaultEncode
instance showTicketBookingPeopleCategoryDetails :: Show TicketBookingPeopleCategoryDetails where show = genericShow
data GetBookingInfoReq = GetBookingInfoReq String 

newtype TicketBookingDetails = TicketBookingDetails
  { ticketShortId :: String,
    ticketPlaceId :: String,
    ticketPlaceName :: String,
    personId :: String,
    amount :: Number,
    visitDate :: String,
    status :: String,
    services :: Array TicketBookingServiceDetails
  }

instance getBookingInfoReq :: RestEndpoint GetBookingInfoReq  where
 makeRequest reqBody@(GetBookingInfoReq shortId) headers = defaultMakeRequestWithoutLogs GET (EP.ticketBookingDetails shortId) headers reqBody Nothing
 encodeRequest req = standardEncode req

derive instance genericGetBookingInfoReq :: Generic GetBookingInfoReq _
instance standardEncodeGetBookingInfoReq :: StandardEncode GetBookingInfoReq where standardEncode (GetBookingInfoReq shortId) = standardEncode {shortId}
instance decodeGetBookingInfoReq :: Decode GetBookingInfoReq where decode = defaultDecode
instance encodeGetBookingInfoReq :: Encode GetBookingInfoReq where encode = defaultEncode
instance showGetBookingInfoReq:: Show GetBookingInfoReq where show = genericShow


derive instance genericTicketBookingDetails :: Generic TicketBookingDetails _
derive instance newtypeTicketBookingDetails :: Newtype TicketBookingDetails _
instance standardEncodeTicketBookingDetails :: StandardEncode TicketBookingDetails where standardEncode _ = standardEncode {}
instance showTicketBookingDetails:: Show TicketBookingDetails where show = genericShow
instance decodeTicketBookingDetails :: Decode TicketBookingDetails where decode = defaultDecode
instance encodeTicketBookingDetails :: Encode TicketBookingDetails where encode = defaultEncode


data TicketServiceReq = TicketServiceReq String String

data TicketPlaceReq = TicketPlaceReq

newtype TicketPlaceResponse = TicketPlaceResponse (Array TicketPlaceResp)

newtype TicketServicesResponse = TicketServicesResponse (Array TicketServiceResp)



data TicketBookingRequest = TicketBookingRequest String TicketBookingReq

newtype TicketBookingReq = TicketBookingReq
  { visitDate :: String,
    services :: Array TicketService
  }

newtype TicketService = TicketService
  { serviceId :: String,
    businessHourId :: String,
    categories :: Array TicketBookingCategory
  }

newtype TicketBookingCategory = TicketBookingCategory
  { categoryId :: String,
    peopleCategories :: Array TicketBookingPeopleCategory
  }

newtype TicketBookingPeopleCategory = TicketBookingPeopleCategory
  { peopleCategoryId :: String,
    numberOfUnits :: Int
  }

newtype CreateOrderRes = CreateOrderRes --TODO:: Move to common
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

data PlaceType = Museum | ThemePark | AmusementPark | WaterPark | WildLifeSanctuary | ArtGallery | HeritageSite | ReligiousSite | Other

data ServiceExpiry = VisitDate String | InstantExpiry Int
derive instance genericServiceExpiry :: Generic ServiceExpiry _
instance showServiceExpiry :: Show ServiceExpiry where show = genericShow
instance decodeServiceExpiry :: Decode ServiceExpiry where decode = defaultDecode
instance encodeServiceExpiry :: Encode ServiceExpiry where encode = defaultEncode
instance standardEncodeServiceExpiry :: StandardEncode ServiceExpiry
  where
    standardEncode (VisitDate param) = standardEncode param
    standardEncode (InstantExpiry body) = standardEncode body

newtype TicketServiceResp = TicketServiceResp
  { id :: String,
    placesId :: String,
    name :: String,
    maxVerification :: Int,
    allowFutureBooking :: Boolean,
    shortDesc :: Maybe String,
    expiry :: ServiceExpiry,
    businessHours :: Array BusinessHoursResp
  }

data SpecialDayType = Open | Closed

derive instance genericSpecialDayType :: Generic SpecialDayType _
instance showSpecialDayType :: Show SpecialDayType where show = genericShow
instance decodeSpecialDayType :: Decode SpecialDayType where decode = defaultEnumDecode
instance encodeSpecialDayType :: Encode SpecialDayType where encode = defaultEnumEncode
instance eqSpecialDayType :: Eq SpecialDayType where eq = genericEq
instance standardEncodeSpecialDayType :: StandardEncode SpecialDayType
  where
    standardEncode _ = standardEncode {}

data TicketPlaceStatus = Active | Inactive | ComingSoon

derive instance genericTicketPlaceStatus :: Generic TicketPlaceStatus _
instance showTicketPlaceStatus :: Show TicketPlaceStatus where show = genericShow
instance decodeTicketPlaceStatus :: Decode TicketPlaceStatus where decode = defaultEnumDecode
instance encodeTicketPlaceStatus :: Encode TicketPlaceStatus where encode = defaultEnumEncode
instance eqTicketPlaceStatus :: Eq TicketPlaceStatus where eq = genericEq
instance standardEncodeTicketPlaceStatus :: StandardEncode TicketPlaceStatus
  where
    standardEncode _ = standardEncode {}

data TicketServiceType = METRO | BUS

derive instance genericTicketServiceType :: Generic TicketServiceType _
instance showTicketServiceType :: Show TicketServiceType where show = genericShow
instance decodeTicketServiceType :: Decode TicketServiceType where decode = defaultEnumDecode
instance encodeTicketServiceType :: Encode TicketServiceType where encode = defaultEnumEncode
instance eqTicketServiceType :: Eq TicketServiceType where eq = genericEq
instance standardEncodeTicketServiceType :: StandardEncode TicketServiceType
  where
    standardEncode _ = standardEncode {}

newtype BusinessHoursResp = BusinessHoursResp {
  id :: String,
  slot :: Maybe String, -- array of slots
  startTime :: Maybe String,
  endTime :: Maybe String,
  specialDayDescription :: Maybe String,
  specialDayType :: Maybe String,
  operationalDays :: Array String,
  operationalDate :: Maybe OperationalDateResp,
  categories :: Array TicketCategoriesResp
}

newtype OperationalDateResp = OperationalDateResp {
  startDate :: String,
  eneDate :: String
}

newtype TicketCategoriesResp = TicketCategoriesResp {
  name :: String,
  id :: String,
  availableSeats :: Maybe Int,
  isClosed :: Maybe Boolean,
  allowedSeats :: Maybe Int,
  bookedSeats :: Int,
  peopleCategories :: Array PeopleCategoriesResp
}

newtype PeopleCategoriesResp = PeopleCategoriesResp {
  name :: String,
  id :: String,
  pricePerUnit :: Number,
  description :: Maybe String
}

newtype TicketPlaceResp = TicketPlaceResp
  { id :: String,
    merchantOperatingCityId :: String,
    name :: String,
    description :: Maybe String,
    lat :: Maybe Number,
    lon :: Maybe Number,
    gallery :: Array String,
    openTimings :: Maybe String,
    closeTimings :: Maybe String,
    iconUrl :: Maybe String,
    shortDesc :: Maybe String,
    mapImageUrl :: Maybe String,
    termsAndConditions :: Array String,
    placeType :: String,
    status :: Maybe TicketPlaceStatus,
    allowSameDayBooking :: Maybe Boolean,
    termsAndConditionsUrl :: Maybe String
  }

data GetTicketStatusReq = GetTicketStatusReq String

data GetTicketStatusResp = GetTicketStatusResp String

instance makeGetTicketStatusReq :: RestEndpoint GetTicketStatusReq  where
  makeRequest reqBody@(GetTicketStatusReq placeId) headers = defaultMakeRequestWithoutLogs GET (EP.ticketStatus placeId) headers reqBody Nothing
  encodeRequest req = standardEncode req

instance makeTicketServiceReq :: RestEndpoint TicketServiceReq  where
 makeRequest reqBody@(TicketServiceReq placeId date) headers = defaultMakeRequestWithoutLogs GET (EP.ticketPlaceServices placeId date) headers reqBody Nothing
 encodeRequest req = standardEncode req

instance makeTicketPlaceReq :: RestEndpoint TicketPlaceReq  where
  makeRequest reqBody headers = defaultMakeRequestWithoutLogs GET (EP.ticketPlaces "") headers reqBody Nothing
  encodeRequest req = standardEncode req

instance makeTicketBookingRequest :: RestEndpoint TicketBookingRequest  where
  makeRequest reqBody@(TicketBookingRequest placeId (TicketBookingReq rqBody)) headers = defaultMakeRequestWithoutLogs POST (EP.ticketPlaceBook placeId) headers reqBody Nothing
  encodeRequest req = standardEncode req

derive instance genericGetTicketStatusReq :: Generic GetTicketStatusReq _
instance standardEncodeGetTicketStatusReq :: StandardEncode GetTicketStatusReq where standardEncode (GetTicketStatusReq id) = standardEncode id
instance showGetTicketStatusReq :: Show GetTicketStatusReq where show = genericShow
instance decodeGetTicketStatusReq :: Decode GetTicketStatusReq where decode = defaultDecode
instance encodeGetTicketStatusReq  :: Encode GetTicketStatusReq where encode = defaultEncode

derive instance genericGetTicketStatusResp :: Generic GetTicketStatusResp _
instance standardEncodeGetTicketStatusResp :: StandardEncode GetTicketStatusResp where standardEncode (GetTicketStatusResp res) = standardEncode res
instance showGetTicketStatusResp :: Show GetTicketStatusResp where show = genericShow
instance decodeGetTicketStatusResp :: Decode GetTicketStatusResp where decode = defaultDecode
instance encodeGetTicketStatusResp  :: Encode GetTicketStatusResp where encode = defaultEncode

derive instance genericTicketService:: Generic TicketService _
derive instance newtypeTicketService :: Newtype TicketService _
instance standardEncodeTicketService :: StandardEncode TicketService where standardEncode (TicketService req) = standardEncode req
instance showTicketService:: Show TicketService where show = genericShow
instance decodeTicketService :: Decode TicketService where decode = defaultDecode
instance encodeSTicketService :: Encode TicketService where encode = defaultEncode

derive instance genericTicketBookingCategory:: Generic TicketBookingCategory _
derive instance newtypeTicketBookingCategory :: Newtype TicketBookingCategory _
instance standardEncodeTicketBookingCategory :: StandardEncode TicketBookingCategory where standardEncode (TicketBookingCategory req) = standardEncode req
instance showTicketBookingCategory:: Show TicketBookingCategory where show = genericShow
instance decodeTicketBookingCategory:: Decode TicketBookingCategory where decode = defaultDecode
instance encodeSTicketBookingCategory :: Encode TicketBookingCategory where encode = defaultEncode

derive instance genericTicketBookingPeopleCategory:: Generic TicketBookingPeopleCategory _
derive instance newtypeTicketBookingPeopleCategory :: Newtype TicketBookingPeopleCategory _
instance standardEncodeTicketBookingPeopleCategory :: StandardEncode TicketBookingPeopleCategory where standardEncode (TicketBookingPeopleCategory req) = standardEncode req
instance showTicketBookingPeopleCategory :: Show TicketBookingPeopleCategory where show = genericShow
instance decodeTicketBookingPeopleCategory :: Decode TicketBookingPeopleCategory where decode = defaultDecode
instance encodeSTicketBookingPeopleCategory :: Encode TicketBookingPeopleCategory where encode = defaultEncode

derive instance genericTicketBookingReq:: Generic TicketBookingReq _
derive instance newtypeTicketBookingReq :: Newtype TicketBookingReq _
instance standardEncodeTicketBookingReq :: StandardEncode TicketBookingReq where standardEncode (TicketBookingReq req) = standardEncode req
instance showTicketBookingReq:: Show TicketBookingReq where show = genericShow
instance decodeTicketBookingReq :: Decode TicketBookingReq where decode = defaultDecode
instance encodeSTicketBookingReq :: Encode TicketBookingReq where encode = defaultEncode

derive instance genericCreateOrderRes :: Generic CreateOrderRes _
derive instance newtypeCreateOrderRes :: Newtype CreateOrderRes _
instance standardEncodeCreateOrderRes :: StandardEncode CreateOrderRes where standardEncode (CreateOrderRes res) = standardEncode res
instance showCreateOrderRes :: Show CreateOrderRes where
  show (CreateOrderRes { id, order_id, payment_links, sdk_payload }) =
    show {id, order_id, payment_links, sdk_payload}
instance decodeCreateOrderRes :: Decode CreateOrderRes where decode = defaultDecode
instance encodeCreateOrderRes :: Encode CreateOrderRes where encode = defaultEncode

derive instance genericPaymentLinks :: Generic PaymentLinks _
derive instance newtypePaymentLinks :: Newtype PaymentLinks _
instance standardEncodePaymentLinks :: StandardEncode PaymentLinks where standardEncode (PaymentLinks id) = standardEncode id
instance showPaymentLinks :: Show PaymentLinks where show = genericShow
instance decodePaymentLinks :: Decode PaymentLinks where decode = defaultDecode
instance encodePaymentLinks :: Encode PaymentLinks where encode = defaultEncode

derive instance genericTicketServiceResp :: Generic TicketServiceResp _
derive instance newtypeTicketServiceResp :: Newtype TicketServiceResp _
instance standardEncodeTicketServiceResp :: StandardEncode TicketServiceResp where standardEncode (TicketServiceResp id) = standardEncode id
instance showTicketServiceResp :: Show TicketServiceResp where show = genericShow
instance decodeTicketServiceResp :: Decode TicketServiceResp where decode = defaultDecode
instance encodeTicketServiceResp :: Encode TicketServiceResp where encode = defaultEncode

derive instance genericTicketServicesResponse :: Generic TicketServicesResponse _
derive instance newtypeTicketServicesResponse :: Newtype TicketServicesResponse _
instance standardEncodeTicketServicesResponse :: StandardEncode TicketServicesResponse where standardEncode (TicketServicesResponse id) = standardEncode id
instance showTicketServicesResponse :: Show TicketServicesResponse where show = genericShow
instance decodeTicketServicesResponse :: Decode TicketServicesResponse where decode = defaultDecode
instance encodeTicketServicesResponse :: Encode TicketServicesResponse where encode = defaultEncode

derive instance genericPeopleCategoriesResp :: Generic PeopleCategoriesResp _
derive instance newtypePeopleCategoriesResp :: Newtype PeopleCategoriesResp _
instance standardEncodePeopleCategoriesResp :: StandardEncode PeopleCategoriesResp where standardEncode (PeopleCategoriesResp body) = standardEncode body
instance showPeopleCategoriesResp :: Show PeopleCategoriesResp where show = genericShow
instance decodePeopleCategoriesResp :: Decode PeopleCategoriesResp where decode = defaultDecode
instance encodePeopleCategoriesResp :: Encode PeopleCategoriesResp where encode = defaultEncode

derive instance genericTicketPlaceResp :: Generic TicketPlaceResp _
derive instance newtypeTicketPlaceResp :: Newtype TicketPlaceResp _
instance standardEncodeTicketPlaceResp :: StandardEncode TicketPlaceResp where standardEncode (TicketPlaceResp body) = standardEncode body
instance showTicketPlaceResp :: Show TicketPlaceResp where show = genericShow
instance decodeTicketPlaceResp :: Decode TicketPlaceResp where decode = defaultDecode
instance encodeTicketPlaceResp :: Encode TicketPlaceResp where encode = defaultEncode

derive instance genericTicketServiceReq :: Generic TicketServiceReq _
instance standardEncodeTicketServiceReq :: StandardEncode TicketServiceReq where standardEncode (TicketServiceReq id date) = standardEncode { id: id, date: date }
instance showTicketServiceReq :: Show TicketServiceReq where show = genericShow
instance decodeTicketServiceReq :: Decode TicketServiceReq where decode = defaultDecode
instance encodeTicketServiceReq  :: Encode TicketServiceReq where encode = defaultEncode

derive instance genericTicketPlaceReq :: Generic TicketPlaceReq _
instance standardEncodeTicketPlaceReq :: StandardEncode TicketPlaceReq where standardEncode TicketPlaceReq = standardEncode {}
instance showTicketPlaceReq :: Show TicketPlaceReq where show = genericShow
instance decodeTicketPlaceReq :: Decode TicketPlaceReq where decode = defaultDecode
instance encodeTicketPlaceReq  :: Encode TicketPlaceReq where encode = defaultEncode

derive instance genericTicketBookingRequest :: Generic TicketBookingRequest _
instance standardEncodeTicketBookingRequest :: StandardEncode TicketBookingRequest where standardEncode (TicketBookingRequest id body) = standardEncode body
instance showTicketBookingRequest :: Show TicketBookingRequest where show = genericShow
instance encodeTicketBookingRequest  :: Encode TicketBookingRequest where encode = defaultEncode

derive instance genericTicketPlaceResponse :: Generic TicketPlaceResponse _
derive instance newtypeTicketPlaceResponse :: Newtype TicketPlaceResponse _
instance showTicketPlaceResponse :: Show TicketPlaceResponse where show = genericShow
instance decodeTicketPlaceResponse :: Decode TicketPlaceResponse where decode = defaultDecode

derive instance genericPlaceType :: Generic PlaceType _
instance standardEncodePlaceType :: StandardEncode PlaceType where standardEncode _ = standardEncode {}
instance showPlaceType :: Show PlaceType where show = genericShow
instance decodePlaceType :: Decode PlaceType where decode = defaultDecode
instance encodePlaceType  :: Encode PlaceType where encode = defaultEncode
instance eqPlaceType :: Eq PlaceType where eq = genericEq

derive instance genericTicketCategoriesResp :: Generic TicketCategoriesResp _
derive instance newtypeTicketCategoriesResp :: Newtype TicketCategoriesResp _
instance standardEncodeTicketCategoriesResp :: StandardEncode TicketCategoriesResp where standardEncode (TicketCategoriesResp id) = standardEncode id
instance showTicketCategoriesResp :: Show TicketCategoriesResp where show = genericShow
instance decodeTicketCategoriesResp :: Decode TicketCategoriesResp where decode = defaultDecode
instance encodeTicketCategoriesResp :: Encode TicketCategoriesResp where encode = defaultEncode

derive instance genericBusinessHoursResp :: Generic BusinessHoursResp _
derive instance newtypeBusinessHoursResp :: Newtype BusinessHoursResp _
instance standardEncodeBusinessHoursResp :: StandardEncode BusinessHoursResp where standardEncode (BusinessHoursResp id) = standardEncode id
instance showBusinessHoursResp :: Show BusinessHoursResp where show = genericShow
instance decodeBusinessHoursResp :: Decode BusinessHoursResp where decode = defaultDecode
instance encodeBusinessHoursResp :: Encode BusinessHoursResp where encode = defaultEncode

derive instance genericOperationalDateResp :: Generic OperationalDateResp _
derive instance newtypeOperationalDateResp :: Newtype OperationalDateResp _
instance standardEncodeOperationalDateResp :: StandardEncode OperationalDateResp where standardEncode (OperationalDateResp id) = standardEncode id
instance showOperationalDateResp :: Show OperationalDateResp where show = genericShow
instance decodeOperationalDateResp :: Decode OperationalDateResp where decode = defaultDecode
instance encodeOperationalDateResp :: Encode OperationalDateResp where encode = defaultEncode

-------------------------------------------- FetchIssueList -------------------------------------------


data FetchIssueListReq = FetchIssueListReq String

newtype FetchIssueListResp = FetchIssueListResp
 {
   issues :: Array IssueReportCustomerListItem
 }

newtype IssueReportCustomerListItem = IssueReportCustomerListItem
  {
    issueReportId :: String,
    status :: String,
    category :: String,
    createdAt :: String,
    issueReportShortId :: Maybe String,
    optionLabel :: Maybe String,
    rideId :: Maybe String
  }

instance makeFetchIssueListReq :: RestEndpoint FetchIssueListReq  where
    makeRequest reqBody@(FetchIssueListReq language) headers = defaultMakeRequestWithoutLogs GET (EP.fetchIssueList language) headers reqBody Nothing
    encodeRequest = standardEncode

derive instance genericFetchIssueListReq :: Generic FetchIssueListReq _
instance showFetchIssueListReq     :: Show FetchIssueListReq where show     = genericShow
instance standardEncodeFetchIssueReq :: StandardEncode FetchIssueListReq where standardEncode req = standardEncode {}
instance decodeFetchIssueListReq :: Decode FetchIssueListReq where decode = defaultDecode
instance encodeFetchIssueListReq :: Encode FetchIssueListReq where encode = defaultEncode

derive instance genericIssueReportCustomerListItem :: Generic IssueReportCustomerListItem _
instance showIssueReportCustomerListItem :: Show IssueReportCustomerListItem where show = genericShow
instance standardEncodeIssueReportCustomerListItem :: StandardEncode IssueReportCustomerListItem where standardEncode (IssueReportCustomerListItem req) = standardEncode req
instance decodeIssueReportCustomerListItem :: Decode IssueReportCustomerListItem where decode = defaultDecode
instance encodeIssueReportCustomerListItem :: Encode IssueReportCustomerListItem where encode = defaultEncode

derive instance genericFetchIssueListResp :: Generic FetchIssueListResp _
instance showFetchIssueListResp :: Show FetchIssueListResp where show = genericShow
instance standardEncodeFetchIssueListResp :: StandardEncode FetchIssueListResp where standardEncode (FetchIssueListResp res) = standardEncode res
instance decodeFetchIssueListResp :: Decode FetchIssueListResp where decode = defaultDecode
instance encodeFetchIssueListResp :: Encode FetchIssueListResp where encode = defaultEncode

--------------------------------------------------- getCategories ----------------------------------------------------
data GetCategoriesReq = GetCategoriesReq String

newtype GetCategoriesRes = GetCategoriesRes { categories :: Array Category }

newtype Category = Category
  { label    :: String
  , logoUrl  :: String
  , category :: String
  , issueCategoryId :: String
  , isRideRequired :: Boolean
  , maxAllowedRideAge :: Maybe Int
  , allowedRideStatuses :: Maybe (Array String)
  , categoryType :: String
  }

instance makeGetCategoriesReq :: RestEndpoint GetCategoriesReq  where
    makeRequest reqBody@(GetCategoriesReq language) headers = defaultMakeRequestWithoutLogs GET (EP.getCategories language) headers reqBody Nothing
    encodeRequest = standardEncode

derive instance genericGetCategoriesReq :: Generic GetCategoriesReq _
instance showGetCategoriesReq     :: Show GetCategoriesReq where show     = genericShow
instance standardGetCategoriesReq :: StandardEncode GetCategoriesReq where standardEncode (GetCategoriesReq req) = standardEncode {}
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
data GetOptionsReq = GetOptionsReq String String String String String

newtype GetOptionsRes = GetOptionsRes { 
  options :: Array Option, 
  messages :: Array Message
}

newtype Option = Option
  { label  :: String
  , option :: String
  , issueOptionId :: String
  , mandatoryUploads :: Maybe (Array MandatoryUploads)
  }

newtype Message = Message 
  { id  :: String
  , message :: String
  , label :: Maybe String 
  , messageTitle :: Maybe String
  , messageAction :: Maybe String 
  , referenceCategoryId :: Maybe String
  , referenceOptionId :: Maybe String
  }

instance makeGetOptionsReq :: RestEndpoint GetOptionsReq  where
    makeRequest reqBody@(GetOptionsReq categoryId optionId rideId issueReportId language) headers = defaultMakeRequestWithoutLogs GET (EP.getOptions categoryId optionId rideId issueReportId language) headers reqBody Nothing
    encodeRequest = standardEncode


newtype MandatoryUploads = MandatoryUploads 
  { fileType :: MediaType,
    limit :: Int
  }

data MediaType = Video
               | Audio
               | Image
               | AudioLink
               | VideoLink
               | ImageLink
               | PortraitVideoLink

derive instance genericMediaType :: Generic MediaType _
instance showMediaType :: Show MediaType where show = genericShow
instance decodeMediaType :: Decode MediaType where decode = defaultEnumDecode
instance encodeMediaType :: Encode MediaType where encode = defaultEnumEncode
instance eqMediaType :: Eq MediaType where eq = genericEq
instance standardEncodeMediaType :: StandardEncode MediaType where standardEncode _ = standardEncode {}

derive instance genericGetOptionsReq :: Generic GetOptionsReq _
instance showGetOptionsReq     :: Show GetOptionsReq where show     = genericShow
instance standardGetOptionsReq :: StandardEncode GetOptionsReq where standardEncode (GetOptionsReq _ _ _ _ _) = standardEncode {}
instance decodeGetOptionsReq   :: Decode GetOptionsReq where decode = defaultDecode
instance encodeGetOptionsReq   :: Encode GetOptionsReq where encode = defaultEncode

derive instance genericOption :: Generic Option _
instance showOption     :: Show Option where show     = genericShow
instance standardOption :: StandardEncode Option where standardEncode (Option opt) = standardEncode opt
instance decodeOption   :: Decode Option where decode = defaultDecode
instance encodeOption   :: Encode Option where encode = defaultEncode

derive instance genericMandatoryUploads :: Generic MandatoryUploads _
instance showMandatoryUploads     :: Show MandatoryUploads where show     = genericShow
instance standardMandatoryUploads :: StandardEncode MandatoryUploads where standardEncode (MandatoryUploads opt) = standardEncode opt
instance decodeMandatoryUploads   :: Decode MandatoryUploads where decode = defaultDecode
instance encodeMandatoryUploads   :: Encode MandatoryUploads where encode = defaultEncode

derive instance genericMessage :: Generic Message _
instance showMessage     :: Show Message where show     = genericShow
instance standardMessage :: StandardEncode Message where standardEncode (Message msg) = standardEncode msg
instance decodeMessage   :: Decode Message where decode = defaultDecode
instance encodeMessage   :: Encode Message where encode = defaultEncode

derive instance genericGetOptionsRes :: Generic GetOptionsRes _
instance showGetGetOptionsRes        :: Show GetOptionsRes where show     = genericShow
instance standardEncodeGetOptionsRes :: StandardEncode GetOptionsRes where standardEncode (GetOptionsRes res) = standardEncode res
instance decodeGetOptionsRes         :: Decode GetOptionsRes where decode = defaultDecode
instance encodeGetOptionsRes         :: Encode GetOptionsRes where encode = defaultEncode

--------------------------------------------------- IssueReport ----------------------------------------------------
data PostIssueReq = PostIssueReq String PostIssueReqBody

newtype PostIssueReqBody = PostIssueReqBody
  { optionId :: Maybe String
  , rideId :: Maybe String
  , categoryId :: String
  , mediaFiles :: Array String
  , description :: String
  , chats :: Array Chat
  , createTicket :: Boolean
  }

newtype Chat = Chat
  { chatId  :: String
  , chatType :: String
  , timestamp :: String
  }

newtype PostIssueRes = PostIssueRes { 
  issueReportId :: String, 
  issueReportShortId :: Maybe String,
  messages :: Array Message
}

instance makePostIssueReq :: RestEndpoint PostIssueReq  where
    makeRequest reqBody@(PostIssueReq language issueDetails) headers = defaultMakeRequestWithoutLogs POST (EP.postIssue language) headers reqBody Nothing
    encodeRequest = standardEncode

derive instance genericChat :: Generic Chat _
instance showChat     :: Show Chat where show     = genericShow
instance standardChat :: StandardEncode Chat where standardEncode (Chat cht) = standardEncode cht
instance decodeChat   :: Decode Chat where decode = defaultDecode
instance encodeChat   :: Encode Chat where encode = defaultEncode

derive instance genericPostIssueReqBody :: Generic PostIssueReqBody _
derive instance newtypePostIssueReqBody :: Newtype PostIssueReqBody _
instance standardEncodePostIssueReqBody :: StandardEncode PostIssueReqBody where standardEncode (PostIssueReqBody reqBody) = standardEncode reqBody
instance showPostIssueReqBody :: Show PostIssueReqBody where show = genericShow
instance decodePostIssueReqBody :: Decode PostIssueReqBody where decode = defaultDecode
instance encodePostIssueReqBody :: Encode PostIssueReqBody where encode = defaultEncode

derive instance genericPostIssueReq :: Generic PostIssueReq _
instance showPostIssueReq     :: Show PostIssueReq where show     = genericShow
instance standardPostIssueReq :: StandardEncode PostIssueReq where standardEncode (PostIssueReq language (PostIssueReqBody req)) = standardEncode req
instance decodePostIssueReq   :: Decode PostIssueReq where decode = defaultDecode
instance encodePostIssueReq   :: Encode PostIssueReq where encode = defaultEncode

derive instance genericPostIssueRes :: Generic PostIssueRes _
instance showGetPostIssueRes        :: Show PostIssueRes where show     = genericShow
instance standardEncodePostIssueRes :: StandardEncode PostIssueRes where standardEncode (PostIssueRes res) = standardEncode res
instance decodePostIssueRes         :: Decode PostIssueRes where decode = defaultDecode
instance encodePostIssueRes         :: Encode PostIssueRes where encode = defaultEncode

--------------------------------------------------- IssueInfo ----------------------------------------------------
data IssueInfoReq = IssueInfoReq String String

newtype IssueInfoRes = IssueInfoRes
  { mediaFiles    :: Array { url :: String, _type :: String }
  , description   :: String
  , issueReportId :: String
  , categoryId    :: String
  , categoryLabel :: String
  , chats :: Array ChatDetail
  , options :: Array Option
  , issueReportShortId :: Maybe String
  }

newtype ChatDetail = ChatDetail
  { timestamp :: String,
    content :: Maybe String,
    title :: Maybe String,
    actionText :: Maybe String,
    id :: String,
    label :: Maybe String,
    chatType :: String,
    sender :: String
  }

instance makeIssueInfoReq :: RestEndpoint IssueInfoReq  where
    makeRequest reqBody@(IssueInfoReq issueId language) headers = defaultMakeRequestWithoutLogs GET (EP.issueInfo issueId language) headers reqBody Nothing
    encodeRequest = standardEncode

derive instance genericIssueInfoReq :: Generic IssueInfoReq _
instance showIssueInfoReq     :: Show IssueInfoReq where show     = genericShow
instance standardIssueInfoReq :: StandardEncode IssueInfoReq where standardEncode (IssueInfoReq _ _) = standardEncode {}
instance decodeIssueInfoReq   :: Decode IssueInfoReq where decode = defaultDecode
instance encodeIssueInfoReq   :: Encode IssueInfoReq where encode = defaultEncode

derive instance genericIssueInfoRes :: Generic IssueInfoRes _
instance showGetIssueInfoRes        :: Show IssueInfoRes where show     = genericShow
instance standardEncodeIssueInfoRes :: StandardEncode IssueInfoRes where standardEncode (IssueInfoRes res) = standardEncode res
instance decodeIssueInfoRes         :: Decode IssueInfoRes where decode = defaultDecode
instance encodeIssueInfoRes         :: Encode IssueInfoRes where encode = defaultEncode

derive instance genericChatDetail :: Generic ChatDetail _
instance showChatDetail     :: Show ChatDetail where show     = genericShow
instance standardChatDetail :: StandardEncode ChatDetail where standardEncode (ChatDetail chtDetail) = standardEncode chtDetail
instance decodeChatDetail   :: Decode ChatDetail where decode = defaultDecode
instance encodeChatDetail   :: Encode ChatDetail where encode = defaultEncode

--------------------------------------------------- updateIssue ----------------------------------------------------
data UpdateIssueReq = UpdateIssueReq String String UpdateIssueReqBody

newtype UpdateIssueReqBody = UpdateIssueReqBody { 
  status :: String
}

newtype UpdateIssueRes = UpdateIssueRes { 
  messages :: Array Message
}

instance makeUpdateIssueReq :: RestEndpoint UpdateIssueReq  where
    makeRequest reqBody@(UpdateIssueReq issueId language req) headers = defaultMakeRequestWithoutLogs PUT (EP.updateIssue language issueId) headers reqBody Nothing
    encodeRequest = standardEncode
  
derive instance genericUpdateIssueReqBody :: Generic UpdateIssueReqBody _
derive instance newtypeUpdateIssueReqBody :: Newtype UpdateIssueReqBody _
instance standardEncodeUpdateIssueReqBody :: StandardEncode UpdateIssueReqBody where standardEncode (UpdateIssueReqBody reqBody) = standardEncode reqBody
instance showUpdateIssueReqBody :: Show UpdateIssueReqBody where show = genericShow
instance decodeUpdateIssueReqBody :: Decode UpdateIssueReqBody where decode = defaultDecode
instance encodeUpdateIssueReqBody :: Encode UpdateIssueReqBody where encode = defaultEncode

derive instance genericUpdateIssueReq :: Generic UpdateIssueReq _
instance showUpdateIssueReq     :: Show UpdateIssueReq where show     = genericShow
instance standardUpdateIssueReq :: StandardEncode UpdateIssueReq where standardEncode (UpdateIssueReq _ _ req) = standardEncode req
instance decodeUpdateIssueReq   :: Decode UpdateIssueReq where decode = defaultDecode
instance encodeUpdateIssueReq   :: Encode UpdateIssueReq where encode = defaultEncode

derive instance genericUpdateIssueRes :: Generic UpdateIssueRes _
instance showUpdateIssueRes        :: Show UpdateIssueRes where show     = genericShow
instance standardEncodeUpdateIssueRes :: StandardEncode UpdateIssueRes where standardEncode (UpdateIssueRes res) = standardEncode res
instance decodeUpdateIssueRes         :: Decode UpdateIssueRes where decode = defaultDecode
instance encodeUpdateIssueRes         :: Encode UpdateIssueRes where encode = defaultEncode


-- =========================================== Safety API's ================================================================

----------------------------------------------------------------------- userGetEmergencySettings api -------------------------------------------------------------------

data GetEmergencySettingsReq = GetEmergencySettingsReq

newtype GetEmergencySettingsRes = GetEmergencySettingsRes
  {
    nightSafetyChecks :: Boolean,
    hasCompletedSafetySetup :: Boolean,
    defaultEmergencyNumbers :: Array ContactDetails,
    enablePoliceSupport :: Boolean,
    localPoliceNumber :: Maybe String,
    hasCompletedMockSafetyDrill :: Boolean,
    shareTripWithEmergencyContacts :: Maybe Boolean,
    shareTripWithEmergencyContactOption :: Maybe RideShareOptions,
    autoCallDefaultContact :: Boolean,
    enableOtpLessRide :: Maybe Boolean,
    enablePostRideSafetyCheck :: RideShareOptions,
    enableUnexpectedEventsCheck :: RideShareOptions,
    informPoliceSos :: Boolean,
    notifySafetyTeamForSafetyCheckFailure :: Boolean,
    notifySosWithEmergencyContacts :: Boolean,
    shakeToActivate :: Boolean,
    safetyCheckStartTime :: Maybe Int,
    safetyCheckEndTime :: Maybe Int
  }

newtype UpdateEmergencySettingsReq = UpdateEmergencySettingsReq
  {
    shareEmergencyContacts :: Maybe Boolean,
    shareTripWithEmergencyContactOption :: Maybe RideShareOptions,
    nightSafetyChecks :: Maybe Boolean,
    hasCompletedSafetySetup :: Maybe Boolean,
    autoCallDefaultContact :: Maybe Boolean,
    enableOtpLessRide :: Maybe Boolean,
    enablePostRideSafetyCheck :: Maybe RideShareOptions,
    enableUnexpectedEventsCheck :: Maybe RideShareOptions,
    hasCompletedMockSafetyDrill :: Maybe Boolean,
    informPoliceSos :: Maybe Boolean,
    notifySafetyTeamForSafetyCheckFailure :: Maybe Boolean,
    notifySosWithEmergencyContacts :: Maybe Boolean,
    shakeToActivate :: Maybe Boolean,
    shareTripWithEmergencyContacts :: Maybe Boolean
  }

data RideShareOptions = ALWAYS_SHARE | SHARE_WITH_TIME_CONSTRAINTS | NEVER_SHARE

derive instance genericRideShareOptions :: Generic RideShareOptions _
instance showRideShareOptions :: Show RideShareOptions where show = genericShow
instance decodeRideShareOptions :: Decode RideShareOptions where decode = defaultEnumDecode
instance encodeRideShareOptions :: Encode RideShareOptions where encode = defaultEnumEncode
instance eqRideShareOptions :: Eq RideShareOptions where eq = genericEq
instance standardEncodeRideShareOptions :: StandardEncode RideShareOptions
  where
    standardEncode _ = standardEncode {}

instance makeGetEmergencySettingsReq :: RestEndpoint GetEmergencySettingsReq  where
  makeRequest reqBody headers = defaultMakeRequestWithoutLogs GET (EP.getEmergencySettings "") headers reqBody Nothing
  encodeRequest req = encode req

instance makeUpdateEmergencySettingsReq :: RestEndpoint UpdateEmergencySettingsReq  where
  makeRequest reqBody headers = defaultMakeRequestWithoutLogs PUT (EP.updateEmergencySettings "") headers reqBody Nothing
  encodeRequest req = encode req

derive instance genericGetEmergencySettingsRes :: Generic GetEmergencySettingsRes _
derive instance newtypeGetEmergencySettingsRes :: Newtype GetEmergencySettingsRes _
instance standardEncodeGetEmergencySettingsRes :: StandardEncode GetEmergencySettingsRes where standardEncode (GetEmergencySettingsRes body) = standardEncode body
instance showGetEmergencySettingsRes :: Show GetEmergencySettingsRes where show = genericShow
instance decodeGetEmergencySettingsRes :: Decode GetEmergencySettingsRes where decode = defaultDecode
instance encodeGetEmergencySettingsRes  :: Encode GetEmergencySettingsRes where encode = defaultEncode


derive instance genericGetEmergencySettingsReq :: Generic GetEmergencySettingsReq _
instance standardEncodeGetEmergencySettingsReq :: StandardEncode GetEmergencySettingsReq where standardEncode (GetEmergencySettingsReq) = standardEncode {}
instance showGetEmergencySettingsReq :: Show GetEmergencySettingsReq where show = genericShow
instance decodeGetEmergencySettingsReq :: Decode GetEmergencySettingsReq where decode = defaultDecode
instance encodeGetEmergencySettingsReq  :: Encode GetEmergencySettingsReq where encode = defaultEncode

derive instance genericUpdateEmergencySettingsReq :: Generic UpdateEmergencySettingsReq _
derive instance newtypeUpdateEmergencySettingsReq :: Newtype UpdateEmergencySettingsReq _
instance standardEncodeUpdateEmergencySettingsReq :: StandardEncode UpdateEmergencySettingsReq where standardEncode (UpdateEmergencySettingsReq body) = standardEncode body
instance showUpdateEmergencySettingsReq :: Show UpdateEmergencySettingsReq where show = genericShow
instance decodeUpdateEmergencySettingsReq :: Decode UpdateEmergencySettingsReq where decode = defaultDecode
instance encodeUpdateEmergencySettingsReq  :: Encode UpdateEmergencySettingsReq where encode = defaultEncode


----------------------------------------------------------------------- getSosDetails api -------------------------------------------------------------------

data GetSosDetailsReq = GetSosDetailsReq String

newtype GetSosDetailsRes = GetSosDetailsRes
  {
    sos :: Maybe Sos
  }

data SosFlow = Police | CustomerCare | EmergencyContact String | SafetyFlow

derive instance genericSosFlow :: Generic SosFlow _
instance showSosFlow :: Show SosFlow where show = genericShow
instance decodeSosFlow :: Decode SosFlow where decode = defaultDecode
instance encodeSosFlow :: Encode SosFlow where encode = defaultEncode
instance eqSosFlow :: Eq SosFlow where eq = genericEq
instance standardEncodeSosFlow :: StandardEncode SosFlow
  where
    standardEncode Police = standardEncode {}
    standardEncode CustomerCare = standardEncode {}
    standardEncode (EmergencyContact param) = standardEncode param
    standardEncode SafetyFlow = standardEncode {}

newtype Sos = Sos 
  {
    flow :: SosFlow,
    id :: String,
    status :: CTA.SosStatus
  }

instance makeGetSosDetailsReq :: RestEndpoint GetSosDetailsReq  where
 makeRequest reqBody@(GetSosDetailsReq rideId) headers = defaultMakeRequestWithoutLogs GET (EP.getSosDetails rideId) headers reqBody Nothing
 encodeRequest req = standardEncode req

derive instance genericGetSosDetailsReq :: Generic GetSosDetailsReq _
instance standardEncodeGetSosDetailsReq :: StandardEncode GetSosDetailsReq where standardEncode (GetSosDetailsReq rideId) = standardEncode rideId
instance showGetSosDetailsReq :: Show GetSosDetailsReq where show = genericShow
instance decodeGetSosDetailsReq :: Decode GetSosDetailsReq where decode = defaultDecode
instance encodeGetSosDetailsReq :: Encode GetSosDetailsReq where encode = defaultEncode

derive instance genericGetSosDetailsRes :: Generic GetSosDetailsRes _
derive instance newtypeGetSosDetailsRes :: Newtype GetSosDetailsRes _
instance standardEncodeGetSosDetailsRes :: StandardEncode GetSosDetailsRes where standardEncode (GetSosDetailsRes res) = standardEncode res
instance showGetSosDetailsRes :: Show GetSosDetailsRes where show = genericShow
instance decodeGetSosDetailsRes :: Decode GetSosDetailsRes where decode = defaultDecode
instance encodeGetSosDetailsRes :: Encode GetSosDetailsRes where encode = defaultEncode

derive instance genericSos :: Generic Sos _
derive instance newtypeSos :: Newtype Sos _
instance standardEncodeSos :: StandardEncode Sos where standardEncode (Sos res) = standardEncode res
instance showSos :: Show Sos where show = genericShow
instance decodeSos :: Decode Sos where decode = defaultDecode
instance encodeSos :: Encode Sos where encode = defaultEncode

data UpdateAsSafeReq = UpdateAsSafeReq String UpdateAsSafeReqBody

newtype UpdateAsSafeReqBody = UpdateAsSafeReqBody {
  isMock :: Boolean,
  isRideEnded :: Boolean
}


instance makeUpdateAsSafeReq  :: RestEndpoint UpdateAsSafeReq  where
 makeRequest reqBody@(UpdateAsSafeReq sosId req) headers = defaultMakeRequestWithoutLogs POST (EP.updateSafeRide sosId) headers reqBody Nothing
 encodeRequest req = standardEncode req

derive instance genericUpdateAsSafeReq :: Generic UpdateAsSafeReq _
instance showUpdateAsSafeReq :: Show UpdateAsSafeReq where show = genericShow
instance standardEncodeUpdateAsSafeReq :: StandardEncode UpdateAsSafeReq where standardEncode (UpdateAsSafeReq _ req) = standardEncode req
instance decodeUpdateAsSafeReq :: Decode UpdateAsSafeReq where decode = defaultDecode
instance encodeUpdateAsSafeReq :: Encode UpdateAsSafeReq where encode = defaultEncode


derive instance genericUpdateAsSafeReqBody :: Generic UpdateAsSafeReqBody _
derive instance newtypeUpdateAsSafeReqBody :: Newtype UpdateAsSafeReqBody _
instance standardEncodeUpdateAsSafeReqBody :: StandardEncode UpdateAsSafeReqBody where standardEncode (UpdateAsSafeReqBody req) = standardEncode req
instance showUpdateAsSafeReqBody :: Show UpdateAsSafeReqBody where show = genericShow
instance decodeUpdateAsSafeReqBody :: Decode UpdateAsSafeReqBody where decode = defaultDecode
instance encodeUpdateAsSafeReqBody :: Encode UpdateAsSafeReqBody where encode = defaultEncode


newtype AskSupportReq = AskSupportReq {
    bookingId :: String,
    isSafe :: Boolean,
    description :: String
  }

instance makeAskSupportRequest :: RestEndpoint AskSupportReq  where
    makeRequest reqBody headers = defaultMakeRequestWithoutLogs POST (EP.safetySupport "") headers reqBody Nothing
    encodeRequest req = defaultEncode req


derive instance genericAskSupportReq :: Generic AskSupportReq _
derive instance newtypeAskSupportReq :: Newtype AskSupportReq _
instance standardEncodeAskSupportReq :: StandardEncode AskSupportReq where standardEncode (AskSupportReq req) = standardEncode req
instance showAskSupportReq :: Show AskSupportReq where show = genericShow
instance decodeAskSupportReq :: Decode AskSupportReq where decode = defaultDecode
instance encodeAskSupportReq :: Encode AskSupportReq where encode = defaultEncode

newtype CreateMockSosReq = CreateMockSosReq {
  onRide :: Boolean,
  startDrill :: Boolean
}


instance makeCreateMockSosReq  :: RestEndpoint CreateMockSosReq  where
 makeRequest reqBody headers = defaultMakeRequestWithoutLogs POST (EP.createMockSos "") headers reqBody Nothing
 encodeRequest req = standardEncode req

derive instance genericCreateMockSosReq :: Generic CreateMockSosReq _
instance showCreateMockSosReq :: Show CreateMockSosReq where show = genericShow
derive instance newtypeCreateMockSosReq :: Newtype CreateMockSosReq _
instance standardEncodeCreateMockSosReq :: StandardEncode CreateMockSosReq where standardEncode (CreateMockSosReq req) = standardEncode req
instance decodeCreateMockSosReq :: Decode CreateMockSosReq where decode = defaultDecode
instance encodeCreateMockSosReq :: Encode CreateMockSosReq where encode = defaultEncode



newtype ShareRideReq = ShareRideReq {
    emergencyContactNumbers :: Array String
  }


instance makeShareRideRequest :: RestEndpoint ShareRideReq  where
    makeRequest reqBody headers = defaultMakeRequestWithoutLogs POST (EP.shareRide "") headers reqBody Nothing
    encodeRequest req = defaultEncode req


derive instance genericShareRideReq :: Generic ShareRideReq _
derive instance newtypeShareRideReq :: Newtype ShareRideReq _
instance standardEncodeShareRideReq :: StandardEncode ShareRideReq where standardEncode (ShareRideReq req) = standardEncode req
instance showShareRideReq :: Show ShareRideReq where show = genericShow
instance decodeShareRideReq :: Decode ShareRideReq where decode = defaultDecode
instance encodeShareRideReq :: Encode ShareRideReq where encode = defaultEncode
--------------------------------------------------- FollowRide ----------------------------------------------------

data FollowRideReq = FollowRideReq

newtype FollowRideRes = FollowRideRes (Array Followers)

newtype Followers = Followers {
  bookingId :: String,
  mobileNumber :: String,
  name :: Maybe String,
  priority :: Int,
  personId :: Maybe String
}

instance makeFollowRideReq :: RestEndpoint FollowRideReq  where
  makeRequest reqBody headers = defaultMakeRequestWithoutLogs GET (EP.followRide "") headers reqBody Nothing
  encodeRequest req = standardEncode req

derive instance genericFollowRideReq :: Generic FollowRideReq _
instance standardEncodeFollowRideReq :: StandardEncode FollowRideReq where standardEncode _ = standardEncode {}
instance showFollowRideReq :: Show FollowRideReq where show = genericShow
instance decodeFollowRideReq :: Decode FollowRideReq where decode = defaultDecode
instance encodeFollowRideReq :: Encode FollowRideReq where encode = defaultEncode

derive instance genericFollowRideRes :: Generic FollowRideRes _
derive instance newtypeFollowRideRes :: Newtype FollowRideRes _
instance standardEncodeFollowRideRes :: StandardEncode FollowRideRes where standardEncode _ = standardEncode {}
instance showFollowRideRes :: Show FollowRideRes where show = genericShow
instance decodeFollowRideRes :: Decode FollowRideRes where decode = defaultDecode
instance encodeFollowRideRes :: Encode FollowRideRes where encode = defaultEncode

derive instance genericFollowers :: Generic Followers _
derive instance newtypeFollowers :: Newtype Followers _
instance standardEncodeFollowers :: StandardEncode Followers where standardEncode _ = standardEncode {}
instance showFollowers :: Show Followers where show = genericShow
instance decodeFollowers :: Decode Followers where decode = defaultDecode
instance encodeFollowers :: Encode Followers where encode = defaultEncode


--------------------------------------------------- GetMetroStation ----------------------------------------------------

data GetMetroStationReq = GetMetroStationReq String String String String String

newtype GetMetroStationResponse = GetMetroStationResponse (Array FRFSStationAPI)

newtype FRFSStationAPI = FRFSStationAPI { 
    address :: Maybe String,
    code :: String,
    color :: Maybe String,
    distance :: Maybe Int,
    lat :: Maybe Number,
    lon :: Maybe Number,
    name :: String,
    sequenceNum :: Maybe Int,
    stationType :: Maybe String
}

derive instance genericGetMetroStationResponse :: Generic GetMetroStationResponse _
derive instance newtypeGetMetroStationResponse :: Newtype GetMetroStationResponse _
instance eqGetMetroStationResponse :: Eq GetMetroStationResponse where eq = genericEq
instance showGetMetroStationResponse :: Show GetMetroStationResponse where show = genericShow
instance decodeGetMetroStationResponse :: Decode GetMetroStationResponse where decode = defaultDecode

data StationType = START | END | TRANSIT | INTERMEDIATE

instance makeGetMetroStationReq :: RestEndpoint GetMetroStationReq  where
    makeRequest reqBody@(GetMetroStationReq vehicleType city routeCode endStationCode location) headers = defaultMakeRequestWithoutLogs GET (EP.getMetroStations vehicleType city routeCode endStationCode location) headers reqBody Nothing
    encodeRequest = standardEncode

derive instance genericGetMetroStationReq :: Generic GetMetroStationReq _
instance showGetMetroStationReq     :: Show GetMetroStationReq where show     = genericShow
instance standardGetMetroStationReq :: StandardEncode GetMetroStationReq where standardEncode (GetMetroStationReq _ _ _ _ _) = standardEncode {}
instance decodeGetMetroStationReq   :: Decode GetMetroStationReq where decode = defaultDecode
instance encodeGetMetroStationReq   :: Encode GetMetroStationReq where encode = defaultEncode

derive instance genericGetMetroStationResp :: Generic FRFSStationAPI _
instance showGetMetroStationResp        :: Show FRFSStationAPI where show     = genericShow
instance standardEncodeGetMetroStationResp :: StandardEncode FRFSStationAPI where standardEncode (FRFSStationAPI res) = standardEncode res
instance eqGetMetroStationResp        :: Eq FRFSStationAPI where eq     = genericEq
instance decodeGetMetroStationResp         :: Decode FRFSStationAPI where decode = defaultDecode
instance encodeGetMetroStationResp         :: Encode FRFSStationAPI where encode = defaultEncode

derive instance genericStationType :: Generic StationType _
instance showStationType :: Show StationType where show = genericShow
instance decodeStationType :: Decode StationType where decode = defaultDecode
instance encodeStationType :: Encode StationType where encode = defaultEncode
instance eqStationType :: Eq StationType where eq = genericEq
instance standardEncodeStationType :: StandardEncode StationType
  where
  standardEncode START = standardEncode $ show START
  standardEncode END = standardEncode $ show END
  standardEncode TRANSIT = standardEncode $ show TRANSIT
  standardEncode INTERMEDIATE = standardEncode $ show INTERMEDIATE

----------------------------------------------------GetBusRoute----------------------------------------------------

data GetBusRoutesReq = GetBusRoutesReq String String String

newtype GetBusRoutesResponse = GetBusRoutesResponse (Array FRFSRouteAPI)


derive instance genericGetBusRoutesResponse :: Generic GetBusRoutesResponse _
derive instance newtypeGetBusRoutesResponse :: Newtype GetBusRoutesResponse _
instance showGetBusRoutesResponse :: Show GetBusRoutesResponse where show = genericShow
instance eqGetBusRouteResp :: Eq FRFSRouteAPI where eq = genericEq
instance decodeGetBusRoutesResponse :: Decode GetBusRoutesResponse where decode = defaultDecode

instance makeGetBusRoutesReq :: RestEndpoint GetBusRoutesReq  where
    makeRequest reqBody@(GetBusRoutesReq city startStationCode endStationCode) headers = defaultMakeRequestWithoutLogs GET (EP.getBusRoutes city startStationCode endStationCode) headers reqBody Nothing
    encodeRequest = standardEncode

derive instance genericGetBusRoutesReq :: Generic GetBusRoutesReq _
instance showGetBusRoutesReq     :: Show GetBusRoutesReq where show     = genericShow
instance standardGetBusRoutesReq :: StandardEncode GetBusRoutesReq where standardEncode (GetBusRoutesReq _ _ _) = standardEncode {}
instance decodeGetBusRoutesReq   :: Decode GetBusRoutesReq where decode = defaultDecode
instance encodeGetBusRoutesReq   :: Encode GetBusRoutesReq where encode = defaultEncode

--------------------------------------------------- frfsSearch ----------------------------------------------------
data FrfsSearchRequest = FrfsSearchRequest FRFSSearchAPIReq String

newtype FRFSSearchAPIReq = FRFSSearchAPIReq {
  fromStationCode :: String,
  toStationCode :: String,
  quantity :: Int,
  routeCode :: Maybe String
}

newtype FrfsSearchResp = FrfsSearchResp {
  searchId :: String
}

instance makeSearchMetroRequest :: RestEndpoint FrfsSearchRequest where
  makeRequest reqBody@(FrfsSearchRequest rqBody vehicleType) headers = defaultMakeRequestWithoutLogs POST (EP.frfsSearch vehicleType) headers reqBody Nothing  
  encodeRequest (FrfsSearchRequest rqBody vehicleType) = standardEncode rqBody



derive instance genericSearchMetroReq :: Generic FRFSSearchAPIReq _
derive instance newtypeSearchMetroReq :: Newtype FRFSSearchAPIReq _
instance standardEncodeSearchMetroReq :: StandardEncode FRFSSearchAPIReq where standardEncode (FRFSSearchAPIReq payload) = standardEncode payload
instance showSearchMetroReq :: Show FRFSSearchAPIReq where show = genericShow
instance decodeSearchMetroReq :: Decode FRFSSearchAPIReq where decode = defaultDecode
instance encodeSearchMetroReq :: Encode FRFSSearchAPIReq where encode = defaultEncode


derive instance genericSearchMetroResp :: Generic FrfsSearchResp _
derive instance newtypeSearchMetroResp :: Newtype FrfsSearchResp _
instance standardEncodeSearchMetroResp :: StandardEncode FrfsSearchResp where standardEncode (FrfsSearchResp id) = standardEncode id
instance showSearchMetroResp :: Show FrfsSearchResp where show = genericShow
instance decodeSearchMetroResp :: Decode FrfsSearchResp where decode = defaultDecode
instance encodeSearchMetroResp :: Encode FrfsSearchResp where encode = defaultEncode

derive instance genericSearchMetroRequest :: Generic FrfsSearchRequest _ 
instance standardEncodeSearchMetroRequest :: StandardEncode FrfsSearchRequest where standardEncode (FrfsSearchRequest req id) = standardEncode req
instance decodeSearchMetroRequest :: Decode  FrfsSearchRequest where decode = defaultDecode
instance encodeSearchMetroRequest :: Encode FrfsSearchRequest where encode = defaultEncode

--------------------------------------------------- getMetroQuote ----------------------------------------------------

data FrfsQuotesReq = FrfsQuotesReq String

newtype FrfsQuotesRes = FrfsQuotesRes (Array FrfsQuote)

newtype FrfsQuote = FrfsQuote {
  quoteId :: String
  , _type :: String--FRFSQuoteType
  , vehicleType :: String--FRFSVehicleType
  , quantity :: Int
  , price :: Number
  , stations :: Array FRFSStationAPI 
  , validTill :: String
  , discountedTickets :: Maybe Int
  , eventDiscountAmount :: Maybe Number
  , routeStations :: Maybe (Array FRFSRouteAPI)
  , discounts :: Maybe (Array FRFSDiscountRes)
}

-- For understanding and debugging types 
-- FRFSQuoteType = SingleJourney | ReturnJourney | Pass
-- FRFSVehicleType = Metro_ | Bus

derive instance genericGetMetroQuotesReq :: Generic FrfsQuotesReq _
instance standardEncodeGetMetroQuotesReq :: StandardEncode FrfsQuotesReq where standardEncode (FrfsQuotesReq body) = standardEncode body
instance showGetMetroQuotesReq :: Show FrfsQuotesReq where show = genericShow
instance decodeGetMetroQuotesReq :: Decode FrfsQuotesReq where decode = defaultDecode
instance encodeGetMetroQuotesReq  :: Encode FrfsQuotesReq where encode = defaultEncode

derive instance genericGetMetroQuotesRes :: Generic FrfsQuotesRes _
derive instance newtypeGetMetroQuotesRes :: Newtype FrfsQuotesRes _
instance standardEncodeGetMetroQuotesRes :: StandardEncode FrfsQuotesRes where standardEncode (FrfsQuotesRes body) = standardEncode body
instance showGetMetroQuotesRes :: Show FrfsQuotesRes where show = genericShow
instance decodeGetMetroQuotesRes :: Decode FrfsQuotesRes where decode = defaultDecode
instance encodeGetMetroQuotesRes :: Encode FrfsQuotesRes where encode = defaultEncode

derive instance genericMetroQuote :: Generic FrfsQuote _
derive instance newtypeMetroQuote :: Newtype FrfsQuote _
instance standardEncodeMetroQuote :: StandardEncode FrfsQuote where standardEncode (FrfsQuote body) = standardEncode body
instance showMetroQuote :: Show FrfsQuote where show = genericShow
instance decodeMetroQuote :: Decode FrfsQuote where decode = defaultDecode
instance encodeMetroQuote :: Encode FrfsQuote where encode = defaultEncode

instance getMetroQuotesReq :: RestEndpoint FrfsQuotesReq  where
 makeRequest reqBody@(FrfsQuotesReq id) headers = defaultMakeRequestWithoutLogs GET (EP.frfsQuotes id) headers reqBody Nothing
 encodeRequest req = standardEncode req

 --------------------------------------------------- confirmMetroQuote ----------------------------------------------------

newtype ConfirmMetroQuoteReq = ConfirmMetroQuoteReq String


newtype ConfirmMetroQuoteResp = ConfirmMetroQuoteResp FRFSTicketBookingStatusAPIRes

newtype FRFSTicketBookingStatusAPIRes = FRFSTicketBookingStatusAPIRes {
  bookingId :: String
  , city :: String
  , updatedAt :: String
  , status :: String --FRFSTicketBookingStatus
  , _type :: String--FRFSQuoteType
  , quantity :: Int
  , vehicleType :: String--FRFSVehicleType
  , price :: Number
  , validTill :: String
  , payment :: Maybe FRFSBookingPaymentAPI
  , tickets :: Array FRFSTicketAPI
  , stations :: Array FRFSStationAPI
  , createdAt :: String
  , routeStations :: Maybe (Array FRFSRouteAPI)
}

newtype FRFSBookingPaymentAPI = FRFSBookingPaymentAPI {
    status :: String
  , paymentOrder :: Maybe CreateOrderRes
  , transactionId :: Maybe String
}

newtype FRFSTicketAPI = FRFSTicketAPI {
    status :: String
  , qrData :: String
  , validTill :: String
  , ticketNumber :: String
  }

instance makeConfirmMetroQuoteReq :: RestEndpoint ConfirmMetroQuoteReq  where
 makeRequest reqBody@(ConfirmMetroQuoteReq quoteId) headers = defaultMakeRequestWithoutLogs POST (EP.confirmMetroQuote quoteId) headers reqBody Nothing
 encodeRequest req = standardEncode req

derive instance genericConfirmMetroQuoteResp :: Generic ConfirmMetroQuoteResp _
derive instance newtypeConfirmMetroQuoteResp :: Newtype ConfirmMetroQuoteResp _
instance standardEncodeConfirmMetroQuoteResp :: StandardEncode ConfirmMetroQuoteResp where standardEncode (ConfirmMetroQuoteResp body) = standardEncode body
instance showConfirmMetroQuoteResp :: Show ConfirmMetroQuoteResp where show = genericShow
instance decodeConfirmMetroQuoteResp :: Decode ConfirmMetroQuoteResp where decode = defaultDecode
instance encodeConfirmMetroQuoteResp  :: Encode ConfirmMetroQuoteResp where encode = defaultEncode

derive instance genericMetroTicketBookingStatus :: Generic FRFSTicketBookingStatusAPIRes _
derive instance newtypeMetroTicketBookingStatus :: Newtype FRFSTicketBookingStatusAPIRes _
instance standardEncodeMetroTicketBookingStatus :: StandardEncode FRFSTicketBookingStatusAPIRes where standardEncode (FRFSTicketBookingStatusAPIRes body) = standardEncode body
instance showMetroTicketBookingStatus :: Show FRFSTicketBookingStatusAPIRes where show = genericShow
instance decodeMetroTicketBookingStatus :: Decode FRFSTicketBookingStatusAPIRes where decode = defaultDecode
instance encodeMetroTicketBookingStatus :: Encode FRFSTicketBookingStatusAPIRes where encode = defaultEncode

derive instance genericConfirmMetroQuoteReq :: Generic ConfirmMetroQuoteReq _
instance standardEncodeConfirmMetroQuoteReq :: StandardEncode ConfirmMetroQuoteReq where standardEncode (ConfirmMetroQuoteReq quoteId) = standardEncode quoteId
instance showConfirmMetroQuoteReq :: Show ConfirmMetroQuoteReq where show = genericShow
instance decodeConfirmMetroQuoteReq :: Decode ConfirmMetroQuoteReq where decode = defaultDecode
instance encodeConfirmMetroQuoteReq  :: Encode ConfirmMetroQuoteReq where encode = defaultEncode

derive instance genericFRFSBookingPaymentAPI :: Generic FRFSBookingPaymentAPI _
derive instance newtypeFRFSBookingPaymentAPI :: Newtype FRFSBookingPaymentAPI _
instance standardEncodeFRFSBookingPaymentAPI :: StandardEncode FRFSBookingPaymentAPI where standardEncode (FRFSBookingPaymentAPI body) = standardEncode body
instance showFRFSBookingPaymentAPI :: Show FRFSBookingPaymentAPI where show = genericShow
instance decodeFRFSBookingPaymentAPI :: Decode FRFSBookingPaymentAPI where decode = defaultDecode
instance encodeFRFSBookingPaymentAPI :: Encode FRFSBookingPaymentAPI where encode = defaultEncode

derive instance genericFRFSTicketAPI :: Generic FRFSTicketAPI _
derive instance newtypeFRFSTicketAPI :: Newtype FRFSTicketAPI _
instance standardEncodeFRFSTicketAPI :: StandardEncode FRFSTicketAPI where standardEncode (FRFSTicketAPI body) = standardEncode body
instance showFRFSTicketAPI :: Show FRFSTicketAPI where show = genericShow
instance decodeFRFSTicketAPI :: Decode FRFSTicketAPI where decode = defaultDecode
instance encodeFRFSTicketAPI :: Encode FRFSTicketAPI where encode = defaultEncode

 --------------------------------------------------- getMetroBookingStatus ----------------------------------------------------

data GetMetroBookingStatusReq = GetMetroBookingStatusReq String

newtype GetMetroBookingStatusResp = GetMetroBookingStatusResp FRFSTicketBookingStatusAPIRes

instance makeGetMetroBookingStatusReq :: RestEndpoint GetMetroBookingStatusReq  where
 makeRequest reqBody@(GetMetroBookingStatusReq bookingId) headers = defaultMakeRequestWithoutLogs GET (EP.getMetroBookingStatus bookingId) headers reqBody Nothing
 encodeRequest req = standardEncode req

derive instance genericGetMetroBookingStatusReq :: Generic GetMetroBookingStatusReq _
instance standardEncodeGetMetroBookingStatusReq :: StandardEncode GetMetroBookingStatusReq where standardEncode (GetMetroBookingStatusReq bookingId) = standardEncode bookingId
instance showGetMetroBookingStatusReq :: Show GetMetroBookingStatusReq where show = genericShow
instance decodeGetMetroBookingStatusReq :: Decode GetMetroBookingStatusReq where decode = defaultDecode
instance encodeGetMetroBookingStatusReq  :: Encode GetMetroBookingStatusReq where encode = defaultEncode

derive instance genericGetMetroBookingStatusResp :: Generic GetMetroBookingStatusResp _
derive instance newtypeGetMetroBookingStatusResp :: Newtype GetMetroBookingStatusResp _
instance standardEncodeGetMetroBookingStatusResp :: StandardEncode GetMetroBookingStatusResp where standardEncode (GetMetroBookingStatusResp body) = standardEncode body
instance showGetMetroBookingStatusResp :: Show GetMetroBookingStatusResp where show = genericShow
instance decodeGetMetroBookingStatusResp :: Decode GetMetroBookingStatusResp where decode = defaultDecode
instance encodeGetMetroBookingStatusResp  :: Encode GetMetroBookingStatusResp where encode = defaultEncode

 --------------------------------------------------- getMetroBookingList ----------------------------------------------------

data GetMetroBookingListReq = GetMetroBookingListReq String (Maybe String) (Maybe String)

newtype GetMetroBookingListResp = GetMetroBookingListResp (Array FRFSTicketBookingStatusAPIRes)

instance makeGetMetroBookingListReq :: RestEndpoint GetMetroBookingListReq  where
    makeRequest reqBody@(GetMetroBookingListReq vehicleType limit offset)headers = defaultMakeRequestWithoutLogs GET (EP.getMetroBookingList vehicleType limit offset) headers reqBody Nothing
    encodeRequest = standardEncode

derive instance genericGetMetroBookingListReq :: Generic GetMetroBookingListReq _
instance showGetMetroBookingListReq     :: Show GetMetroBookingListReq where show     = genericShow
instance standardGetMetroBookingListReq :: StandardEncode GetMetroBookingListReq where standardEncode (GetMetroBookingListReq _ _ _) = standardEncode {}
instance decodeGetMetroBookingListReq   :: Decode GetMetroBookingListReq where decode = defaultDecode
instance encodeGetMetroBookingListReq   :: Encode GetMetroBookingListReq where encode = defaultEncode

derive instance genericGetMetroBookingListResp :: Generic GetMetroBookingListResp _
derive instance newtypeGetMetroBookingListResp :: Newtype GetMetroBookingListResp _
instance standardEncodeGetMetroBookingListResp :: StandardEncode GetMetroBookingListResp where standardEncode (GetMetroBookingListResp body) = standardEncode body
instance showGetMetroBookingListResp :: Show GetMetroBookingListResp where show = genericShow
instance decodeGetMetroBookingListResp :: Decode GetMetroBookingListResp where decode = defaultDecode
instance encodeGetMetroBookingListResp  :: Encode GetMetroBookingListResp where encode = defaultEncode

--------------------------------------------------- retryMetrTicketPayment ----------------------------------------------------

data RetryMetrTicketPaymentReq = RetryMetrTicketPaymentReq String

newtype RetryMetrTicketPaymentResp = RetryMetrTicketPaymentResp FRFSTicketBookingStatusAPIRes


instance makeRetryMetrTicketPaymentReq :: RestEndpoint RetryMetrTicketPaymentReq  where
 makeRequest reqBody@(RetryMetrTicketPaymentReq quoteId) headers = defaultMakeRequestWithoutLogs POST (EP.retryMetrTicketPayment quoteId) headers reqBody Nothing
 encodeRequest req = standardEncode req

derive instance genericRetryMetrTicketPaymentReq :: Generic RetryMetrTicketPaymentReq _
instance standardEncodeRetryMetrTicketPaymentReq :: StandardEncode RetryMetrTicketPaymentReq where standardEncode (RetryMetrTicketPaymentReq quoteId) = standardEncode quoteId
instance showRetryMetrTicketPaymentReq :: Show RetryMetrTicketPaymentReq where show = genericShow
instance decodeRetryMetrTicketPaymentReq :: Decode RetryMetrTicketPaymentReq where decode = defaultDecode
instance encodeRetryMetrTicketPaymentReq  :: Encode RetryMetrTicketPaymentReq where encode = defaultEncode

derive instance genericRetryMetrTicketPaymentResp :: Generic RetryMetrTicketPaymentResp _
derive instance newtypeRetryMetrTicketPaymentResp :: Newtype RetryMetrTicketPaymentResp _
instance standardEncodeRetryMetrTicketPaymentResp :: StandardEncode RetryMetrTicketPaymentResp where standardEncode (RetryMetrTicketPaymentResp body) = standardEncode body
instance showRetryMetrTicketPaymentResp :: Show RetryMetrTicketPaymentResp where show = genericShow
instance decodeRetryMetrTicketPaymentResp :: Decode RetryMetrTicketPaymentResp where decode = defaultDecode
instance encodeRetryMetrTicketPaymentResp  :: Encode RetryMetrTicketPaymentResp where encode = defaultEncode

----------------------------------------------------- softCancelMetroBooking ---------------------------------------------------

data MetroBookingSoftCancelReq = MetroBookingSoftCancelReq String

instance metroBookingSoftCancelReq :: RestEndpoint MetroBookingSoftCancelReq  where
 makeRequest reqBody@(MetroBookingSoftCancelReq bookingId) headers = defaultMakeRequestWithoutLogs POST (EP.metroBookingSoftCancel bookingId) headers reqBody Nothing
 encodeRequest = standardEncode

derive instance genericMetroBookingSoftCancelReq :: Generic MetroBookingSoftCancelReq _
instance showMetroBookingSoftCancelReq     :: Show MetroBookingSoftCancelReq where show     = genericShow
instance standardMetroBookingSoftCancelReq :: StandardEncode MetroBookingSoftCancelReq where standardEncode (MetroBookingSoftCancelReq _) = standardEncode {}
instance decodeMetroBookingSoftCancelReq   :: Decode MetroBookingSoftCancelReq where decode = defaultDecode
instance encodeMetroBookingSoftCancelReq   :: Encode MetroBookingSoftCancelReq where encode = defaultEncode

----------------------------------------------------- softCancelMetroBookingStatus ---------------------------------------------------

data MetroBookingSoftCancelStatusReq = MetroBookingSoftCancelStatusReq String

newtype MetroBookingSoftCancelStatusResp = MetroBookingSoftCancelStatusResp 
  { cancellationCharges :: Maybe Number,
    isCancellable :: Maybe Boolean,
    refundAmount :: Maybe Number
  }

derive instance genericMetroBookingSoftCancelStatusResp :: Generic MetroBookingSoftCancelStatusResp _
derive instance newtypeMetroBookingSoftCancelStatusResp :: Newtype MetroBookingSoftCancelStatusResp _
instance showMetroBookingSoftCancelStatusResp :: Show MetroBookingSoftCancelStatusResp where show = genericShow
instance decodeMetroBookingSoftCancelStatusResp :: Decode MetroBookingSoftCancelStatusResp where decode = defaultDecode
instance encodeMetroBookingSoftCancelStatusResp :: Encode MetroBookingSoftCancelStatusResp where encode = defaultEncode

instance metroBookingSoftCancelStatusReq :: RestEndpoint MetroBookingSoftCancelStatusReq  where
 makeRequest reqBody@(MetroBookingSoftCancelStatusReq bookingId) headers = defaultMakeRequestWithoutLogs GET (EP.getMetroBookingSoftCancelStatus bookingId) headers reqBody Nothing
 encodeRequest = standardEncode

derive instance genericMetroBookingSoftCancelStatusReq :: Generic MetroBookingSoftCancelStatusReq _
instance showMetroBookingSoftCancelStatusReq     :: Show MetroBookingSoftCancelStatusReq where show     = genericShow
instance standardMetroBookingSoftCancelStatusReq :: StandardEncode MetroBookingSoftCancelStatusReq where standardEncode (MetroBookingSoftCancelStatusReq _) = standardEncode {}
instance decodeMetroBookingSoftCancelStatusReq   :: Decode MetroBookingSoftCancelStatusReq where decode = defaultDecode
instance encodeMetroBookingSoftCancelStatusReq   :: Encode MetroBookingSoftCancelStatusReq where encode = defaultEncode

------------------------------------------------------ hardCancelMetroBooking --------------------------------------------------

data MetroBookingHardCancelReq = MetroBookingHardCancelReq String

instance metroBookingHardCancelReq :: RestEndpoint MetroBookingHardCancelReq  where
 makeRequest reqBody@(MetroBookingHardCancelReq bookingId) headers = defaultMakeRequestWithoutLogs POST (EP.metroBookingHardCancel bookingId) headers reqBody Nothing
 encodeRequest = standardEncode

derive instance genericMetroBookingHardCancelReq :: Generic MetroBookingHardCancelReq _
instance showMetroBookingHardCancelReq     :: Show MetroBookingHardCancelReq where show     = genericShow
instance standardMetroBookingHardCancelReq :: StandardEncode MetroBookingHardCancelReq where standardEncode (MetroBookingHardCancelReq _) = standardEncode {}
instance decodeMetroBookingHardCancelReq   :: Decode MetroBookingHardCancelReq where decode = defaultDecode
instance encodeMetroBookingHardCancelReq   :: Encode MetroBookingHardCancelReq where encode = defaultEncode

------------------------------------------------------- hardCancelMetroBookingStatus ----------------------------------------------

data MetroBookingHardCancelStatusReq = MetroBookingHardCancelStatusReq String

newtype MetroBookingHardCancelStatusResp = MetroBookingHardCancelStatusResp 
  { cancellationCharges :: Maybe Number,
    refundAmount :: Maybe Number,
    payment :: Maybe FRFSBookingPaymentAPI
  }

derive instance genericMetroBookingHardCancelStatusResp :: Generic MetroBookingHardCancelStatusResp _
derive instance newtypeMetroBookingHardCancelStatusResp :: Newtype MetroBookingHardCancelStatusResp _
instance showMetroBookingHardCancelStatusResp :: Show MetroBookingHardCancelStatusResp where show = genericShow
instance decodeMetroBookingHardCancelStatusResp :: Decode MetroBookingHardCancelStatusResp where decode = defaultDecode
instance encodeMetroBookingHardCancelStatusResp :: Encode MetroBookingHardCancelStatusResp where encode = defaultEncode

instance metroBookingHardCancelStatusReq :: RestEndpoint MetroBookingHardCancelStatusReq  where
 makeRequest reqBody@(MetroBookingHardCancelStatusReq bookingId) headers = defaultMakeRequestWithoutLogs GET (EP.getMetroBookingHardCancelStatus bookingId) headers reqBody Nothing
 encodeRequest = standardEncode

derive instance genericMetroBookingHardCancelStatusReq :: Generic MetroBookingHardCancelStatusReq _
instance showMetroBookingHardCancelStatusReq     :: Show MetroBookingHardCancelStatusReq where show     = genericShow
instance standardMetroBookingHardCancelStatusReq :: StandardEncode MetroBookingHardCancelStatusReq where standardEncode (MetroBookingHardCancelStatusReq _) = standardEncode {}
instance decodeMetroBookingHardCancelStatusReq   :: Decode MetroBookingHardCancelStatusReq where decode = defaultDecode
instance encodeMetroBookingHardCancelStatusReq   :: Encode MetroBookingHardCancelStatusReq where encode = defaultEncode

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

instance makeSDKEventsReq :: RestEndpoint SDKEventsReq  where
 makeRequest reqBody headers = defaultMakeRequestWithoutLogs POST (EP.pushSDKEvents "") headers reqBody Nothing
 encodeRequest req = standardEncode req

derive instance genericSDKEventsReq :: Generic SDKEventsReq _
derive instance newtypeSDKEventsReq :: Newtype SDKEventsReq _
instance standardEncodeSDKEventsReq :: StandardEncode SDKEventsReq where standardEncode (SDKEventsReq reqBody) = standardEncode reqBody
instance showSDKEventsReq :: Show SDKEventsReq where show = genericShow
instance decodeSDKEventsReq :: Decode SDKEventsReq where decode = defaultDecode
instance encodeSDKEventsReq :: Encode SDKEventsReq where encode = defaultEncode

----------------------- ################### ADD OR EDIT STOP ####################### ----------------------------
data AddStopRequest = AddStopRequest String AddStopReq

data EditStopRequest = EditStopRequest String EditStopReq

newtype AddStopReq = AddStopReq
 {  address :: LocationAddress,
    gps :: LatLong
 }

newtype EditStopReq = EditStopReq
  {  address :: LocationAddress,
      gps :: LatLong
  }

newtype StopReq = StopReq
  { address :: LocationAddress,
    gps :: LatLong
  }


data StopRequest = StopRequest String Boolean StopReq 

instance makeStopReq :: RestEndpoint StopRequest  where 
    makeRequest reqBody@(StopRequest rideBookingId isEdit rqBody) headers = defaultMakeRequestWithoutLogs POST (EP.addOrEditStop isEdit rideBookingId) headers reqBody Nothing
    encodeRequest (StopRequest rideBookingId isEdit rqBody) = defaultEncode rqBody

derive instance genericStopReq :: Generic StopReq _
derive instance newtypeStopReq:: Newtype StopReq _
instance standardEncodeStopReq :: StandardEncode StopReq where standardEncode (StopReq req) = standardEncode req
instance showStopReq :: Show StopReq where show = genericShow
instance decodeStopReq :: Decode StopReq where decode = defaultDecode
instance encodeStopReq :: Encode StopReq where encode = defaultEncode


derive instance genericStopRequest :: Generic StopRequest _ 
instance standardEncodeStopRequest :: StandardEncode StopRequest where standardEncode (StopRequest id isEdit req) = standardEncode req
instance decodeStopRequest :: Decode  StopRequest where decode = defaultDecode
instance encodeStopRequest :: Encode StopRequest where encode = defaultEncode



derive instance genericEditStopReq :: Generic EditStopReq _
instance showEditStopReq        :: Show EditStopReq where show     = genericShow
instance standardEncodeEditStopReq :: StandardEncode EditStopReq where standardEncode (EditStopReq res) = standardEncode res
instance decodeEditStopReq         :: Decode EditStopReq where decode = defaultDecode
instance encodeEditStopReq         :: Encode EditStopReq where encode = defaultEncode

derive instance genericAddStopReq :: Generic AddStopReq _ 
instance showAddStopReq :: Show AddStopReq where show = genericShow 
instance standardEncodeAddStopReq :: StandardEncode AddStopReq where standardEncode (AddStopReq req) = standardEncode req
instance decodeAddStopReq :: Decode  AddStopReq where decode = defaultDecode
instance encodeAddStopReq :: Encode AddStopReq where encode = defaultEncode

derive instance genericAddStopRequest :: Generic AddStopRequest _ 
instance standardEncodeAddStopRequest :: StandardEncode AddStopRequest where standardEncode (AddStopRequest id req) = standardEncode req
instance decodeAddStopRequest :: Decode  AddStopRequest where decode = defaultDecode
instance encodeAddStopRequest :: Encode AddStopRequest where encode = defaultEncode


derive instance genericEditStopRequest :: Generic EditStopRequest _ 
instance standardEncodeEditStopRequest :: StandardEncode EditStopRequest where standardEncode (EditStopRequest id req) = standardEncode req
instance decodeEditStopRequest :: Decode  EditStopRequest where decode = defaultDecode
instance encodeEditStopRequest :: Encode EditStopRequest where encode = defaultEncode



instance makeAddStopReq :: RestEndpoint AddStopRequest  where
  makeRequest reqBody@(AddStopRequest bookingId (AddStopReq rqBody)) headers = defaultMakeRequestWithoutLogs POST (EP.addStop bookingId ) headers reqBody Nothing
  encodeRequest = standardEncode

instance makeEditStopReq :: RestEndpoint EditStopRequest  where 
  makeRequest reqBody@(EditStopRequest bookingId (EditStopReq rqBody)) headers = defaultMakeRequestWithoutLogs POST (EP.editStop bookingId ) headers reqBody Nothing
  encodeRequest = standardEncode  


----------------------- Edit Location API --------------------------------------------

data EditLocationRequest = EditLocationRequest String EditLocationReq

newtype EditLocationReq = EditLocationReq {
  destination :: Maybe SearchReqLocation ,
  origin :: Maybe SearchReqLocation
}

newtype EditLocationRes = EditLocationRes {
    bookingUpdateRequestId :: Maybe String, 
    result :: String
  }

instance makeEditLocationReq :: RestEndpoint EditLocationRequest  where
 makeRequest reqBody@(EditLocationRequest rideId (EditLocationReq rqBody)) headers = defaultMakeRequestWithoutLogs POST (EP.editLocation rideId) headers reqBody Nothing
 encodeRequest req = standardEncode req

derive instance genericEditLocationRes :: Generic EditLocationRes _
derive instance newtypeEditLocationRes :: Newtype EditLocationRes _
instance standardEncodeEditLocationRes :: StandardEncode EditLocationRes where standardEncode (EditLocationRes res) = standardEncode res
instance showEditLocationRes :: Show EditLocationRes where show = genericShow
instance decodeEditLocationRes :: Decode EditLocationRes where decode = defaultDecode
instance encodeEditLocationRes :: Encode EditLocationRes where encode = defaultEncode

derive instance genericEditLocationReq :: Generic EditLocationReq _
derive instance newtypeEditLocationReq :: Newtype EditLocationReq _
instance standardEncodeEditLocationReq :: StandardEncode EditLocationReq where standardEncode (EditLocationReq reqBody) = standardEncode reqBody
instance showEditLocationReq :: Show EditLocationReq where show = genericShow
instance decodeEditLocationReq :: Decode EditLocationReq where decode = defaultDecode
instance encodeEditLocationReq :: Encode EditLocationReq where encode = defaultEncode

derive instance genericEditLocationRequest :: Generic EditLocationRequest _
instance standardEncodeEditLocationRequest :: StandardEncode EditLocationRequest where standardEncode (EditLocationRequest rideId reqBody) = standardEncode reqBody
instance decodeEditLocationRequest :: Decode EditLocationRequest where decode = defaultDecode
instance encodeEditLocationRequest :: Encode EditLocationRequest where encode = defaultEncode

-----------------------Edit Location Result API --------------------------------------------

data GetEditLocResultReq = GetEditLocResultReq String

newtype GetEditLocResultResp = GetEditLocResultResp {
    bookingUpdateRequestDetails :: BookingUpdateRequestDetails
  } 

newtype BookingUpdateRequestDetails = BookingUpdateRequestDetails { 
  bookingId :: String,
  currentPointLat :: Maybe Number,
  currentPointLon :: Maybe Number,
  estimatedDistance :: Maybe Number,
  estimatedFare :: Maybe Number,
  id :: String,
  oldEstimatedDistance :: Maybe Int,
  errorObj :: Maybe ErrorObj,
  oldEstimatedFare :: Number,
  status :: String,
  totalDistance :: Maybe Int,
  travelledDistance :: Maybe Int
  }

newtype ErrorObj = ErrorObj {
  errorCode :: String,
  errorMessage :: String
}

data BookingUpdateRequestStatus = SOFT | CONFIRM

instance makeGetEditLocResultReq :: RestEndpoint GetEditLocResultReq  where
 makeRequest reqBody@(GetEditLocResultReq bookingUpdateRequestId) headers = defaultMakeRequestWithoutLogs GET (EP.getEditLocResult bookingUpdateRequestId) headers reqBody Nothing
 encodeRequest req = standardEncode req

derive instance genericGetEditLocResultResp :: Generic GetEditLocResultResp _
derive instance newtypeGetEditLocResultResp :: Newtype GetEditLocResultResp _
instance standardEncodeGetEditLocResultResp :: StandardEncode GetEditLocResultResp where standardEncode (GetEditLocResultResp body) = standardEncode body
instance showGetEditLocResultResp :: Show GetEditLocResultResp where show = genericShow
instance decodeGetEditLocResultResp :: Decode GetEditLocResultResp where decode = defaultDecode
instance encodeGetEditLocResultResp  :: Encode GetEditLocResultResp where encode = defaultEncode

derive instance genericGetEditLocResultReq :: Generic GetEditLocResultReq _
instance standardEncodeGetEditLocResultReq :: StandardEncode GetEditLocResultReq where standardEncode (GetEditLocResultReq body) = standardEncode body
instance showGetEditLocResultReq :: Show GetEditLocResultReq where show = genericShow
instance decodeGetEditLocResultReq :: Decode GetEditLocResultReq where decode = defaultDecode
instance encodeGetEditLocResultReq  :: Encode GetEditLocResultReq where encode = defaultEncode

derive instance genericBookingUpdateRequestDetails :: Generic BookingUpdateRequestDetails _
derive instance newtypeBookingUpdateRequestDetails :: Newtype BookingUpdateRequestDetails _
instance standardEncodeBookingUpdateRequestDetails :: StandardEncode BookingUpdateRequestDetails where standardEncode (BookingUpdateRequestDetails req) = standardEncode req
instance showBookingUpdateRequestDetails :: Show BookingUpdateRequestDetails where show = genericShow
instance decodeBookingUpdateRequestDetails :: Decode BookingUpdateRequestDetails where decode = defaultDecode
instance encodeBookingUpdateRequestDetails :: Encode BookingUpdateRequestDetails where encode = defaultEncode

derive instance genericErrorObj :: Generic ErrorObj _
derive instance newtypeErrorObj :: Newtype ErrorObj _
instance standardEncodeErrorObj :: StandardEncode ErrorObj where standardEncode (ErrorObj req) = standardEncode req
instance showErrorObj :: Show ErrorObj where show = genericShow
instance decodeErrorObj :: Decode ErrorObj where decode = defaultDecode
instance encodeErrorObj :: Encode ErrorObj where encode = defaultEncode

derive instance genericBookingUpdateRequestStatus :: Generic BookingUpdateRequestStatus _
instance showBookingUpdateRequestStatus :: Show BookingUpdateRequestStatus where show = genericShow
instance decodeBookingUpdateRequestStatus :: Decode BookingUpdateRequestStatus where decode = defaultDecode
instance encodeBookingUpdateRequestStatus :: Encode BookingUpdateRequestStatus where encode = defaultEncode
instance standardEncodeBookingUpdateRequestStatus :: StandardEncode BookingUpdateRequestStatus
  where
  standardEncode SOFT = standardEncode $ show SOFT
  standardEncode CONFIRM = standardEncode $ show CONFIRM

  -----------------------Edit Location Result Confirm API --------------------------------------------

data EditLocResultConfirmReq = EditLocResultConfirmReq String

type EditLocResultConfirmResp = APISuccessResp

instance makeEditLocResultConfirmReq :: RestEndpoint EditLocResultConfirmReq  where
 makeRequest reqBody@(EditLocResultConfirmReq bookingUpdateRequestId) headers = defaultMakeRequestWithoutLogs POST (EP.confirmEditLocResult bookingUpdateRequestId) headers reqBody Nothing
 encodeRequest req = standardEncode req

derive instance genericEditLocResultConfirmReq :: Generic EditLocResultConfirmReq _
instance standardEncodeEditLocResultConfirmReq :: StandardEncode EditLocResultConfirmReq where standardEncode (EditLocResultConfirmReq body) = standardEncode body
instance showEditLocResultConfirmReq :: Show EditLocResultConfirmReq where show = genericShow
instance decodeEditLocResultConfirmReq :: Decode EditLocResultConfirmReq where decode = defaultDecode
instance encodeEditLocResultConfirmReq  :: Encode EditLocResultConfirmReq where encode = defaultEncode

newtype APISuccessResp = APISuccessResp {
  result :: String
}

derive instance genericAPISuccessResp :: Generic APISuccessResp _
derive instance newtypeAPISuccessResp :: Newtype APISuccessResp _
instance standardEncodeAPISuccessResp :: StandardEncode APISuccessResp where standardEncode (APISuccessResp id) = standardEncode id
instance showAPISuccessResp :: Show APISuccessResp where show = genericShow
instance decodeAPISuccessResp :: Decode APISuccessResp where decode = defaultDecode
instance encodeAPISuccessResp :: Encode APISuccessResp where encode = defaultEncode


------------------------------------------------------- MetroBookingConfig -----------------------------------------------------

newtype MetroBookingConfigReq = MetroBookingConfigReq String

newtype FRFSConfigAPIRes = FRFSConfigAPIRes {
  bookingStartTime :: String,
  bookingEndTime :: String,
  roundTripTicketLimit :: Int,
  oneWayTicketLimit :: Int,
  metroStationTtl :: Int,
  discount :: Number,
  customEndTime :: String,
  customDates :: Array String,
  isEventOngoing :: Maybe Boolean,
  freeTicketInterval :: Maybe Int,
  isCancellationAllowed :: Maybe Boolean,
  maxFreeTicketCashback :: Maybe Int,
  ticketsBookedInEvent :: Maybe Int
}

instance makeMetroBookingConfigReq :: RestEndpoint MetroBookingConfigReq where
 makeRequest reqBody@(MetroBookingConfigReq city) headers = defaultMakeRequestWithoutLogs GET (EP.getFRFSBookingConfig city) headers reqBody Nothing
 encodeRequest req = defaultEncode req

derive instance genericMetroBookingConfigReq :: Generic MetroBookingConfigReq _
derive instance newtypeMetroBookingConfigReq :: Newtype MetroBookingConfigReq _
instance standardEncodeMetroBookingConfigReq :: StandardEncode MetroBookingConfigReq where standardEncode (MetroBookingConfigReq reqBody) = standardEncode reqBody
instance showMetroBookingConfigReq :: Show MetroBookingConfigReq where show = genericShow
instance decodeMetroBookingConfigReq :: Decode MetroBookingConfigReq where decode = defaultDecode
instance encodeMetroBookingConfigReq :: Encode MetroBookingConfigReq where encode = defaultEncode

derive instance genericMetroBookingConfigRes :: Generic FRFSConfigAPIRes _
derive instance newtypeMetroBookingConfigRes :: Newtype FRFSConfigAPIRes _
instance standardEncodeMetroBookingConfigRes :: StandardEncode FRFSConfigAPIRes where standardEncode (FRFSConfigAPIRes resp) = standardEncode resp
instance showMetroBookingConfigRes :: Show FRFSConfigAPIRes where show = genericShow
instance decodeMetroBookingConfigRes :: Decode FRFSConfigAPIRes where decode = defaultDecode
instance encodeMetroBookingConfigRes :: Encode FRFSConfigAPIRes where encode = defaultEncode

data EmergencyContactsTrackingReq = EmergencyContactsTrackingReq String

newtype EmergencyContactsTrackingRes = EmergencyContactsTrackingRes {
  details :: Array CurrentTrackingDetails
}

newtype CurrentTrackingDetails = CurrentTrackingDetails {
  personId :: String,
  updateTime :: String
}

instance makeEmergencyContactsTrackingReq :: RestEndpoint EmergencyContactsTrackingReq where
 makeRequest reqBody@(EmergencyContactsTrackingReq rideId) headers = defaultMakeRequestWithoutLogs GET (EP.getEmergencyContactsTrackingStatus rideId) headers reqBody Nothing
 encodeRequest req = defaultEncode req

derive instance genericEmergencyContactsTrackingReq :: Generic EmergencyContactsTrackingReq _
instance standardEncodeEmergencyContactsTrackingReq :: StandardEncode EmergencyContactsTrackingReq where standardEncode (EmergencyContactsTrackingReq reqBody) = standardEncode reqBody

derive instance genericEmergencyContactsTrackingRes :: Generic EmergencyContactsTrackingRes _
derive instance newtypeEmergencyContactsTrackingRes :: Newtype EmergencyContactsTrackingRes _
instance standardEncodeEmergencyContactsTrackingRes :: StandardEncode EmergencyContactsTrackingRes where standardEncode (EmergencyContactsTrackingRes resp) = standardEncode resp
instance showEmergencyContactsTrackingRes :: Show EmergencyContactsTrackingRes where show = genericShow
instance decodeEmergencyContactsTrackingRes :: Decode EmergencyContactsTrackingRes where decode = defaultDecode
instance encodeEmergencyContactsTrackingRes :: Encode EmergencyContactsTrackingRes where encode = defaultEncode

derive instance genericCurrentTrackingDetails :: Generic CurrentTrackingDetails _
derive instance newtypeCurrentTrackingDetails :: Newtype CurrentTrackingDetails _
instance standardEncodeCurrentTrackingDetails :: StandardEncode CurrentTrackingDetails where standardEncode (CurrentTrackingDetails resp) = standardEncode resp
instance showCurrentTrackingDetails :: Show CurrentTrackingDetails where show = genericShow
instance decodeCurrentTrackingDetails :: Decode CurrentTrackingDetails where decode = defaultDecode
instance encodeCurrentTrackingDetails :: Encode CurrentTrackingDetails where encode = defaultEncode

----------------------------- GetManualFollowerDetails -----------------------------------------------------

data GetManuallySharedDetailsReq = GetManuallySharedDetailsReq String

newtype GetManuallySharedDetailsRes = GetManuallySharedDetailsRes {
  bookingId :: String,
  customerName :: String,
  customerPhone :: String,
  customerId :: Maybe String
}

instance makeGetManuallySharedDetailsReq :: RestEndpoint GetManuallySharedDetailsReq where
 makeRequest reqBody@(GetManuallySharedDetailsReq rideId) headers = defaultMakeRequestWithoutLogs GET (EP.getManuallySharedRideDetails rideId) headers reqBody Nothing
 encodeRequest req = defaultEncode req

derive instance genericGetManuallySharedDetailsReq :: Generic GetManuallySharedDetailsReq _
instance standardEncodeGetManuallySharedDetailsReq :: StandardEncode GetManuallySharedDetailsReq where standardEncode (GetManuallySharedDetailsReq resp) = standardEncode resp
instance showGetManuallySharedDetailsReq :: Show GetManuallySharedDetailsReq where show = genericShow
instance decodeGetManuallySharedDetailsReq :: Decode GetManuallySharedDetailsReq where decode = defaultDecode
instance encodeGetManuallySharedDetailsReq :: Encode GetManuallySharedDetailsReq where encode = defaultEncode

derive instance genericGetManuallySharedDetailsRes :: Generic GetManuallySharedDetailsRes _
derive instance newtypeGetManuallySharedDetailsRes :: Newtype GetManuallySharedDetailsRes _
instance standardEncodeGetManuallySharedDetailsRes :: StandardEncode GetManuallySharedDetailsRes where standardEncode (GetManuallySharedDetailsRes resp) = standardEncode resp
instance showGetManuallySharedDetailsRes :: Show GetManuallySharedDetailsRes where show = genericShow
instance decodeGetManuallySharedDetailsRes :: Decode GetManuallySharedDetailsRes where decode = defaultDecode
instance encodeGetManuallySharedDetailsRes :: Encode GetManuallySharedDetailsRes where encode = defaultEncode


----------------------------------------------------------------- Multichat API ---------------------------------------------------------------------


newtype MultiChatReq = MultiChatReq {
  chatPersonId :: String,
  body :: String,
  title :: String,
  channelId :: Maybe String,
  showNotification :: Maybe Boolean,
  source :: Maybe MessageSource
}

instance makeMultiChatReq :: RestEndpoint MultiChatReq  where
 makeRequest reqBody@(MultiChatReq payload) headers = defaultMakeRequestWithoutLogs POST (EP.multiChat "") headers reqBody Nothing
 encodeRequest req = defaultEncode req

derive instance genericMultiChatReq :: Generic MultiChatReq _
derive instance newtypeMultiChatReq :: Newtype MultiChatReq _
instance standardEncodeMultiChatReq :: StandardEncode MultiChatReq where standardEncode (MultiChatReq reqBody) = standardEncode reqBody
instance showMultiChatReq :: Show MultiChatReq where show = genericShow
instance decodeMultiChatReq :: Decode MultiChatReq where decode = defaultDecode
instance encodeMultiChatReq :: Encode MultiChatReq where encode = defaultEncode

data MessageSource = USER | TRUSTED_CONTACT

derive instance genericMessageSource :: Generic MessageSource _
instance standardEncodeMessageSource :: StandardEncode MessageSource where standardEncode _ = standardEncode {}
instance showMessageSource :: Show MessageSource where show = genericShow
instance decodeMessageSource :: Decode MessageSource where decode = defaultDecode
instance encodeMessageSource  :: Encode MessageSource where encode = defaultEnumEncode
instance eqMessageSource :: Eq MessageSource where eq = genericEq




-----------------------------------------------------------------RemoveFavouriteDriver-------------------------------------------

data RemoveFavouriteDriverReq = RemoveFavouriteDriverReq String  

newtype RemoveFavouriteDriverResp = RemoveFavouriteDriverResp {
  result :: String
}

derive instance genericRemoveFavouriteDriverReq:: Generic RemoveFavouriteDriverReq _
instance standardRemoveFavouriteDriverReq :: StandardEncode RemoveFavouriteDriverReq where standardEncode (RemoveFavouriteDriverReq driverId) = standardEncode {}
instance showRemoveFavouriteDriverReq :: Show RemoveFavouriteDriverReq where show = genericShow
instance decodeRemoveFavouriteDriverReq :: Decode RemoveFavouriteDriverReq where decode = defaultDecode
instance encodeRemoveFavouriteDriverReq :: Encode RemoveFavouriteDriverReq where encode = defaultEncode

derive instance genericRemoveFavouriteDriverResp:: Generic RemoveFavouriteDriverResp _
instance standardRemoveFavouriteDriverResp :: StandardEncode RemoveFavouriteDriverResp where standardEncode (RemoveFavouriteDriverResp body) = standardEncode body
instance showRemoveFavouriteDriverResp :: Show RemoveFavouriteDriverResp where show = genericShow
instance decodeRemoveFavouriteDriverResp :: Decode RemoveFavouriteDriverResp where decode = defaultDecode
instance encodeRemoveFavouriteDriverResp :: Encode RemoveFavouriteDriverResp where encode = defaultEncode

instance makeRemoveFavouriteDriverReq :: RestEndpoint RemoveFavouriteDriverReq where
 makeRequest reqBody@(RemoveFavouriteDriverReq driverId) headers = defaultMakeRequestWithoutLogs POST (EP.removeFavouriteDriver driverId) headers reqBody Nothing
 encodeRequest req = standardEncode req

------------------------------------------------------------------------------GetFavouriteDriverList---------------------------------------------------

newtype FavouriteDriverList = FavouriteDriverList {
  driverName :: String,
  driverPhone :: String,
  driverRating :: Number,
  favCount :: Int,
  id :: Maybe String
}
data GetFavouriteDriverListReq = GetFavouriteDriverListReq 

newtype GetFavouriteDriverListRes = GetFavouriteDriverListRes (Array FavouriteDriverList)

derive instance genericGetFavouriteDriverListReq:: Generic GetFavouriteDriverListReq _
instance standardGetFavouriteDriverListReq :: StandardEncode GetFavouriteDriverListReq where standardEncode (GetFavouriteDriverListReq ) = standardEncode {}
instance showGetFavouriteDriverListReq :: Show GetFavouriteDriverListReq where show = genericShow
instance decodeGetFavouriteDriverListReq :: Decode GetFavouriteDriverListReq where decode = defaultDecode
instance encodeGetFavouriteDriverListReq :: Encode GetFavouriteDriverListReq where encode = defaultEncode

derive instance genericGetFavouriteDriverListRes:: Generic GetFavouriteDriverListRes _
derive instance newtypeGetFavouriteDriverListRes :: Newtype GetFavouriteDriverListRes _
instance standardGetFavouriteDriverListRes :: StandardEncode GetFavouriteDriverListRes where standardEncode (GetFavouriteDriverListRes body) = standardEncode body
instance showGetFavouriteDriverListRes :: Show GetFavouriteDriverListRes where show = genericShow
instance decodeGetFavouriteDriverListRes :: Decode GetFavouriteDriverListRes where decode = defaultDecode
instance encodeGetFavouriteDriverListRes :: Encode GetFavouriteDriverListRes where encode = defaultEncode


instance makeGetFavouriteDriverListReq :: RestEndpoint GetFavouriteDriverListReq where
 makeRequest reqBody@(GetFavouriteDriverListReq) headers = defaultMakeRequestWithoutLogs GET (EP.getFavouriteDriverList) headers reqBody Nothing
 encodeRequest req = standardEncode req

derive instance genericFavouriteDriverList :: Generic FavouriteDriverList _
derive instance newtypeFavouriteDriverList :: Newtype FavouriteDriverList _
instance standardEncodeFavouriteDriverList :: StandardEncode FavouriteDriverList where standardEncode (FavouriteDriverList body) = standardEncode body
instance showFavouriteDriverList :: Show FavouriteDriverList where show = genericShow
instance decodeFavouriteDriverList :: Decode FavouriteDriverList where decode = defaultDecode
instance encodeFavouriteDriverList  :: Encode FavouriteDriverList where encode = defaultEncode

----------------------------------------------------------------GetFavouriteDriverTrips--------------------------------------------------

data FavouriteDriverTripsReq = FavouriteDriverTripsReq String String String (Maybe String) (Maybe String) FavouriteDriverTripsBody

newtype FavouriteDriverTripsBody = FavouriteDriverTripsBody
  { 
    driverNumber :: String
  }

newtype LocationAPIEntity = LocationAPIEntity
  { 
    address :: LocationAddress,
    createdAt :: String,
    id :: String,
    lat :: Number,
    lon :: Number,
    updatedAt :: String
  }

newtype FavouriteDriverRides = FavouriteDriverRides {
  rideRating :: Maybe Int,
  fromLocation :: LocationAPIEntity,
  toLocation :: Maybe LocationAPIEntity,
  totalFare :: Maybe Int,
  startTime :: Maybe String,
  id :: Maybe String
}

newtype FavouriteDriverTripsResp = FavouriteDriverTripsResp {
  list :: Array FavouriteDriverRides
}

instance makeFavouriteDriverTripsReq :: RestEndpoint FavouriteDriverTripsReq where
  makeRequest reqBody@(FavouriteDriverTripsReq limit offset isActive status clientId (FavouriteDriverTripsBody abcBody)) headers = defaultMakeRequestWithoutLogs POST (EP.getFavouriteDriverTrips limit offset isActive status clientId ) headers reqBody Nothing
  encodeRequest req = standardEncode req

derive instance genericFavouriteDriverTripsReq :: Generic FavouriteDriverTripsReq _
instance standardFavouriteDriverTripsReq :: StandardEncode FavouriteDriverTripsReq where standardEncode (FavouriteDriverTripsReq limit offset isActive status clientId req) = standardEncode req
instance decodeFavouriteDriverTripsReq :: Decode FavouriteDriverTripsReq where decode = defaultDecode
instance encodeFavouriteDriverTripsReq :: Encode FavouriteDriverTripsReq where encode = defaultEncode

derive instance genericFavouriteDriverTripsResp :: Generic FavouriteDriverTripsResp _
derive instance newtypeFavouriteDriverTripsResp :: Newtype FavouriteDriverTripsResp _
instance standardEncodeFavouriteDriverTripsResp :: StandardEncode FavouriteDriverTripsResp where standardEncode (FavouriteDriverTripsResp req) = standardEncode req
instance showFavouriteDriverTripsResp :: Show FavouriteDriverTripsResp where show = genericShow
instance decodeFavouriteDriverTripsResp :: Decode FavouriteDriverTripsResp where decode = defaultDecode
instance encodeFavouriteDriverTripsResp :: Encode FavouriteDriverTripsResp where encode = defaultEncode

derive instance genericFavouriteDriverTripsBody :: Generic FavouriteDriverTripsBody _
derive instance newtypeFavouriteDriverTripsBody :: Newtype FavouriteDriverTripsBody _
instance standardEncodeFavouriteDriverTripsBody :: StandardEncode FavouriteDriverTripsBody where standardEncode (FavouriteDriverTripsBody req) = standardEncode req
instance showFavouriteDriverTripsBody :: Show FavouriteDriverTripsBody where show = genericShow
instance decodeFavouriteDriverTripsBody :: Decode FavouriteDriverTripsBody where decode = defaultDecode
instance encodeFavouriteDriverTripsBody :: Encode FavouriteDriverTripsBody where encode = defaultEncode

derive instance genericFavouriteDriverRides :: Generic FavouriteDriverRides _
derive instance newtypeFavouriteDriverRides :: Newtype FavouriteDriverRides _
instance standardEncodeFavouriteDriverRides :: StandardEncode FavouriteDriverRides where standardEncode (FavouriteDriverRides req) = standardEncode req
instance showFavouriteDriverRides :: Show FavouriteDriverRides where show = genericShow
instance decodeFavouriteDriverRides :: Decode FavouriteDriverRides where decode = defaultDecode
instance encodeFavouriteDriverRides :: Encode FavouriteDriverRides where encode = defaultEncode

derive instance genericLocationAPIEntity :: Generic LocationAPIEntity _
derive instance newtypeLocationAPIEntity :: Newtype LocationAPIEntity _
instance standardEncodeLocationAPIEntity :: StandardEncode LocationAPIEntity where standardEncode (LocationAPIEntity req) = standardEncode req
instance showLocationAPIEntity :: Show LocationAPIEntity where show = genericShow
instance decodeLocationAPIEntity :: Decode LocationAPIEntity where decode = defaultDecode
instance encodeLocationAPIEntity :: Encode LocationAPIEntity where encode = defaultEncode

----------------------------- DELIVERY -----------------------------------------------------

data GetDeliveryImageReq = GetDeliveryImageReq String

newtype GetDeliveryImageResponse = GetDeliveryImageResponse String

instance makeGetDeliveryImageReq :: RestEndpoint GetDeliveryImageReq where
 makeRequest reqBody@(GetDeliveryImageReq bookingId) headers = defaultMakeRequestWithoutLogs GET (EP.getDeliveryImage bookingId) headers reqBody Nothing
 encodeRequest req = defaultEncode req
 
derive instance genericGetDeliveryImageReq :: Generic GetDeliveryImageReq _
instance standardEncodeGetDeliveryImageReq :: StandardEncode GetDeliveryImageReq where standardEncode (GetDeliveryImageReq resp) = standardEncode resp
instance showGetDeliveryImageReq :: Show GetDeliveryImageReq where show = genericShow
instance decodeGetDeliveryImageReq :: Decode GetDeliveryImageReq where decode = defaultDecode
instance encodeGetDeliveryImageReq :: Encode GetDeliveryImageReq where encode = defaultEncode

derive instance genericGetDeliveryImageResponse :: Generic GetDeliveryImageResponse _
derive instance newtypeGetDeliveryImageResponse :: Newtype GetDeliveryImageResponse _
instance standardEncodeGetDeliveryImageResponse :: StandardEncode GetDeliveryImageResponse where standardEncode (GetDeliveryImageResponse resp) = standardEncode resp
instance showGetDeliveryImageResponse :: Show GetDeliveryImageResponse where show = genericShow
instance decodeGetDeliveryImageResponse :: Decode GetDeliveryImageResponse where decode = defaultDecode
instance encodeGetDeliveryImageResponse :: Encode GetDeliveryImageResponse where encode = defaultEncode


data SearchRideType = BUS_ROUTE | BUS_SOURCE | BUS_DESTINATION

derive instance genericSearchRideType :: Generic SearchRideType _

instance eqSearchRideType :: Eq SearchRideType where eq = genericEq

instance showSearchRideType :: Show SearchRideType where show = genericShow



------------------------------------------------- AutoComplete API Types --------------------------------------------

data BusAutoCompleteReq = BusAutoCompleteReq String String String (Maybe String) String (Maybe String)

newtype AutoCompleteResp = AutoCompleteResp {
    routes :: Array FRFSRouteAPI,
    stops :: Array FRFSStationAPI
}

derive instance genericBusAutoCompleteReq :: Generic BusAutoCompleteReq _
instance showBusAutoCompleteReq    :: Show BusAutoCompleteReq where show     = genericShow
instance standardBusAutoCompleteReq :: StandardEncode BusAutoCompleteReq where standardEncode (BusAutoCompleteReq _ _ _ _ _ _) = standardEncode {}
instance decodeBusAutoCompleteReq   :: Decode BusAutoCompleteReq where decode = defaultDecode
instance encodeBusAutoCompleteReq  :: Encode BusAutoCompleteReq where encode = defaultEncode

derive instance genericAutoCompleteResp :: Generic AutoCompleteResp _
instance standardAutoCompleteResp :: StandardEncode AutoCompleteResp where
    standardEncode (AutoCompleteResp body) = standardEncode body
instance showAutoCompleteResp :: Show AutoCompleteResp where show = genericShow
instance decodeAutoCompleteResp :: Decode AutoCompleteResp where decode = defaultDecode
instance encodeAutoCompleteResp :: Encode AutoCompleteResp where encode = defaultEncode

instance makeAutoCompleteReq :: RestEndpoint BusAutoCompleteReq where
    makeRequest reqBody@(BusAutoCompleteReq vehicleType city location input limit offset) headers = defaultMakeRequestWithoutLogs GET (EP.busAutoComplete vehicleType city location input limit offset) headers reqBody Nothing
    encodeRequest = standardEncode


data BusTrackingRouteReq = BusTrackingRouteReq String 

newtype BusTrackingRouteResp = BusTrackingRouteResp {
    vehicleTrackingInfo :: Array VehicleInfo
}

newtype VehicleInfo = VehicleInfo {
  -- location:: LatLong,
  nextStop:: RouteStopMapping,
  vehicleId:: String,
  vehicleInfo:: VehicleInfoForRoute,
  nextStopTravelTime :: Maybe Int
}

newtype VehicleInfoForRoute = VehicleInfoForRoute {
    startTime :: Maybe String,
    startDate :: Maybe String,
    scheduleRelationship :: Maybe String,
    tripId :: Maybe String,
    latitude :: Maybe Number,
    longitude :: Maybe Number,
    speed :: Maybe Number,
    timestamp :: Maybe String
}

newtype RouteStopMapping = RouteStopMapping { 
    routeCode :: String,
    sequenceNum :: Int,
    stopCode :: String,
    stopPoint :: LatLong,
    stopName :: String,
    -- timeBounds :: Kernel.Types.TimeBound.TimeBound,
    vehicleType :: String--,
    -- merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    -- merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    -- createdAt :: Kernel.Prelude.UTCTime,
    -- updatedAt :: Kernel.Prelude.UTCTime
  }

derive instance genericBusTrackingRouteReq :: Generic BusTrackingRouteReq _
instance showBusTrackingRouteReq    :: Show BusTrackingRouteReq where show     = genericShow
instance standardBusTrackingRouteReq :: StandardEncode BusTrackingRouteReq where standardEncode (BusTrackingRouteReq _) = standardEncode {}
instance decodeBusTrackingRouteReq   :: Decode BusTrackingRouteReq where decode = defaultDecode
instance encodeBusTrackingRouteReq  :: Encode BusTrackingRouteReq where encode = defaultEncode

derive instance genericBusTrackingRouteResp :: Generic BusTrackingRouteResp _
instance standardBusTrackingRouteResp :: StandardEncode BusTrackingRouteResp where
    standardEncode (BusTrackingRouteResp body) = standardEncode body
instance showBusTrackingRouteResp :: Show BusTrackingRouteResp where show = genericShow
instance decodeBusTrackingRouteResp :: Decode BusTrackingRouteResp where decode = defaultDecode
instance encodeBusTrackingRouteResp :: Encode BusTrackingRouteResp where encode = defaultEncode

derive instance genericVehicleInfo :: Generic VehicleInfo _
instance standardVehicleInfo :: StandardEncode VehicleInfo where
    standardEncode (VehicleInfo body) = standardEncode body
instance showVehicleInfo :: Show VehicleInfo where show = genericShow
instance decodeVehicleInfo :: Decode VehicleInfo where decode = defaultDecode
instance encodeVehicleInfo :: Encode VehicleInfo where encode = defaultEncode

derive instance genericRouteStopMapping :: Generic RouteStopMapping _
instance standardRouteStopMapping :: StandardEncode RouteStopMapping where
    standardEncode (RouteStopMapping body) = standardEncode body
instance showRouteStopMapping :: Show RouteStopMapping where show = genericShow
instance decodeRouteStopMapping :: Decode RouteStopMapping where decode = defaultDecode
instance encodeRouteStopMapping :: Encode RouteStopMapping where encode = defaultEncode

instance makeBusTrackingRouteReq :: RestEndpoint BusTrackingRouteReq where
    makeRequest reqBody@(BusTrackingRouteReq code) headers = defaultMakeRequestWithoutLogs GET (EP.trackRouteBus code) headers reqBody Nothing
    encodeRequest = standardEncode
    
------------------------------------------------- Aadhaar Verification API Types --------------------------------------------

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
  , code :: String
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

type PriceObj = 
  { amount :: Number
  , currency :: String
  }

type FRFSDiscountRes = 
  { code :: String
  , description :: String
  , eligibility :: Boolean
  , price :: PriceObj
  , title :: String
  , tnc :: String
  }

derive instance genericVehicleInfoForRoute :: Generic VehicleInfoForRoute _
instance showVehicleInfoForRoute    :: Show VehicleInfoForRoute where show     = genericShow
instance standardVehicleInfoForRoute :: StandardEncode VehicleInfoForRoute where standardEncode (VehicleInfoForRoute _) = standardEncode {}
instance decodeVehicleInfoForRoute   :: Decode VehicleInfoForRoute where decode = defaultDecode
instance encodeVehicleInfoForRoute  :: Encode VehicleInfoForRoute where encode = defaultEncode


data FrfsGetRouteReq = FrfsGetRouteReq String String String

newtype FRFSRouteAPI = FRFSRouteAPI {
  code :: String,
  endPoint :: LatLong,
  longName :: String,
  shortName :: String,
  startPoint :: LatLong,
  totalStops :: Maybe Int,
  waypoints :: Maybe (Array LatLong),
  stations :: Maybe (Array FRFSStationAPI),
  travelTime :: Maybe Int,
  vehicleServiceTier :: Maybe FRFSVehicleServiceTierAPI
  -- priceWithCurrency :: Maybe PriceAPIEntity
}

derive instance genericFrfsGetRouteReq :: Generic FrfsGetRouteReq _
instance showFrfsGetRouteReq    :: Show FrfsGetRouteReq where show     = genericShow
instance standardFrfsGetRouteReq :: StandardEncode FrfsGetRouteReq where standardEncode (FrfsGetRouteReq _ _ _) = standardEncode {}
instance decodeFrfsGetRouteReq   :: Decode FrfsGetRouteReq where decode = defaultDecode
instance encodeFrfsGetRouteReq  :: Encode FrfsGetRouteReq where encode = defaultEncode

derive instance genericFrfsGetRouteResp :: Generic FRFSRouteAPI _
instance standardFrfsGetRouteResp :: StandardEncode FRFSRouteAPI where
    standardEncode (FRFSRouteAPI body) = standardEncode body
instance showFrfsGetRouteResp :: Show FRFSRouteAPI where show = genericShow
instance decodeFrfsGetRouteResp :: Decode FRFSRouteAPI where decode = defaultDecode
instance encodeFrfsGetRouteResp :: Encode FRFSRouteAPI where encode = defaultEncode

instance makeFrfsGetRouteReq :: RestEndpoint FrfsGetRouteReq where
    makeRequest reqBody@(FrfsGetRouteReq routeCode city vehicleType) headers = defaultMakeRequestWithoutLogs GET (EP.frfsRoute routeCode city vehicleType) headers reqBody Nothing
    encodeRequest = standardEncode

newtype FRFSVehicleServiceTierAPI = FRFSVehicleServiceTierAPI
  { _type :: String
  , shortName :: String
  , longName :: String
  , description :: String
  }

derive instance genericFRFSVehicleServiceTierAPI :: Generic FRFSVehicleServiceTierAPI _
-- derive instance newtypeFRFSVehicleServiceTierAPI :: Newtype FRFSVehicleServiceTierAPI _
instance standardFRFSVehicleServiceTierAPI :: StandardEncode FRFSVehicleServiceTierAPI where 
  standardEncode (FRFSVehicleServiceTierAPI body) = standardEncode body
instance showFRFSVehicleServiceTierAPI :: Show FRFSVehicleServiceTierAPI where show = genericShow
instance encodeFRFSVehicleServiceTierAPI :: Encode FRFSVehicleServiceTierAPI where encode = defaultEncode
instance decodeFRFSVehicleServiceTierAPI :: Decode FRFSVehicleServiceTierAPI where decode = defaultDecode
instance eqFRFSVehicleServiceTierAPI :: Eq FRFSVehicleServiceTierAPI where eq = genericEq


 --------------------------------------------------- confirmMetroQuote ----------------------------------------------------

data ConfirmFRFSQuoteReqV2 = ConfirmFRFSQuoteReqV2 String FRFSQuoteConfirmReq

newtype FRFSQuoteConfirmReq = FRFSQuoteConfirmReq {
  discounts :: Array FRFSDiscountReq
}

newtype FRFSDiscountReq = FRFSDiscountReq { 
    code :: String
  , quantity :: Int
  }

instance makeConfirmMetroQuoteReqV2 :: RestEndpoint ConfirmFRFSQuoteReqV2  where
 makeRequest reqBody@(ConfirmFRFSQuoteReqV2 quoteId (FRFSQuoteConfirmReq rqBody)) headers = defaultMakeRequestWithoutLogs POST (EP.confirmMetroQuoteV2 quoteId) headers reqBody Nothing
 encodeRequest req = standardEncode req

derive instance genericConfirmMetroQuoteReqV2 :: Generic ConfirmFRFSQuoteReqV2 _
-- derive instance genericConfirmMetroQuoteReqV2 :: Newtype ConfirmFRFSQuoteReqV2 _
instance standardEncodeConfirmMetroQuoteReqV2 :: StandardEncode ConfirmFRFSQuoteReqV2 where standardEncode (ConfirmFRFSQuoteReqV2 token reqBody) = standardEncode reqBody
instance decodeConfirmMetroQuoteReqV2 :: Decode ConfirmFRFSQuoteReqV2 where decode = defaultDecode
instance encodeConfirmMetroQuoteReqV2 :: Encode ConfirmFRFSQuoteReqV2 where encode = defaultEncode

derive instance genericConfirmMetroQuoteReqV2Body :: Generic FRFSQuoteConfirmReq _
instance standardConfirmMetroQuoteReqV2Body :: StandardEncode FRFSQuoteConfirmReq where
    standardEncode (FRFSQuoteConfirmReq body) = standardEncode body
instance showConfirmMetroQuoteReqV2Body :: Show FRFSQuoteConfirmReq where show = genericShow
instance decodeConfirmMetroQuoteReqV2Body :: Decode FRFSQuoteConfirmReq where decode = defaultDecode
instance encodeConfirmMetroQuoteReqV2Body :: Encode FRFSQuoteConfirmReq where encode = defaultEncode

derive instance genericDiscountItem :: Generic FRFSDiscountReq _
instance standardDiscountItem :: StandardEncode FRFSDiscountReq where
    standardEncode (FRFSDiscountReq body) = standardEncode body
instance showDiscountItem :: Show FRFSDiscountReq where show = genericShow
instance decodeDiscountItem :: Decode FRFSDiscountReq where decode = defaultDecode
instance encodeDiscountItem :: Encode FRFSDiscountReq where encode = defaultEncode
