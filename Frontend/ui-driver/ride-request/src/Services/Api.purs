module Api.Types where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Foreign.Generic (class Decode, decodeJSON)
import Presto.Core.Types.API (class RestEndpoint, class StandardEncode, Method(..), defaultMakeRequest, standardEncode)
import Presto.Core.Utils.Encoding (defaultDecode)
import Services.Endpoint (nearBySearchRequest)

instance makeRideBooking :: RestEndpoint NearBySearchRequest NearBySearchRequestRes where
  makeRequest reqBody headers = defaultMakeRequest GET (nearBySearchRequest) headers reqBody Nothing
  decodeResponse = decodeJSON
  encodeRequest req = standardEncode req

data NearBySearchRequest
  = NearBySearchRequest String

newtype NearBySearchRequestRes
  = NearBySearchRequestRes
  { searchRequestsForDriver :: Array SearchRequest
  }

newtype SearchRequest
  = SearchRequest
  { airConditioned :: Maybe Boolean
  , bapLogo :: Maybe String
  , bapName :: Maybe String
  , baseFare :: Int
  , customerCancellationDues :: Number
  , customerExtraFee :: Maybe Int
  , disabilityTag :: Maybe String
  , distance :: Maybe Int
  , distanceToPickup :: Maybe Int
  , driverMaxExtraFee :: Maybe Int
  , driverMinExtraFee :: Maybe Int
  , driverPickUpCharges :: Maybe Int
  , duration :: Maybe Int
  , durationToPickup :: Maybe Int
  , fromLocation :: BookingLocationAPIEntity
  , isTranslated :: Maybe Boolean
  , isValueAddNP :: Maybe Boolean
  , keepHiddenForSeconds :: Maybe Int
  , pickupZone :: Maybe Boolean
  , requestedVehicleVariant :: String
  , rideRequestPopupDelayDuration :: Maybe Int
  , searchRequestId :: String
  , searchRequestValidTill :: String
  , searchTryId :: String
  , specialLocationTag :: Maybe String
  , specialZoneExtraTip :: Maybe Int
  , toLocation :: BookingLocationAPIEntity
  , tollCharges :: Number
  , tripCategory :: String
  , vehicleServiceTier :: Maybe String
  }

newtype BookingLocationAPIEntity
  = BookingLocationAPIEntity
  { area :: Maybe String
  , state :: Maybe String
  , country :: Maybe String
  , building :: Maybe String
  , door :: Maybe String
  , street :: Maybe String
  , lat :: Number
  , city :: Maybe String
  , areaCode :: Maybe String
  , lon :: Number
  , ward :: Maybe String
  , placeId :: Maybe String
  }

derive instance genericBookingLocationAPIEntity :: Generic BookingLocationAPIEntity _

derive instance newtypeBookingLocationAPIEntity :: Newtype BookingLocationAPIEntity _

instance standardEncodeBookingLocationAPIEntity :: StandardEncode BookingLocationAPIEntity where
  standardEncode (BookingLocationAPIEntity body) = standardEncode body

instance decodeBookingLocationAPIEntity :: Decode BookingLocationAPIEntity where
  decode = defaultDecode

derive instance genericNearBySearchRequest :: Generic NearBySearchRequest _

instance standardEncodeNearBySearchRequest :: StandardEncode NearBySearchRequest where
  standardEncode (NearBySearchRequest body) = standardEncode body

instance decodeNearBySearchRequest :: Decode NearBySearchRequest where
  decode = defaultDecode

derive instance genericSearchRequest :: Generic SearchRequest _

derive instance newtypeSearchRequest :: Newtype SearchRequest _

instance standardEncodeSearchRequest :: StandardEncode SearchRequest where
  standardEncode (SearchRequest body) = standardEncode body

instance decodeSearchRequest :: Decode SearchRequest where
  decode = defaultDecode

derive instance genericNearBySearchRequestRes :: Generic NearBySearchRequestRes _

derive instance newtypeNearBySearchRequestRes :: Newtype NearBySearchRequestRes _

instance standardEncodeNearBySearchRequestRes :: StandardEncode NearBySearchRequestRes where
  standardEncode (NearBySearchRequestRes body) = standardEncode body

instance decodeNearBySearchRequestRes :: Decode NearBySearchRequestRes where
  decode = defaultDecode
