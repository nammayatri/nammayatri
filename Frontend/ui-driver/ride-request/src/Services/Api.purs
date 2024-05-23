module Api.Types where

import Prelude

import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Ord.Generic (genericCompare)
import Foreign.Generic (class Decode, decodeJSON)
import Presto.Core.Types.API (class RestEndpoint, class StandardEncode, Method(..), defaultMakeRequest, standardEncode)
import Presto.Core.Types.Language.Flow (APIResult)
import Presto.Core.Utils.Encoding (defaultDecode, defaultEnumEncode)
import Services.Endpoint (nearBySearchRequest, quoteOffer)


data APISuccesResult = APISuccesResult {
  result :: String
}

instance makeRideBooking :: RestEndpoint NearBySearchRequest NearBySearchRequestRes where
  makeRequest reqBody@(NearBySearchRequest id) headers = defaultMakeRequest GET (nearBySearchRequest id) headers reqBody Nothing
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
  , baseFare :: Number
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
  , tollCharges :: Maybe Number
  , tripCategory :: TripCategory
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
  , full_address :: Maybe String
  }


newtype TripCategory
  = TripCategory
  { tag :: String
  , contents :: String
  }

derive instance genericBookingLocationAPIEntity :: Generic BookingLocationAPIEntity _

derive instance newtypeBookingLocationAPIEntity :: Newtype BookingLocationAPIEntity _

instance standardEncodeBookingLocationAPIEntity :: StandardEncode BookingLocationAPIEntity where
  standardEncode (BookingLocationAPIEntity body) = standardEncode body

instance decodeBookingLocationAPIEntity :: Decode BookingLocationAPIEntity where
  decode = defaultDecode

derive instance genericNearBySearchRequest :: Generic NearBySearchRequest _

instance standardEncodeNearBySearchRequest :: StandardEncode NearBySearchRequest where
  standardEncode _ = standardEncode {}

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

derive instance genericTripCategory :: Generic TripCategory _

derive instance newtypeTripCategory :: Newtype TripCategory _

instance standardEncodeTripCategory :: StandardEncode TripCategory where
  standardEncode (TripCategory body) = standardEncode body

instance decodeTripCategory :: Decode TripCategory where
  decode = defaultDecode


data PriceAPIEntity = PriceAPIEntity {
  amount :: Number,
  currency :: Currency
}

data OfferType = Accept | Decline | Pulled

data Currency = INR | USD | EUR

data QuoteOfferReq = QuoteOfferReq {
  offeredFareWithCurrency :: Maybe PriceAPIEntity,
  response :: OfferType,
  searchTryId :: Maybe String
}


derive instance genericQuoteOfferReq :: Generic QuoteOfferReq _
instance standardEncodeQuoteOfferReq :: StandardEncode QuoteOfferReq where standardEncode (QuoteOfferReq body) = standardEncode body
instance decodeQuoteOfferReq :: Decode QuoteOfferReq where decode = defaultDecode

derive instance genericOfferType :: Generic OfferType _
instance standardEncodeOfferType :: StandardEncode OfferType where standardEncode body = defaultEnumEncode body
instance decodeOfferType :: Decode OfferType where decode = defaultDecode

derive instance genericCurrency :: Generic Currency _
instance standardEncodeCurrency :: StandardEncode Currency where standardEncode body = defaultEnumEncode body
instance decodeCurrency :: Decode Currency where decode = defaultDecode

derive instance genericPriceAPIEntity :: Generic PriceAPIEntity _
instance standardEncodePriceAPIEntity :: StandardEncode PriceAPIEntity where standardEncode (PriceAPIEntity body) = standardEncode body
instance decodePriceAPIEntity :: Decode PriceAPIEntity where decode = defaultDecode

derive instance genericAPISuccesResult :: Generic APISuccesResult _
instance standardEncodeAPISuccesResult :: StandardEncode APISuccesResult where standardEncode (APISuccesResult body) = standardEncode body
instance decodeAPISuccesResult :: Decode APISuccesResult where decode = defaultDecode

instance makeQuoteOffer :: RestEndpoint QuoteOfferReq APISuccesResult where
  makeRequest reqBody headers = defaultMakeRequest POST (quoteOffer "") headers reqBody Nothing
  decodeResponse = decodeJSON
  encodeRequest req = standardEncode req