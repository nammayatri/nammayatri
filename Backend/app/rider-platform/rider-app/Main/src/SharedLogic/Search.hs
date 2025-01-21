module SharedLogic.Search where

import qualified BecknV2.OnDemand.Tags as Beckn
import Data.Aeson
import Data.OpenApi hiding (Header, description, email)
import qualified Data.OpenApi as OpenApi hiding (Header)
import Domain.Types.Location as Location
import Domain.Types.LocationAddress
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.RefereeLink as DRL
import qualified Domain.Types.SearchRequest as DSearchReq
import qualified Domain.Types.SearchRequest as SearchRequest
import qualified Domain.Types.Trip as DTrip
import Kernel.External.Maps
import Kernel.Prelude
import Kernel.Types.Beckn.Context (City)
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id
import Storage.Beam.SystemConfigs ()
import qualified Tools.JSON as J
import qualified Tools.Maps as Maps

data SearchRes = SearchRes
  { origin :: SearchReqLocation,
    stops :: [SearchReqLocation],
    startTime :: UTCTime,
    returnTime :: Maybe UTCTime,
    riderPreferredOption :: SearchRequest.RiderPreferredOption,
    roundTrip :: Bool,
    searchRequest :: DSearchReq.SearchRequest,
    now :: UTCTime,
    gatewayUrl :: BaseUrl,
    searchRequestExpiry :: UTCTime,
    merchant :: DM.Merchant,
    city :: City,
    device :: Maybe Text,
    distance :: Maybe Meters,
    duration :: Maybe Seconds,
    shortestRouteInfo :: Maybe Maps.RouteInfo,
    isReallocationEnabled :: Maybe Bool,
    fareParametersInRateCard :: Maybe Bool,
    multipleRoutes :: Maybe [Maps.RouteInfo],
    taggings :: Maybe Beckn.Taggings,
    merchantOperatingCityId :: Id DMOC.MerchantOperatingCity
  }

data SearchReq = OneWaySearch OneWaySearchReq | RentalSearch RentalSearchReq | InterCitySearch InterCitySearchReq | AmbulanceSearch OneWaySearchReq | DeliverySearch OneWaySearchReq
  deriving (Generic, Show)

instance ToJSON SearchReq where
  toJSON = genericToJSON fareProductOptions

instance FromJSON SearchReq where
  parseJSON = genericParseJSON fareProductOptions

instance ToSchema SearchReq where
  declareNamedSchema = genericDeclareNamedSchema fareProductSchemaOptions

fareProductOptions :: Options
fareProductOptions =
  defaultOptions
    { sumEncoding = J.fareProductTaggedObject,
      constructorTagModifier = fareProductConstructorModifier
    }

fareProductSchemaOptions :: OpenApi.SchemaOptions
fareProductSchemaOptions =
  OpenApi.defaultSchemaOptions
    { OpenApi.sumEncoding = J.fareProductTaggedObject,
      OpenApi.constructorTagModifier = fareProductConstructorModifier
    }

data SearchResp = SearchResp
  { searchId :: Id SearchRequest.SearchRequest,
    searchExpiry :: UTCTime,
    routeInfo :: Maybe Maps.RouteInfo
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

fareProductConstructorModifier :: String -> String
fareProductConstructorModifier = \case
  "OneWaySearch" -> "ONE_WAY"
  "RentalSearch" -> "RENTAL"
  "InterCitySearch" -> "INTER_CITY"
  "AmbulanceSearch" -> "AMBULANCE"
  "DeliverySearch" -> "DELIVERY"
  x -> x

data OneWaySearchReq = OneWaySearchReq
  { origin :: SearchReqLocation,
    destination :: SearchReqLocation,
    stops :: Maybe [SearchReqLocation],
    isSourceManuallyMoved :: Maybe Bool,
    isDestinationManuallyMoved :: Maybe Bool,
    isSpecialLocation :: Maybe Bool,
    startTime :: Maybe UTCTime,
    isReallocationEnabled :: Maybe Bool,
    fareParametersInRateCard :: Maybe Bool,
    quotesUnifiedFlow :: Maybe Bool,
    sessionToken :: Maybe Text,
    placeNameSource :: Maybe Text,
    driverIdentifier :: Maybe DRL.DriverIdentifier
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

data RentalSearchReq = RentalSearchReq
  { origin :: SearchReqLocation,
    stops :: Maybe [SearchReqLocation],
    isSourceManuallyMoved :: Maybe Bool,
    isSpecialLocation :: Maybe Bool,
    startTime :: UTCTime,
    estimatedRentalDistance :: Meters,
    estimatedRentalDuration :: Seconds,
    quotesUnifiedFlow :: Maybe Bool,
    isReallocationEnabled :: Maybe Bool,
    fareParametersInRateCard :: Maybe Bool,
    placeNameSource :: Maybe Text
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

data InterCitySearchReq = InterCitySearchReq
  { origin :: SearchReqLocation,
    stops :: Maybe [SearchReqLocation],
    roundTrip :: Bool,
    isSourceManuallyMoved :: Maybe Bool,
    isDestinationManuallyMoved :: Maybe Bool,
    isSpecialLocation :: Maybe Bool,
    startTime :: UTCTime,
    returnTime :: Maybe UTCTime,
    sessionToken :: Maybe Text,
    quotesUnifiedFlow :: Maybe Bool,
    isReallocationEnabled :: Maybe Bool,
    fareParametersInRateCard :: Maybe Bool,
    placeNameSource :: Maybe Text
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

data RouteDetails = RouteDetails
  { longestRouteDistance :: Maybe Meters,
    shortestRouteDistance :: Maybe Meters,
    shortestRouteDuration :: Maybe Seconds,
    shortestRouteStaticDuration :: Maybe Seconds,
    shortestRouteInfo :: Maybe Maps.RouteInfo,
    multipleRoutes :: Maybe [Maps.RouteInfo]
  }

data SearchDetails = SearchDetails
  { riderPreferredOption :: SearchRequest.RiderPreferredOption,
    origin :: SearchReqLocation,
    roundTrip :: Bool,
    stops :: [SearchReqLocation],
    isSourceManuallyMoved :: Maybe Bool,
    isSpecialLocation :: Maybe Bool,
    startTime :: UTCTime,
    returnTime :: Maybe UTCTime,
    hasStops :: Maybe Bool,
    isReallocationEnabled :: Maybe Bool,
    fareParametersInRateCard :: Maybe Bool,
    quotesUnifiedFlow :: Maybe Bool,
    placeNameSource :: Maybe Text,
    driverIdentifier_ :: Maybe DRL.DriverIdentifier
  }
  deriving (Generic, Show)

data JourneyPlannerLeg = JourneyPlannerLeg
  { mode :: DTrip.MultimodalTravelMode,
    agency :: Text,
    originGps :: LatLong,
    destinationGps :: LatLong,
    legOrder :: Int,
    estimatedDistance :: Distance,
    estimatedDuration :: Seconds,
    originAddress :: LocationAddress,
    destinationAddress :: LocationAddress
  }

data SearchReqLocation = SearchReqLocation
  { gps :: LatLong,
    address :: LocationAddress
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

buildSearchReqLoc ::
  MonadFlow m =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  SearchReqLocation ->
  m Location.Location
buildSearchReqLoc merchantId merchantOperatingCityId SearchReqLocation {..} = do
  now <- getCurrentTime
  locId <- generateGUID
  return
    Location.Location
      { id = locId,
        lat = gps.lat,
        lon = gps.lon,
        address = address,
        merchantId = Just merchantId,
        merchantOperatingCityId = Just merchantOperatingCityId,
        createdAt = now,
        updatedAt = now
      }
