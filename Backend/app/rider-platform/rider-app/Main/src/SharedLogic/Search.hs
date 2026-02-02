module SharedLogic.Search where

import qualified API.Types.UI.RiderLocation as RL
import qualified BecknV2.OnDemand.Enums as Enums
import qualified BecknV2.OnDemand.Tags as Beckn
import Data.Aeson
import Data.List (group, sort)
import Data.OpenApi hiding (Header, description, email)
import qualified Data.OpenApi as OpenApi hiding (Header)
import Data.Ord (comparing)
import qualified Domain.Types.IntegratedBPPConfig as DIBPC
import Domain.Types.Location as Location
import Domain.Types.LocationAddress
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.NyRegularSubscription as DNyRegularSubscription
import qualified Domain.Types.RecentLocation as DTRL
import qualified Domain.Types.RefereeLink as DRL
import qualified Domain.Types.RiderPreferredOption as DRPO
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
    riderPreferredOption :: DRPO.RiderPreferredOption,
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

data SearchReq = OneWaySearch OneWaySearchReq | RentalSearch RentalSearchReq | InterCitySearch InterCitySearchReq | AmbulanceSearch OneWaySearchReq | DeliverySearch OneWaySearchReq | PTSearch PublicTransportSearchReq | FixedRouteSearch FixedRouteSearchReq
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
  "FixedRouteSearch" -> "FIXED_ROUTE"
  x -> x

data OneWaySearchReq = OneWaySearchReq
  { origin :: SearchReqLocation,
    destination :: Maybe SearchReqLocation,
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
    driverIdentifier :: Maybe DRL.DriverIdentifier,
    isMeterRideSearch :: Maybe Bool,
    recentLocationId :: Maybe (Id DTRL.RecentLocation),
    platformType :: Maybe DIBPC.PlatformType,
    isReserveRide :: Maybe Bool,
    subscriptionId :: Maybe (Id DNyRegularSubscription.NyRegularSubscription),
    verifyBeforeCancellingOldBooking :: Maybe Bool,
    numberOfLuggages :: Maybe Int,
    doMultimodalSearch :: Maybe Bool
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

data PublicTransportSearchReq = PublicTransportSearchReq
  { origin :: SearchReqLocation,
    destination :: Maybe SearchReqLocation,
    originStopCode :: Text,
    destinationStopCode :: Text,
    startTime :: Maybe UTCTime,
    routeCode :: Maybe Text,
    vehicleNumber :: Maybe Text,
    recentLocationId :: Maybe (Id DTRL.RecentLocation),
    vehicleCategory :: Maybe Enums.VehicleCategory,
    platformType :: Maybe DIBPC.PlatformType,
    currentLocation :: Maybe LatLong,
    busLocationData :: Maybe [RL.BusLocation],
    firstMileRemoved :: Maybe Bool,
    doMultimodalSearch :: Maybe Bool
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
    placeNameSource :: Maybe Text,
    recentLocationId :: Maybe (Id DTRL.RecentLocation),
    numberOfLuggages :: Maybe Int,
    doMultimodalSearch :: Maybe Bool
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
    placeNameSource :: Maybe Text,
    recentLocationId :: Maybe (Id DTRL.RecentLocation),
    platformType :: Maybe DIBPC.PlatformType,
    numberOfLuggages :: Maybe Int,
    doMultimodalSearch :: Maybe Bool
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

data FixedRouteSearchReq = FixedRouteSearchReq
  { fromSpecialLocationId :: Text, -- SpecialLocation ID for origin area
    toSpecialLocationId :: Text, -- SpecialLocation ID for destination area
    startTime :: Maybe UTCTime,
    numberOfLuggages :: Maybe Int,
    origin :: SearchReqLocation,
    destination :: SearchReqLocation,
    doMultimodalSearch :: Maybe Bool
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
  { riderPreferredOption :: DRPO.RiderPreferredOption,
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
    -- isPetRide :: Maybe Bool,
    numberOfLuggages :: Maybe Int,
    driverIdentifier_ :: Maybe DRL.DriverIdentifier,
    recentLocationId :: Maybe (Id DTRL.RecentLocation),
    routeCode :: Maybe Text,
    destinationStopCode :: Maybe Text,
    originStopCode :: Maybe Text,
    vehicleCategory :: Maybe Enums.VehicleCategory,
    platformType :: Maybe DIBPC.PlatformType,
    currentLocation :: Maybe LatLong,
    busLocationData :: [RL.BusLocation],
    fromSpecialLocationId :: Maybe Text, -- Fixed route: origin area ID
    toSpecialLocationId :: Maybe Text -- Fixed route: destination area ID
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

-- Get the most frequent element in the list

mostFrequent :: (Ord a) => [a] -> Maybe a
mostFrequent [] = Nothing
mostFrequent xs = Just $ fst $ maximumBy (comparing snd) frequencyList
  where
    grouped = group . sort $ xs
    frequencyList = [(head g, length g) | g <- grouped]
