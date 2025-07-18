{-# LANGUAGE DuplicateRecordFields #-}

module SharedLogic.External.Nandi.Types where

import qualified BecknV2.FRFS.Enums
import Data.Aeson
import Domain.Types.Station
import qualified Kernel.External.Maps.Types
import Kernel.Prelude
import qualified Kernel.Types.Time

newtype NandiPatternsRes = NandiPatternsRes
  { patterns :: [NandiPattern]
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

data NandiPattern = NandiPattern
  { id :: Text,
    desc :: Text,
    routeId :: Text
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

data NandiPatternDetails = NandiPatternDetails
  { id :: Text,
    desc :: Maybe Text,
    routeId :: Text,
    stops :: [NandiStop],
    trips :: [NandiTrip]
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

data NandiStop = NandiStop
  { id :: Text,
    code :: Text,
    name :: Text,
    lat :: Double,
    lon :: Double
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

data RouteStopMappingInMemoryServer = RouteStopMappingInMemoryServer
  { estimatedTravelTimeFromPreviousStop :: Kernel.Prelude.Maybe Kernel.Types.Time.Seconds,
    providerCode :: Kernel.Prelude.Text,
    routeCode :: Kernel.Prelude.Text,
    sequenceNum :: Kernel.Prelude.Int,
    stopCode :: Kernel.Prelude.Text,
    stopName :: Kernel.Prelude.Text,
    stopPoint :: Kernel.External.Maps.Types.LatLong,
    vehicleType :: BecknV2.FRFS.Enums.VehicleCategory,
    geoJson :: Maybe Value,
    gates :: Maybe [Gate]
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

data VehicleServiceTypeResponse = VehicleServiceTypeResponse
  { service_type :: Text,
    vehicle_no :: Text,
    last_updated :: Maybe UTCTime,
    schedule_no :: Text
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

newtype StopCodeResponse = StopCodeResponse
  { stop_code :: Text
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

data TripInfoResponse = TripInfoResponse
  { tripId :: Text,
    routeId :: Text,
    routeName :: Text,
    direction :: Maybe Text,
    stops :: [StopInfo],
    schedule :: [StopSchedule],
    lastUpdated :: UTCTime,
    source :: Text
  }
  deriving (Show, Generic)

instance FromJSON TripInfoResponse

instance ToJSON TripInfoResponse

data StopInfo = StopInfo
  { stopId :: Text,
    stopCode :: Text,
    stopName :: Text,
    sequenceNum :: Int,
    lat :: Double,
    lon :: Double
  }
  deriving (Show, Generic)

instance FromJSON StopInfo where
  parseJSON = withObject "StopInfo" $ \v ->
    StopInfo
      <$> v .: "stopId"
      <*> v .: "stopCode"
      <*> v .: "stopName"
      <*> v .: "sequence"
      <*> v .: "lat"
      <*> v .: "lon"

instance ToJSON StopInfo where
  toJSON (StopInfo stopId stopCode stopName sequenceNum lat lon) =
    object
      [ "stopId" .= stopId,
        "stopCode" .= stopCode,
        "stopName" .= stopName,
        "sequence" .= sequenceNum,
        "lat" .= lat,
        "lon" .= lon
      ]

data StopSchedule = StopSchedule
  { stopCode :: Text,
    arrivalTime :: Int,
    departureTime :: Int,
    sequenceNum :: Int
  }
  deriving (Show, Generic)

instance FromJSON StopSchedule where
  parseJSON = withObject "StopSchedule" $ \v ->
    StopSchedule
      <$> v .: "stopCode"
      <*> v .: "arrivalTime"
      <*> v .: "departureTime"
      <*> v .: "sequence"

instance ToJSON StopSchedule where
  toJSON (StopSchedule stopCode arrivalTime departureTime sequenceNum) =
    object
      [ "stopCode" .= stopCode,
        "arrivalTime" .= arrivalTime,
        "departureTime" .= departureTime,
        "sequence" .= sequenceNum
      ]

data RouteInfoNandi = RouteInfoNandi
  { id :: Text,
    shortName :: Maybe Text,
    longName :: Maybe Text,
    mode :: BecknV2.FRFS.Enums.VehicleCategory,
    agencyName :: Maybe Text,
    tripCount :: Maybe Int,
    startPoint :: Kernel.External.Maps.Types.LatLong,
    endPoint :: Kernel.External.Maps.Types.LatLong,
    stopCount :: Maybe Int
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

data NandiTrip = NandiTrip
  { id :: Text,
    direction :: Maybe Text
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

data NandiRoutesRes = NandiRoutesRes
  { id :: Text,
    shortName :: Maybe Text,
    longName :: Maybe Text,
    mode :: Text,
    agencyName :: Maybe Text
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

data RouteStopMappingNandi = RouteStopMappingNandi
  { dailyTripCount :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    endPoint :: Kernel.External.Maps.Types.LatLong,
    routeLongName :: Kernel.Prelude.Text,
    routeShortName :: Kernel.Prelude.Text,
    startPoint :: Kernel.External.Maps.Types.LatLong,
    stopCount :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    estimatedTravelTimeFromPreviousStop :: Kernel.Prelude.Maybe Kernel.Types.Time.Seconds,
    routeCode :: Kernel.Prelude.Text,
    sequenceNum :: Kernel.Prelude.Int,
    stopCode :: Kernel.Prelude.Text,
    stopName :: Kernel.Prelude.Text,
    stopPoint :: Kernel.External.Maps.Types.LatLong,
    vehicleType :: BecknV2.FRFS.Enums.VehicleCategory
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

data GtfsGraphQLRequest = GtfsGraphQLRequest
  { query :: Text,
    variables :: Maybe Value,
    operation_name :: Maybe Text,
    city :: Maybe Text, -- todo: remove this
    feedId :: Text
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)
