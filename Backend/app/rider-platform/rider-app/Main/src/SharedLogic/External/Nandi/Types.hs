{-# LANGUAGE DuplicateRecordFields #-}

module SharedLogic.External.Nandi.Types where

import qualified BecknV2.FRFS.Enums
import Data.Aeson
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

-- StopGeojson type that matches the Rust StopGeojson struct
data StopGeojson = StopGeojson
  { stopCode :: Kernel.Prelude.Text,
    gtfsId :: Kernel.Prelude.Text,
    geoJson :: Kernel.Prelude.Text,
    gates :: Kernel.Prelude.Text
  }
  deriving (Generic, ToSchema, Show)

instance FromJSON StopGeojson where
  parseJSON = withObject "StopGeojson" $ \o -> do
    stopCode <- o .: "stop_code"
    gtfsId <- o .: "gtfs_id"
    geoJson <- o .: "geo_json"
    gates <- o .: "gates"
    pure StopGeojson {..}

instance ToJSON StopGeojson where
  toJSON StopGeojson {..} =
    object
      [ "stop_code" .= stopCode,
        "gtfs_id" .= gtfsId,
        "geo_json" .= geoJson,
        "gates" .= gates
      ]

data RouteStopMappingInMemoryServer = RouteStopMappingInMemoryServer
  { estimatedTravelTimeFromPreviousStop :: Kernel.Prelude.Maybe Kernel.Types.Time.Seconds,
    providerCode :: Kernel.Prelude.Text,
    routeCode :: Kernel.Prelude.Text,
    sequenceNum :: Kernel.Prelude.Int,
    stopCode :: Kernel.Prelude.Text,
    stopName :: Kernel.Prelude.Text,
    stopPoint :: Kernel.External.Maps.Types.LatLong,
    vehicleType :: BecknV2.FRFS.Enums.VehicleCategory
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

data RouteStopMappingInMemoryServerWithGeojson = RouteStopMappingInMemoryServerWithGeojson
  { estimatedTravelTimeFromPreviousStop :: Kernel.Prelude.Maybe Kernel.Types.Time.Seconds,
    providerCode :: Kernel.Prelude.Text,
    routeCode :: Kernel.Prelude.Text,
    sequenceNum :: Kernel.Prelude.Int,
    stopCode :: Kernel.Prelude.Text,
    stopName :: Kernel.Prelude.Text,
    stopPoint :: Kernel.External.Maps.Types.LatLong,
    vehicleType :: BecknV2.FRFS.Enums.VehicleCategory,
    stopInfo :: Kernel.Prelude.Maybe StopGeojson
  }
  deriving (Generic, ToSchema, Show)

instance FromJSON RouteStopMappingInMemoryServerWithGeojson where
  parseJSON = withObject "RouteStopMappingInMemoryServerWithGeojson" $ \o -> do
    estimatedTravelTimeFromPreviousStop <- o .:? "estimatedTravelTimeFromPreviousStop"
    providerCode <- o .: "providerCode"
    routeCode <- o .: "routeCode"
    sequenceNum <- o .: "sequenceNum"
    stopCode <- o .: "stopCode"
    stopName <- o .: "stopName"
    stopPoint <- o .: "stopPoint"
    vehicleType <- o .: "vehicleType"
    stopInfo <- o .:? "stopGeojson"
    pure RouteStopMappingInMemoryServerWithGeojson {..}

instance ToJSON RouteStopMappingInMemoryServerWithGeojson where
  toJSON RouteStopMappingInMemoryServerWithGeojson {..} =
    object
      [ "estimatedTravelTimeFromPreviousStop" .= estimatedTravelTimeFromPreviousStop,
        "providerCode" .= providerCode,
        "routeCode" .= routeCode,
        "sequenceNum" .= sequenceNum,
        "stopCode" .= stopCode,
        "stopName" .= stopName,
        "stopPoint" .= stopPoint,
        "vehicleType" .= vehicleType,
        "stopGeojson" .= stopInfo
      ]

data VehicleServiceTypeResponse = VehicleServiceTypeResponse
  { service_type :: Text,
    vehicle_no :: Text,
    last_updated :: Maybe UTCTime,
    schedule_no :: Text
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

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
