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

data GtfsGraphQLRequest = GtfsGraphQLRequest
  { query :: Text,
    variables :: Maybe Value,
    operation_name :: Maybe Text,
    city :: Maybe Text
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)
