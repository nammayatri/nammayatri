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
  deriving (Generic, FromJSON, ToJSON, ToSchema)

data NandiPattern = NandiPattern
  { id :: Text,
    desc :: Text,
    routeId :: Text
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema)

data NandiPatternDetails = NandiPatternDetails
  { id :: Text,
    desc :: Text,
    routeId :: Text,
    stops :: [NandiStop],
    trips :: [NandiTrip]
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema)

data NandiStop = NandiStop
  { id :: Text,
    code :: Text,
    name :: Text,
    lat :: Double,
    lon :: Double
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema)

data NandiTrip = NandiTrip
  { id :: Text,
    direction :: Text
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema)

data NandiRoutesRes = NandiRoutesRes
  { id :: Text,
    shortName :: Text,
    longName :: Text,
    mode :: Text,
    agencyName :: Text
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema)

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
