module Lib.GtfsDataServer.Domain.Types.RouteStopMapping where

import qualified BecknV2.FRFS.Enums
import qualified Kernel.External.Maps.Types
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.TimeBound

data RouteStopMapping = RouteStopMapping
  { createdAt :: UTCTime,
    estimatedTravelTimeFromPreviousStop :: Maybe Kernel.Types.Common.Seconds,
    integratedBppConfigId :: Text,
    merchantId :: Text,
    merchantOperatingCityId :: Text,
    providerCode :: Text,
    routeCode :: Text,
    sequenceNum :: Int,
    stopCode :: Text,
    stopName :: Text,
    stopPoint :: Kernel.External.Maps.Types.LatLong,
    timeBounds :: Kernel.Types.TimeBound.TimeBound,
    updatedAt :: UTCTime,
    vehicleType :: BecknV2.FRFS.Enums.VehicleCategory
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON, Ord)
