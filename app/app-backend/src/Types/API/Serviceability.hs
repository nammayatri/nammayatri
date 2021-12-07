module Types.API.Serviceability where

import Beckn.Types.MapSearch (LatLong)
import Data.OpenApi (ToSchema)
import EulerHS.Prelude

newtype ServiceabilityReq = ServiceabilityReq
  { location :: LatLong
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

newtype ServiceabilityRes = ServiceabilityRes
  { serviceable :: Bool
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON, ToSchema)

data RideServiceabilityReq = RideServiceabilityReq
  { origin :: LatLong,
    destination :: LatLong
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

newtype RideServiceabilityRes = RideServiceabilityRes
  { serviceable :: Bool
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON, ToSchema)
