module Types.API.Serviceability where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude
import Types.Common

newtype ServiceabilityReq = ServiceabilityReq
  { location :: GPS
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

newtype ServiceabilityRes = ServiceabilityRes
  { serviceable :: Bool
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON, ToSchema)

data RideServiceabilityReq = RideServiceabilityReq
  { origin :: GPS,
    destination :: GPS
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

newtype RideServiceabilityRes = RideServiceabilityRes
  { serviceable :: Bool
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON, ToSchema)
