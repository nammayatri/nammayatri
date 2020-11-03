module Types.API.Serviceability where

import EulerHS.Prelude
import Types.Common

newtype ServiceabilityReq = ServiceabilityReq
  { location :: GPS
  }
  deriving (Generic, Show, FromJSON, ToJSON)

newtype ServiceabilityRes = ServiceabilityRes
  { serviceable :: Bool
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

data RideServiceabilityReq = RideServiceabilityReq
  { origin :: GPS,
    destination :: GPS
  }
  deriving (Generic, Show, FromJSON, ToJSON)

newtype RideServiceabilityRes = RideServiceabilityRes
  { serviceable :: Bool
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)
