module Types.API.Ride where

import EulerHS.Prelude hiding (id)

data GetDriverLocRes = GetDriverLocRes
  { lat :: Double,
    long :: Double
  }
  deriving (Generic, Show, FromJSON, ToJSON)