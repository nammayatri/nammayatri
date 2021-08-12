module Types.API.Ride where

import EulerHS.Prelude hiding (id)
import Data.OpenApi (ToSchema)

data GetDriverLocRes = GetDriverLocRes
  { lat :: Double,
    long :: Double
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)