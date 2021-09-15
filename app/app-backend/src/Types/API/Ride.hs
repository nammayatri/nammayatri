module Types.API.Ride where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)

data GetDriverLocRes = GetDriverLocRes
  { lat :: Double,
    long :: Double
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)
