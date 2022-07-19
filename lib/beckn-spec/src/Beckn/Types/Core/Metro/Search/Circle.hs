module Beckn.Types.Core.Metro.Search.Circle (Circle (..)) where

import Beckn.Types.Core.Metro.Search.Gps (Gps)
import Beckn.Types.Core.Metro.Search.Scalar (Scalar)
import Data.OpenApi (ToSchema)
import EulerHS.Prelude

data Circle = Circle
  { gps :: Gps,
    radius :: Scalar
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)
