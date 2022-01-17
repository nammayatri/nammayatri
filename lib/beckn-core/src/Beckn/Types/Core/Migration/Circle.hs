module Beckn.Types.Core.Migration.Circle (Circle (..)) where

import Beckn.Types.Core.Migration.Gps (Gps)
import Beckn.Types.Core.Migration.Scalar (Scalar)
import Data.OpenApi (ToSchema)
import EulerHS.Prelude

data Circle = Circle
  { gps :: Gps,
    radius :: Scalar
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)
