module Beckn.Types.Core.Migration.Dimensions where

import Beckn.Types.Core.Migration.Scalar (Scalar)
import EulerHS.Prelude hiding (length)

data Dimensions = Dimensions
  { length :: Maybe Scalar,
    breadth :: Maybe Scalar,
    height :: Maybe Scalar
  }
  deriving (Generic, FromJSON, ToJSON, Show)
