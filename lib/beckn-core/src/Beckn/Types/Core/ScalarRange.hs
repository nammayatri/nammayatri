module Beckn.Types.Core.ScalarRange where

import Beckn.Utils.Example
import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (max, min)

data ScalarRange = ScalarRange
  { min :: Double,
    max :: Double
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

instance Example ScalarRange where
  example =
    ScalarRange
      { min = 0.00,
        max = 10000.00
      }
