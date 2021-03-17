module Beckn.Types.Core.ScalarRange where

import Beckn.Utils.Example
import EulerHS.Prelude

data ScalarRange = ScalarRange
  { _min :: Double,
    _max :: Double
  }
  deriving (Generic, Show)

instance FromJSON ScalarRange where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON ScalarRange where
  toJSON = genericToJSON stripAllLensPrefixOptions

instance Example ScalarRange where
  example =
    ScalarRange
      { _min = 0.00,
        _max = 10000.00
      }
