module Beckn.Types.Core.ScalarRange where

import Beckn.Utils.Example
import Beckn.Utils.JSON
import EulerHS.Prelude hiding (max, min)

data ScalarRange = ScalarRange
  { min :: Double,
    max :: Double
  }
  deriving (Generic, Show)

instance FromJSON ScalarRange where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON ScalarRange where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

instance Example ScalarRange where
  example =
    ScalarRange
      { min = 0.00,
        max = 10000.00
      }
