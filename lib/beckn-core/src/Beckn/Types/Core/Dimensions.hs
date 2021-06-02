module Beckn.Types.Core.Dimensions where

import Beckn.Types.Core.Scalar
import Beckn.Utils.Example
import Beckn.Utils.JSON
import EulerHS.Prelude

data Dimensions = Dimensions
  { length :: Scalar,
    breadth :: Scalar,
    height :: Scalar
  }
  deriving (Generic, Show)

instance FromJSON Dimensions where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Dimensions where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

instance Example Dimensions where
  example =
    Dimensions
      { length = example,
        breadth = example,
        height = example
      }
