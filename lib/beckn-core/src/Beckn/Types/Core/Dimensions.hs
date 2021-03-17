module Beckn.Types.Core.Dimensions where

import Beckn.Types.Core.Scalar
import Beckn.Utils.Example
import EulerHS.Prelude

data Dimensions = Dimensions
  { _length :: Scalar,
    _breadth :: Scalar,
    _height :: Scalar
  }
  deriving (Generic, Show)

instance FromJSON Dimensions where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Dimensions where
  toJSON = genericToJSON stripAllLensPrefixOptions

instance Example Dimensions where
  example =
    Dimensions
      { _length = example,
        _breadth = example,
        _height = example
      }
