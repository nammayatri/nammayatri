module Beckn.Types.Core.Migration.Dimensions where

import Beckn.Types.Core.Migration.Scalar (Scalar)
import EulerHS.Prelude

data Dimensions = Dimensions
  { _length :: Maybe Scalar,
    _breadth :: Maybe Scalar,
    _height :: Maybe Scalar
  }
  deriving (Generic, Show)

instance FromJSON Dimensions where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Dimensions where
  toJSON = genericToJSON stripAllLensPrefixOptions
