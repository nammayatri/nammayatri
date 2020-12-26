module Beckn.Types.Core.Migration.Circle (Circle (..)) where

import Beckn.Types.Core.Migration.Gps (Gps)
import Beckn.Types.Core.Migration.Scalar (Scalar)
import EulerHS.Prelude

data Circle = Circle
  { _gps :: Gps,
    _radius :: Scalar
  }
  deriving (Generic, Show)

instance FromJSON Circle where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Circle where
  toJSON = genericToJSON stripAllLensPrefixOptions
