module Beckn.Types.Core.Migration.Dimensions where

import Beckn.Types.Core.Migration.Scalar (Scalar)
import Beckn.Utils.JSON
import EulerHS.Prelude

data Dimensions = Dimensions
  { length :: Maybe Scalar,
    breadth :: Maybe Scalar,
    height :: Maybe Scalar
  }
  deriving (Generic, Show)

instance FromJSON Dimensions where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Dimensions where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny
