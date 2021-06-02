module Beckn.Types.Core.Migration.Circle (Circle (..)) where

import Beckn.Types.Core.Migration.Gps (Gps)
import Beckn.Types.Core.Migration.Scalar (Scalar)
import Beckn.Utils.JSON
import EulerHS.Prelude

data Circle = Circle
  { gps :: Gps,
    radius :: Scalar
  }
  deriving (Generic, Show)

instance FromJSON Circle where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Circle where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny
