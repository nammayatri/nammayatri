module Beckn.Types.Core.City where

import Beckn.Utils.JSON
import Data.Text
import EulerHS.Prelude

data City = City
  { name :: Text,
    code :: Text
  }
  deriving (Generic, Show)

instance FromJSON City where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON City where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny
