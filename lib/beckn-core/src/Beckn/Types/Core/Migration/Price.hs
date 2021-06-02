module Beckn.Types.Core.Migration.Price (Price (..)) where

import Beckn.Types.Core.Migration.DecimalValue (DecimalValue)
import Beckn.Utils.JSON
import EulerHS.Prelude

data Price = Price
  { currency :: Maybe Text,
    value :: Maybe DecimalValue,
    estimated_value :: Maybe DecimalValue,
    computed_value :: Maybe DecimalValue,
    listed_value :: Maybe DecimalValue,
    offered_value :: Maybe DecimalValue,
    minimum_value :: Maybe DecimalValue,
    maximum_value :: Maybe DecimalValue
  }
  deriving (Generic, Show)

instance FromJSON Price where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Price where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny
