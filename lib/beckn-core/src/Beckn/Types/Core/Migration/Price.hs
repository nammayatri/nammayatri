module Beckn.Types.Core.Migration.Price (Price (..)) where

import Beckn.Types.Core.Migration.DecimalValue (DecimalValue)
import EulerHS.Prelude

-- allOf case
data Price = Price
  { _currency :: Maybe Text,
    _value :: Maybe DecimalValue,
    _estimated_value :: Maybe DecimalValue,
    _computed_value :: Maybe DecimalValue,
    _listed_value :: Maybe DecimalValue,
    _offered_value :: Maybe DecimalValue,
    _minimum_value :: Maybe DecimalValue,
    _maximum_value :: Maybe DecimalValue
  }
  deriving (Generic, Show)

instance FromJSON Price where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Price where
  toJSON = genericToJSON stripAllLensPrefixOptions
