module Beckn.Types.Core.Price where

import Beckn.Types.Core.DecimalValue
import Beckn.Utils.Common
import Data.Text
import EulerHS.Prelude

data Price = Price
  { _currency :: Text,
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

instance Example Price where
  example =
    Price
      { _currency = "INR",
        _value = example,
        _estimated_value = example,
        _computed_value = example,
        _listed_value = example,
        _offered_value = example,
        _minimum_value = example,
        _maximum_value = example
      }
