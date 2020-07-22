module Beckn.Types.Core.Price where

import Beckn.Types.Core.DecimalValue
import Beckn.Utils.Common
import Data.Text
import EulerHS.Prelude

data Price = Price
  { _currency :: Text,
    _value :: DecimalValue,
    _estimated_value :: DecimalValue,
    _computed_value :: DecimalValue,
    _listed_value :: DecimalValue,
    _offered_value :: DecimalValue,
    _minimum_value :: DecimalValue,
    _maximum_value :: DecimalValue
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
