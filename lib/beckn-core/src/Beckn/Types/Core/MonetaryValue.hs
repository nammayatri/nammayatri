module Beckn.Types.Core.MonetaryValue where

import Beckn.Types.Core.DecimalValue
import Beckn.Utils.Common
import EulerHS.Prelude

data MonetaryValue = MonetaryValue
  { _currency :: Text,
    _value :: DecimalValue
  }
  deriving (Eq, Generic, Show)

instance FromJSON MonetaryValue where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON MonetaryValue where
  toJSON = genericToJSON stripAllLensPrefixOptions

instance Example MonetaryValue where
  example =
    MonetaryValue
      { _currency = "INR",
        _value = example
      }
