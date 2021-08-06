module Beckn.Types.Core.MonetaryValue where

import Beckn.Types.Core.DecimalValue
import Beckn.Utils.Example
import EulerHS.Prelude

data MonetaryValue = MonetaryValue
  { currency :: Text,
    value :: DecimalValue
  }
  deriving (Eq, Generic, FromJSON, ToJSON, Show)

instance Example MonetaryValue where
  example =
    MonetaryValue
      { currency = "INR",
        value = example
      }
