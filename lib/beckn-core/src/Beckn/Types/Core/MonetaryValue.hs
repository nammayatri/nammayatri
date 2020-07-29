module Beckn.Types.Core.MonetaryValue where

import Beckn.Types.Core.DecimalValue
import Beckn.Utils.Common
import Data.Text
import EulerHS.Prelude

data MonetaryValue = MonetaryValue
  { currency :: Text,
    value :: DecimalValue
  }
  deriving (Generic, Show, ToJSON, FromJSON)

instance Example MonetaryValue where
  example =
    MonetaryValue
      { currency = "INR",
        value = example
      }
