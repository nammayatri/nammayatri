module Beckn.Types.Core.MonetaryValue where

import Beckn.Types.Core.DecimalValue
import Beckn.Utils.Example
import Beckn.Utils.JSON
import EulerHS.Prelude

data MonetaryValue = MonetaryValue
  { currency :: Text,
    value :: DecimalValue
  }
  deriving (Eq, Generic, Show)

instance FromJSON MonetaryValue where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON MonetaryValue where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

instance Example MonetaryValue where
  example =
    MonetaryValue
      { currency = "INR",
        value = example
      }
