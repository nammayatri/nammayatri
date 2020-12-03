module Beckn.Types.Core.Migration.MonetaryValue (MonetaryValue (..)) where

import Beckn.Types.Core.Migration.DecimalValue (DecimalValue)
import EulerHS.Prelude

data MonetaryValue = MonetaryValue
  { _currency :: Maybe Text,
    _value :: Maybe DecimalValue
  }
  deriving (Eq, Generic, Show)

instance FromJSON MonetaryValue where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON MonetaryValue where
  toJSON = genericToJSON stripAllLensPrefixOptions
