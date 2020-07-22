module Beckn.Types.Core.DecimalValue where

import Data.Text
import EulerHS.Prelude

data DecimalValue = DecimalValue
  { _integral :: Text,
    _fraction :: Text
  }
  deriving (Generic, Show)

instance FromJSON DecimalValue where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON DecimalValue where
  toJSON = genericToJSON stripAllLensPrefixOptions
