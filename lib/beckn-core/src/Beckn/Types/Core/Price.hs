module Beckn.Types.Core.Price where

import Beckn.Types.Core.Amount
import Beckn.Types.Core.ScalarRange
import Beckn.Utils.Common
import Data.Text
import EulerHS.Prelude

data Price = Price
  { _currency :: Text,
    _estimated_value :: Amount,
    _computed_value :: Amount,
    _listed_value :: Amount,
    _offered_value :: Amount,
    _range :: Maybe ScalarRange,
    _breakup :: [Breakup]
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
        _estimated_value = example,
        _computed_value = example,
        _listed_value = example,
        _offered_value = example,
        _range = example,
        _breakup = example
      }

data Breakup = Breakup
  { _title :: Text,
    _amount :: Amount
  }
  deriving (Generic, Show)

instance FromJSON Breakup where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Breakup where
  toJSON = genericToJSON stripAllLensPrefixOptions

instance Example Breakup where
  example =
    Breakup
      { _title = "line item 1",
        _amount = example
      }
