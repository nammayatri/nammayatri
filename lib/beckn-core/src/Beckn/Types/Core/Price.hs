module Beckn.Types.Core.Price where

import Beckn.Types.Core.Amount
import Beckn.Utils.Common
import Data.Text
import EulerHS.Prelude

data Price = Price
  { _currency :: Text,
    _estimated_value :: Amount,
    _computed_value :: Amount,
    _listed_value :: Amount,
    _offered_value :: Amount,
    _unit :: Text,
    _discount :: Amount,
    _tax :: Maybe Tax
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
        _unit = "1",
        _discount = example,
        _tax = example
      }

data Tax = Tax
  { _computed :: Amount,
    _breakup :: [TaxBreakup]
  }
  deriving (Generic, Show)

instance FromJSON Tax where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Tax where
  toJSON = genericToJSON stripAllLensPrefixOptions

instance Example Tax where
  example =
    Tax
      { _computed = example,
        _breakup = example
      }

data TaxBreakup = TaxBreakup
  { _line_item :: Text,
    _amount :: Amount
  }
  deriving (Generic, Show)

instance FromJSON TaxBreakup where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON TaxBreakup where
  toJSON = genericToJSON stripAllLensPrefixOptions

instance Example TaxBreakup where
  example =
    TaxBreakup
      { _line_item = "",
        _amount = example
      }
