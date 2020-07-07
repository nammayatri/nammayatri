module Beckn.Types.Core.Price where

import Beckn.Types.Core.Currency
import Data.Text
import EulerHS.Prelude

data Price = Price
  { _currency :: Text,
    _estimated_value :: Money,
    _computed_value :: Money,
    _listed_value :: Money,
    _offered_value :: Money,
    _unit :: Text,
    _discount :: Money,
    _tax :: Maybe Tax
  }
  deriving (Generic, Show)

instance FromJSON Price where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Price where
  toJSON = genericToJSON stripAllLensPrefixOptions

data Tax = Tax
  { _computed :: Money,
    _breakup :: [TaxBreakup]
  }
  deriving (Generic, Show)

instance FromJSON Tax where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Tax where
  toJSON = genericToJSON stripAllLensPrefixOptions

data TaxBreakup = TaxBreakup
  { _line_item :: Text,
    _amount :: Money
  }
  deriving (Generic, Show)

instance FromJSON TaxBreakup where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON TaxBreakup where
  toJSON = genericToJSON stripAllLensPrefixOptions
