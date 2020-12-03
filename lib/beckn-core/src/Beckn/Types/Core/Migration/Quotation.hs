module Beckn.Types.Core.Migration.Quotation where

import Beckn.Types.Core.Migration.Duration
import Beckn.Types.Core.Migration.Price
import Beckn.Utils.JSON (constructorsWithHyphensToLowerOptions)
import EulerHS.Prelude

data Quotation = Quotation
  { _price :: Maybe Price,
    _breakup :: [BreakupItem],
    _ttl :: Maybe Duration
  }
  deriving (Generic, Show)

data BreakupItem = BreakupItem
  { _type :: Maybe BreakupItemType,
    _ref_id :: Maybe Text,
    _title :: Maybe Text,
    _price :: Maybe Price
  }
  deriving (Generic, Show)

data BreakupItemType
  = ITEM
  | OFFER
  | ADD_ON
  | FULFILLMENT
  deriving (Generic, Show)

instance FromJSON Quotation where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Quotation where
  toJSON = genericToJSON stripAllLensPrefixOptions

instance FromJSON BreakupItem where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON BreakupItem where
  toJSON = genericToJSON stripAllLensPrefixOptions

instance FromJSON BreakupItemType where
  parseJSON = genericParseJSON constructorsWithHyphensToLowerOptions

instance ToJSON BreakupItemType where
  toJSON = genericToJSON constructorsWithHyphensToLowerOptions
