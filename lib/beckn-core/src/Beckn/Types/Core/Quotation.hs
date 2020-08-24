module Beckn.Types.Core.Quotation where

import Beckn.Types.Core.Duration
import Beckn.Types.Core.Price
import Beckn.Utils.Common
import Data.Text
import EulerHS.Prelude

data BreakupItem = BreakupItem
  { _item_id :: Text,
    _offer_id :: Text,
    _title :: Text,
    _price :: Price
  }
  deriving (Generic, Show)

instance FromJSON BreakupItem where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON BreakupItem where
  toJSON = genericToJSON stripAllLensPrefixOptions

data Quotation = Quotation
  { _id :: Text,
    _price :: Price,
    _ttl :: Maybe Duration,
    _breakup :: [BreakupItem]
  }
  deriving (Generic, Show)

instance FromJSON Quotation where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Quotation where
  toJSON = genericToJSON stripAllLensPrefixOptions

instance Example Quotation where
  example =
    Quotation
      { _id = idExample,
        _price = example,
        _ttl = example,
        _breakup = []
      }
