module Beckn.Types.Core.Quotation where

import Beckn.Types.Core.Price
import Beckn.Utils.Example
import Beckn.Utils.JSON
import Data.Text
import Data.Time
import EulerHS.Prelude

data BreakupItem = BreakupItem
  { item_id :: Text,
    offer_id :: Text,
    title :: Text,
    price :: Price
  }
  deriving (Generic, Show)

instance FromJSON BreakupItem where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON BreakupItem where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

data Quotation = Quotation
  { id :: Text,
    price :: Maybe Price,
    ttl :: Maybe UTCTime,
    breakup :: Maybe [BreakupItem]
  }
  deriving (Generic, Show)

instance FromJSON Quotation where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Quotation where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

instance Example Quotation where
  example =
    Quotation
      { id = idExample,
        price = example,
        ttl = example,
        breakup = Nothing
      }
