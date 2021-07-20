module Beckn.Types.Core.Migration.Quotation where

import Beckn.Types.Core.Migration.Duration
import Beckn.Types.Core.Migration.Price
import Beckn.Utils.Example
import Beckn.Utils.JSON
import EulerHS.Prelude

data Quotation = Quotation
  { price :: Maybe Price,
    breakup :: Maybe [BreakupItem],
    ttl :: Maybe Duration
  }
  deriving (Generic, Show)

data BreakupItem = BreakupItem
  { title :: Maybe Text,
    price :: Maybe Price
  }
  deriving (Generic, Show)

instance FromJSON Quotation where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Quotation where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

instance Example Quotation where
  example =
    Quotation
      { price = Nothing,
        breakup = Nothing,
        ttl = Nothing
      }

instance FromJSON BreakupItem where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON BreakupItem where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny
