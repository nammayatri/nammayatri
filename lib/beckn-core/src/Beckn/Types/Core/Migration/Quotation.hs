module Beckn.Types.Core.Migration.Quotation where

import Beckn.Types.Core.Migration.Duration
import Beckn.Types.Core.Migration.Price
import Beckn.Utils.Example
import EulerHS.Prelude

data Quotation = Quotation
  { price :: Maybe Price,
    breakup :: Maybe [BreakupItem],
    ttl :: Maybe Duration
  }
  deriving (Generic, FromJSON, ToJSON, Show)

data BreakupItem = BreakupItem
  { title :: Maybe Text,
    price :: Maybe Price
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance Example Quotation where
  example =
    Quotation
      { price = Nothing,
        breakup = Nothing,
        ttl = Nothing
      }
