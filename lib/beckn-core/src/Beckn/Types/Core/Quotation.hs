module Beckn.Types.Core.Quotation where

import Beckn.Types.Core.Price
import Beckn.Utils.Example
import Data.Text
import Data.Time
import EulerHS.Prelude hiding (id)

data BreakupItem = BreakupItem
  { item_id :: Text,
    offer_id :: Text,
    title :: Text,
    price :: Price
  }
  deriving (Generic, FromJSON, ToJSON, Show)

data Quotation = Quotation
  { id :: Text,
    price :: Maybe Price,
    ttl :: Maybe UTCTime,
    breakup :: Maybe [BreakupItem]
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance Example Quotation where
  example =
    Quotation
      { id = idExample,
        price = example,
        ttl = example,
        breakup = Nothing
      }
