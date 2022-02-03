module Core.Common.Quotation where

import Beckn.Types.Core.Migration.Duration
import Core.Common.Price
import Data.Aeson
import Relude

data Quotation = Quotation
  { price :: Price,
    breakup :: [BreakupItem],
    ttl :: Maybe Duration
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data BreakupItem = BreakupItem
  { title :: Text,
    price :: Price
  }
  deriving (Generic, Show, ToJSON, FromJSON)
