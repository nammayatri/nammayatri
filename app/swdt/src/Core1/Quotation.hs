module Core1.Quotation where

import Beckn.Prelude
import Beckn.Types.Core.Migration.Duration
import Core1.Price

data OnInitQuotation = OnInitQuotation
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

type ConfirmQuotation = OnInitQuotation

type OnConfirmQuotation = OnInitQuotation
