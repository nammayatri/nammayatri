module Core.Quotation where

import Beckn.Prelude
import Beckn.Types.Core.Migration.Duration
import Core.Price

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

type OnStatusQuotation = OnInitQuotation
