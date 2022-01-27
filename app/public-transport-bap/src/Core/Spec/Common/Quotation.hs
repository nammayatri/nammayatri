module Core.Spec.Common.Quotation where

import Beckn.Prelude
import Beckn.Types.Core.Migration.Duration
import Beckn.Utils.GenericPretty (PrettyShow)
import Core.Spec.Common.Price

data Quotation = Quotation
  { price :: Price,
    breakup :: [BreakupItem],
    ttl :: Maybe Duration
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema, PrettyShow)

data BreakupItem = BreakupItem
  { title :: Text,
    price :: Price
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema, PrettyShow)
