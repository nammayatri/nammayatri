module Core.Common.Price where

import Beckn.Prelude
import Core.Common.DecimalValue (DecimalValue)

data Price = Price
  { currency :: Text,
    value :: DecimalValue
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)
