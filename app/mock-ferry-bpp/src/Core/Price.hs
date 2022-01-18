module Core.Price where

import Beckn.Prelude
import Beckn.Types.Core.Migration.DecimalValue
import Data.OpenApi (ToSchema)

data Price = Price
  { currency :: Text,
    value :: DecimalValue
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)
