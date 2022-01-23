module Core.Price where

import Beckn.Types.Core.Migration.DecimalValue
import Data.Aeson
import Data.OpenApi (ToSchema)
import Relude

data Price = Price
  { currency :: Text,
    value :: DecimalValue
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)
