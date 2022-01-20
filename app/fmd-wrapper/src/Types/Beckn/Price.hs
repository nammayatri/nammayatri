module Types.Beckn.Price (Price (..)) where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude
import Types.Beckn.DecimalValue (DecimalValue)

data Price = Price
  { currency :: Text,
    estimated_value :: DecimalValue
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)
