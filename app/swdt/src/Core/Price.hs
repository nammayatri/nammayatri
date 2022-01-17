module Core.Price (Price (..)) where

import Beckn.Prelude
import Beckn.Types.Core.Migration.DecimalValue (DecimalValue (..))
import Data.OpenApi (ToSchema)

data Price = Price
  { currency :: Text,
    value :: DecimalValue
    -- estimated_value :: Maybe DecimalValue,
    -- computed_value :: Maybe DecimalValue,
    -- listed_value :: Maybe DecimalValue,
    -- offered_value :: Maybe DecimalValue,
    -- minimum_value :: Maybe DecimalValue,
    -- maximum_value :: Maybe DecimalValue
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)
