-- module Beckn.Types.Core.Migration.Price (Price (..)) where
module Core.Price (Price (..)) where

-- import Core.DecimalValue (DecimalValue)
import Beckn.Types.Core.Migration.DecimalValue (DecimalValue (..))

import Data.Aeson
import Data.OpenApi (ToSchema)
import Data.Text
import GHC.Generics
--           import EulerHS.Prelude

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