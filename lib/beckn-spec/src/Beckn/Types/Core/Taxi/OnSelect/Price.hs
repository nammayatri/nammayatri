module Beckn.Types.Core.Taxi.OnSelect.Price
  ( Price (..),
    module Reexport,
  )
where

import Beckn.Prelude
import Beckn.Types.Core.Taxi.Common.DecimalValue as Reexport

data Price = Price
  { currency :: Text,
    value :: DecimalValue
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)
