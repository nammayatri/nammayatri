module Beckn.Types.Core.Taxi.OnSearch.Price
  ( Price (..),
    module Reexport,
  )
where

import Beckn.Types.Core.Taxi.Common.DecimalValue as Reexport
import Kernel.Prelude

data Price = Price
  { currency :: Text,
    value :: DecimalValue
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)
