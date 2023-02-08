module Beckn.Types.Core.Taxi.Common.Price
  ( Price (..),
    module Reexport,
  )
where

import Beckn.Types.Core.Taxi.Common.DecimalValue as Reexport
import Kernel.Prelude

newtype Price = Price
  { value :: DecimalValue
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)
