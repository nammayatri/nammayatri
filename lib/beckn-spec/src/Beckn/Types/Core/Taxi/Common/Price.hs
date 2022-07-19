module Beckn.Types.Core.Taxi.Common.Price
  ( Price (..),
    module Reexport,
  )
where

import Beckn.Prelude
import Beckn.Types.Core.Taxi.Common.DecimalValue as Reexport

newtype Price = Price
  { value :: DecimalValue
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)
