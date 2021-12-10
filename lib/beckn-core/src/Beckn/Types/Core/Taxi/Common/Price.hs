module Beckn.Types.Core.Taxi.Common.Price
  ( Price (..),
    module Reexport,
  )
where

import Beckn.Types.Core.Taxi.Common.DecimalValue as Reexport
import Data.OpenApi (ToSchema)
import EulerHS.Prelude

newtype Price = Price
  { value :: DecimalValue
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)
