module Beckn.Types.Core.Cabs.Common.Price
  ( Price (..),
  )
where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude

newtype Price = Price
  { value :: Double
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)
