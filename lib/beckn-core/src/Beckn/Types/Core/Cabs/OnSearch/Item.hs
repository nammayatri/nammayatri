module Beckn.Types.Core.Cabs.OnSearch.Item
  ( module Beckn.Types.Core.Cabs.OnSearch.Item,
    module Reexport,
  )
where

import Beckn.Types.Core.Cabs.Common.Price as Reexport
import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)

data Item = Item
  { id :: Text,
    vehicle_variant :: Text,
    estimated_price :: Price,
    discount :: Maybe Price,
    discounted_price :: Price,
    nearest_driver_distance :: DecimalValue
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)
