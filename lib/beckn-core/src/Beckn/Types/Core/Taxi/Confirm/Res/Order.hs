module Beckn.Types.Core.Taxi.Confirm.Res.Order
  ( module Beckn.Types.Core.Taxi.Confirm.Res.Order,
    module Reexport,
  )
where

import Beckn.Types.Core.Taxi.Common.Price as Reexport
import Beckn.Utils.Example
import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (State, id, state)

data Order = Order
  { id :: Text,
    items :: [OrderItem],
    estimated_total_fare :: Price
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

newtype OrderItem = OrderItem
  { id :: Text
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

instance Example Order where
  example =
    Order
      { id = "ride_booking_id",
        items = [OrderItem "quote_id"],
        estimated_total_fare = example
      }
