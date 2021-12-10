module Beckn.Types.Core.Taxi.OnConfirm.Order
  ( module Beckn.Types.Core.Taxi.OnConfirm.Order,
    module Reexport,
  )
where

import Beckn.Types.Core.Taxi.Common.Payment as Reexport
import Beckn.Utils.Example
import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (State, id, state)

data Order = Order
  { id :: Text,
    items :: [OrderItem],
    payment :: Payment
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
        payment = example
      }
