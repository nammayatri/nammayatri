module Beckn.Types.Core.Migration1.OnConfirm.Order
  ( module Beckn.Types.Core.Migration1.OnConfirm.Order,
    module Reexport,
  )
where

import Beckn.Types.Core.Migration1.Common.Payment as Reexport
import Beckn.Types.Core.Migration1.OnConfirm.Provider
import Beckn.Utils.Example
import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (State, id, state)

data Order = Order
  { id :: Text,
    provider :: Provider,
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
        provider = example,
        items = [OrderItem "quote_id"],
        payment = example
      }
