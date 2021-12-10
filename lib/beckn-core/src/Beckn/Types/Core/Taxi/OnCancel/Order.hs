module Beckn.Types.Core.Taxi.OnCancel.Order where

import Beckn.Utils.Example
import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (State, id, state)

newtype Order = Order
  { id :: Text
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

instance Example Order where
  example =
    Order
      { id = "ride_booking_id"
      }
