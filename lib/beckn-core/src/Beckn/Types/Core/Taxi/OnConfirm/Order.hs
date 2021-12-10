module Beckn.Types.Core.Taxi.OnConfirm.Order
  ( module Beckn.Types.Core.Taxi.OnConfirm.Order,
    module Reexport,
  )
where

import Beckn.Types.Core.Taxi.Common.Price as Reexport
import Beckn.Utils.Example
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Data.OpenApi (ToSchema (..), defaultSchemaOptions)
import EulerHS.Prelude hiding (State, id, state)

data Order = Order
  { id :: Text,
    items :: [OrderItem],
    estimated_total_fare :: Price
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance ToSchema Order where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

newtype OrderItem = OrderItem
  { id :: Text
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance ToSchema OrderItem where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

instance Example Order where
  example =
    Order
      { id = "ride_booking_id",
        items = [OrderItem "quote_id"],
        estimated_total_fare = example
      }
