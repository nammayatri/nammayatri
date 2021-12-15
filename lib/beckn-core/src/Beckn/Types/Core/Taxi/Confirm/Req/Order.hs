module Beckn.Types.Core.Taxi.Confirm.Req.Order
  ( module Beckn.Types.Core.Taxi.Confirm.Req.Order,
  )
where

import Beckn.Utils.Example
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Data.OpenApi (ToSchema (..), defaultSchemaOptions)
import EulerHS.Prelude hiding (State, id, state)

newtype Order = Order
  { items :: [OrderItem]
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
      { items = [OrderItem "quote_id"]
      }
