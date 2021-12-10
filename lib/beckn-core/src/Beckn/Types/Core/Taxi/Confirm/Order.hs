module Beckn.Types.Core.Taxi.Confirm.Order
  ( module Beckn.Types.Core.Taxi.Confirm.Order,
  )
where

import Beckn.Utils.Example
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Data.OpenApi (ToSchema (..), defaultSchemaOptions)
import EulerHS.Prelude hiding (State, id, state)

data Order = Order
  { items :: [OrderItem],
    fulfillment :: Fulfillment
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

newtype Fulfillment = Fulfillment
  { customer :: Customer
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance ToSchema Fulfillment where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

newtype Customer = Customer
  { mobile_number :: Text
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance ToSchema Customer where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

instance Example Order where
  example =
    Order
      { items =
          [ OrderItem
              { id = "quote_id"
              }
          ],
        fulfillment =
          Fulfillment $
            Customer {mobile_number = "+99999999999"}
      }
