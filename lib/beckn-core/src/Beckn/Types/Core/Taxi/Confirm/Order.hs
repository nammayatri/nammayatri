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
  { contact :: Contact
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance ToSchema Customer where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

newtype Contact = Contact
  { phone :: Phone
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance ToSchema Contact where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

data Phone = Phone
  { country_code :: Text,
    number :: Text
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance ToSchema Phone where
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
            Customer
              { contact =
                  Contact
                    { phone =
                        Phone
                          { country_code = "+9",
                            number = "9999999999"
                          }
                    }
              }
      }
