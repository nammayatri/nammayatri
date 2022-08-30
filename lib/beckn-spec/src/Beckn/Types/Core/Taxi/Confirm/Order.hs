module Beckn.Types.Core.Taxi.Confirm.Order
  ( module Beckn.Types.Core.Taxi.Confirm.Order,
  )
where

import Beckn.Types.Core.Taxi.Confirm.Fulfillment
import Beckn.Types.Core.Taxi.Confirm.Payment
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Data.OpenApi (ToSchema (..), defaultSchemaOptions)
import EulerHS.Prelude hiding (State, id, state)

data Order = Order
  { id :: Text,
    fulfillment :: FulfillmentInfo,
    customer :: OrderCustomer,
    payment :: Payment
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance ToSchema Order where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

data OrderCustomer = OrderCustomer
  { contact :: Contact,
    person :: Maybe OrderPerson
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance ToSchema OrderCustomer where
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

newtype OrderPerson = OrderPerson
  { name :: Text
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance ToSchema OrderPerson where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
