module Beckn.Types.Core.Taxi.Select.Order where

import Beckn.Types.Core.Taxi.Select.Descriptor
import Beckn.Types.Core.Taxi.Select.Fulfillment
--import Beckn.Types.Core.Taxi.Select.Payment

import Data.OpenApi (ToSchema (..), defaultSchemaOptions)
import EulerHS.Prelude hiding (State, id, state)
import Kernel.Utils.Schema (genericDeclareUnNamedSchema)

data Order = Order
  { items :: [OrderItem],
    fulfillment :: FulfillmentInfo
    --    payment :: Payment
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance ToSchema Order where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

data OrderItem = OrderItem
  { id :: Maybe Text, -- for those cases where SELECT API can't be stateless
    descriptor :: Descriptor
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance ToSchema OrderItem where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
