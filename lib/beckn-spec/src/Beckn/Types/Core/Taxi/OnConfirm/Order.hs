module Beckn.Types.Core.Taxi.OnConfirm.Order where

import Beckn.Types.Core.Taxi.OnConfirm.Descriptor
import Beckn.Types.Core.Taxi.OnConfirm.Fulfillment
import Beckn.Types.Core.Taxi.OnConfirm.Payment
import Beckn.Types.Core.Taxi.OnConfirm.Quote
import Data.OpenApi (ToSchema (..), defaultSchemaOptions)
import EulerHS.Prelude hiding (State, id, state)
import Kernel.Utils.Schema (genericDeclareUnNamedSchema)

data Order = Order
  { id :: Text,
    state :: Text,
    items :: [OrderItem],
    fulfillment :: FulfillmentInfo,
    quote :: Quote,
    payment :: Payment
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance ToSchema Order where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

newtype OrderItem = OrderItem
  { descriptor :: Descriptor
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance ToSchema OrderItem where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
