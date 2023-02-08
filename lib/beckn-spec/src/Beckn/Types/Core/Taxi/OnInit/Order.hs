module Beckn.Types.Core.Taxi.OnInit.Order where

import Beckn.Types.Core.Taxi.OnInit.OrderState
import Beckn.Types.Core.Taxi.OnInit.Payment
import Beckn.Types.Core.Taxi.OnInit.Quote
import Data.OpenApi (ToSchema (..), defaultSchemaOptions)
import EulerHS.Prelude hiding (State, id, state)
import Kernel.Utils.Schema (genericDeclareUnNamedSchema)

data Order = Order
  { id :: Text,
    state :: OrderState,
    quote :: Quote,
    payment :: Payment
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance ToSchema Order where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
