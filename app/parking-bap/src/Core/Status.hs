module Core.Status where

import Beckn.Prelude
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Data.OpenApi (ToSchema (declareNamedSchema), defaultSchemaOptions)

newtype StatusMessage = StatusMessage
  { order :: Order
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema)

newtype Order = Order
  { id :: Text
  }
  deriving (Generic, FromJSON, ToJSON)

instance ToSchema Order where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
