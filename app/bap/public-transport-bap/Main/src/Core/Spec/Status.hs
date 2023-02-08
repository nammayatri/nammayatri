module Core.Spec.Status where

import Data.OpenApi (ToSchema (declareNamedSchema), defaultSchemaOptions)
import Kernel.Prelude
import Kernel.Utils.GenericPretty
import Kernel.Utils.Schema (genericDeclareUnNamedSchema)

newtype StatusMessage = StatusMessage
  { order :: Order
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, PrettyShow, ToSchema)

newtype Order = Order
  { id :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, PrettyShow)

instance ToSchema Order where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
