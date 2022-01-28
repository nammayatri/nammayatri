module Core.OnStatus
  ( module Core.OnStatus,
  )
where

import Beckn.Prelude
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Core.Order
import Data.OpenApi (ToSchema (declareNamedSchema), defaultSchemaOptions)

newtype OnStatusMessage = OnStatusMessage
  { order :: Order
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

instance ToSchema OnStatusMessage where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
