module Core.Confirm.StartEnd where

import Beckn.Prelude
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Core.Time (Time)
import Data.OpenApi (ToSchema (declareNamedSchema), defaultSchemaOptions)

newtype StartEnd = StartEnd
  { time :: Time
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

instance ToSchema StartEnd where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
