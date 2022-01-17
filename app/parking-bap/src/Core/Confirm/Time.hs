module Core.Confirm.Time where

import Beckn.Prelude
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Data.OpenApi (ToSchema (declareNamedSchema), defaultSchemaOptions)

newtype Time = Time
  { timestamp :: UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)

instance ToSchema Time where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
