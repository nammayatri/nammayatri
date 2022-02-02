module Core.Confirm.Fulfillment where

import Beckn.Prelude
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Core.Common.Time
import Core.Common.Vehicle
import Data.OpenApi (ToSchema (declareNamedSchema), defaultSchemaOptions)

data Fulfillment = Fulfillment
  { start :: StartEnd,
    end :: StartEnd,
    vehicle :: Vehicle
  }
  deriving (Generic, ToJSON)

instance ToSchema Fulfillment where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

newtype StartEnd = StartEnd
  { time :: Time
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

instance ToSchema StartEnd where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
