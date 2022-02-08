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
  deriving (Generic, ToJSON, Show, FromJSON)

instance ToSchema Fulfillment where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

newtype StartEnd = StartEnd
  { time :: Time
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON)

instance ToSchema StartEnd where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
