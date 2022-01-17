module Core.Confirm.Fulfillment where

import Beckn.Prelude
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Core.Confirm.StartEnd
import Core.Confirm.Vehicle
import Data.OpenApi (ToSchema (declareNamedSchema), defaultSchemaOptions)

data Fulfillment = Fulfillment
  { start :: StartEnd,
    end :: StartEnd,
    vehicle :: Vehicle
  }
  deriving (Generic, ToJSON)

instance ToSchema Fulfillment where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
