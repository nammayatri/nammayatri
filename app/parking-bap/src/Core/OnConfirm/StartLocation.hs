module Core.OnConfirm.StartLocation where

import Beckn.Prelude
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Core.OnConfirm.Descriptor
import Data.OpenApi (ToSchema (declareNamedSchema), defaultSchemaOptions)

data StartLocation = StartLocation
  { id :: Text,
    descriptor :: Descriptor,
    gps :: Text
  }
  deriving (Generic, FromJSON, ToJSON)

instance ToSchema StartLocation where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
