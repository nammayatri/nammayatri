module Core.OnConfirm.Provider where

import Beckn.Prelude
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Core.Location
import Core.OnConfirm.Descriptor
import Data.OpenApi (ToSchema (declareNamedSchema), defaultSchemaOptions)

data Provider = Provider
  { id :: Text,
    descriptor :: Descriptor,
    locations :: [Location]
  }
  deriving (Generic, FromJSON, ToJSON)

instance ToSchema Provider where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
