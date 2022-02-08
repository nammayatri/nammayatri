module Core.Confirm.Provider where

import Beckn.Prelude
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Data.OpenApi (ToSchema (declareNamedSchema), defaultSchemaOptions)

newtype Provider = Provider
  { locations :: [Location]
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON)

instance ToSchema Provider where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

newtype Location = Location
  { id :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON)

instance ToSchema Location where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
