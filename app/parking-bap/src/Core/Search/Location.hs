module Core.Search.Location where

import Beckn.Prelude
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Core.Common.Gps (Gps)
import Data.OpenApi (ToSchema (declareNamedSchema), defaultSchemaOptions)

newtype Location = Location
  { gps :: Gps
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

instance ToSchema Location where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions