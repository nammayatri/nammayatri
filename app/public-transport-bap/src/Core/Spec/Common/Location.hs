module Core.Spec.Common.Location where

import Beckn.Prelude
import Beckn.Types.Core.Migration.Gps (Gps)
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Data.OpenApi (ToSchema (declareNamedSchema), defaultSchemaOptions)

data Location = Location
  { id :: Text,
    gps :: Gps,
    stop_code :: Text,
    descriptor :: Descriptor
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

newtype Descriptor = Descriptor
  { name :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON)

instance ToSchema Descriptor where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
