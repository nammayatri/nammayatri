module Core.OnSearch.Descriptor where

import Beckn.Prelude
import Beckn.Types.App ()
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Data.OpenApi (ToSchema (declareNamedSchema), defaultSchemaOptions)

data Descriptor = Descriptor
  { name :: Text,
    images :: [BaseUrl]
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance ToSchema Descriptor where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
