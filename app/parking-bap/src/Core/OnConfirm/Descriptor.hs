module Core.OnConfirm.Descriptor where

import Beckn.Prelude
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Data.OpenApi (ToSchema (declareNamedSchema), defaultSchemaOptions)

data Descriptor = Descriptor
  { name :: Text,
    short_desc :: Text,
    images :: [Text]
  }
  deriving (Generic, FromJSON, ToJSON)

instance ToSchema Descriptor where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
