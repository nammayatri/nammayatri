module Beckn.Types.Core.Taxi.OnSelect.Descriptor (Descriptor (..)) where

import Beckn.Prelude
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Data.OpenApi (ToSchema (..), defaultSchemaOptions)

newtype Descriptor = Descriptor
  { name :: Text
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance ToSchema Descriptor where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
