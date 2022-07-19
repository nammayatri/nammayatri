module Beckn.Types.Core.Taxi.OnSelect.Addon where

import Beckn.Prelude
import Beckn.Types.Core.Taxi.OnSelect.Descriptor
import Beckn.Types.Core.Taxi.OnSelect.Price
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Data.OpenApi (ToSchema (..), defaultSchemaOptions)

data Addon = Addon
  { id :: Text,
    descriptor :: Descriptor,
    price :: Price
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance ToSchema Addon where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
