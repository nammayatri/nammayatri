module Beckn.Types.Core.Taxi.OnSelect.Addon where

import Beckn.Types.Core.Taxi.OnSelect.Descriptor
import Beckn.Types.Core.Taxi.OnSelect.Price
import Data.OpenApi (ToSchema (..), defaultSchemaOptions)
import Kernel.Prelude
import Kernel.Utils.Schema (genericDeclareUnNamedSchema)

data Addon = Addon
  { id :: Text,
    descriptor :: Descriptor,
    price :: Price
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance ToSchema Addon where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
