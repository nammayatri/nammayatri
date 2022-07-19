module Beckn.Types.Core.Taxi.OnSelect.Offer where

import Beckn.Prelude
import Beckn.Types.Core.Taxi.OnSelect.Descriptor
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Data.OpenApi (ToSchema (..), defaultSchemaOptions)

data Offer = Offer
  { id :: Text,
    descriptor :: Descriptor
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance ToSchema Offer where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
