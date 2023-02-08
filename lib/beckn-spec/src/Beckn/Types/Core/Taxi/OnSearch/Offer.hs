module Beckn.Types.Core.Taxi.OnSearch.Offer where

import Beckn.Types.Core.Taxi.OnSearch.Descriptor
import Data.OpenApi (ToSchema (..), defaultSchemaOptions)
import Kernel.Prelude
import Kernel.Utils.Schema (genericDeclareUnNamedSchema)

data Offer = Offer
  { id :: Text,
    descriptor :: Descriptor
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance ToSchema Offer where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
