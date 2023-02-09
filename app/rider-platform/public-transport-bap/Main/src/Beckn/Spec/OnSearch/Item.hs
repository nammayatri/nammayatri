module Beckn.Spec.OnSearch.Item where

import Data.OpenApi hiding (name)
import Kernel.Prelude
import Kernel.Utils.Schema (genericDeclareUnNamedSchema)

data Item = Item
  { departure_id :: Text,
    fare_id :: Text,
    canBook :: Bool
  }
  deriving (Show, Generic, FromJSON, ToJSON)

instance ToSchema Item where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
