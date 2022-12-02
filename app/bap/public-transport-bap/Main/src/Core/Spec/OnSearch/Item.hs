module Core.Spec.OnSearch.Item where

import Beckn.Prelude
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Data.OpenApi hiding (name)

data Item = Item
  { departure_id :: Text,
    fare_id :: Text,
    canBook :: Bool
  }
  deriving (Show, Generic, FromJSON, ToJSON)

instance ToSchema Item where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
