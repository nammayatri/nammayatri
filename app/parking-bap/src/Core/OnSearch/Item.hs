module Core.OnSearch.Item where

import Beckn.Prelude
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Core.Common.Price
import Core.OnSearch.ItemQuantity
import Data.OpenApi (ToSchema (declareNamedSchema), defaultSchemaOptions)

data Item = Item
  { id :: Text,
    descriptor :: ItemDescriptor,
    price :: Price,
    category_id :: Text,
    location_id :: Text,
    matched :: Bool,
    quantity :: ItemQuantity
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance ToSchema Item where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

data ItemDescriptor = ItemDescriptor
  { name :: Text,
    images :: [BaseUrl]
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance ToSchema ItemDescriptor where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
