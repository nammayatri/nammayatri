module Core.OnSearch.Item where

import Beckn.Prelude
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Core.OnSearch.ItemQuantity
import Core.Price
import Data.OpenApi (ToSchema (declareNamedSchema), defaultSchemaOptions)

-- 'Maybe' fields are fields, that is present in request, but we do not use it anyhow
data Item = Item
  { id :: Text,
    descriptor :: ItemDescriptor,
    price :: Price,
    category_id :: Maybe Text,
    location_id :: Text,
    matched :: Maybe Bool,
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
