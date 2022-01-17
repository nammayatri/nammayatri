module Core.Confirm.Item where

import Beckn.Prelude
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Data.OpenApi (ToSchema (declareNamedSchema), defaultSchemaOptions)

data Item = Item
  { id :: Text,
    quantity :: Quantity
  }
  deriving (Generic, ToJSON)

instance ToSchema Item where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

newtype Quantity = Quantity
  { count :: Int
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

instance ToSchema Quantity where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
