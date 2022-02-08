module Core.Confirm.Item where

import Beckn.Prelude
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Data.OpenApi (ToSchema (declareNamedSchema), defaultSchemaOptions)

data Item = Item
  { id :: Text,
    quantity :: Quantity
  }
  deriving (Generic, ToJSON, Show, FromJSON)

instance ToSchema Item where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

newtype Quantity = Quantity
  { count :: Int
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON)

instance ToSchema Quantity where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
