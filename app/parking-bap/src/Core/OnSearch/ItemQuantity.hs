module Core.OnSearch.ItemQuantity where

import Beckn.Prelude
import Beckn.Types.Core.Migration.Scalar
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Data.OpenApi (ToSchema (declareNamedSchema), defaultSchemaOptions)

data Quantity = Quantity
  { count :: Int,
    measure :: Maybe Scalar
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance ToSchema Quantity where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

newtype ItemQuantity = ItemQuantity
  { available :: Quantity
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance ToSchema ItemQuantity where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
