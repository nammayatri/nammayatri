module Core.OnSearch.Provider (Provider (..)) where

import Beckn.Prelude hiding (exp)
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Core.Common.Descriptor
import Core.OnSearch.Category (Category)
import Core.OnSearch.Item (Item)
import Core.OnSearch.Location (Location)
import Data.OpenApi (ToSchema (declareNamedSchema), defaultSchemaOptions)

data Provider = Provider
  { id :: Maybe Text,
    descriptor :: Descriptor,
    categories :: [Category],
    locations :: [Location],
    items :: Maybe [Item]
  }
  deriving (Generic, FromJSON, ToJSON)

instance ToSchema Provider where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
