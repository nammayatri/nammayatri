module Types.Beckn.Provider (Provider (..), DescriptorInfo (..)) where

import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Data.OpenApi (ToSchema (..), defaultSchemaOptions)
import EulerHS.Prelude hiding (id)
import Types.Beckn.Category (Category)
import Types.Beckn.Item (Item)

data Provider = Provider
  { descriptor :: DescriptorInfo,
    categories :: [Category],
    items :: [Item]
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

newtype DescriptorInfo = DescriptorInfo
  { name :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

instance ToSchema DescriptorInfo where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
