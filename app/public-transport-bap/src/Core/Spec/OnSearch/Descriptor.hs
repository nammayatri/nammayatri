module Core.Spec.OnSearch.Descriptor where

import Beckn.Prelude
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Core.Spec.OnSearch.Image (Image (..))
import Data.OpenApi hiding (name)

data Descriptor
  = Descriptor
      { name :: Text,
        code :: Text,
        symbol :: Text,
        short_desc :: Text,
        long_desc :: Text,
        images :: [Image]
      }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

instance ToSchema Descriptor where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

newtype DescriptorId
  = DescriptorId
      { name :: Text
      }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

instance ToSchema DescriptorId where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
