module Core.Spec.OnSearch.Descriptor where

import Core.Spec.OnSearch.Image (Image (..))
import Data.OpenApi hiding (name)
import Kernel.Prelude
import Kernel.Utils.Schema (genericDeclareUnNamedSchema)

data Descriptor = Descriptor
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

newtype DescriptorId = DescriptorId
  { name :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

instance ToSchema DescriptorId where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
