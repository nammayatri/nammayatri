module Core.Spec.OnConfirm.Descriptor where

import Data.Aeson
import Data.OpenApi
import Kernel.Utils.Schema (genericDeclareUnNamedSchema)
import Relude

newtype DescriptorCode = DescriptorCode
  { code :: Text
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)

instance ToSchema DescriptorCode where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
