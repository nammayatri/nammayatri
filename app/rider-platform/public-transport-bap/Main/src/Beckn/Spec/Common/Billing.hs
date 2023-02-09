module Beckn.Spec.Common.Billing where

import Data.OpenApi hiding (name)
import Kernel.Prelude
import Kernel.Utils.GenericPretty (PrettyShow)
import Kernel.Utils.Schema (genericDeclareUnNamedSchema)

newtype Billing = Billing
  { name :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON, PrettyShow)

instance ToSchema Billing where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
