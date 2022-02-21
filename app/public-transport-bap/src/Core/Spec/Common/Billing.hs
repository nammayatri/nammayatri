module Core.Spec.Common.Billing where

import Beckn.Prelude
import Beckn.Utils.GenericPretty (PrettyShow)
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Data.OpenApi hiding (name)

newtype Billing = Billing
  { name :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON, PrettyShow)

instance ToSchema Billing where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
