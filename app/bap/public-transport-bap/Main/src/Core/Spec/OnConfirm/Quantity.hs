module Core.Spec.OnConfirm.Quantity where

import Data.OpenApi (ToSchema (..), defaultSchemaOptions)
import Kernel.Prelude
import Kernel.Utils.GenericPretty
import Kernel.Utils.Schema

newtype Quantity = Quantity
  { count :: Int
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON, PrettyShow)

instance ToSchema Quantity where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
