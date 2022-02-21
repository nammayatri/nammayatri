module Core.Spec.OnConfirm.Quantity where

import Beckn.Prelude
import Beckn.Utils.GenericPretty
import Beckn.Utils.Schema
import Data.OpenApi (ToSchema (..), defaultSchemaOptions)

newtype Quantity = Quantity
  { count :: Int
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON, PrettyShow)

instance ToSchema Quantity where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
