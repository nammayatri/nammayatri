module Beckn.Spec.Search (module Beckn.Spec.Search, module Reexport) where

import Beckn.Spec.Search.Fulfillment as Reexport
import Beckn.Spec.Search.LocationGps as Reexport
import Data.OpenApi
import Kernel.Prelude
import Kernel.Utils.GenericPretty (PrettyShow)
import Kernel.Utils.Schema

newtype SearchMessage = SearchMessage
  { intent :: Intent
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON, PrettyShow, ToSchema)

newtype Intent = Intent
  { fulfillment :: Fulfillment
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON, PrettyShow)

instance ToSchema Intent where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
