module Core.Spec.Search (module Core.Spec.Search, module Reexport) where

import Core.Spec.Search.Fulfillment as Reexport
import Core.Spec.Search.LocationGps as Reexport
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
