module Core.Spec.Search (module Core.Spec.Search, module Reexport) where

import Beckn.Prelude
import Beckn.Utils.GenericPretty (PrettyShow)
import Beckn.Utils.Schema
import Core.Spec.Search.Fulfillment as Reexport
import Core.Spec.Search.LocationGps as Reexport
import Data.OpenApi

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
