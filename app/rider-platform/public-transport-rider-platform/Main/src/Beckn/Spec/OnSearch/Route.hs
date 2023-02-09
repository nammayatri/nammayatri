module Beckn.Spec.OnSearch.Route where

import Data.OpenApi hiding (items)
import Kernel.Prelude
import Kernel.Utils.Schema (genericDeclareUnNamedSchema)

data Route = Route
  { id :: Text,
    route_code :: Text,
    start_stop :: Text,
    end_stop :: Text
  }
  deriving (Show, Generic, ToJSON, FromJSON)

instance ToSchema Route where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
