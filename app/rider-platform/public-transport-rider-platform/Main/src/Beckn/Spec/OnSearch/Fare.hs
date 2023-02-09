module Beckn.Spec.OnSearch.Fare where

import Beckn.Spec.Common.Price
import Data.OpenApi hiding (name)
import Kernel.Prelude
import Kernel.Utils.Schema (genericDeclareUnNamedSchema)

data Fare = Fare
  { id :: Text,
    route_id :: Text,
    price :: Price
  }
  deriving (Show, Generic, FromJSON, ToJSON)

instance ToSchema Fare where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
