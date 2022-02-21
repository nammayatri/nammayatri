module Core.Spec.OnSearch.Fare where

import Beckn.Prelude
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Core.Spec.Common.Price
import Data.OpenApi hiding (name)

data Fare = Fare
  { id :: Text,
    route_id :: Text,
    price :: Price
  }
  deriving (Show, Generic, FromJSON, ToJSON)

instance ToSchema Fare where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
