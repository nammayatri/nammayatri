module Types.Domain.Outgoing.Search where

import Beckn.Prelude
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Core.Spec.Search.Intent
import Data.OpenApi (ToSchema (..), defaultSchemaOptions)

newtype SearchIntent = SearchIntent
  { intent :: Intent
  }
  deriving (Generic, Show, ToJSON, FromJSON)

instance ToSchema SearchIntent where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
