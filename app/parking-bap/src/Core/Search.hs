module Core.Search
  ( module Core.Search,
    module Reexport,
  )
where

import Beckn.Prelude
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Core.Search.Intent as Reexport
import Core.Search.Location as Reexport
import Data.OpenApi (ToSchema (..), defaultSchemaOptions)

newtype SearchIntent = SearchIntent
  { intent :: Intent
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON)

instance ToSchema SearchIntent where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
