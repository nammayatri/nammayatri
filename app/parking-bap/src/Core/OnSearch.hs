module Core.OnSearch
  ( module Core.OnSearch,
    module Reexport,
  )
where

import Beckn.Prelude
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Core.OnSearch.Address as Reexport
import Core.OnSearch.Catalog as Reexport
import Core.OnSearch.Category as Reexport
import Core.OnSearch.Item as Reexport
import Core.OnSearch.ItemQuantity as Reexport
import Core.OnSearch.Location as Reexport
import Core.OnSearch.Provider as Reexport
import Data.OpenApi (ToSchema (declareNamedSchema), defaultSchemaOptions)

newtype OnSearchCatalog = OnSearchCatalog
  { catalog :: Catalog
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

instance ToSchema OnSearchCatalog where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
