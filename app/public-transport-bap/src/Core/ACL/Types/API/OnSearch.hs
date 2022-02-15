module Core.ACL.Types.API.OnSearch where

import Beckn.Prelude
import Core.Spec.OnSearch.Catalog

newtype OnSearchCatalog = OnSearchCatalog
  { catalog :: Catalog
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema)
