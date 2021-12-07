module Beckn.Types.Core.Cabs.OnSearch
  ( module Beckn.Types.Core.Cabs.OnSearch,
    module Reexport,
  )
where

import Beckn.Types.Core.Cabs.OnSearch.Catalog as Reexport
import Beckn.Types.Core.Cabs.OnSearch.Item as Reexport
import Beckn.Types.Core.Cabs.OnSearch.Provider as Reexport
import Data.OpenApi (ToSchema)
import EulerHS.Prelude

newtype OnSearchMessage = OnSearchMessage
  { catalog :: Catalog
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)
