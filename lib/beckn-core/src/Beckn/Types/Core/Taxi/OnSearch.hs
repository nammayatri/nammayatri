module Beckn.Types.Core.Taxi.OnSearch
  ( module Beckn.Types.Core.Taxi.OnSearch,
    module Reexport,
  )
where

import Beckn.Types.Core.Taxi.OnSearch.Catalog as Reexport
import Beckn.Types.Core.Taxi.OnSearch.Item as Reexport
import Beckn.Types.Core.Taxi.OnSearch.Provider as Reexport
import Data.OpenApi (ToSchema)
import EulerHS.Prelude

newtype OnSearchMessage = OnSearchMessage
  { catalog :: Catalog
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)
