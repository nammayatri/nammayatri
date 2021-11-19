module Beckn.Types.Core.Migration1.OnSearch
  ( module Beckn.Types.Core.Migration1.OnSearch,
    module Reexport,
  )
where

import Beckn.Types.Core.Migration1.OnSearch.Catalog as Reexport
import Beckn.Types.Core.Migration1.OnSearch.Item as Reexport
import Beckn.Types.Core.Migration1.OnSearch.Provider as Reexport
import Data.OpenApi (ToSchema)
import EulerHS.Prelude

newtype OnSearchMessage = OnSearchMessage
  { catalog :: Catalog
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)
