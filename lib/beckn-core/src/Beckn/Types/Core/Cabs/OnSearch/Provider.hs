module Beckn.Types.Core.Cabs.OnSearch.Provider
  ( Provider (..),
    module Reexport,
  )
where

import Beckn.Types.Core.Cabs.OnSearch.Item (Item)
import Beckn.Types.Core.Cabs.OnSearch.Tags as Reexport
import Beckn.Utils.Example
import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (exp, id)

data Provider = Provider
  { name :: Text,
    items :: [Item],
    tags :: Tags
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

instance Example Provider where
  example =
    Provider
      { name = "name",
        items = [],
        tags = example
      }
