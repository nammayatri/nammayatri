module Beckn.Types.Core.Cabs.OnSearch.Provider
  ( Provider (..),
  )
where

import Beckn.Types.Core.Cabs.OnSearch.Item (Item)
import Beckn.Utils.Example
import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (exp, id)

data Provider = Provider
  { name :: Text,
    items :: [Item],
    contacts :: Text,
    rides_inprogress :: Int,
    rides_completed :: Int,
    rides_confirmed :: Int
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

instance Example Provider where
  example =
    Provider
      { name = "name",
        items = [],
        contacts = "99999999999",
        rides_inprogress = 12,
        rides_completed = 32,
        rides_confirmed = 3
      }
