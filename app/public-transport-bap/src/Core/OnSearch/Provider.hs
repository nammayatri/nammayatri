module Core.OnSearch.Provider where

import Beckn.Prelude hiding (exp)
import Core.Descriptor (Descriptor)
import Core.Item (Item)
import Core.Location (Location)
import Core.OnSearch.Category
import Core.OnSearch.Routes

data Provider = Provider
  { id :: Maybe Text,
    descriptor :: Maybe Descriptor,
    category :: Maybe [Category],
    locations :: [Location],
    routes :: [Routes],
    items :: [Item]
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema)