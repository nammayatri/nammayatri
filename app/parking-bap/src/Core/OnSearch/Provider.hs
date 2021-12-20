module Core.OnSearch.Provider (Provider (..)) where

import Beckn.Prelude hiding (exp)
import Beckn.Types.Core.Migration.Category (Category)
import Beckn.Types.Core.Migration.Descriptor (Descriptor)
import Core.Item (Item)
import Core.Location (Location)

data Provider = Provider
  { id :: Maybe Text,
    descriptor :: Descriptor,
    categories :: [Category],
    locations :: [Location],
    items :: Maybe [Item]
  }
  deriving (Generic, FromJSON, ToJSON)
