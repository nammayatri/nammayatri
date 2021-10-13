module Types.CoreMetro.Provider where

import Beckn.Types.Core.Migration.Descriptor (Descriptor)
import EulerHS.Prelude hiding (exp, id)
import Types.CoreMetro.Item
import Types.CoreMetro.Location

data Provider = Provider
  { id :: Text,
    descriptor :: Descriptor,
    locations :: [Location],
    items :: [Item]
  }
  deriving (Generic, FromJSON, ToJSON, Show)
