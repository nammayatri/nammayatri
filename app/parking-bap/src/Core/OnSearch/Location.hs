module Core.OnSearch.Location where

import Beckn.Prelude
import Beckn.Types.Core.Migration.Gps (Gps)
import Core.Address (Address)

data Location = Location
  { id :: Text,
    gps :: Gps,
    address :: Address
  }
  deriving (Generic, Show, FromJSON, ToJSON)
