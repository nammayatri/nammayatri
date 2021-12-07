module Core.OnSearch.Location where

import Beckn.Prelude
import Beckn.Types.Core.Migration.City (City)
import Beckn.Types.Core.Migration.Country (Country)
import Beckn.Types.Core.Migration.Gps (Gps)
import Core.Address (Address)

data Location = Location
  { id :: Text,
    gps :: Gps,
    address :: Address,
    station_code :: Maybe Text,
    city :: Maybe City,
    country :: Maybe Country
  }
  deriving (Generic, Show, FromJSON, ToJSON)
