module Core.OnSearch.Provider where

import Beckn.Prelude
import Core.OnSearch.LocationDetails
import Core.OnSearch.Departure
import Core.OnSearch.Fare
import Core.OnSearch.Item
import Core.OnSearch.Route
import Core.OnSearch.Descriptor

data Provider = Provider
  { id :: Text,
    descriptor :: DescriptorId,
    -- categories?
    locations :: [LocationDetails],
    routes :: [Route],
    fares :: [Fare],
    departures :: [Departure],
    items :: [Item]
  }
  deriving (Generic, FromJSON, Show, ToJSON)
