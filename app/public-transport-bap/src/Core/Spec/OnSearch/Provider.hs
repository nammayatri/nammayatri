module Core.Spec.OnSearch.Provider where

import Beckn.Prelude hiding (exp)
import Core.Spec.Common.Descriptor (Descriptor)
import Core.Spec.Common.Item (Item)
import Core.Spec.Common.Location (Location)
import Core.Spec.OnSearch.Category
import Core.Spec.OnSearch.Departure
import Core.Spec.OnSearch.Fares
import Core.Spec.OnSearch.Route

data Provider = Provider
  { id :: Maybe Text,
    descriptor :: Maybe Descriptor,
    categories_classs :: Maybe [Category],
    locations :: [Location],
    routes :: [Route],
    fares :: [Fares],
    departures :: [Departure],
    items :: [Item]
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema)
