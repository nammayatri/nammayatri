module Storage.CachedQueries.SavedReqLocationExtra where

import Domain.Types.Person (Person)
import Kernel.External.Maps
import Kernel.Prelude
import Kernel.Types.Id

-- Extra code goes here --
makeIdKey :: Id Person -> LatLong -> Text
makeIdKey personId latLong = "CachedQueries:SavedLocation:-" <> show latLong <> show personId
