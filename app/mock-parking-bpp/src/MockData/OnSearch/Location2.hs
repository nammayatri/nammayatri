module MockData.OnSearch.Location2 where

import Beckn.Types.Core.Migration.Gps
import Core.OnSearch.Item
import Core.OnSearch.Location
import MockData.OnSearch.Common
import Relude hiding (id, state)

parkingLocationId :: Text
parkingLocationId = "P2"

mockLocation :: Location
mockLocation =
  let gps = mockGps
      id = parkingLocationId
      address = mockAddress "Test Address2"
   in Location {..}

mockGps :: Gps
mockGps =
  Gps
    { lat = 20.5437,
      lon = 78.9529
    }

--------------------------------------------

parkingItemId19 :: Text
parkingItemId19 = "19"

mockItem19 :: Item
mockItem19 = makeMockItem parkingItemId19 "Four wheeler parking for all" 12 parkingLocationId parkingPlacesAvailable4

parkingPlacesAvailable4 :: Int
parkingPlacesAvailable4 = 50
