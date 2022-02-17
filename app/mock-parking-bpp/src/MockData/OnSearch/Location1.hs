module MockData.OnSearch.Location1 where

import Beckn.Types.Core.Migration.Gps
import Core.OnSearch.Item
import Core.OnSearch.Location
import MockData.OnSearch.Common
import Relude hiding (id, state)

parkingLocationId :: Text
parkingLocationId = "P1"

mockLocation :: Location
mockLocation =
  let gps = mockGps
      id = parkingLocationId
      address = mockAddress "Test Address"
   in Location {..}

mockGps :: Gps
mockGps =
  Gps
    { lat = 20.5937,
      lon = 78.9629
    }

--------------------------------------------

parkingItemId4 :: Text
parkingItemId4 = "4"

mockItem4 :: Item
mockItem4 = makeMockItem parkingItemId4 "Four wheeler parking" 10 parkingLocationId parkingPlacesAvailable4

parkingPlacesAvailable4 :: Int
parkingPlacesAvailable4 = 14

-------------------------------------------------

parkingItemId9 :: Text
parkingItemId9 = "9"

mockItem9 :: Item
mockItem9 = makeMockItem "Four wheeler parking (luxury)" parkingItemId9 20 parkingLocationId parkingPlacesAvailable9

parkingPlacesAvailable9 :: Int
parkingPlacesAvailable9 = 2
