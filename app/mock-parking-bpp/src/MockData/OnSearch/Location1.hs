module MockData.OnSearch.Location1 where

import Beckn.Types.Core.Migration.Gps
import qualified Beckn.Utils.Example as Core
import Core.OnSearch.Address
import Core.OnSearch.Item
import Core.OnSearch.ItemQuantity
import Core.OnSearch.Location
import MockData.OnSearch.Common
import Relude hiding (id, state)
import Utils

parkingLocationId :: Text
parkingLocationId = "P1"

mockLocation :: Location
mockLocation =
  let gps = Core.example
      id = parkingLocationId
      address = mockAddress
   in Location {..}

mockAddress :: Address
mockAddress =
  let name = "Test Address"
      street_address = "mock street address"
      locality = "Panangad"
      city = Nothing
      state = "Kerala"
      country = "IND"
      area_code = "682506"
   in Address {..}

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
mockItem4 =
  let id = parkingItemId4
      descriptor =
        ItemDescriptor
          { name = "Four wheeler parking",
            images = []
          }
      price = buildPrice (10 :: Int)
      category_id = cat4wheeler
      location_id = parkingLocationId
      matched = True
      quantity =
        ItemQuantity
          { available =
              Quantity
                { count = parkingPlacesAvailable4,
                  measure = Nothing
                }
          }
   in Item {..}

parkingPlacesAvailable4 :: Int
parkingPlacesAvailable4 = 14

-------------------------------------------------

parkingItemId9 :: Text
parkingItemId9 = "9"

mockItem9 :: Item
mockItem9 =
  let id = parkingItemId9
      descriptor =
        ItemDescriptor
          { name = "Four wheeler parking (luxury)",
            images = []
          }
      price = buildPrice (20 :: Int)
      category_id = cat4wheeler
      location_id = parkingLocationId
      matched = True
      quantity =
        ItemQuantity
          { available =
              Quantity
                { count = parkingPlacesAvailable9,
                  measure = Nothing
                }
          }
   in Item {..}

parkingPlacesAvailable9 :: Int
parkingPlacesAvailable9 = 2
