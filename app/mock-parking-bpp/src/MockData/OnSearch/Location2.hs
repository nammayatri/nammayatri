module MockData.OnSearch.Location2 where

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
parkingLocationId = "P2"

mockLocation :: Location
mockLocation =
  let gps = Core.example
      id = parkingLocationId
      address = mockAddress
   in Location {..}

mockAddress :: Address
mockAddress =
  let name = "Test Address2"
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
    { lat = 20.5437,
      lon = 78.9529
    }

--------------------------------------------

parkingItemId19 :: Text
parkingItemId19 = "19"

mockItem19 :: Item
mockItem19 =
  let id = parkingItemId19
      descriptor =
        ItemDescriptor
          { name = "Four wheeler parking for all",
            images = []
          }
      price = buildPrice (12 :: Int)
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
parkingPlacesAvailable4 = 50
