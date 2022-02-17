module MockData.OnSearch.Common where

import Core.OnSearch.Address
import Core.OnSearch.Category
import Core.OnSearch.Item
import Core.OnSearch.ItemQuantity
import Relude hiding (id, state)
import Utils (buildPrice)

cat4wheeler :: Text
cat4wheeler = "4-wheeler-parking"

mockProviderCategories :: Category
mockProviderCategories =
  let id_ = Just cat4wheeler
      descriptor_ = Just $ CategoryDescriptor {name = "4 wheeler parking"}
   in Category
        { id = id_,
          descriptor = descriptor_
        }

mockAddress :: Text -> Address
mockAddress name =
  let street_address = "mock street address"
      locality = "Panangad"
      city = Nothing
      state = "Kerala"
      country = "IND"
      area_code = "682506"
   in Address {..}

makeMockItem :: Text -> Text -> Int -> Text -> Int -> Item
makeMockItem _id description rupeesPrice locationId availablePlaces =
  let id = _id
      descriptor =
        ItemDescriptor
          { name = description,
            images = []
          }
      price = buildPrice rupeesPrice
      category_id = cat4wheeler
      location_id = locationId
      matched = True
      quantity =
        ItemQuantity
          { available =
              Quantity
                { count = availablePlaces,
                  measure = Nothing
                }
          }
   in Item {..}
