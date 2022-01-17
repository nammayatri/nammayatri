module MockData.OnSearch where

import Beckn.Prelude
import Beckn.Types.Core.Migration.DecimalValue
import Beckn.Types.Core.Migration.Gps
import Core.Descriptor
import Core.Location
import Core.OnSearch.Catalog
import Core.OnSearch.Fulfillment
import Core.OnSearch.Item
import Core.Price
import Core.Provider
import Core.Time
import Data.Maybe
import Utils

mockBppDescriptor :: DescriptorDetails
mockBppDescriptor =
  DescriptorDetails
    { name = "State Water Transport Department",
      code = Just "SWTD",
      symbol = Just "SWTD bpp symbol",
      short_desc = "SWTD bpp description",
      long_desc = Just "SWTD bpp long description",
      images = []
    }

providerName :: Text
providerName = "State Water Transport Department"

providerDescriptorId :: DescriptorId
providerDescriptorId =
  DescriptorId
    { name = providerName
    }

onSearchCatalog :: OnSearchCatalog
onSearchCatalog =
  let bpp_descriptor = mockBppDescriptor
      bpp_providers = [mockProvider]
      catalog = Catalog {..}
   in OnSearchCatalog {..}

mockProviderId :: Text
mockProviderId = "SWTD"

mockProvider :: Provider
mockProvider =
  let id = mockProviderId
      descriptor = providerDescriptorId
      fulfillments = mockFulfillments
      locations = mockLocations
      items = mockItems
   in Provider {..}

mockFulfillments :: [Fulfillment]
mockFulfillments = [mockFulfillmentEMB, mockFulfillmentABC]

findFulfillment :: Text -> Maybe Fulfillment
findFulfillment fulfId = find (\f -> f.id == fulfId) mockFulfillments

tripIdEMB, tripIdABC :: Text
tripIdEMB = "TRIP001_EKM_EMB"
tripIdABC = "TRIP001_EKM_ABC"

buildMockFulfillment :: Text -> Text -> Text -> Fulfillment
buildMockFulfillment tripId depart arrival =
  let id = tripId
      start =
        FulfillmentDetails
          { location = LocationId depart,
            time =
              Time
                { label = "Departure Time",
                  timestamp = fromJust $ readUTCTime "2021-11-17 09:54"
                }
          }
      end =
        FulfillmentDetails
          { location = LocationId arrival,
            time =
              Time
                { label = "Arrival time",
                  timestamp = fromJust $ readUTCTime "2021-11-17 12:54"
                }
          }
   in Fulfillment {..}

mockFulfillmentEMB, mockFulfillmentABC :: Fulfillment
mockFulfillmentEMB = buildMockFulfillment tripIdEMB locationLabelEKM locationLabelEMB
mockFulfillmentABC = buildMockFulfillment tripIdABC locationLabelEKM locationLabelABC

locationLabelEKM, locationLabelEMB, locationLabelABC :: Text
locationLabelEKM = "EKM"
locationLabelEMB = "EMB"
locationLabelABC = "ABC"

mockLocations :: [LocationDetails]
mockLocations = [locationEKM, locationEMB, locationABC]

buildLocation :: Text -> Text -> Gps -> LocationDetails
buildLocation id name gps =
  let station_code = id
      descriptor = DescriptorId {..}
   in LocationDetails {..}

exampleGps :: Gps
exampleGps =
  Gps
    { lat = 9.898,
      lon = 76.324
    }

locationGpsEKM, locationGpsEMB, locationGpsABC :: Gps
locationGpsEKM = exampleGps
locationGpsEMB = exampleGps {lat = exampleGps.lat + 1}
locationGpsABC = exampleGps {lat = exampleGps.lat + 1, lon = exampleGps.lon + 0.2}

locationEKM, locationEMB, locationABC :: LocationDetails
locationEKM = buildLocation locationLabelEKM "Ernakulam" locationGpsEKM
locationEMB = buildLocation locationLabelEMB "Embarkment" locationGpsEMB
locationABC = buildLocation locationLabelABC "Test Station" locationGpsABC

mockItems :: [Item]
mockItems = [itemEMB, itemABC]

itemEMB, itemABC :: Item
itemEMB = buildItem tripIdEMB priceEMB
itemABC = buildItem tripIdABC priceABC

buildItem :: Text -> Price -> Item
buildItem tripId price =
  let id = "ONE_WAY_TICKET"
      fulfillment_id = tripId
      descriptor = DescriptorId {name = "One Way Ticket"}
   in --  quantity = Nothing
      Item {..}

buildPrice :: Int -> Price
buildPrice int =
  Price
    { currency = "INR",
      value = DecimalValue $ show int
    }

priceEMB, priceABC :: Price
priceEMB = buildPrice 30
priceABC = buildPrice 40

findItem :: Text -> Text -> Maybe Item
findItem itId fulfId = find (\i -> i.fulfillment_id == fulfId && i.id == itId) mockItems
