module MockData.OnSearch where

import Core.Common.Descriptor
import Core.OnSearch
import MockData.OnSearch.Common
import qualified MockData.OnSearch.Location1 as L1
import qualified MockData.OnSearch.Location2 as L2
import Relude hiding (id, state)

onSearchCatalog :: OnSearchCatalog
onSearchCatalog = OnSearchCatalog mockCatalog

mockCatalog :: Catalog
mockCatalog =
  Catalog
    { bpp_descriptor = mockBppDescriptor,
      bpp_providers = [mockProvider]
    }

mockBppDescriptor :: Descriptor
mockBppDescriptor =
  let name = "Pinpark"
      short_desc = "Pinpark bpp"
      images =
        (: [])
          "https://bpp3.beckn.org/pub/media/beckn/business/logo/default/adam_walsh.png"
   in Descriptor {..}

mockProvider :: Provider
mockProvider =
  let id = Just mockProviderId
      descriptor = mockProviderDescriptor
      categories = [mockProviderCategories]
      locations = [L1.mockLocation, L2.mockLocation]
      items = Just mockItems
   in Provider {..}

mockProviderId :: Text
mockProviderId = "pinpark"

mockProviderName, mockProviderImage, mockProviderShortDesc :: Text
mockProviderName = "Pinpark"
mockProviderImage = "https://bpp3.beckn.org/pub/media/beckn/provider/logo/default/pinpark.jpg"
mockProviderShortDesc = "Lorem Ipsum is that it has a more-or-less normal distribution of letters."

mockProviderDescriptor :: Descriptor
mockProviderDescriptor =
  let name = mockProviderName
      images = (: []) mockProviderImage
      short_desc = mockProviderShortDesc
   in Descriptor {..}

mockItems :: [Item]
mockItems = [L1.mockItem4, L1.mockItem9, L2.mockItem19]

allLocations :: [Location]
allLocations = [L1.mockLocation, L2.mockLocation]
