module Core.ACL.OnSearch where

import Beckn.Prelude
import Beckn.Types.Core.Taxi.Common.Gps as Common
import Beckn.Types.Core.Taxi.Common.TimeTimestamp as Common
import qualified Beckn.Types.Core.Taxi.Common.VehicleVariant as Common
import qualified Beckn.Types.Core.Taxi.OnSearch as OS
import qualified Domain.Action.Beckn.Search as DSearch
import qualified Domain.Types.Vehicle.Variant as Variant

autoOneWayCategory :: OS.Category
autoOneWayCategory =
  OS.Category
    { id = OS.AUTO_TRIP,
      descriptor =
        OS.Descriptor
          { name = ""
          }
    }

mkOnSearchMessage ::
  DSearch.DSearchRes ->
  OS.OnSearchMessage
mkOnSearchMessage res@DSearch.DSearchRes {..} = do
  let fulfillmentId = "fulfillment1"
      fulfillment = mkFulfillment fulfillmentId res
      category = autoOneWayCategory
      categoryId = category.id
      item = mkItem categoryId fulfillmentId res

  let provider =
        OS.Provider
          { id = transporterInfo.shortId.getShortId,
            descriptor = OS.Descriptor {name = ""},
            locations = [],
            categories = [category],
            items = [item],
            offers = [],
            add_ons = [],
            fulfillments = [fulfillment],
            contacts = transporterInfo.contacts,
            tags =
              OS.ProviderTags
                { rides_inprogress = transporterInfo.ridesInProgress,
                  rides_completed = transporterInfo.ridesCompleted,
                  rides_confirmed = transporterInfo.ridesConfirmed
                },
            payment =
              OS.Payment
                { collected_by = "BPP",
                  _type = OS.ON_FULFILLMENT,
                  time = OS.TimeDuration "P2A" -- FIXME: what is this?
                }
          }
  OS.OnSearchMessage $
    OS.Catalog
      { bpp_providers = [provider],
        bpp_descriptor = OS.Descriptor transporterInfo.shortId.getShortId
      }

castVariant :: Variant.Variant -> Common.VehicleVariant
castVariant Variant.SEDAN = Common.SEDAN
castVariant Variant.HATCHBACK = Common.HATCHBACK
castVariant Variant.SUV = Common.SUV
castVariant Variant.AUTO = Common.AUTO

mkFulfillment :: Text -> DSearch.DSearchRes -> OS.FulfillmentInfo
mkFulfillment fulfillmentId dRes = do
  let fromLocation = dRes.searchRequest.fromLocation
  let toLocation = dRes.searchRequest.toLocation
  OS.FulfillmentInfo
    { id = fulfillmentId,
      start =
        OS.StartInfo
          { location = OS.Location $ Common.Gps {lat = fromLocation.lat, lon = fromLocation.lon},
            time = Common.TimeTimestamp dRes.now
          },
      end =
        Just
          OS.StopInfo
            { location = OS.Location $ Common.Gps {lat = toLocation.lat, lon = toLocation.lon}
            },
      vehicle =
        OS.FulfillmentVehicle
          { category = castVariant dRes.vehicleVariant
          }
    }

mkItem :: OS.FareProductType -> Text -> DSearch.DSearchRes -> OS.Item
mkItem categoryId fulfillmentId dRes =
  OS.Item
    { category_id = categoryId,
      fulfillment_id = fulfillmentId,
      offer_id = Nothing,
      price = price_,
      descriptor =
        OS.ItemDescriptor
          { name = "",
            code =
              OS.ItemCode
                { fareProductType = OS.AUTO_TRIP,
                  vehicleVariant = castVariant dRes.vehicleVariant,
                  distance = Nothing,
                  duration = Nothing
                }
          },
      quote_terms = [],
      tags =
        Just $
          OS.ItemTags
            { distance_to_nearest_driver = OS.DecimalValue $ toRational dRes.distanceToPickup.getMeters
            },
      base_distance = Nothing,
      base_duration = Nothing
    }
  where
    price_ = do
      let value_ = OS.DecimalValue $ toRational dRes.baseFare
      OS.ItemPrice
        { currency = "INR",
          value = value_,
          offered_value = value_
        }
