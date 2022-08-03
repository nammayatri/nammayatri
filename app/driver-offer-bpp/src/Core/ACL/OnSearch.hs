module Core.ACL.OnSearch where

import Beckn.Prelude
import qualified Beckn.Types.Core.Taxi.Common.VehicleVariant as Common
import Beckn.Types.Core.Taxi.OnSearch (Item (base_distance))
import qualified Beckn.Types.Core.Taxi.OnSearch as OS
import qualified Domain.Action.Beckn.Search as DSearch
import qualified Domain.Types.Vehicle.Variant as Variant

autoOneWayCategory :: OS.Category
autoOneWayCategory =
  OS.Category
    { id = OS.DRIVER_OFFER_ESTIMATE,
      descriptor =
        OS.Descriptor
          { name = ""
          }
    }

mkOnSearchMessage ::
  DSearch.DSearchRes ->
  OS.OnSearchMessage
mkOnSearchMessage res@DSearch.DSearchRes {..} = do
  let startInfo = mkStartInfo res
  let stopInfo = mkStopInfo res
  let quoteEntitiesList = map (mkQuoteEntities startInfo stopInfo) estimateList
      items = map (.item) quoteEntitiesList
      fulfillments = map (.fulfillment) quoteEntitiesList
      contacts = transporterInfo.contacts
      tags =
        OS.ProviderTags
          { rides_inprogress = transporterInfo.ridesInProgress,
            rides_completed = transporterInfo.ridesCompleted,
            rides_confirmed = transporterInfo.ridesConfirmed
          }
      payment =
        OS.Payment
          { collected_by = "BPP",
            _type = OS.ON_FULFILLMENT,
            time = OS.TimeDuration "P2A" -- FIXME: what is this?
          }

  let provider =
        OS.Provider
          { id = transporterInfo.shortId.getShortId,
            descriptor = OS.Descriptor {name = transporterInfo.name},
            locations = [],
            categories = [autoOneWayCategory],
            items,
            offers = [],
            add_ons = [],
            fulfillments,
            contacts,
            tags,
            payment
          }
  OS.OnSearchMessage $
    OS.Catalog
      { bpp_providers = [provider],
        bpp_descriptor = OS.Descriptor transporterInfo.shortId.getShortId
      }

mkStartInfo :: DSearch.DSearchRes -> OS.StartInfo
mkStartInfo dReq =
  OS.StartInfo
    { location = OS.Location $ OS.Gps {lat = dReq.searchRequest.fromLocation.lat, lon = dReq.searchRequest.fromLocation.lon},
      time = OS.TimeTimestamp dReq.now
    }

mkStopInfo :: DSearch.DSearchRes -> OS.StopInfo
mkStopInfo res =
  OS.StopInfo
    { location = OS.Location $ OS.Gps {lat = res.searchRequest.toLocation.lat, lon = res.searchRequest.toLocation.lon}
    }

data QuoteEntities = QuoteEntities
  { fulfillment :: OS.FulfillmentInfo,
    item :: OS.Item
  }

currency :: Text
currency = "INR"

mkQuoteEntities :: OS.StartInfo -> OS.StopInfo -> DSearch.EstimateItem -> QuoteEntities
mkQuoteEntities start end it = do
  let variant = castVariant it.vehicleVariant
      priceDecimalValue = OS.DecimalValue $ toRational it.baseFare
      fulfillment =
        OS.FulfillmentInfo
          { start,
            end = Just end,
            id = "ARDU_" <> show it.vehicleVariant,
            vehicle = OS.FulfillmentVehicle {category = castVariant it.vehicleVariant}
          }
      item =
        OS.Item
          { category_id = autoOneWayCategory.id,
            fulfillment_id = fulfillment.id,
            offer_id = Nothing,
            price =
              OS.ItemPrice
                { currency,
                  value = priceDecimalValue,
                  offered_value = priceDecimalValue
                },
            descriptor =
              OS.ItemDescriptor
                { name = "",
                  code = OS.ItemCode OS.DRIVER_OFFER_ESTIMATE variant Nothing Nothing
                },
            quote_terms = [],
            tags = Just $ OS.ItemTags (OS.DecimalValue $ toRational it.distanceToPickup),
            base_distance = Nothing,
            base_duration = Nothing
          }
  QuoteEntities
    { fulfillment,
      item
    }

castVariant :: Variant.Variant -> Common.VehicleVariant
castVariant Variant.SEDAN = Common.SEDAN
castVariant Variant.HATCHBACK = Common.HATCHBACK
castVariant Variant.SUV = Common.SUV
castVariant Variant.AUTO_VARIANT = Common.AUTO
