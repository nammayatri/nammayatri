module Core.ACL.OnSearch where

import Beckn.Prelude
import Beckn.Types.Core.Taxi.Common.Gps as Common
import Beckn.Types.Core.Taxi.Common.TimeTimestamp as Common
import qualified Beckn.Types.Core.Taxi.Common.VehicleVariant as Common
import qualified Beckn.Types.Core.Taxi.OnSearch as OS
import Beckn.Types.Id (ShortId)
import qualified Domain.Types.DriverQuote as DQuote
import qualified Domain.Types.Organization as DOrg
import Domain.Types.SearchRequest
import qualified Domain.Types.Vehicle.Variant as Variant

data DOnSearchReq = DOnSearchReq
  { transporterInfo :: TransporterInfo,
    searchRequest :: SearchRequest,
    quotes :: [DQuote.DriverQuote],
    now :: UTCTime
  }

data TransporterInfo = TransporterInfo
  { shortId :: ShortId DOrg.Organization,
    name :: Text,
    contacts :: Text,
    ridesInProgress :: Int,
    ridesCompleted :: Int,
    ridesConfirmed :: Int
  }

oneWayCategory :: OS.Category
oneWayCategory =
  OS.Category
    { id = OS.ONE_WAY_TRIP,
      descriptor =
        OS.Descriptor
          { name = ""
          }
    }

mkOnSearchMessage ::
  DOnSearchReq ->
  OS.OnSearchMessage
mkOnSearchMessage req@DOnSearchReq {..} = do
  let quoteEntitiesList :: [QuoteEntities]
      quoteEntitiesList = map (mkQuoteEntities req) quotes
      fulfillments_ = map (.fulfillment) quoteEntitiesList
      categories_ = map (.category) quoteEntitiesList
      offers_ = mapMaybe (.offer) quoteEntitiesList
      items_ = map (.item) quoteEntitiesList

  let provider =
        OS.Provider
          { id = transporterInfo.shortId.getShortId,
            descriptor = OS.Descriptor {name = ""},
            locations = [],
            categories = categories_,
            items = items_,
            offers = offers_,
            add_ons = [],
            fulfillments = fulfillments_,
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
castVariant Variant.AUTO = Common.HATCHBACK -- FIXME

data QuoteEntities = QuoteEntities
  { fulfillment :: OS.FulfillmentInfo,
    category :: OS.Category,
    offer :: Maybe OS.Offer,
    item :: OS.Item
  }

mkQuoteEntities :: DOnSearchReq -> DQuote.DriverQuote -> QuoteEntities
mkQuoteEntities dReq quote = do
  let fulfillment = mkFulfillment dReq quote
      category = oneWayCategory
      offer = Nothing
      item = mkItem category.id fulfillment.id quote
  QuoteEntities {..}

mkFulfillment :: DOnSearchReq -> DQuote.DriverQuote -> OS.FulfillmentInfo
mkFulfillment dReq quote = do
  let fromLocation = dReq.searchRequest.fromLocation
  let toLocation = dReq.searchRequest.toLocation
  OS.FulfillmentInfo
    { id = mkFulfId quote.id.getId,
      start =
        OS.StartInfo
          { location = OS.Location $ Common.Gps {lat = fromLocation.lat, lon = fromLocation.lon},
            time = Common.TimeTimestamp dReq.now
          },
      end =
        Just
          OS.StopInfo
            { location = OS.Location $ Common.Gps {lat = toLocation.lat, lon = toLocation.lon}
            },
      vehicle =
        OS.FulfillmentVehicle
          { category = castVariant quote.vehicleVariant
          }
    }
  where
    mkFulfId quoteId = "fulf_" <> quoteId

mkItem :: OS.FareProductType -> Text -> DQuote.DriverQuote -> OS.Item
mkItem categoryId fulfillmentId q =
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
                { fareProductType = OS.ONE_WAY_TRIP,
                  vehicleVariant = castVariant q.vehicleVariant,
                  distance = Nothing,
                  duration = Nothing
                }
          },
      quote_terms = [],
      tags =
        Just $
          OS.ItemTags
            { distance_to_nearest_driver = OS.DecimalValue $ toRational q.distanceToPickup.getMeters
            },
      base_distance = Nothing,
      base_duration = Nothing
    }
  where
    price_ = do
      let value_ = OS.DecimalValue $ toRational $ q.baseFare + fromMaybe 0 q.extraFareSelected
      OS.ItemPrice
        { currency = "INR",
          value = value_,
          offered_value = value_
        }
