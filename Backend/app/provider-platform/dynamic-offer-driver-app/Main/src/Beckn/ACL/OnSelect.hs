{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.OnSelect where

import Beckn.ACL.Common
import Beckn.Types.Core.Taxi.Common.Gps as Common
import Beckn.Types.Core.Taxi.Common.TimeTimestamp as Common
import qualified Beckn.Types.Core.Taxi.OnSelect as OS
import qualified Domain.Types.DriverQuote as DQuote
import qualified Domain.Types.Merchant as DM
import Domain.Types.SearchRequest (SearchRequest)
import Kernel.Prelude
import Kernel.Types.Id (ShortId)

data DOnSelectReq = DOnSelectReq
  { transporterInfo :: TransporterInfo,
    searchRequest :: SearchRequest,
    driverQuote :: DQuote.DriverQuote,
    now :: UTCTime
  }

data TransporterInfo = TransporterInfo
  { subscriberId :: ShortId DM.Subscriber,
    name :: Text,
    contacts :: Text,
    ridesInProgress :: Int,
    ridesCompleted :: Int,
    ridesConfirmed :: Int
  }

driverOfferCategory :: OS.Category
driverOfferCategory =
  OS.Category
    { id = OS.DRIVER_OFFER,
      descriptor =
        OS.Descriptor
          { name = ""
          }
    }

mkOnSelectMessage ::
  DOnSelectReq ->
  OS.OnSelectMessage
mkOnSelectMessage req@DOnSelectReq {..} = do
  -- let quoteEntitiesList :: QuoteEntities
  --     quote = mkQuoteEntities req driverQuote
  let fulfillment = mkFulfillment req driverQuote
      -- fulfillments_ = quote.fulfillment quote
      categories_ = [driverOfferCategory]
      -- offers_ = mapMaybe (.offer) quoteEntitiesList
      item = mkItem fulfillment.id driverQuote
      items_ = [item]

  let provider =
        OS.Provider
          { id = transporterInfo.subscriberId.getShortId,
            descriptor = OS.Descriptor {name = transporterInfo.name},
            locations = [],
            categories = categories_,
            items = items_,
            -- offers = offers_,
            add_ons = [],
            fulfillment = fulfillment,
            contacts = transporterInfo.contacts,
            quote = mkQuote driverQuote,
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
  OS.OnSelectMessage $
    OS.Order {..}

-- data QuoteEntities = QuoteEntities
--   { category :: OS.Category,
--     -- offer :: Maybe OS.Offer,
--     item :: OS.Item
--   }

-- mkQuoteEntities :: DOnSelectReq -> DQuote.DriverQuote -> QuoteEntities
-- mkQuoteEntities dReq quote = do
--   let fulfillment = mkFulfillment dReq quote
--       category = driverOfferCategory
--       -- offer = Nothing
--       item = mkItem fulfillment.id quote
--   QuoteEntities {..}

mkFulfillment :: DOnSelectReq -> DQuote.DriverQuote -> OS.FulfillmentInfo
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
          },
      agent =
        OS.Agent
          { name = Just quote.driverName,
            rateable = Just True,
            tags =
              Just $
                OS.AgentTags
                  { agent_info_rating = maybe Nothing (\rating -> Just $ show $ rating.getCenti) quote.driverRating,
                    agent_info_duration_to_pickup_in_s = Just $ show $ quote.durationToPickup.getSeconds
                  }
          }
    }
  where
    mkFulfId quoteId = "fulf_" <> quoteId

mkItem :: Text -> DQuote.DriverQuote -> OS.Item
mkItem fulfillmentId q =
  OS.Item
    { id = q.id.getId,
      category_id = driverOfferCategory.id,
      fulfillment_id = fulfillmentId,
      -- offer_id = Nothing,
      price = mkPrice q,
      descriptor =
        OS.ItemDescriptor
          { name = "",
            code =
              OS.ItemCode
                { fareProductType = OS.DRIVER_OFFER,
                  vehicleVariant = castVariant q.vehicleVariant,
                  distance = Nothing,
                  duration = Nothing
                }
          },
      -- quote_terms = [],
      tags =
        Just $
          OS.ItemTags
            { distance_to_nearest_driver = OS.DecimalValue $ toRational q.distanceToPickup.getMeters,
              special_location_tag = q.specialLocationTag
            },
      -- base_distance = Nothing,
      -- base_duration = Nothing,
      driver_name = Just q.driverName,
      duration_to_pickup = Just q.durationToPickup.getSeconds,
      valid_till = Just q.validTill,
      rating = q.driverRating
    }

-- where
--   price_ = do
--     let value_ = fromIntegral q.estimatedFare
--     OS.ItemPrice
--       { currency = "INR",
--         value = value_,
--         offered_value = value_
--       }

mkPrice :: DQuote.DriverQuote -> OS.Price
mkPrice quote =
  let value_ = fromIntegral quote.estimatedFare
   in OS.Price
        { currency = "INR",
          value = value_,
          offered_value = value_
        }

mkQuote :: DQuote.DriverQuote -> OS.Quote
mkQuote driverQuote =
  OS.Quote
    { price = mkPrice driverQuote,
      ttl = Nothing, --------- todo
      breakup = Nothing
    }

-- (fromIntegral $ div (fromEnum . nominalDiffTimeToSeconds $ latency) 1000000000000)
-- let Duration =
