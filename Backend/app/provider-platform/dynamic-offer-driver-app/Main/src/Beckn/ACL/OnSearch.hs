{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.OnSearch where

import qualified Beckn.Types.Core.Taxi.Common.VehicleVariant as Common
import qualified Beckn.Types.Core.Taxi.OnSearch as OS
import Beckn.Types.Core.Taxi.OnSearch.Item (BreakupItem (..), BreakupPrice (..))
import qualified Domain.Action.Beckn.Search as DSearch
import qualified Domain.Types.Vehicle.Variant as Variant
import Kernel.Prelude

autoOneWayCategory :: OS.Category
autoOneWayCategory =
  OS.Category
    { id = OS.DRIVER_OFFER_ESTIMATE,
      descriptor =
        OS.Descriptor
          { name = ""
          }
    }

oneWaySpecialZoneCategory :: OS.Category
oneWaySpecialZoneCategory =
  OS.Category
    { id = OS.ONE_WAY_SPECIAL_ZONE,
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
  let quoteEntitiesList = case (estimateList, specialQuoteList) of
        (Just estimates, _) -> map (mkQuoteEntities startInfo stopInfo) estimates
        (Nothing, Just quotes) -> map (mkQuoteEntitiesSpecialZone startInfo stopInfo) quotes
        (_, _) -> map (mkQuoteEntities startInfo stopInfo) [] --this won't happen
  let items = map (.item) quoteEntitiesList
      fulfillments = map (.fulfillment) quoteEntitiesList
      contacts = fromMaybe "" provider.mobileNumber
      tags =
        OS.ProviderTags
          { rides_inprogress = 0, --FIXME
            rides_completed = 0, --FIXME
            rides_confirmed = 0 --FIXME
          }
      payment =
        OS.Payment
          { collected_by = "BPP",
            _type = OS.ON_FULFILLMENT,
            time = OS.TimeDuration "P2A" -- FIXME: what is this?
          }
  let providerSpec =
        OS.Provider
          { id = provider.subscriberId.getShortId,
            descriptor = OS.Descriptor {name = provider.name},
            locations = [],
            categories = [autoOneWayCategory, oneWaySpecialZoneCategory],
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
      { bpp_providers = pure providerSpec,
        bpp_descriptor = OS.Descriptor provider.subscriberId.getShortId
      }

mkStartInfo :: DSearch.DSearchRes -> OS.StartInfo
mkStartInfo dReq =
  OS.StartInfo
    { location =
        OS.Location
          { gps = OS.Gps {lat = dReq.fromLocation.lat, lon = dReq.fromLocation.lon},
            address = Nothing
          },
      time = OS.TimeTimestamp dReq.now
    }

mkStopInfo :: DSearch.DSearchRes -> OS.StopInfo
mkStopInfo res =
  OS.StopInfo
    { location =
        OS.Location
          { gps = OS.Gps {lat = res.toLocation.lat, lon = res.toLocation.lon},
            address = Nothing
          }
    }

data QuoteEntities = QuoteEntities
  { fulfillment :: OS.FulfillmentInfo,
    item :: OS.Item
  }

currency' :: Text
currency' = "INR"

mkQuoteEntities :: OS.StartInfo -> OS.StopInfo -> DSearch.EstimateItem -> QuoteEntities
mkQuoteEntities start end it = do
  let variant = castVariant it.vehicleVariant
      minPriceDecimalValue = OS.DecimalValue $ toRational it.minFare
      maxPriceDecimalValue = OS.DecimalValue $ toRational it.maxFare
      estimateBreakupList = buildEstimateBreakUpList <$> it.estimateBreakupList
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
            id = show autoOneWayCategory.id,
            price =
              OS.ItemPrice
                { currency = currency',
                  value = minPriceDecimalValue,
                  offered_value = minPriceDecimalValue,
                  minimum_value = minPriceDecimalValue,
                  maximum_value = maxPriceDecimalValue,
                  value_breakup = estimateBreakupList
                },
            descriptor =
              OS.ItemDescriptor
                { name = "",
                  code = OS.ItemCode OS.DRIVER_OFFER_ESTIMATE variant Nothing Nothing
                },
            quote_terms = [],
            tags =
              Just $
                OS.ItemTags
                  { distance_to_nearest_driver = Just $ OS.DecimalValue $ toRational it.distanceToPickup,
                    night_shift_multiplier = OS.DecimalValue . toRational <$> ((.nightShiftMultiplier) =<< it.nightShiftRate),
                    night_shift_start = (.nightShiftStart) =<< it.nightShiftRate,
                    night_shift_end = (.nightShiftEnd) =<< it.nightShiftRate,
                    waiting_charge_per_min = it.waitingCharges.waitingChargePerMin,
                    waiting_time_estimated_threshold = it.waitingCharges.waitingTimeEstimatedThreshold,
                    drivers_location = it.driversLatLong
                  },
            base_distance = Nothing,
            base_duration = Nothing
          }
  QuoteEntities
    { fulfillment,
      item
    }

mkQuoteEntitiesSpecialZone :: OS.StartInfo -> OS.StopInfo -> DSearch.SpecialZoneQuoteInfo -> QuoteEntities
mkQuoteEntitiesSpecialZone start end it = do
  let variant = castVariant it.vehicleVariant
      estimatedFare = OS.DecimalValue $ toRational it.estimatedFare
      fulfillment =
        OS.FulfillmentInfo
          { start,
            end = Just end,
            id = "fulf_" <> show it.quoteId,
            vehicle = OS.FulfillmentVehicle {category = castVariant it.vehicleVariant}
          }
      item =
        OS.Item
          { id = it.quoteId.getId,
            category_id = oneWaySpecialZoneCategory.id,
            fulfillment_id = fulfillment.id,
            offer_id = Nothing,
            price =
              OS.ItemPrice
                { currency = currency',
                  value = estimatedFare,
                  offered_value = estimatedFare,
                  minimum_value = estimatedFare,
                  maximum_value = estimatedFare,
                  value_breakup = []
                },
            descriptor =
              OS.ItemDescriptor
                { name = "",
                  code = OS.ItemCode OS.ONE_WAY_SPECIAL_ZONE variant Nothing Nothing
                },
            quote_terms = [],
            tags =
              Just $
                OS.ItemTags
                  { distance_to_nearest_driver = Nothing,
                    night_shift_multiplier = Nothing,
                    night_shift_start = Nothing,
                    night_shift_end = Nothing,
                    waiting_charge_per_min = Nothing,
                    waiting_time_estimated_threshold = Nothing,
                    drivers_location = []
                  },
            base_distance = Nothing,
            base_duration = Nothing
          }
  QuoteEntities
    { fulfillment,
      item
    }

buildEstimateBreakUpList ::
  DSearch.BreakupItem ->
  BreakupItem
buildEstimateBreakUpList DSearch.BreakupItem {..} = do
  BreakupItem
    { title = title,
      price =
        BreakupPrice
          { currency = price.currency,
            value = realToFrac price.value
          }
    }

castVariant :: Variant.Variant -> Common.VehicleVariant
castVariant Variant.SEDAN = Common.SEDAN
castVariant Variant.HATCHBACK = Common.HATCHBACK
castVariant Variant.SUV = Common.SUV
castVariant Variant.AUTO_RICKSHAW = Common.AUTO_RICKSHAW
