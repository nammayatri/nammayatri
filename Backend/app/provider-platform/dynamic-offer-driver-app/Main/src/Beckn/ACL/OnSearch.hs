{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.OnSearch where

import qualified Beckn.ACL.Common as Common
import qualified Beckn.Types.Core.Taxi.Common.VehicleVariant as Common
import qualified Beckn.Types.Core.Taxi.OnSearch as OS
import Beckn.Types.Core.Taxi.OnSearch.Item (BreakupItem (..), BreakupPrice (..), ItemTags (night_shift_charge))
import qualified Domain.Action.Beckn.Search as DSearch
import qualified Domain.Types.Estimate as DEst
import qualified Domain.Types.Merchant.MerchantPaymentMethod as DMPM
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
      payments = Just $ mkPayment <$> paymentMethodsInfo
      -- TODO For backwards compatibility, remove it. Only payments field used in logic.
      payment =
        OS.Payment
          { collected_by = OS.BPP,
            _type = OS.ON_FULFILLMENT,
            time = OS.TimeDuration "P2A", -- FIXME: what is this?
            instrument = Nothing
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
            payment,
            payments
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

mkQuoteEntities :: OS.StartInfo -> OS.StopInfo -> DSearch.EstimateInfo -> QuoteEntities
mkQuoteEntities start end estInfo = do
  let estimate = estInfo.estimate
      variant = castVariant estimate.vehicleVariant
      minPriceDecimalValue = OS.DecimalValue $ toRational estimate.minFare
      maxPriceDecimalValue = OS.DecimalValue $ toRational estimate.maxFare
      estimateBreakupList = buildEstimateBreakUpList <$> estimate.estimateBreakupList
      fulfillment =
        OS.FulfillmentInfo
          { start,
            end = Just end,
            id = "ARDU_" <> show estimate.vehicleVariant,
            vehicle = OS.FulfillmentVehicle {category = castVariant estimate.vehicleVariant}
          }
      item =
        OS.Item
          { id = estInfo.estimate.id.getId,
            category_id = autoOneWayCategory.id,
            fulfillment_id = fulfillment.id,
            offer_id = Nothing,
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
                  { distance_to_nearest_driver = Just $ realToFrac estInfo.distanceToNearestDriver,
                    night_shift_charge = estimate.nightShiftInfo <&> (.nightShiftCharge),
                    old_night_shift_charge = OS.DecimalValue . toRational <$> (estimate.nightShiftInfo <&> (.oldNightShiftCharge)),
                    night_shift_start = estimate.nightShiftInfo <&> (.nightShiftStart),
                    night_shift_end = estimate.nightShiftInfo <&> (.nightShiftEnd),
                    waiting_charge_per_min = estimate.waitingCharges.waitingChargePerMin,
                    drivers_location = toList estInfo.driverLatLongs,
                    special_location_tag = estimate.specialLocationTag
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
                    night_shift_charge = Nothing,
                    old_night_shift_charge = Nothing,
                    night_shift_start = Nothing,
                    night_shift_end = Nothing,
                    waiting_charge_per_min = Nothing,
                    drivers_location = [],
                    special_location_tag = it.specialLocationTag
                  },
            base_distance = Nothing,
            base_duration = Nothing
          }
  QuoteEntities
    { fulfillment,
      item
    }

buildEstimateBreakUpList ::
  DEst.EstimateBreakup ->
  BreakupItem
buildEstimateBreakUpList DEst.EstimateBreakup {..} = do
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
castVariant Variant.TAXI = Common.TAXI
castVariant Variant.TAXI_PLUS = Common.TAXI_PLUS

mkPayment :: DMPM.PaymentMethodInfo -> OS.Payment
mkPayment DMPM.PaymentMethodInfo {..} =
  OS.Payment
    { collected_by = Common.castPaymentCollector collectedBy,
      _type = Common.castPaymentType paymentType,
      instrument = Just $ Common.castPaymentInstrument paymentInstrument,
      time = OS.TimeDuration "P2A" -- FIXME: what is this?
    }
