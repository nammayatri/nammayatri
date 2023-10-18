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
import qualified Beckn.Types.Core.Taxi.OnSearch as OS
import Beckn.Types.Core.Taxi.OnSearch.Item (BreakupItem (..), BreakupPrice (..), ItemPrice (value))
import Beckn.Types.Core.Taxi.OnSelect (TagGroup (display), TimeTimestamp (..))
import qualified Domain.Action.Beckn.Search as DSearch
import qualified Domain.Types.Estimate as DEst
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.MerchantPaymentMethod as DMPM
import GHC.Float (double2Int)
import Kernel.Prelude
import Kernel.Types.Beckn.DecimalValue as DecimalValue

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
  let (quoteEntitiesList :: [QuoteEntities]) =
        maybe [] (map (mkQuoteEntities startInfo stopInfo provider)) estimateList
          <> maybe [] (map (mkQuoteEntitiesSpecialZone startInfo stopInfo provider)) specialQuoteList
          <> maybe [] (map (mkQuoteEntitiesRental startInfo provider)) rentalQuoteList
  let items = map (.item) quoteEntitiesList
      fulfillments = map (.fulfillment) quoteEntitiesList
  let providerSpec =
        OS.Provider
          { id = provider.subscriberId.getShortId,
            descriptor = OS.Descriptor {name = provider.name},
            locations = maybe [] mkProviderLocations estimateList,
            items,
            fulfillments
          }
  OS.OnSearchMessage $
    OS.Catalog
      { bpp_providers = pure providerSpec,
        bpp_descriptor = OS.Descriptor provider.name
      }
  where
    mkProviderLocations estimatesList =
      foldl (<>) [] $ map mkProviderLocation estimatesList
    mkProviderLocation DSearch.EstimateInfo {..} = toList driverLatLongs

mkStartInfo :: DSearch.DSearchRes -> OS.StartInfo
mkStartInfo dReq =
  OS.StartInfo
    { location =
        OS.Location
          { gps = OS.Gps {lat = dReq.fromLocation.lat, lon = dReq.fromLocation.lon},
            address = Nothing
          },
      time = TimeTimestamp dReq.now
    }

mkStopInfo :: DSearch.DSearchRes -> Maybe OS.StopInfo
mkStopInfo res =
  res.toLocation
    <&> ( \toLoc ->
            OS.StopInfo
              { location =
                  OS.Location
                    { gps = OS.Gps {lat = toLoc.lat, lon = toLoc.lon},
                      address = Nothing
                    }
              }
        )

data QuoteEntities = QuoteEntities
  { fulfillment :: OS.FulfillmentInfo,
    item :: OS.Item
  }

currency' :: Text
currency' = "INR"

mkQuoteEntities :: OS.StartInfo -> Maybe OS.StopInfo -> DM.Merchant -> DSearch.EstimateInfo -> QuoteEntities
mkQuoteEntities start end provider estInfo = do
  let estimate = estInfo.estimate
      variant = Common.castVariant estimate.vehicleVariant
      minPriceDecimalValue = OS.DecimalValue $ toRational estimate.minFare
      maxPriceDecimalValue = OS.DecimalValue $ toRational estimate.maxFare
      estimateBreakupList = buildEstimateBreakUpListTags <$> estimate.estimateBreakupList
      fulfillment =
        OS.FulfillmentInfo
          { start,
            end = end,
            id = estimate.id.getId,
            _type = OS.RIDE,
            vehicle = OS.Vehicle {category = variant}
          }
      item =
        OS.Item
          { id = Common.mkItemId provider.shortId.getShortId estimate.vehicleVariant,
            fulfillment_id = fulfillment.id,
            price =
              OS.ItemPrice
                { currency = currency',
                  value = minPriceDecimalValue,
                  offered_value = minPriceDecimalValue,
                  minimum_value = minPriceDecimalValue,
                  maximum_value = maxPriceDecimalValue
                },
            tags =
              Just $
                OS.TG
                  [ mkGeneralInfoTag estimate,
                    mkFarePolicyTag estimateBreakupList,
                    mkRateCardTag estimate
                  ]
          }
  QuoteEntities
    { fulfillment,
      item
    }
  where
    mkGeneralInfoTag estimate =
      let specialLocationTag = estimate.specialLocationTag
       in OS.TagGroup
            { display = False,
              code = "general_info",
              name = "General Information",
              list =
                [ OS.Tag
                    { display = (\_ -> Just True) =<< specialLocationTag,
                      code = (\_ -> Just "special_location_tag") =<< specialLocationTag,
                      name = (\_ -> Just "Special Location Tag") =<< specialLocationTag,
                      value = specialLocationTag
                    },
                  OS.Tag
                    { display = Just False,
                      code = Just "distance_to_nearest_driver",
                      name = Just "Distance To Nearest Driver",
                      value = Just $ show . double2Int . realToFrac $ estInfo.distanceToNearestDriver
                    }
                ]
            }
    mkFarePolicyTag estimateBreakupList =
      OS.TagGroup
        { display = False,
          code = "fare_breakup",
          name = "Fare Breakup",
          list = estimateBreakupList
        }
    mkRateCardTag estimate =
      let nightShiftCharges = (estimate.nightShiftInfo <&> (.nightShiftCharge))
          oldNightShiftCharges = (OS.DecimalValue . toRational <$> (estimate.nightShiftInfo <&> (.oldNightShiftCharge)))
          nightShiftStart = (estimate.nightShiftInfo <&> (.nightShiftStart))
          waitingChargePerMin = (estimate.waitingCharges.waitingChargePerMin)
          nightShiftEnd = (estimate.nightShiftInfo <&> (.nightShiftEnd))
       in OS.TagGroup
            { display = False,
              code = "rate_card",
              name = "Rate Card",
              list =
                [ OS.Tag
                    { display = (\_ -> Just False) =<< nightShiftCharges,
                      code = (\_ -> Just "night_shift_charge") =<< nightShiftCharges,
                      name = (\_ -> Just "Night Shift Charges") =<< nightShiftCharges,
                      value = (\charges -> Just $ show charges.getMoney) =<< nightShiftCharges
                    },
                  OS.Tag
                    { display = (\_ -> Just False) =<< oldNightShiftCharges,
                      code = (\_ -> Just "old_night_shift_charge") =<< oldNightShiftCharges,
                      name = (\_ -> Just "Old Night Shift Charges") =<< oldNightShiftCharges,
                      value = (Just . DecimalValue.valueToString) =<< oldNightShiftCharges
                    },
                  OS.Tag
                    { display = (\_ -> Just False) =<< nightShiftStart,
                      code = (\_ -> Just "night_shift_start") =<< nightShiftStart,
                      name = (\_ -> Just "Night Shift Start Timings") =<< nightShiftStart,
                      value = (Just . show) =<< nightShiftStart
                    },
                  OS.Tag
                    { display = (\_ -> Just False) =<< waitingChargePerMin,
                      code = (\_ -> Just "waiting_charge_per_min") =<< waitingChargePerMin,
                      name = (\_ -> Just "Waiting Charges Per Min") =<< waitingChargePerMin,
                      value = (\charges -> Just $ show charges.getMoney) =<< waitingChargePerMin
                    },
                  OS.Tag
                    { display = (\_ -> Just False) =<< nightShiftEnd,
                      code = (\_ -> Just "night_shift_end") =<< nightShiftEnd,
                      name = (\_ -> Just "Night Shift End Timings") =<< nightShiftEnd,
                      value = (Just . show) =<< nightShiftEnd
                    }
                ]
            }

mkQuoteEntitiesRental :: OS.StartInfo -> DM.Merchant -> DSearch.RentalQuoteInfo -> QuoteEntities
mkQuoteEntitiesRental start provider it = do
  let variant = Common.castVariant it.vehicleVariant
      baseFare = OS.DecimalValue $ toRational it.baseFare
      fulfillment =
        OS.FulfillmentInfo
          { start,
            end = Nothing,
            id = it.quoteId.getId,
            _type = OS.RENTAL,
            vehicle = OS.Vehicle {category = variant}
          }
      item =
        OS.Item
          { id = Common.mkItemId provider.shortId.getShortId it.vehicleVariant,
            fulfillment_id = fulfillment.id,
            price =
              OS.ItemPrice
                { currency = currency',
                  value = baseFare,
                  offered_value = baseFare,
                  minimum_value = baseFare,
                  maximum_value = baseFare
                },
            tags = Just $ OS.TG [mkRentalTag it]
          }
  QuoteEntities
    { fulfillment,
      item
    }
  where
    mkRentalTag info =
      OS.TagGroup
        { display = False,
          code = "general_info",
          name = "General Information",
          list =
            [ OS.Tag
                { display = Just True,
                  code = Just "rental_base_duration",
                  name = Just "Base Duration",
                  value = Just $ show info.baseDuration.getSeconds
                },
              OS.Tag
                { display = Just True,
                  code = Just "rental_base_distance",
                  name = Just "Base Distance",
                  value = Just $ show info.baseDistance.getMeters
                },
              OS.Tag
                { display = Just True,
                  code = Just "per_hour_charge",
                  name = Just "Per Hour Charge",
                  value = Just $ show info.perHourCharge
                },
              OS.Tag
                { display = Just True,
                  code = Just "per_hour_free_kms",
                  name = Just "Per Hour Free Kms",
                  value = Just $ show info.perHourFreeKms
                },
              OS.Tag
                { display = Just True,
                  code = Just "per_extra_km_rate",
                  name = Just "Per Extra Km Rate",
                  value = Just $ show info.perHourFreeKms
                },
              OS.Tag
                { display = Just True,
                  code = Just "night_shift_charge",
                  name = Just "Night Shift Charge",
                  value = (show . (.getMoney)) <$> info.nightShiftCharge
                }
            ]
        }

mkQuoteEntitiesSpecialZone :: OS.StartInfo -> Maybe OS.StopInfo -> DM.Merchant -> DSearch.SpecialZoneQuoteInfo -> QuoteEntities
mkQuoteEntitiesSpecialZone start end provider it = do
  let variant = Common.castVariant it.vehicleVariant
      estimatedFare = OS.DecimalValue $ toRational it.estimatedFare
      fulfillment =
        OS.FulfillmentInfo
          { start,
            end = end,
            id = it.quoteId.getId,
            _type = OS.RIDE_OTP,
            vehicle = OS.Vehicle {category = variant}
          }
      item =
        OS.Item
          { id = Common.mkItemId provider.shortId.getShortId it.vehicleVariant,
            fulfillment_id = fulfillment.id,
            price =
              OS.ItemPrice
                { currency = currency',
                  value = estimatedFare,
                  offered_value = estimatedFare,
                  minimum_value = estimatedFare,
                  maximum_value = estimatedFare
                },
            tags =
              if isJust it.specialLocationTag
                then Just $ OS.TG [mkSpecialLocationTag it.specialLocationTag]
                else Nothing
          }
  QuoteEntities
    { fulfillment,
      item
    }
  where
    mkSpecialLocationTag specialLocationTag =
      OS.TagGroup
        { display = False,
          code = "general_info",
          name = "General Information",
          list =
            [ OS.Tag
                { display = (\_ -> Just True) =<< specialLocationTag,
                  code = (\_ -> Just "special_location_tag") =<< specialLocationTag,
                  name = (\_ -> Just "Special Location Tag") =<< specialLocationTag,
                  value = specialLocationTag
                }
            ]
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

buildEstimateBreakUpListTags ::
  DEst.EstimateBreakup ->
  OS.Tag
buildEstimateBreakUpListTags DEst.EstimateBreakup {..} = do
  OS.Tag
    { display = Just False,
      code = Just title,
      name = Just title,
      value = Just $ show price.value.getMoney
    }

mkPayment :: DMPM.PaymentMethodInfo -> OS.Payment
mkPayment DMPM.PaymentMethodInfo {..} =
  OS.Payment
    { params =
        OS.PaymentParams
          { collected_by = Common.castDPaymentCollector collectedBy,
            instrument = Just $ Common.castDPaymentInstrument paymentInstrument,
            currency = currency',
            amount = Nothing
          },
      _type = Common.castDPaymentType paymentType,
      uri = Nothing
    }
