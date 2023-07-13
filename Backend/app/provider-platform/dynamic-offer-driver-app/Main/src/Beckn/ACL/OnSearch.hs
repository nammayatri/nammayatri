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
import Beckn.Types.Core.Taxi.OnSearch.Item (BreakupItem (..), BreakupPrice (..))
import qualified Domain.Action.Beckn.Search as DSearch
import qualified Domain.Types.Estimate as DEst
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.MerchantPaymentMethod as DMPM
import GHC.Float (double2Int)
import Kernel.Prelude
import Kernel.Types.Beckn.DecimalValue as DecimalValue

-- import qualified Domain.Types.BlackListOrg as provider

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
  let (quoteEntitiesList :: [QuoteEntities]) = case (estimateList, specialQuoteList) of
        (Just estimates, _) -> map (mkQuoteEntities startInfo stopInfo provider) estimates
        (Nothing, Just quotes) -> map (mkQuoteEntitiesSpecialZone startInfo stopInfo provider) quotes
        (_, _) -> map (mkQuoteEntities startInfo stopInfo provider) [] --this won't happen
  let items = map (.item) quoteEntitiesList
      fulfillments = map (.fulfillment) quoteEntitiesList
  -- contacts = fromMaybe "" provider.mobileNumber
  -- tags =
  --   OS.ProviderTags
  --     { rides_inprogress = 0, --FIXME
  --       rides_completed = 0, --FIXME
  --       rides_confirmed = 0 --FIXME
  --     }
  -- payments = Just $ mkPayment <$> paymentMethodsInfo
  -- TODO For backwards compatibility, remove it. Only payments field used in logic.
  -- payment =
  --   OS.Payment
  --     { collected_by = OS.BPP,
  --       _type = OS.ON_FULFILLMENT,
  --       time = OS.TimeDuration "P2A", -- FIXME: what is this?
  --       instrument = Nothing
  --     }
  let providerSpec =
        OS.Provider
          { id = provider.subscriberId.getShortId,
            descriptor = OS.Descriptor {name = provider.name},
            locations = maybe [] mkProviderLocations estimateList,
            -- categories = [autoOneWayCategory, oneWaySpecialZoneCategory],
            items,
            -- offers = [],
            -- add_ons = [],
            fulfillments
            -- contacts,
            -- tags,
            -- payment,
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
          }
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

mkQuoteEntities :: OS.StartInfo -> OS.StopInfo -> DM.Merchant -> DSearch.EstimateInfo -> QuoteEntities
mkQuoteEntities start end provider estInfo = do
  let estimate = estInfo.estimate
      variant = Common.castVariant estimate.vehicleVariant
      minPriceDecimalValue = OS.DecimalValue $ toRational estimate.minFare
      maxPriceDecimalValue = OS.DecimalValue $ toRational estimate.maxFare
      estimateBreakupList = buildEstimateBreakUpList <$> estimate.estimateBreakupList
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
          { id = Common.mkItemId provider.subscriberId.getShortId estimate.vehicleVariant,
            -- category_id = autoOneWayCategory.id,
            fulfillment_id = fulfillment.id,
            -- offer_id = Nothing,
            price =
              OS.ItemPrice
                { currency = currency',
                  value = minPriceDecimalValue,
                  offered_value = minPriceDecimalValue,
                  minimum_value = minPriceDecimalValue,
                  maximum_value = maxPriceDecimalValue,
                  value_breakup = estimateBreakupList
                },
            -- descriptor =
            --   OS.ItemDescriptor
            --     { name = "",
            --       code = OS.ItemCode OS.DRIVER_OFFER_ESTIMATE variant Nothing Nothing
            --     },
            -- quote_terms = [],
            tags =
              Just $
                OS.TG
                  [ mkGeneralInfoTag estimate,
                    mkFarePolicyTag estimate
                  ]
                  -- OS.TagGroup
                  --   { code_1 = Just "fare_policy",
                  --     name_1 = Just "Fare Policy",
                  --     list_1_code = maybe Nothing (\_ -> Just "night_shift_charge") (estimate.nightShiftInfo <&> (.nightShiftCharge)), --"night_shift_charge",
                  --     list_1_name = maybe Nothing (\_ -> Just "Night Shift Charges") (estimate.nightShiftInfo <&> (.nightShiftCharge)),
                  --     list_1_value = maybe Nothing (\charges -> Just $ show charges.getMoney) (estimate.nightShiftInfo <&> (.nightShiftCharge)),
                  --     list_2_code = maybe Nothing (\_ -> Just "old_night_shift_charge") (OS.DecimalValue . toRational <$> (estimate.nightShiftInfo <&> (.oldNightShiftCharge))),
                  --     list_2_name = maybe Nothing (\_ -> Just "Old Night Shift Charges") (OS.DecimalValue . toRational <$> (estimate.nightShiftInfo <&> (.oldNightShiftCharge))),
                  --     list_2_value = maybe Nothing (\charges -> Just $ DecimalValue.valueToString charges) (OS.DecimalValue . toRational <$> (estimate.nightShiftInfo <&> (.oldNightShiftCharge))),
                  --     list_3_code = maybe Nothing (\_ -> Just "night_shift_start") (estimate.nightShiftInfo <&> (.nightShiftStart)),
                  --     list_3_name = maybe Nothing (\_ -> Just "Night Shift Start Timings") (estimate.nightShiftInfo <&> (.nightShiftStart)),
                  --     list_3_value = maybe Nothing (\time -> Just $ show time) (estimate.nightShiftInfo <&> (.nightShiftStart)),
                  --     list_4_code = maybe Nothing (\_ -> Just "waiting_charge_per_min") estimate.waitingCharges.waitingChargePerMin,
                  --     list_4_name = maybe Nothing (\_ -> Just "Waiting Charges Per Min") estimate.waitingCharges.waitingChargePerMin,
                  --     list_4_value = maybe Nothing (\charges -> Just $ show charges.getMoney) estimate.waitingCharges.waitingChargePerMin,
                  --     list_5_code = maybe Nothing (\_ -> Just "night_shift_end") (estimate.nightShiftInfo <&> (.nightShiftEnd)),
                  --     list_5_name = maybe Nothing (\_ -> Just "Night Shift End Timings") (estimate.nightShiftInfo <&> (.nightShiftEnd)),
                  --     list_5_value = maybe Nothing (\time -> Just $ show time) (estimate.nightShiftInfo <&> (.nightShiftEnd)),
                  --     code_2 = Just "general_info",
                  --     name_2 = Just "General Information",
                  --     list_2_1_code = Just "distance_to_nearest_driver",
                  --     list_2_1_name = Just "Distance To Nearest Driver",
                  --     list_2_1_value = Just $ show . double2Int . realToFrac $ estInfo.distanceToNearestDriver,
                  --     list_2_2_code = maybe Nothing (\_ -> Just "special_location_tag") estimate.specialLocationTag,
                  --     list_2_2_name = maybe Nothing (\_ -> Just "Special Location Tag") estimate.specialLocationTag,
                  --     list_2_2_value = estimate.specialLocationTag
                  --   },
                  -- { distance_to_nearest_driver = Just $ realToFrac estInfo.distanceToNearestDriver,
                  --   -- night_shift_charge = estimate.nightShiftInfo <&> (.nightShiftCharge),
                  --   -- old_night_shift_charge = OS.DecimalValue . toRational <$> (estimate.nightShiftInfo <&> (.oldNightShiftCharge)),
                  --   night_shift_start = estimate.nightShiftInfo <&> (.nightShiftStart),
                  --   night_shift_end = estimate.nightShiftInfo <&> (.nightShiftEnd),
                  --   waiting_charge_per_min = estimate.waitingCharges.waitingChargePerMin,
                  --   -- drivers_location = toList estInfo.driverLatLongs,
                  --   special_location_tag = estimate.specialLocationTag
                  -- },
                  -- data ItemTags = ItemTags
                  --   { distance_to_nearest_driver :: Maybe DecimalValue,
                  --     night_shift_charge :: Maybe Money,
                  --     old_night_shift_charge :: Maybe DecimalValue, -- TODO: Doesn't make sense, to be removed
                  --     night_shift_start :: Maybe TimeOfDay,
                  --     night_shift_end :: Maybe TimeOfDay,
                  --     waiting_charge_per_min :: Maybe Money,
                  --     -- drivers_location :: [LatLong],
                  --     special_location_tag :: Maybe Text
                  --   }

                  -- code_1 :: Maybe Text,
                  -- name_1 :: Maybe Text,
                  -- -- display_1 :: Bool,
                  -- list_1_code :: Maybe Text,
                  -- list_1_name :: Maybe Text,
                  -- list_1_value :: Maybe Text,
                  -- list_2_code :: Maybe Text,
                  -- list_2_name :: Maybe Text,
                  -- list_2_value :: Maybe Text,
                  -- list_3_code :: Maybe Text,
                  -- list_3_name :: Maybe Text,
                  -- list_3_value :: Maybe Text,
                  -- list_4_code :: Maybe Text,
                  -- list_4_name :: Maybe Text,
                  -- list_4_value :: Maybe Text,
                  -- list_5_code :: Maybe Text,
                  -- list_5_name :: Maybe Text,
                  -- list_5_value :: Maybe Text,
                  -- code_2 :: Maybe Text,
                  -- name_2 :: Maybe Text,
                  -- list_2_1_code :: Maybe Text,
                  -- list_2_1_name :: Maybe Text,
                  -- list_2_1_value :: Maybe Text,
                  -- list_2_2_code :: Maybe Text,
                  -- list_2_2_name :: Maybe Text,
                  -- list_2_2_value :: Maybe Text,
                  -- base_distance = Nothing,
                  -- base_duration = Nothing
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
                    { display = (\_ -> Just False) =<< specialLocationTag,
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
    mkFarePolicyTag estimate =
      let nightShiftCharges = (estimate.nightShiftInfo <&> (.nightShiftCharge))
          oldNightShiftCharges = (OS.DecimalValue . toRational <$> (estimate.nightShiftInfo <&> (.oldNightShiftCharge)))
          nightShiftStart = (estimate.nightShiftInfo <&> (.nightShiftStart))
          waitingChargePerMin = (estimate.waitingCharges.waitingChargePerMin)
          nightShiftEnd = (estimate.nightShiftInfo <&> (.nightShiftEnd))
       in OS.TagGroup
            { display = False,
              code = "fare_policy",
              name = "Fare Policy",
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

mkQuoteEntitiesSpecialZone :: OS.StartInfo -> OS.StopInfo -> DM.Merchant -> DSearch.SpecialZoneQuoteInfo -> QuoteEntities
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
          { id = Common.mkItemId provider.subscriberId.getShortId it.vehicleVariant,
            -- category_id = oneWaySpecialZoneCategory.id,
            fulfillment_id = fulfillment.id,
            -- offer_id = Nothing,
            price =
              OS.ItemPrice
                { currency = currency',
                  value = estimatedFare,
                  offered_value = estimatedFare,
                  minimum_value = estimatedFare,
                  maximum_value = estimatedFare,
                  value_breakup = []
                },
            -- descriptor =
            --   OS.ItemDescriptor
            --     { name = "",
            --       code = OS.ItemCode OS.ONE_WAY_SPECIAL_ZONE variant Nothing Nothing
            --     },
            -- quote_terms = [],
            tags =
              if isJust it.specialLocationTag
                then Just $ OS.TG [mkSpecialLocationTag it.specialLocationTag]
                else -- OS.ItemTags
                --   { code_1 = Nothing,
                --     name_1 = Nothing,
                --     list_1_code = Nothing,
                --     list_1_name = Nothing,
                --     list_1_value = Nothing,
                --     list_2_code = Nothing,
                --     list_2_name = Nothing,
                --     list_2_value = Nothing,
                --     list_3_code = Nothing,
                --     list_3_name = Nothing,
                --     list_3_value = Nothing,
                --     list_4_code = Nothing,
                --     list_4_name = Nothing,
                --     list_4_value = Nothing,
                --     list_5_code = Nothing,
                --     list_5_name = Nothing,
                --     list_5_value = Nothing,
                --     code_2 = Just "general_info",
                --     name_2 = Just "General Information",
                --     list_2_1_code = Nothing,
                --     list_2_1_name = Nothing,
                --     list_2_1_value = Nothing,
                --     list_2_2_code = maybe Nothing (\_ -> Just "special_location_tag") it.specialLocationTag,
                --     list_2_2_name = maybe Nothing (\_ -> Just "Special Location Tag") it.specialLocationTag,
                --     list_2_2_value = it.specialLocationTag
                --   }
                  Nothing
                  -- base_distance = Nothing,
                  -- base_duration = Nothing
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
                { display = (\_ -> Just False) =<< specialLocationTag,
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

mkPayment :: DMPM.PaymentMethodInfo -> OS.Payment
mkPayment DMPM.PaymentMethodInfo {..} =
  OS.Payment
    { params =
        OS.PaymentParams
          { collected_by = Common.castDPaymentCollector collectedBy,
            instrument = Just $ Common.castDPaymentInstrument paymentInstrument,
            currency = Nothing,
            amount = Nothing
          },
      _type = Common.castDPaymentType paymentType,
      time = OS.TimeDuration "P2A",
      uri = Nothing
    }

-- mkSpecialLocationTag ::

--     mkSpecialLocationTag =
--       Search.TagGroup
--         { display = False,
--           code = "general_info",
--           name = "General Information",
--           list =
--             [ Search.Tag
--                 { display = maybe Nothing (\_ -> Just False) specialLocationTag,
--                   code = maybe Nothing (\_ -> Just "special_location_tag") specialLocationTag,
--                   name = maybe Nothing (\_ -> Just "Special Location Tag") specialLocationTag,
--                   value = it.specialLocationTag
--                 }
--             ]
--         }
