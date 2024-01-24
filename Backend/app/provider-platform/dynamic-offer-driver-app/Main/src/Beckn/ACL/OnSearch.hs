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
import qualified Beckn.OnDemand.Transformer.OnSearch as TOnSearch
import qualified Beckn.OnDemand.Utils.OnSearch as UOnSearch
import qualified Beckn.Types.Core.Taxi.OnSearch as OS
import qualified BecknV2.OnDemand.Types as Spec
import qualified Domain.Action.Beckn.Search as DSearch
import qualified Domain.Types.Merchant as DM
import GHC.Float (double2Int)
import Kernel.Prelude
import Kernel.Types.App
import qualified Kernel.Types.Beckn.Context as Context
import SharedLogic.FareCalculator
import SharedLogic.FarePolicy

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
  let pricings = (map UOnSearch.convertEstimateToPricing estimates) <> (map UOnSearch.convertQuoteToPricing quotes)
  let (pricingEntities :: [PricingEntities]) = map (mkPricingEntities startInfo stopInfo provider) pricings
  let items = map (.item) pricingEntities
      fulfillments = map (.fulfillment) pricingEntities
  let providerSpec =
        OS.Provider
          { id = provider.subscriberId.getShortId,
            descriptor = OS.Descriptor {name = provider.name},
            locations = mkProviderLocations ((map snd estimates) <> (map snd quotes)),
            items,
            fulfillments
          }
  OS.OnSearchMessage $
    OS.Catalog
      { bpp_providers = pure providerSpec,
        bpp_descriptor = OS.Descriptor provider.name
      }
  where
    mkProviderLocations nearestDriverlocations =
      foldl (<>) [] $ map mkProviderLocation (catMaybes nearestDriverlocations)
    mkProviderLocation DSearch.NearestDriverInfo {..} = toList driverLatLongs

mkOnSearchRequest ::
  (MonadFlow m) =>
  DSearch.DSearchRes ->
  Context.Action ->
  Context.Domain ->
  Text ->
  Maybe Text ->
  Text ->
  BaseUrl ->
  Maybe Text ->
  Maybe BaseUrl ->
  Context.City ->
  Context.Country ->
  m Spec.OnSearchReq
mkOnSearchRequest = TOnSearch.buildOnSearchRideReq

mkStartInfo :: DSearch.DSearchRes -> OS.StartInfo
mkStartInfo dReq =
  OS.StartInfo
    { location =
        OS.Location
          { gps = OS.Gps {lat = dReq.fromLocation.lat, lon = dReq.fromLocation.lon},
            address = Nothing
          }
    }

mkStopInfo :: DSearch.DSearchRes -> Maybe OS.StopInfo
mkStopInfo res =
  ( \toLoc ->
      OS.StopInfo
        { location =
            OS.Location
              { gps = OS.Gps {lat = toLoc.lat, lon = toLoc.lon},
                address = Nothing
              }
        }
  )
    <$> res.toLocation

data PricingEntities = PricingEntities
  { fulfillment :: OS.FulfillmentInfo,
    item :: OS.Item
  }

currency' :: Text
currency' = "INR"

mkPricingEntities :: OS.StartInfo -> Maybe OS.StopInfo -> DM.Merchant -> UOnSearch.Pricing -> PricingEntities
mkPricingEntities start end provider pricing = do
  let variant = Common.castVariant pricing.vehicleVariant
      minPriceDecimalValue = OS.DecimalValue $ toRational pricing.pricingMinFare
      maxPriceDecimalValue = OS.DecimalValue $ toRational pricing.pricingMaxFare
      fareParamsBreakups = maybe [] (mkFareParamsBreakups UOnSearch.mkPrice UOnSearch.mkFareParamsBreakupItem) pricing.fareParams
      fareParamsBreakupsTags = buildFareParamsBreakupsTags <$> fareParamsBreakups

      rateCardBreakups = maybe [] (mkFarePolicyBreakups UOnSearch.mkValue UOnSearch.mkRateCardBreakupItem pricing.estimatedDistance) pricing.farePolicy
      rateCardTags = buildRateCardTags <$> rateCardBreakups

      fulfillment =
        OS.FulfillmentInfo
          { start,
            end = end,
            id = pricing.pricingId,
            _type = pricing.fulfillmentType,
            vehicle = OS.Vehicle {category = variant}
          }
      item =
        OS.Item
          { id = Common.mkItemId provider.shortId.getShortId pricing.vehicleVariant,
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
                  [ mkGeneralInfoTag,
                    mkFareParamsTag fareParamsBreakupsTags,
                    mkRateCardTag rateCardTags
                  ]
          }
  PricingEntities
    { fulfillment,
      item
    }
  where
    mkGeneralInfoTag =
      let specialLocationTag = pricing.specialLocationTag
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
                      value = (show . double2Int . realToFrac) <$> pricing.distanceToNearestDriver
                    }
                ]
            }

    mkFareParamsTag fareParamsBreakupsTags =
      OS.TagGroup
        { display = False,
          code = "fare_breakup",
          name = "Fare Breakup",
          list = fareParamsBreakupsTags
        }

    mkRateCardTag rateCardTags =
      OS.TagGroup
        { display = False,
          code = "rate_card",
          name = "Rate Card",
          list = rateCardTags
        }

buildFareParamsBreakupsTags ::
  UOnSearch.FareParamsBreakupItem ->
  OS.Tag
buildFareParamsBreakupsTags UOnSearch.FareParamsBreakupItem {..} = do
  OS.Tag
    { display = Just False,
      code = Just title,
      name = Just title,
      value = Just $ show price.getMoney
    }

buildRateCardTags ::
  UOnSearch.RateCardBreakupItem ->
  OS.Tag
buildRateCardTags UOnSearch.RateCardBreakupItem {..} = do
  OS.Tag
    { display = Just False,
      code = Just title,
      name = Just title,
      value = Just value
    }
