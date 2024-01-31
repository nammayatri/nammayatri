{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.OnDemand.Utils.OnSearch where

import qualified BecknV2.OnDemand.Types as Spec
import Control.Lens
import qualified Data.Aeson as A
import qualified Data.List as List
import Domain.Action.Beckn.Search
import qualified Domain.Types.Common as DTC
import Domain.Types.Estimate
import qualified Domain.Types.Estimate as DEst
import qualified Domain.Types.FareParameters as Params
import qualified Domain.Types.FarePolicy as Policy
import qualified Domain.Types.Quote as DQuote
import qualified Domain.Types.Vehicle.Variant as Variant
import EulerHS.Prelude hiding (id, view, (^?))
import GHC.Float (double2Int)
import qualified Kernel.Types.Beckn.Gps as Gps
import Kernel.Types.Common
import SharedLogic.FareCalculator
import SharedLogic.FarePolicy

data Pricing = Pricing
  { pricingId :: Text,
    pricingMaxFare :: Money,
    pricingMinFare :: Money,
    vehicleVariant :: Variant.Variant,
    tripCategory :: DTC.TripCategory,
    fareParams :: Maybe Params.FareParameters,
    farePolicy :: Maybe Policy.FarePolicy,
    estimatedDistance :: Maybe Meters,
    specialLocationTag :: Maybe Text,
    fulfillmentType :: Text,
    distanceToNearestDriver :: Maybe Meters
  }

convertEstimateToPricing :: (DEst.Estimate, Maybe NearestDriverInfo) -> Pricing
convertEstimateToPricing (DEst.Estimate {..}, mbDriverLocations) =
  Pricing
    { pricingId = id.getId,
      pricingMaxFare = maxFare,
      pricingMinFare = minFare,
      fulfillmentType = "RIDE",
      distanceToNearestDriver = mbDriverLocations <&> (.distanceToNearestDriver),
      ..
    }

convertQuoteToPricing :: (DQuote.Quote, Maybe NearestDriverInfo) -> Pricing
convertQuoteToPricing (DQuote.Quote {..}, mbDriverLocations) =
  Pricing
    { pricingId = id.getId,
      pricingMaxFare = estimatedFare,
      pricingMinFare = estimatedFare,
      estimatedDistance = distance,
      fareParams = Just fareParams,
      fulfillmentType = mapToFulfillmentType tripCategory,
      distanceToNearestDriver = mbDriverLocations <&> (.distanceToNearestDriver),
      ..
    }
  where
    mapToFulfillmentType (DTC.OneWay DTC.OneWayRideOtp) = "RIDE_OTP"
    mapToFulfillmentType (DTC.RoundTrip DTC.RideOtp) = "RIDE_OTP"
    mapToFulfillmentType (DTC.RideShare DTC.RideOtp) = "RIDE_OTP"
    mapToFulfillmentType (DTC.Rental _) = "RENTAL"
    mapToFulfillmentType _ = "RIDE_OTP" -- backward compatibility

mkProviderLocations :: [Maybe NearestDriverInfo] -> [Spec.Location]
mkProviderLocations driverLocationsInfo =
  foldl (<>) [] $ map mkProviderLocation (catMaybes driverLocationsInfo)

mkProviderLocation :: NearestDriverInfo -> [Spec.Location]
mkProviderLocation NearestDriverInfo {..} = do
  let driverLocations = toList driverLatLongs
      driverGps = map (\loc -> Gps.Gps {lat = loc.lat, lon = loc.lon}) driverLocations
  flip map driverGps $
    \gps ->
      Spec.Location
        { locationAddress = Nothing,
          locationAreaCode = Nothing,
          locationCity = Nothing,
          locationCountry = Nothing,
          locationGps = A.decode $ A.encode gps,
          locationId = Nothing,
          locationState = Nothing
        }

mkItemTags :: Pricing -> [Spec.TagGroup]
mkItemTags pricing =
  [mkGeneralInfoTag pricing, mkFareParamsTag pricing, mkRateCardTag pricing]

mkGeneralInfoTag :: Pricing -> Spec.TagGroup
mkGeneralInfoTag pricing =
  let specialLocationTag = pricing.specialLocationTag
   in Spec.TagGroup
        { tagGroupDisplay = Just False,
          tagGroupDescriptor =
            Just
              Spec.Descriptor
                { descriptorCode = Just "general_info",
                  descriptorName = Just "General Information",
                  descriptorShortDesc = Nothing
                },
          tagGroupList =
            Just $
              specialLocationTagSingleton specialLocationTag
                ++ distanceToNearestDriverTagSingleton pricing.distanceToNearestDriver
        }
  where
    specialLocationTagSingleton specialLocationTag
      | isNothing specialLocationTag = []
      | otherwise =
        List.singleton $
          Spec.Tag
            { tagDisplay = Just True,
              tagDescriptor =
                Just
                  Spec.Descriptor
                    { descriptorCode = Just "special_location_tag",
                      descriptorName = Just "Special Location Tag",
                      descriptorShortDesc = Nothing
                    },
              tagValue = specialLocationTag
            }

    distanceToNearestDriverTagSingleton Nothing = []
    distanceToNearestDriverTagSingleton (Just distanceToNearestDriver) =
      List.singleton $
        Spec.Tag
          { tagDisplay = Just False,
            tagDescriptor =
              Just
                Spec.Descriptor
                  { descriptorCode = Just "distance_to_nearest_driver",
                    descriptorName = Just "Distance To Nearest Driver",
                    descriptorShortDesc = Nothing
                  },
            tagValue = Just $ show . double2Int . realToFrac $ distanceToNearestDriver
          }

buildFareParamsBreakupsTags :: FareParamsBreakupItem -> Spec.Tag
buildFareParamsBreakupsTags FareParamsBreakupItem {..} = do
  Spec.Tag
    { tagDisplay = Just False,
      tagDescriptor =
        Just
          Spec.Descriptor
            { descriptorCode = Just title,
              descriptorName = Just title,
              descriptorShortDesc = Nothing
            },
      tagValue = Just $ show price.getMoney
    }

mkPrice :: Money -> Money
mkPrice a = a

data FareParamsBreakupItem = FareParamsBreakupItem
  { title :: Text,
    price :: Money
  }

mkFareParamsBreakupItem :: Text -> Money -> FareParamsBreakupItem
mkFareParamsBreakupItem = FareParamsBreakupItem

mkFareParamsTag :: Pricing -> Spec.TagGroup
mkFareParamsTag pricing = do
  let fareParamsBreakups = maybe [] (mkFareParamsBreakups mkPrice mkFareParamsBreakupItem) pricing.fareParams
      fareParamsBreakupsTags = buildFareParamsBreakupsTags <$> fareParamsBreakups
  Spec.TagGroup
    { tagGroupDisplay = Just False,
      tagGroupDescriptor =
        Just
          Spec.Descriptor
            { descriptorCode = Just "fare_breakup",
              descriptorName = Just "Fare Breakup",
              descriptorShortDesc = Nothing
            },
      tagGroupList = Just fareParamsBreakupsTags
    }

buildRateCardTags :: RateCardBreakupItem -> Spec.Tag
buildRateCardTags RateCardBreakupItem {..} = do
  Spec.Tag
    { tagDisplay = Just False,
      tagDescriptor =
        Just
          Spec.Descriptor
            { descriptorCode = Just title,
              descriptorName = Just title,
              descriptorShortDesc = Nothing
            },
      tagValue = Just value
    }

mkValue :: Text -> Text
mkValue a = a

data RateCardBreakupItem = RateCardBreakupItem
  { title :: Text,
    value :: Text
  }

mkRateCardBreakupItem :: Text -> Text -> RateCardBreakupItem
mkRateCardBreakupItem = RateCardBreakupItem

mkRateCardTag :: Pricing -> Spec.TagGroup
mkRateCardTag pricing = do
  let farePolicyBreakups = maybe [] (mkFarePolicyBreakups mkValue mkRateCardBreakupItem pricing.estimatedDistance) pricing.farePolicy
      farePolicyBreakupsTags = buildRateCardTags <$> farePolicyBreakups
  Spec.TagGroup
    { tagGroupDisplay = Just False,
      tagGroupDescriptor =
        Just
          Spec.Descriptor
            { descriptorCode = Just "rate_card",
              descriptorName = Just "Rate Card",
              descriptorShortDesc = Nothing
            },
      tagGroupList = Just farePolicyBreakupsTags
    }
