{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.OnDemand.Utils.OnSearch where

import qualified Beckn.OnDemand.Utils.Common as CUtils
import qualified BecknV2.OnDemand.Enums as Enums
import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.OnDemand.Utils.Payment as OUP
import qualified Data.Aeson as A
import qualified Data.List as List
import qualified Data.Text as T
import Domain.Action.Beckn.Search
import Domain.Types
import Domain.Types.BecknConfig as DBC
import Domain.Types.Merchant as DM
import EulerHS.Prelude hiding (id, view, (^?))
import qualified Kernel.Types.Beckn.Gps as Gps
import Kernel.Types.Common

mkProviderLocations :: [Maybe NearestDriverInfo] -> [Spec.Location]
mkProviderLocations driverLocationsInfo =
  let duplicatesRemoved = List.nubBy ((==) `on` locationId) (catMaybes driverLocationsInfo)
   in foldl (<>) [] $ map mkProviderLocation duplicatesRemoved

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
          locationId = Just locationId,
          locationUpdatedAt = Nothing,
          locationState = Nothing
        }

mkItemLocationIds :: [Maybe NearestDriverInfo] -> Maybe [Text]
mkItemLocationIds driverLocationsInfo = do
  let dLocInfo' = List.nubBy ((==) `on` locationId) (catMaybes driverLocationsInfo)
  case dLocInfo' of
    [] -> Nothing
    dLocInfo -> Just $ map (.locationId) dLocInfo

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

mkPayment :: DM.Merchant -> DBC.BecknConfig -> Maybe Text -> [Spec.Payment]
mkPayment merchant bppConfig mbPaymentId = do
  let mkParams :: (Maybe BknPaymentParams) = (readMaybe . T.unpack) =<< bppConfig.paymentParamsJson
  List.singleton $ OUP.mkPayment (show merchant.city) (show bppConfig.collectedBy) Enums.NOT_PAID Nothing mbPaymentId mkParams bppConfig.settlementType bppConfig.settlementWindow bppConfig.staticTermsUrl bppConfig.buyerFinderFee False Nothing Nothing

mkItemTags :: CUtils.Pricing -> Bool -> Maybe Bool -> Maybe [Spec.TagGroup]
mkItemTags pricing isValueAddNP fareParametersInRateCard = do
  let rateCardTag = CUtils.mkRateCardTag pricing.estimatedDistance (pricing.fareParams >>= (.customerCancellationDues)) (pricing.fareParams >>= (.tollCharges)) pricing.pricingMaxFare (pricing.fareParams >>= (.congestionChargeViaDp)) pricing.farePolicy fareParametersInRateCard pricing.fareParams Nothing
  let vehicleIconTag = CUtils.mkVehicleIconTag pricing.vehicleIconUrl
  vehicleIconTag <> rateCardTag <> (List.singleton <$> CUtils.mkGeneralInfoTagGroup pricing isValueAddNP)

-- | Map TripCategory to ONDC 2.1.0 category descriptor code
-- Re-exports from Common for backward compatibility
tripCategoryToCategoryCode :: TripCategory -> Text
tripCategoryToCategoryCode = CUtils.tripCategoryToCategoryCode

-- | Human-readable name for category code
categoryCodeToName :: Text -> Text
categoryCodeToName = \case
  "ON_DEMAND_TRIP" -> "On Demand Trip"
  "ON_DEMAND_RENTAL" -> "On Demand Rental"
  "SCHEDULED_TRIP" -> "Scheduled Trip"
  "SCHEDULED_RENTAL" -> "Scheduled Rental"
  other -> other

-- | Build a Spec.Category from a category code
mkCategory :: Text -> Spec.Category
mkCategory code =
  Spec.Category
    { categoryDescriptor =
        Just $
          Spec.Descriptor
            { descriptorCode = Just code,
              descriptorName = Just $ categoryCodeToName code,
              descriptorShortDesc = Nothing
            },
      categoryId = Just code
    }

-- | Build deduplicated list of categories from pricings
mkTripCategories :: [CUtils.Pricing] -> [Spec.Category]
mkTripCategories pricings =
  let codes = List.nub $ map (tripCategoryToCategoryCode . (.tripCategory)) pricings
   in map mkCategory codes
