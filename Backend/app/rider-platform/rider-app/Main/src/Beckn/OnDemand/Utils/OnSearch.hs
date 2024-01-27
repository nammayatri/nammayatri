{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.OnDemand.Utils.OnSearch where

import Beckn.ACL.Common (getTagV2)
import Beckn.OnDemand.Utils.Common as Common
import qualified BecknV2.OnDemand.Types as Spec
import Control.Lens
import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import Data.Time (TimeOfDay (..))
import Domain.Action.Beckn.OnSearch as OnSearch
import Domain.Types.Estimate as Estimate
import Domain.Types.VehicleVariant as VehicleVariant
import EulerHS.Prelude hiding (id, view, (^?))
import Kernel.External.Maps as Maps
import Kernel.Prelude (roundToIntegral)
import Kernel.Types.Beckn.DecimalValue as DecimalValue
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import Tools.Error

getProviderName :: MonadFlow m => Spec.OnSearchReq -> m Text
getProviderName req = do
  providerName <-
    req.onSearchReqMessage
      >>= (.onSearchReqMessageCatalog.catalogProviders)
      >>= safeHead
      >>= (.providerDescriptor)
      >>= (.descriptorName)
      & fromMaybeM (InvalidRequest "Missing Provider Name")
  return providerName

getFulfillmentId :: MonadFlow m => Spec.Item -> m (Id Estimate.BPPEstimate)
getFulfillmentId item = do
  fulfillmentId <-
    item.itemFulfillmentIds
      >>= listToMaybe
      & fromMaybeM (InvalidRequest "Missing Fulfillment Ids")
  return $ Id fulfillmentId

getQuoteFulfillmentId :: MonadFlow m => Spec.Item -> m Text
getQuoteFulfillmentId item = do
  fulfillmentId <-
    item.itemFulfillmentIds
      >>= listToMaybe
      & fromMaybeM (InvalidRequest "Missing Fulfillment Ids")
  return fulfillmentId

getVehicleVariant :: MonadFlow m => Spec.Provider -> Spec.Item -> m (VehicleVariant.VehicleVariant)
getVehicleVariant provider item = do
  variant <-
    item.itemFulfillmentIds
      >>= listToMaybe
      >>= (\fulfillmentId -> provider.providerFulfillments >>= find (\fulf -> fulfId fulf == fulfillmentId))
      >>= (.fulfillmentVehicle)
      >>= (.vehicleVariant)
      & fromMaybeM (InvalidRequest "Missing Vehicle Variant")
  case variant of
    "SEDAN" -> return VehicleVariant.SEDAN
    "SUV" -> return VehicleVariant.SUV
    "HATCHBACK" -> return VehicleVariant.HATCHBACK
    "AUTO_RICKSHAW" -> return VehicleVariant.AUTO_RICKSHAW
    "TAXI" -> return VehicleVariant.TAXI
    "TAXI_PLUS" -> return VehicleVariant.TAXI_PLUS
    _ -> throwError (InvalidRequest $ "Invalid Vehicle Variant" <> variant)
  where
    fulfId :: Spec.Fulfillment -> Text
    fulfId fulf = fulf.fulfillmentId & fromMaybe ("Missing Fulfillment Id")

getEstimatedFare :: MonadFlow m => Spec.Item -> m Money
getEstimatedFare item = do
  price <- item.itemPrice & fromMaybeM (InvalidRequest "Missing Price")
  let value = price.priceValue
  tagValue <- (DecimalValue.valueFromString =<< value) & fromMaybeM (InvalidRequest "Missing fare breakup item: tagValue")
  return $ Money $ roundToIntegral tagValue

getItemId :: MonadFlow m => Spec.Item -> m Text
getItemId item = do
  item.itemId & fromMaybeM (InvalidRequest "Missing Item Id")

getTotalFareRange :: MonadFlow m => Spec.Item -> m (Estimate.FareRange)
getTotalFareRange item = do
  minValue <-
    item.itemPrice
      >>= (.priceMinimumValue)
      >>= DecimalValue.valueFromString
      & fromMaybeM (InvalidRequest "Missing Minimum Value")
  maxValue <-
    item.itemPrice
      >>= (.priceMaximumValue)
      >>= DecimalValue.valueFromString
      & fromMaybeM (InvalidRequest "Missing Maximum Value")
  return $
    Estimate.FareRange
      { Estimate.minFare = Money $ roundToIntegral minValue,
        Estimate.maxFare = Money $ roundToIntegral maxValue
      }

buildEstimateBreakupList :: MonadFlow m => Spec.Item -> m [OnSearch.EstimateBreakupInfo]
buildEstimateBreakupList item = do
  currency <-
    item.itemPrice
      >>= (.priceCurrency)
      & fromMaybeM (InvalidRequest "Missing Currency")
  tagGroups <- item.itemTags & fromMaybeM (InvalidRequest "Missing Tag Groups")
  tagGroup <- find (\tagGroup -> descriptorCode tagGroup.tagGroupDescriptor == Just "fare_breakup") tagGroups & fromMaybeM (InvalidRequest "Missing fare breakup")
  tagList <- tagGroup.tagGroupList & fromMaybeM (InvalidRequest "Missing Tag List")
  mapM (buildEstimateBreakUpItem currency) tagList
  where
    descriptorCode :: Maybe Spec.Descriptor -> Maybe Text
    descriptorCode (Just desc) = desc.descriptorCode
    descriptorCode Nothing = Nothing

buildEstimateBreakUpItem ::
  (MonadThrow m, Log m) =>
  Text ->
  Spec.Tag ->
  m OnSearch.EstimateBreakupInfo
buildEstimateBreakUpItem currency tag = do
  descriptor <- tag.tagDescriptor & fromMaybeM (InvalidRequest "Missing fare breakup item: tagDescriptor")
  let value = tag.tagValue
  tagValue <- (DecimalValue.valueFromString =<< value) & fromMaybeM (InvalidRequest "Missing fare breakup item: tagValue")
  title <- descriptor.descriptorCode & fromMaybeM (InvalidRequest "Missing fare breakup item")
  pure
    OnSearch.EstimateBreakupInfo
      { title = title,
        price =
          OnSearch.BreakupPriceInfo
            { currency = currency,
              value = Money $ roundToIntegral tagValue
            }
      }

buildNightShiftInfo :: Spec.Item -> Maybe OnSearch.NightShiftInfo
buildNightShiftInfo item = do
  itemTags <- item.itemTags
  nightShiftCharge <- getNightShiftCharge itemTags
  let oldNightShiftCharge = getOldNightShiftCharge itemTags
  nightShiftStart <- getNightShiftStart itemTags
  nightShiftEnd <- getNightShiftEnd itemTags
  Just $
    OnSearch.NightShiftInfo
      { oldNightShiftCharge = realToFrac <$> oldNightShiftCharge,
        ..
      }

getNightShiftCharge :: [Spec.TagGroup] -> Maybe Money
getNightShiftCharge tagGroup = do
  tagValue <- getTagV2 "rate_card" "night_shift_charge" tagGroup
  nightShiftCharge <- DecimalValue.valueFromString tagValue
  Just . Money $ roundToIntegral nightShiftCharge

getOldNightShiftCharge :: [Spec.TagGroup] -> Maybe DecimalValue
getOldNightShiftCharge tagGroups = do
  tagValue <- getTagV2 "rate_card" "old_night_shift_charge" tagGroups
  DecimalValue.valueFromString tagValue

getNightShiftStart :: [Spec.TagGroup] -> Maybe TimeOfDay
getNightShiftStart tagGroups = do
  tagValue <- getTagV2 "rate_card" "night_shift_start" tagGroups
  readMaybe $ T.unpack tagValue

getNightShiftEnd :: [Spec.TagGroup] -> Maybe TimeOfDay
getNightShiftEnd tagGroups = do
  tagValue <- getTagV2 "rate_card" "night_shift_end" tagGroups
  readMaybe $ T.unpack tagValue

buildWaitingChargeInfo' :: [Spec.TagGroup] -> Maybe Money
buildWaitingChargeInfo' tagGroups = do
  tagValue <- getTagV2 "rate_card" "waiting_charge_per_min" tagGroups
  waitingChargeValue <- DecimalValue.valueFromString tagValue
  Just . Money $ roundToIntegral waitingChargeValue

buildWaitingChargeInfo :: MonadFlow m => Spec.Item -> m (Maybe OnSearch.WaitingChargesInfo)
buildWaitingChargeInfo item = do
  itemTags <- item.itemTags & fromMaybeM (InvalidRequest "Missing Tags")
  return $
    Just
      OnSearch.WaitingChargesInfo
        { waitingChargePerMin = buildWaitingChargeInfo' itemTags
        }

getProviderLocation :: MonadFlow m => Spec.Provider -> m [Maps.LatLong]
getProviderLocation provider = do
  locations <- provider.providerLocations & fromMaybeM (InvalidRequest "Missing Locations")
  mapM makeLatLong locations

makeLatLong :: MonadFlow m => Spec.Location -> m Maps.LatLong
makeLatLong location = do
  gps <- location.locationGps & fromMaybeM (InvalidRequest "Missing GPS")
  return $ Common.parseLatLong gps

buildSpecialLocationTag :: MonadFlow m => Spec.Item -> m (Maybe Text)
buildSpecialLocationTag item = do
  itemTags <- item.itemTags & fromMaybeM (InvalidRequest "Missing Tags")
  return $ getTagV2 "general_info" "special_location_tag" itemTags
