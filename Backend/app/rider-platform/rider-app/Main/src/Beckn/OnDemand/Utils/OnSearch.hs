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
import qualified Data.Text as T
import Domain.Action.Beckn.OnSearch as OnSearch
import Domain.Types.Estimate as Estimate
import Domain.Types.VehicleVariant as VehicleVariant
import EulerHS.Prelude hiding (id, view, (^?))
import Kernel.External.Maps as Maps
import Kernel.Prelude (TimeOfDay)
import Kernel.Types.Beckn.DecimalValue as DecimalValue
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import Tools.Error

getProviderName :: Spec.OnSearchReq -> Text
getProviderName req = do
  let message = req.onSearchReqMessage & fromMaybe (error "Missing Message")
  let catalog = message.onSearchReqMessageCatalog
  let providers = catalog.catalogProviders & fromMaybe (error "Missing Providers")
  let provider = safeHead providers & fromMaybe (error "Missing Provider")
  let descriptor = provider.providerDescriptor & fromMaybe (error "Missing Descriptor")
  descriptor.descriptorName & fromMaybe (error "Missing Name")

getFulfillmentId :: Spec.Item -> Id Estimate.BPPEstimate
getFulfillmentId item = do
  let fulfillmentIds = item.itemFulfillmentIds & fromMaybe (error "Missing Fulfillment Ids")
  Id $ safeHead fulfillmentIds & fromMaybe (error "Missing Fulfillment Id")

getQuoteFulfillmentId :: Spec.Item -> Text
getQuoteFulfillmentId item = do
  let fulfillmentIds = item.itemFulfillmentIds & fromMaybe (error "Missing Fulfillment Ids")
  safeHead fulfillmentIds & fromMaybe (error "Missing Fulfillment Id")

getVehicleVariant :: Spec.Provider -> Spec.Item -> VehicleVariant.VehicleVariant
getVehicleVariant provider item = do
  let fulfillmentIds = item.itemFulfillmentIds & fromMaybe (error "Missing Fulfillment Ids")
  let fulfillmentId = safeHead fulfillmentIds & fromMaybe (error "Missing Fulfillment Id")
  let fulfillments = provider.providerFulfillments & fromMaybe (error "Missing Fulfillments")
  let fulfillment = find (\fulf -> fulfId fulf == fulfillmentId) fulfillments & fromMaybe (error "Missing fulfillment")
  let vehicle = fulfillment.fulfillmentVehicle & fromMaybe (error "Missing Vehicle")
  let variant = vehicle.vehicleVariant & fromMaybe (error "Missing Variant")
  case variant of
    "SEDAN" -> VehicleVariant.SEDAN
    "SUV" -> VehicleVariant.SUV
    "HATCHBACK" -> VehicleVariant.HATCHBACK
    "AUTO_RICKSHAW" -> VehicleVariant.AUTO_RICKSHAW
    "TAXI" -> VehicleVariant.TAXI
    "TAXI_PLUS" -> VehicleVariant.TAXI_PLUS
    _ -> error "Invalid Vehicle Variant"
  where
    fulfId :: Spec.Fulfillment -> Text
    fulfId fulf = fulf.fulfillmentId & fromMaybe (error "Missing Fulfillment Id")

getEstimatedFare :: Spec.Item -> Money
getEstimatedFare item = do
  let price = item.itemPrice & fromMaybe (error "Missing Price")
  let value = price.priceValue
  let tagValue = (readMaybe . T.unpack =<< value) & fromMaybe (error "Missing fare breakup item: tagValue")
  Money tagValue

getItemId :: Spec.Item -> Text
getItemId item = do
  item.itemId & fromMaybe (error "Missing Item Id")

getTotalFareRange :: Spec.Item -> Estimate.FareRange
getTotalFareRange item = do
  let price = item.itemPrice & fromMaybe (error "Missing Price")
  let mini = price.priceMinimumValue
  let minValue = (readMaybe . T.unpack =<< mini) & fromMaybe (error "Missing Minimum Value")
  let maxi = price.priceMaximumValue
  let maxValue = (readMaybe . T.unpack =<< maxi) & fromMaybe (error "Missing Maximum Value")
  Estimate.FareRange
    { Estimate.minFare = Money minValue,
      Estimate.maxFare = Money maxValue
    }

buildEstimateBreakupList :: MonadFlow m => Spec.Item -> m [OnSearch.EstimateBreakupInfo]
buildEstimateBreakupList item = do
  let price = item.itemPrice & fromMaybe (error "Missing Price")
  let currency = price.priceCurrency & fromMaybe (error "Missing Currency")
  let tagGroups = item.itemTags & fromMaybe (error "Missing Tags")
  tagGroup <- find (\tagGroup -> descriptorCode tagGroup.tagGroupDescriptor == Just "fare_breakup") tagGroups & fromMaybeM (InvalidRequest "Missing fare breakup")
  let tagList = tagGroup.tagGroupList & fromMaybe (error "Missing Tag List")
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
  tagValue <- (readMaybe . T.unpack =<< value) & fromMaybeM (InvalidRequest "Missing fare breakup item: tagValue")
  title <- descriptor.descriptorCode & fromMaybeM (InvalidRequest "Missing fare breakup item")
  pure
    OnSearch.EstimateBreakupInfo
      { title = title,
        price =
          OnSearch.BreakupPriceInfo
            { currency = currency,
              value = Money tagValue
            }
      }

buildNightShiftInfo :: Spec.Item -> Maybe OnSearch.NightShiftInfo
buildNightShiftInfo item = do
  let itemTags = item.itemTags & fromMaybe (error "Missing Tags")
  nightShiftCharge <- getNightShiftCharge itemTags
  oldNightShiftCharge <- getOldNightShiftCharge itemTags
  nightShiftStart <- getNightShiftStart itemTags
  nightShiftEnd <- getNightShiftEnd itemTags
  Just $
    OnSearch.NightShiftInfo
      { oldNightShiftCharge = realToFrac oldNightShiftCharge,
        ..
      }

getNightShiftCharge :: [Spec.TagGroup] -> Maybe Money
getNightShiftCharge tagGroup = do
  tagValue <- getTagV2 "rate_card" "night_shift_charge" tagGroup
  nightShiftCharge <- readMaybe $ T.unpack tagValue
  Just $ Money nightShiftCharge

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
  waitingChargeValue <- readMaybe $ T.unpack tagValue
  Just $ Money waitingChargeValue

buildWaitingChargeInfo :: Spec.Item -> Maybe OnSearch.WaitingChargesInfo
buildWaitingChargeInfo item = do
  let itemTags = item.itemTags & fromMaybe (error "Missing Tags")
  Just
    OnSearch.WaitingChargesInfo
      { waitingChargePerMin = buildWaitingChargeInfo' itemTags
      }

getProviderLocation :: Spec.Provider -> [Maps.LatLong]
getProviderLocation provider = do
  let locations = provider.providerLocations & fromMaybe (error "Missing Locations")
  map makeLatLong locations

makeLatLong :: Spec.Location -> Maps.LatLong
makeLatLong location = do
  let gps = location.locationGps & fromMaybe (error "Missing GPS")
  Common.parseLatLong gps

buildSpecialLocationTag :: Spec.Item -> Maybe Text
buildSpecialLocationTag item = do
  let itemTags = item.itemTags & fromMaybe (error "Missing Tags")
  getTagV2 "general_info" "special_location_tag" itemTags
