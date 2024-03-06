{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.OnDemand.Utils.OnSearch where

import Beckn.OnDemand.Utils.Common as Common
import qualified BecknV2.OnDemand.Tags as Tag
import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.Utils as Utils
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
import Kernel.Utils.Common
import Tools.Error

getProviderName :: MonadFlow m => Spec.OnSearchReq -> m Text
getProviderName req =
  req.onSearchReqMessage
    >>= (.onSearchReqMessageCatalog.catalogProviders)
    >>= safeHead
    >>= (.providerDescriptor)
    >>= (.descriptorName)
    & fromMaybeM (InvalidRequest "Missing Provider Name")

getQuoteFulfillmentId :: MonadFlow m => Spec.Item -> m Text
getQuoteFulfillmentId item =
  item.itemFulfillmentIds
    >>= listToMaybe
    & fromMaybeM (InvalidRequest "Missing Fulfillment Ids")

getVehicleVariant :: MonadFlow m => Spec.Provider -> Spec.Item -> m VehicleVariant.VehicleVariant
getVehicleVariant provider item = do
  let variant =
        item.itemFulfillmentIds >>= listToMaybe
          >>= (\fulfillmentId -> provider.providerFulfillments >>= find (\fulf -> fulf.fulfillmentId == Just fulfillmentId))
          >>= (.fulfillmentVehicle)
          >>= (.vehicleVariant)
      category =
        item.itemFulfillmentIds >>= listToMaybe
          >>= (\fulfillmentId -> provider.providerFulfillments >>= find (\fulf -> fulf.fulfillmentId == Just fulfillmentId))
          >>= (.fulfillmentVehicle)
          >>= (.vehicleCategory)
  case (category, variant) of
    (Just "CAB", Just "SEDAN") -> return VehicleVariant.SEDAN
    (Just "CAB", Just "SUV") -> return VehicleVariant.SUV
    (Just "CAB", Just "HATCHBACK") -> return VehicleVariant.HATCHBACK
    (Just "AUTO_RICKSHAW", Just "AUTO_RICKSHAW") -> return VehicleVariant.AUTO_RICKSHAW
    (Just "CAB", Just "TAXI") -> return VehicleVariant.TAXI
    (Just "CAB", Just "TAXI_PLUS") -> return VehicleVariant.TAXI_PLUS
    _ -> throwError (InvalidRequest $ "Unable to parse vehicle category:-" <> show category <> ",vehicle variant:-" <> show variant)

getEstimatedFare :: MonadFlow m => Spec.Item -> m Money
getEstimatedFare item = do
  price <- item.itemPrice & fromMaybeM (InvalidRequest "Missing Price")
  let value = price.priceValue
  tagValue <- (DecimalValue.valueFromString =<< value) & fromMaybeM (InvalidRequest "Missing fare breakup item: tagValue")
  return $ Money $ roundToIntegral tagValue

getItemId :: MonadFlow m => Spec.Item -> m Text
getItemId item = do
  item.itemId & fromMaybeM (InvalidRequest "Missing Item Id")

getTotalFareRange :: MonadFlow m => Spec.Item -> m Estimate.FareRange
getTotalFareRange item = do
  minValue <-
    item.itemPrice
      >>= getPriceField (.priceMinimumValue) (.priceValue)
      >>= DecimalValue.valueFromString
      & fromMaybeM (InvalidBecknSchema $ "Missing Price Value:-" <> show item.itemPrice)
  maxValue <-
    item.itemPrice
      >>= getPriceField (.priceMaximumValue) (.priceValue)
      >>= DecimalValue.valueFromString
      & fromMaybeM (InvalidBecknSchema $ "Missing Price Value:-" <> show item.itemPrice)
  return $
    Estimate.FareRange
      { Estimate.minFare = Money $ roundToIntegral minValue,
        Estimate.maxFare = Money $ roundToIntegral maxValue
      }
  where
    getPriceField :: (Spec.Price -> Maybe Text) -> (Spec.Price -> Maybe Text) -> Spec.Price -> Maybe Text
    getPriceField f1 f2 price = maybe (f2 price) Just (f1 price)

buildEstimateBreakupList :: MonadFlow m => Spec.Item -> m [OnSearch.EstimateBreakupInfo]
buildEstimateBreakupList item = do
  currency <-
    item.itemPrice
      >>= (.priceCurrency)
      & fromMaybeM (InvalidRequest "Missing Currency")
  tagGroups <- item.itemTags & fromMaybeM (InvalidRequest "Missing Tag Groups")
  tagGroupRateCard <- find (\tagGroup_ -> descriptorCode tagGroup_.tagGroupDescriptor == Just (show Tag.FARE_POLICY)) tagGroups & fromMaybeM (InvalidRequest "Missing fare policy") -- consume this from now on
  tagListRateCard <- tagGroupRateCard.tagGroupList & fromMaybeM (InvalidRequest "Missing Tag List")
  let breakups = map (buildEstimateBreakUpItem currency) tagListRateCard
  return (catMaybes breakups)
  where
    descriptorCode :: Maybe Spec.Descriptor -> Maybe Text
    descriptorCode (Just desc) = desc.descriptorCode
    descriptorCode Nothing = Nothing

buildEstimateBreakUpItem ::
  Text ->
  Spec.Tag ->
  Maybe OnSearch.EstimateBreakupInfo
buildEstimateBreakUpItem currency tag = do
  descriptor <- tag.tagDescriptor
  value <- tag.tagValue
  tagValue <- DecimalValue.valueFromString value
  title <- descriptor.descriptorCode
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
  let itemTags = item.itemTags
  nightShiftCharge <- getNightShiftCharge itemTags
  let oldNightShiftCharge = getOldNightShiftCharge itemTags
  nightShiftStart <- getNightShiftStart itemTags
  nightShiftEnd <- getNightShiftEnd itemTags
  Just $
    OnSearch.NightShiftInfo
      { oldNightShiftCharge = realToFrac <$> oldNightShiftCharge,
        ..
      }

getNightShiftCharge :: Maybe [Spec.TagGroup] -> Maybe Money
getNightShiftCharge tagGroup = do
  tagValue <- Utils.getTagV2 Tag.FARE_POLICY Tag.NIGHT_CHARGE_MULTIPLIER tagGroup
  nightShiftCharge <- DecimalValue.valueFromString tagValue
  Just . Money $ roundToIntegral nightShiftCharge

getOldNightShiftCharge :: Maybe [Spec.TagGroup] -> Maybe DecimalValue
getOldNightShiftCharge tagGroups = do
  tagValue <- Utils.getTagV2 Tag.FARE_POLICY Tag.NIGHT_SHIFT_CHARGE tagGroups
  DecimalValue.valueFromString tagValue

getNightShiftStart :: Maybe [Spec.TagGroup] -> Maybe TimeOfDay
getNightShiftStart tagGroups = do
  tagValue <- Utils.getTagV2 Tag.FARE_POLICY Tag.NIGHT_SHIFT_START_TIME tagGroups
  readMaybe $ T.unpack tagValue

getNightShiftEnd :: Maybe [Spec.TagGroup] -> Maybe TimeOfDay
getNightShiftEnd tagGroups = do
  tagValue <- Utils.getTagV2 Tag.FARE_POLICY Tag.NIGHT_SHIFT_END_TIME tagGroups
  readMaybe $ T.unpack tagValue

getRentalBaseFare :: Maybe [Spec.TagGroup] -> Maybe Money
getRentalBaseFare tagGroups = do
  tagValue <- Utils.getTagV2 Tag.FARE_POLICY Tag.MIN_FARE tagGroups
  baseFare <- DecimalValue.valueFromString tagValue
  Just . Money $ roundToIntegral baseFare

getRentalPerHourCharge :: Maybe [Spec.TagGroup] -> Maybe Money
getRentalPerHourCharge tagGroups = do
  tagValue <- Utils.getTagV2 Tag.FARE_POLICY Tag.PER_HOUR_CHARGE tagGroups
  perHourCharge <- DecimalValue.valueFromString tagValue
  Just . Money $ roundToIntegral perHourCharge

getRentalPerExtraMinRate :: Maybe [Spec.TagGroup] -> Maybe Money
getRentalPerExtraMinRate tagGroups = do
  tagValue <- Utils.getTagV2 Tag.FARE_POLICY Tag.PER_MINUTE_CHARGE tagGroups
  perExtraMinRate <- DecimalValue.valueFromString tagValue
  Just . Money $ roundToIntegral perExtraMinRate

getRentalPerExtraKmRate :: Maybe [Spec.TagGroup] -> Maybe Money
getRentalPerExtraKmRate tagGroups = do
  tagValue <- Utils.getTagV2 Tag.FARE_POLICY Tag.UNPLANNED_PER_KM_CHARGE tagGroups
  perExtraKmRate <- DecimalValue.valueFromString tagValue
  Just . Money $ roundToIntegral perExtraKmRate

getRentalIncludedKmPerHr :: Maybe [Spec.TagGroup] -> Maybe Kilometers
getRentalIncludedKmPerHr tagGroups = do
  tagValue <- Utils.getTagV2 Tag.FARE_POLICY Tag.PER_HOUR_DISTANCE_KM tagGroups
  includedKmPerHr <- DecimalValue.valueFromString tagValue
  Just . Kilometers $ roundToIntegral includedKmPerHr

getRentalPlannedPerKmRate :: Maybe [Spec.TagGroup] -> Maybe Money
getRentalPlannedPerKmRate tagGroups = do
  tagValue <- Utils.getTagV2 Tag.FARE_POLICY Tag.PLANNED_PER_KM_CHARGE tagGroups
  plannedPerKmRate <- DecimalValue.valueFromString tagValue
  Just . Money $ roundToIntegral plannedPerKmRate

buildWaitingChargeInfo' :: Maybe [Spec.TagGroup] -> Maybe Money
buildWaitingChargeInfo' tagGroups = do
  tagValue <- Utils.getTagV2 Tag.FARE_POLICY Tag.WAITING_CHARGE_PER_MIN tagGroups
  waitingChargeValue <- DecimalValue.valueFromString tagValue
  Just . Money $ roundToIntegral waitingChargeValue

buildWaitingChargeInfo :: MonadFlow m => Spec.Item -> m (Maybe OnSearch.WaitingChargesInfo)
buildWaitingChargeInfo item = do
  let waitingChargePerMin' = buildWaitingChargeInfo' item.itemTags
  return $ ---------- FIX TODO_______SOUMYAJIT
    Just
      OnSearch.WaitingChargesInfo
        { waitingChargePerMin = waitingChargePerMin'
        }

getProviderLocation :: MonadFlow m => Spec.Provider -> m [Maps.LatLong]
getProviderLocation provider =
  let locations = provider.providerLocations & fromMaybe []
   in mapM makeLatLong locations

makeLatLong :: MonadFlow m => Spec.Location -> m Maps.LatLong
makeLatLong location = do
  gps <- location.locationGps & fromMaybeM (InvalidRequest "Missing GPS")
  Common.parseLatLong gps

buildSpecialLocationTag :: MonadFlow m => Spec.Item -> m (Maybe Text)
buildSpecialLocationTag item =
  return $ Utils.getTagV2 Tag.INFO Tag.SPECIAL_LOCATION_TAG item.itemTags

getDescriptorInfo :: Spec.Item -> Maybe Text
getDescriptorInfo item = item.itemDescriptor >>= (.descriptorName)
