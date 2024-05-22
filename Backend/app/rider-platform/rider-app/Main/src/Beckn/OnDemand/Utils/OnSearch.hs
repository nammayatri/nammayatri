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
import Domain.Types.VehicleServiceTier as DVST
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
  let variant' =
        item.itemFulfillmentIds >>= listToMaybe
          >>= (\fulfillmentId -> provider.providerFulfillments >>= find (\fulf -> fulf.fulfillmentId == Just fulfillmentId))
          >>= (.fulfillmentVehicle)
          >>= (.vehicleVariant)
      category =
        item.itemFulfillmentIds >>= listToMaybe
          >>= (\fulfillmentId -> provider.providerFulfillments >>= find (\fulf -> fulf.fulfillmentId == Just fulfillmentId))
          >>= (.fulfillmentVehicle)
          >>= (.vehicleCategory)
  let variant = map T.toUpper variant'
      mbDVehVariant = Common.parseVehicleVariant category variant
  mbDVehVariant & fromMaybeM (InvalidRequest $ "Unable to parse vehicle category:-" <> show category <> ",vehicle variant:-" <> show variant)

isValidVehVariant :: MonadFlow m => Spec.Fulfillment -> m Bool
isValidVehVariant fulfillment = do
  let variant' = fulfillment.fulfillmentVehicle >>= (.vehicleVariant)
      variant = map T.toUpper variant'
      category = fulfillment.fulfillmentVehicle >>= (.vehicleCategory)
      mbDVehVariant = Common.parseVehicleVariant category variant
  unless (isJust mbDVehVariant) $ do
    logWarning $ "Unable to parse vehicle category:-" <> show category <> ",vehicle variant:-" <> show variant
  return $ isJust mbDVehVariant

isValidItem :: [Spec.Fulfillment] -> Spec.Item -> Bool
isValidItem fulfillments item =
  let fulfId = item.itemFulfillmentIds >>= listToMaybe
   in any (\f -> f.fulfillmentId == fulfId) fulfillments

getServiceTierType :: Spec.Item -> Maybe DVST.VehicleServiceTierType
getServiceTierType item = item.itemDescriptor >>= (.descriptorCode) >>= (readMaybe . T.unpack)

getServiceTierName :: Spec.Item -> Maybe Text
getServiceTierName item = item.itemDescriptor >>= (.descriptorName)

getServiceTierShortDesc :: Spec.Item -> Maybe Text
getServiceTierShortDesc item = item.itemDescriptor >>= (.descriptorShortDesc)

getEstimatedFare :: MonadFlow m => Spec.Item -> Currency -> m Price
getEstimatedFare item currency = do
  price <- item.itemPrice & fromMaybeM (InvalidRequest "Missing Price")
  let value = price.priceValue
  tagValue <- (DecimalValue.valueFromString =<< value) & fromMaybeM (InvalidRequest "Missing fare breakup item: tagValue")
  return $ decimalValueToPrice currency tagValue

getItemId :: MonadFlow m => Spec.Item -> m Text
getItemId item = do
  item.itemId & fromMaybeM (InvalidRequest "Missing Item Id")

getTotalFareRange :: MonadFlow m => Spec.Item -> Currency -> m Estimate.FareRange
getTotalFareRange item currency = do
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
      { Estimate.minFare = decimalValueToPrice currency minValue,
        Estimate.maxFare = decimalValueToPrice currency maxValue
      }
  where
    getPriceField :: (Spec.Price -> Maybe Text) -> (Spec.Price -> Maybe Text) -> Spec.Price -> Maybe Text
    getPriceField f1 f2 price = f1 price <|> f2 price

buildEstimateBreakupList :: MonadFlow m => Spec.Item -> Currency -> m [OnSearch.EstimateBreakupInfo]
buildEstimateBreakupList item currency = do
  let tagGroups = item.itemTags
      tagGroupRateCard = find (\tagGroup_ -> descriptorCode tagGroup_.tagGroupDescriptor == Just (show Tag.FARE_POLICY)) =<< tagGroups -- consume this from now on
      tagListRateCard = (.tagGroupList) =<< tagGroupRateCard
  let breakups = maybe [] (map (buildEstimateBreakUpItem currency)) tagListRateCard
  return (catMaybes breakups)
  where
    descriptorCode :: Maybe Spec.Descriptor -> Maybe Text
    descriptorCode (Just desc) = desc.descriptorCode
    descriptorCode Nothing = Nothing

buildEstimateBreakUpItem ::
  Currency ->
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
            { value = decimalValueToPrice currency tagValue
            }
      }

buildTollChargesInfo :: Spec.Item -> Currency -> Maybe OnSearch.TollChargesInfo
buildTollChargesInfo item currency = do
  let itemTags = item.itemTags
  tollCharges <- getTollCharges itemTags currency
  tollNames <- getTollNames item
  Just $
    OnSearch.TollChargesInfo {..}

getTollCharges :: Maybe [Spec.TagGroup] -> Currency -> Maybe Price
getTollCharges tagGroup currency = do
  tagValue <- Utils.getTagV2 Tag.FARE_POLICY Tag.TOLL_CHARGES tagGroup
  tollCharges <- DecimalValue.valueFromString tagValue
  Just $ decimalValueToPrice currency tollCharges

getTollNames :: Spec.Item -> Maybe [Text]
getTollNames item = do
  tagValueStr <- Utils.getTagV2 Tag.INFO Tag.TOLL_NAMES item.itemTags
  parsedTagValue <- readMaybe tagValueStr :: Maybe [Text]
  return parsedTagValue

getestimatedPickupDuration :: Spec.Item -> Maybe Seconds
getestimatedPickupDuration item = do
  tagValueStr <- Utils.getTagV2 Tag.INFO Tag.DURATION_TO_NEAREST_DRIVER_MINUTES item.itemTags
  parsedTagValue <- readMaybe tagValueStr :: Maybe Seconds
  return parsedTagValue

buildNightShiftInfo :: Spec.Item -> Currency -> Maybe OnSearch.NightShiftInfo
buildNightShiftInfo item currency = do
  let itemTags = item.itemTags
  nightShiftCharge <- getNightShiftCharge itemTags currency
  let oldNightShiftCharge = getOldNightShiftCharge itemTags
  nightShiftStart <- getNightShiftStart itemTags
  nightShiftEnd <- getNightShiftEnd itemTags
  Just $
    OnSearch.NightShiftInfo
      { oldNightShiftCharge = realToFrac <$> oldNightShiftCharge,
        ..
      }

getNightShiftCharge :: Maybe [Spec.TagGroup] -> Currency -> Maybe Price
getNightShiftCharge tagGroup currency = do
  tagValue <- Utils.getTagV2 Tag.FARE_POLICY Tag.NIGHT_CHARGE_MULTIPLIER tagGroup
  nightShiftCharge <- DecimalValue.valueFromString tagValue
  Just $ decimalValueToPrice currency nightShiftCharge

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

getBaseFare :: Maybe [Spec.TagGroup] -> Currency -> Maybe Price
getBaseFare tagGroups currency = do
  tagValue <- Utils.getTagV2 Tag.FARE_POLICY Tag.MIN_FARE tagGroups
  baseFare <- DecimalValue.valueFromString tagValue
  Just $ decimalValueToPrice currency baseFare

getPerHourCharge :: Maybe [Spec.TagGroup] -> Currency -> Maybe Price
getPerHourCharge tagGroups currency = do
  tagValue <- Utils.getTagV2 Tag.FARE_POLICY Tag.PER_HOUR_CHARGE tagGroups
  perHourCharge <- DecimalValue.valueFromString tagValue
  Just $ decimalValueToPrice currency perHourCharge

getPerExtraMinRate :: Maybe [Spec.TagGroup] -> Currency -> Maybe Price
getPerExtraMinRate tagGroups currency = do
  tagValue <- Utils.getTagV2 Tag.FARE_POLICY Tag.PER_MINUTE_CHARGE tagGroups
  perExtraMinRate <- DecimalValue.valueFromString tagValue
  Just $ decimalValueToPrice currency perExtraMinRate

getPerExtraKmRate :: Maybe [Spec.TagGroup] -> Currency -> Maybe Price
getPerExtraKmRate tagGroups currency = do
  tagValue <- Utils.getTagV2 Tag.FARE_POLICY Tag.UNPLANNED_PER_KM_CHARGE tagGroups
  perExtraKmRate <- DecimalValue.valueFromString tagValue
  Just $ decimalValueToPrice currency perExtraKmRate

getIncludedKmPerHr :: Maybe [Spec.TagGroup] -> Maybe Distance
getIncludedKmPerHr tagGroups = do
  tagValue <- Utils.getTagV2 Tag.FARE_POLICY Tag.PER_HOUR_DISTANCE_KM tagGroups
  includedKmPerHr <- DecimalValue.valueFromString tagValue
  Just . metersToDistance . kilometersToMeters . Kilometers $ roundToIntegral includedKmPerHr

getPlannedPerKmRate :: Maybe [Spec.TagGroup] -> Currency -> Maybe Price
getPlannedPerKmRate tagGroups currency = do
  tagValue <- Utils.getTagV2 Tag.FARE_POLICY Tag.PLANNED_PER_KM_CHARGE tagGroups
  plannedPerKmRate <- DecimalValue.valueFromString tagValue
  Just $ decimalValueToPrice currency plannedPerKmRate

getPlannedPerKmRateRoundTrip :: Maybe [Spec.TagGroup] -> Currency -> Maybe Price
getPlannedPerKmRateRoundTrip tagGroups currency = do
  tagValue <- Utils.getTagV2 Tag.FARE_POLICY Tag.PLANNED_PER_KM_CHARGE_ROUND_TRIP tagGroups
  plannedPerKmRateRoundTrip <- DecimalValue.valueFromString tagValue
  Just $ decimalValueToPrice currency plannedPerKmRateRoundTrip

getPerDayMaxHourAllowance :: Maybe [Spec.TagGroup] -> Maybe Hours
getPerDayMaxHourAllowance tagGroups = do
  tagValue <- Utils.getTagV2 Tag.FARE_POLICY Tag.PER_DAY_MAX_HOUR_ALLOWANCE tagGroups
  readMaybe $ T.unpack tagValue

getDeadKilometerFare :: Maybe [Spec.TagGroup] -> Currency -> Maybe Price
getDeadKilometerFare tagGroups currency = do
  tagValue <- Utils.getTagV2 Tag.FARE_POLICY Tag.DEAD_KILOMETER_FARE tagGroups
  deadKmFare <- DecimalValue.valueFromString tagValue
  Just $ decimalValueToPrice currency deadKmFare

buildWaitingChargeInfo' :: Maybe [Spec.TagGroup] -> Currency -> Maybe Price
buildWaitingChargeInfo' tagGroups currency = do
  tagValue <- Utils.getTagV2 Tag.FARE_POLICY Tag.WAITING_CHARGE_PER_MIN tagGroups
  waitingChargeValue <- DecimalValue.valueFromString tagValue
  Just $ decimalValueToPrice currency waitingChargeValue

buildWaitingChargeInfo :: MonadFlow m => Spec.Item -> Currency -> m (Maybe OnSearch.WaitingChargesInfo)
buildWaitingChargeInfo item currency = do
  let waitingChargePerMin' = buildWaitingChargeInfo' item.itemTags currency
  return $ ---------- FIX TODO_______SOUMYAJIT
    Just
      OnSearch.WaitingChargesInfo
        { waitingChargePerMin = waitingChargePerMin'
        }

getProviderLocation :: MonadFlow m => Spec.Provider -> VehicleVariant.VehicleVariant -> m [Maps.LatLong]
getProviderLocation provider vehicleVariant = do
  let locations = provider.providerLocations & fromMaybe []
  latLongs <- mapM (makeLatLong provider vehicleVariant) locations
  return $ catMaybes latLongs

makeLatLong :: (MonadFlow m) => Spec.Provider -> VehicleVariant.VehicleVariant -> Spec.Location -> m (Maybe Maps.LatLong)
makeLatLong provider vehicleVariant location = do
  gps <- location.locationGps & fromMaybeM (InvalidRequest "Missing GPS")
  maybe (return Nothing) (makeLatLongHelper gps) location.locationId
  where
    makeLatLongHelper gps locId = maybe (return Nothing) (parseLatLongHelper locId gps) provider.providerItems

    parseLatLongHelper locId gps providerItems = do
      let providerItem = filter (\item -> maybe False (\locIds -> locId `elem` locIds) item.itemLocationIds) providerItems
          vehicleVariants = traverse extractVehicleVariants providerItem
      if maybe False (\vehVars -> Just vehicleVariant `elem` vehVars) (listToMaybe vehicleVariants)
        then Just <$> Common.parseLatLong gps
        else return Nothing

    extractVehicleVariants item = do
      fromMaybe
        []
        ( item.itemFulfillmentIds
            >>= ( \itemfullfillment ->
                    provider.providerFulfillments
                      >>= ( \provFul ->
                              Just (filterFulfillmentsByFulfillmentId provFul itemfullfillment)
                          )
                )
        )

    filterFulfillmentsByFulfillmentId providerFulfillments arrFullFIllment = do
      let result = find (\fulf -> maybe False (`elem` arrFullFIllment) fulf.fulfillmentId) providerFulfillments
      [ result
          >>= ( \fulf ->
                  fulf.fulfillmentVehicle
                    >>= ( \fVehicle ->
                            Common.parseVehicleVariant
                              fVehicle.vehicleCategory
                              (map T.toUpper (fVehicle.vehicleVariant))
                        )
              )
        ]

buildSpecialLocationTag :: MonadFlow m => Spec.Item -> m (Maybe Text)
buildSpecialLocationTag item =
  return $ Utils.getTagV2 Tag.INFO Tag.SPECIAL_LOCATION_TAG item.itemTags

getIsCustomerPrefferedSearchRoute :: Spec.Item -> Maybe Bool
getIsCustomerPrefferedSearchRoute item = do
  tagValueStr <- Utils.getTagV2 Tag.INFO Tag.IS_CUSTOMER_PREFFERED_SEARCH_ROUTE item.itemTags
  parsedTagValue <- readMaybe tagValueStr :: Maybe Bool
  return parsedTagValue

getIsBlockedRoute :: Spec.Item -> Maybe Bool
getIsBlockedRoute item = do
  tagValueStr <- Utils.getTagV2 Tag.INFO Tag.IS_BLOCKED_SEARCH_ROUTE item.itemTags
  parsedTagValue <- readMaybe tagValueStr :: Maybe Bool
  return parsedTagValue
