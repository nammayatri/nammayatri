{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.OnSearch where

import Beckn.ACL.Common (validatePrices)
import qualified Beckn.ACL.Common as Common
import qualified Beckn.Types.Core.Taxi.API.OnSearch as OnSearch
import qualified Beckn.Types.Core.Taxi.OnSearch as OnSearch
import Beckn.Types.Core.Taxi.OnSearch.Item (BreakupItem (..))
import qualified Data.Text as T
import qualified Domain.Action.Beckn.OnSearch as DOnSearch
import qualified Domain.Types.Estimate as DEstimate
import qualified Domain.Types.Merchant.MerchantPaymentMethod as DMPM
import Domain.Types.OnSearchEvent
import qualified Domain.Types.VehicleVariant as VehVar
import EulerHS.Prelude hiding (id, state, unpack)
import GHC.Float (int2Double)
import Kernel.External.Maps (LatLong)
import Kernel.Prelude
import Kernel.Product.Validation.Context (validateContext)
import Kernel.Storage.Esqueleto (runTransaction)
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Beckn.DecimalValue as DecimalValue
import Kernel.Types.Beckn.ReqTypes
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.OnSearchEvent as OnSearchEvent
import Tools.Error

buildOnSearchReq ::
  ( HasFlowEnv m r '["coreVersion" ::: Text],
    EsqDBFlow m r
  ) =>
  BecknCallbackReq OnSearch.OnSearchMessage ->
  m (Maybe DOnSearch.DOnSearchReq)
buildOnSearchReq req = do
  validateContext Context.ON_SEARCH $ req.context
  logOnSearchEvent req
  case req.contents of
    Right msg -> do
      let catalog = msg.catalog
      Just <$> searchCbService req.context catalog
    Left err -> do
      logTagError "on_search req" $ "on_search error: " <> show err
      pure Nothing

searchCbService :: MonadFlow m => Context.Context -> OnSearch.Catalog -> m DOnSearch.DOnSearchReq
searchCbService context catalog = do
  providerId <- context.bpp_id & fromMaybeM (InvalidRequest "Missing bpp_id")
  providerUrl <- context.bpp_uri & fromMaybeM (InvalidRequest "Missing bpp_uri")
  -- do we need throw an error when we have more than one provider?
  let (provider :| _) = catalog.bpp_providers
  let items = provider.items
  (estimatesInfo, quotesInfo) <- partitionEithers <$> traverse (buildEstimateOrQuoteInfo provider.locations) items
  let providerInfo =
        DOnSearch.ProviderInfo
          { providerId = providerId,
            name = provider.descriptor.name,
            url = providerUrl,
            mobileNumber = provider.contacts,
            ridesCompleted = provider.tags.rides_completed
          }
  let paymentMethodsInfo = mkPayment `mapMaybe` (fromMaybe [] provider.payments)
  pure
    DOnSearch.DOnSearchReq
      { requestId = Id context.message_id,
        ..
      }

logOnSearchEvent :: EsqDBFlow m r => OnSearch.OnSearchReq -> m ()
logOnSearchEvent (BecknCallbackReq context (leftToMaybe -> mbErr)) = do
  createdAt <- getCurrentTime
  id <- generateGUID
  bppId <- context.bpp_id & fromMaybeM (InvalidRequest "Missing context.bpp_id")
  let messageId = context.message_id
  let errorType = show.(._type) <$> mbErr
  let errorCode = (.code) <$> mbErr
  let errorMessage = (.message) =<< mbErr
  runTransaction $
    OnSearchEvent.create $
      OnSearchEvent {..}

buildEstimateOrQuoteInfo ::
  (MonadThrow m, Log m) =>
  [LatLong] ->
  OnSearch.Item ->
  m (Either DOnSearch.EstimateInfo DOnSearch.QuoteInfo)
buildEstimateOrQuoteInfo driversLocation item = do
  let itemCode = item.descriptor.code
      vehicleVariant = castVehicleVariant itemCode.vehicleVariant
      estimatedFare = roundToIntegral item.price.value
      estimatedTotalFare = roundToIntegral item.price.offered_value
      estimateBreakupList = buildEstimateBreakUpList <$> item.price.value_breakup
      descriptions = item.quote_terms
      nightShiftInfo = buildNightShiftInfo =<< item.tags
      waitingCharges = buildWaitingChargeInfo <$> item.tags
      -- driversLocation = provider_locations--fromMaybe [] $ item.tags <&> (.drivers_location)
      specialLocationTag = buildSpecialLocationTag =<< item.tags -- >>= (.special_location_tag)
  validatePrices estimatedFare estimatedTotalFare
  let totalFareRange =
        DEstimate.FareRange
          { minFare = roundToIntegral item.price.minimum_value,
            maxFare = roundToIntegral item.price.maximum_value
          }
  validateFareRange estimatedTotalFare totalFareRange

  -- if we get here, the discount >= 0, estimatedFare >= estimatedTotalFare
  let discount = if estimatedTotalFare == estimatedFare then Nothing else Just $ estimatedFare - estimatedTotalFare
  case item.category_id of
    OnSearch.ONE_WAY_TRIP -> do
      quoteDetails <- DOnSearch.OneWayDetails <$> buildOneWayQuoteDetails item
      pure $ Right DOnSearch.QuoteInfo {..}
    OnSearch.RENTAL_TRIP -> do
      quoteDetails <- DOnSearch.RentalDetails <$> buildRentalQuoteDetails item
      pure $ Right DOnSearch.QuoteInfo {..}
    OnSearch.DRIVER_OFFER_ESTIMATE -> pure $ Left DOnSearch.EstimateInfo {bppEstimateId = Id item.id, ..}
    OnSearch.DRIVER_OFFER -> throwError $ InvalidRequest "DRIVER_OFFER supported in on_select, use DRIVER_OFFER_ESTIMATE"
    OnSearch.ONE_WAY_SPECIAL_ZONE -> do
      quoteDetails <- DOnSearch.OneWaySpecialZoneDetails <$> buildOneWaySpecialZoneQuoteDetails item
      pure $ Right DOnSearch.QuoteInfo {..}
  where
    castVehicleVariant = \case
      OnSearch.SEDAN -> VehVar.SEDAN
      OnSearch.SUV -> VehVar.SUV
      OnSearch.HATCHBACK -> VehVar.HATCHBACK
      OnSearch.AUTO_RICKSHAW -> VehVar.AUTO_RICKSHAW
      OnSearch.TAXI -> VehVar.TAXI
      OnSearch.TAXI_PLUS -> VehVar.TAXI_PLUS

buildOneWayQuoteDetails ::
  (MonadThrow m, Log m) =>
  OnSearch.Item ->
  m DOnSearch.OneWayQuoteDetails
buildOneWayQuoteDetails item = do
  distanceToNearestDriver <-
    (item.tags >>= buildDistanceToNearestDriver)
      & fromMaybeM (InvalidRequest "Trip type is ONE_WAY, but distanceToNearestDriver is Nothing")
  pure
    DOnSearch.OneWayQuoteDetails
      { distanceToNearestDriver = realToFrac distanceToNearestDriver
      }

buildOneWaySpecialZoneQuoteDetails ::
  (MonadThrow m, Log m) =>
  OnSearch.Item ->
  m DOnSearch.OneWaySpecialZoneQuoteDetails
buildOneWaySpecialZoneQuoteDetails item = do
  pure
    DOnSearch.OneWaySpecialZoneQuoteDetails
      { quoteId = item.id
      }

-- FIXME remove round by using Kilometers and Hours in spec
buildRentalQuoteDetails ::
  (MonadThrow m, Log m) =>
  OnSearch.Item ->
  m DOnSearch.RentalQuoteDetails
buildRentalQuoteDetails item = do
  baseDistance <- item.base_distance & fromMaybeM (InvalidRequest "Missing base_distance in rental search item")
  baseDuration <- item.base_duration & fromMaybeM (InvalidRequest "Missing base_duration in rental search item")
  pure DOnSearch.RentalQuoteDetails {..}

validateFareRange :: (MonadThrow m, Log m) => Money -> DEstimate.FareRange -> m ()
validateFareRange totalFare DEstimate.FareRange {..} = do
  when (minFare < 0) $ throwError $ InvalidRequest "Minimum discounted price is less than zero"
  when (maxFare < 0) $ throwError $ InvalidRequest "Maximum discounted price is less than zero"
  when (maxFare < minFare) $ throwError $ InvalidRequest "Maximum discounted price is less than minimum discounted price"
  when (totalFare > maxFare || totalFare < minFare) $ throwError $ InvalidRequest "Discounted price outside of range"

buildEstimateBreakUpList ::
  BreakupItem ->
  DOnSearch.EstimateBreakupInfo
buildEstimateBreakUpList BreakupItem {..} = do
  DOnSearch.EstimateBreakupInfo
    { title = title,
      price =
        DOnSearch.BreakupPriceInfo
          { currency = price.currency,
            value = roundToIntegral price.value
          }
    }

buildNightShiftInfo ::
  OnSearch.ItemTags ->
  Maybe DOnSearch.NightShiftInfo
buildNightShiftInfo itemTags = do
  nightShiftCharge <- getNightShiftCharge itemTags
  oldNightShiftCharge <- getOldNightShiftCharge itemTags
  nightShiftStart <- getNightShiftStart itemTags
  nightShiftEnd <- getNightShiftEnd itemTags
  -- ((,,,) <$> nightShiftCharge <*> oldNightShiftCharge <*> nightShiftStart <*> nightShiftEnd)
  --   <&> \(nightShiftCharge, oldNightShiftCharge, nightShiftStart, nightShiftEnd) ->
  Just $
    DOnSearch.NightShiftInfo
      { oldNightShiftCharge = realToFrac oldNightShiftCharge,
        ..
      }

buildWaitingChargeInfo' :: OnSearch.ItemTags -> Maybe Money
buildWaitingChargeInfo' tags = do
  code_1 <- tags.code_1
  list4Code <- tags.list_4_code
  if list4Code == "waiting_charge_per_min" && code_1 == "fare_policy"
    then do
      list4Value <- tags.list_4_value
      waitingChargeValue <- readMaybe $ T.unpack list4Value
      Just $ Money waitingChargeValue
    else Nothing

buildWaitingChargeInfo :: OnSearch.ItemTags -> DOnSearch.WaitingChargesInfo
buildWaitingChargeInfo tags = do
  DOnSearch.WaitingChargesInfo
    { waitingChargePerMin = buildWaitingChargeInfo' tags
    }

buildSpecialLocationTag :: OnSearch.ItemTags -> Maybe Text
buildSpecialLocationTag tags = do
  code_2 <- tags.code_2
  list22Code <- tags.list_2_2_code
  if list22Code == "special_location_tag" && code_2 == "general_info"
    then do
      tags.list_2_2_value
    else -- waitingChargeValue <- readMaybe $ T.unpack list22Value
    -- Just $ Meters waitingChargeValue
      Nothing

getNightShiftCharge :: OnSearch.ItemTags -> Maybe Money
getNightShiftCharge tags = do
  code_1 <- tags.code_1
  list1Code <- tags.list_1_code
  if list1Code == "night_shift_charge" && code_1 == "fare_policy"
    then do
      list1Value <- tags.list_1_value
      nightShiftCharge <- readMaybe $ T.unpack list1Value
      Just $ Money nightShiftCharge
    else Nothing

getOldNightShiftCharge :: OnSearch.ItemTags -> Maybe DecimalValue
getOldNightShiftCharge tags = do
  code_1 <- tags.code_1
  list2Code <- tags.list_2_code
  if list2Code == "old_night_shift_charge" && code_1 == "fare_policy"
    then do
      list2Value <- tags.list_2_value
      DecimalValue.valueFromString list2Value
    else -- Just $ Money oldNightShiftCharge
      Nothing

buildDistanceToNearestDriver :: OnSearch.ItemTags -> Maybe DecimalValue
buildDistanceToNearestDriver tags = do
  code_2 <- tags.code_2
  list21Code <- tags.list_2_1_code
  if list21Code == "distance_to_nearest_driver" && code_2 == "general_info"
    then do
      list21Value <- tags.list_2_1_value
      distanceToNearestDriver <- readMaybe $ T.unpack list21Value
      -- HighPrecMeters . realToFrac $ int2Double n
      Just $ realToFrac $ int2Double distanceToNearestDriver
    else Nothing

-- buildWaitingChargeInfo' tags

-- buildWaitingChargeInfo ::
--   OnSearch.ItemTags ->
--   DOnSearch.WaitingChargesInfo
-- buildWaitingChargeInfo itemTags = do
--   DOnSearch.WaitingChargesInfo
--     { waitingChargePerMin = itemTags.waiting_charge_per_min
--     }

getNightShiftStart :: OnSearch.ItemTags -> Maybe TimeOfDay
getNightShiftStart tags = do
  code_1 <- tags.code_1
  list3Code <- tags.list_3_code
  if list3Code == "night_shift_start" && code_1 == "fare_policy"
    then do
      list3Value <- tags.list_3_value
      readMaybe $ T.unpack list3Value
    else -- Just $ Money nightShiftStart
      Nothing

getNightShiftEnd :: OnSearch.ItemTags -> Maybe TimeOfDay
getNightShiftEnd tags = do
  code_1 <- tags.code_1
  list5Code <- tags.list_5_code
  if list5Code == "night_shift_start" && code_1 == "fare_policy"
    then do
      list5Value <- tags.list_5_value
      readMaybe $ T.unpack list5Value
    else -- Just $ Money nightShiftStart
      Nothing

mkPayment :: OnSearch.Payment -> Maybe DMPM.PaymentMethodInfo
mkPayment OnSearch.Payment {..} =
  instrument <&> \instrument' -> do
    DMPM.PaymentMethodInfo
      { collectedBy = Common.castPaymentCollector collected_by,
        paymentType = Common.castPaymentType _type,
        paymentInstrument = Common.castPaymentInstrument instrument'
      }
