{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.OnSearch where

import Beckn.ACL.Common (getTag, validatePrices)
-- import Beckn.Types.Core.Taxi.OnSearch.Item (BreakupItem (..))

-- import Kernel.External.Maps (LatLong)

-- import Kernel.Storage.Esqueleto (runTransaction)

import qualified Beckn.OnDemand.Transformer.OnSearch as TOnSearch
import qualified Beckn.OnDemand.Utils.Common as Utils
import qualified Beckn.Types.Core.Taxi.API.OnSearch as OnSearch
import qualified Beckn.Types.Core.Taxi.OnSearch as OnSearch
import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.OnDemand.Utils.Context as ContextUtils
import qualified Data.Text as T
import qualified Domain.Action.Beckn.OnSearch as DOnSearch
import qualified Domain.Types.Estimate as DEstimate
import Domain.Types.OnSearchEvent
import qualified Domain.Types.VehicleVariant as VehVar
import EulerHS.Prelude hiding (find, id, map, readMaybe, state, unpack)
import GHC.Float (int2Double)
import Kernel.Prelude
import Kernel.Product.Validation.Context (validateContext)
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

buildOnSearchReqV2 ::
  ( HasFlowEnv m r '["_version" ::: Text],
    EsqDBFlow m r
  ) =>
  Spec.OnSearchReq ->
  m (Maybe DOnSearch.DOnSearchReq)
buildOnSearchReqV2 req = do
  ContextUtils.validateContext Context.ON_SEARCH req.onSearchReqContext
  logOnSearchEventV2 req
  case req.onSearchReqError of
    Nothing -> do
      -- generated function
      message <- req.onSearchReqMessage & fromMaybeM (InvalidRequest "Missing message")
      let catalog = message.onSearchReqMessageCatalog
      providers <- catalog.catalogProviders & fromMaybeM (InvalidRequest "Missing Providers")
      provider <- safeHead providers & fromMaybeM (InvalidRequest "Missing Provider")
      items <- provider.providerItems & fromMaybeM (InvalidRequest "Missing Items")
      Just <$> TOnSearch.buildOnSearchReq req provider items
    Just err -> do
      logTagError "on_search req" $ "on_search error: " <> show err
      pure Nothing

searchCbService :: MonadFlow m => Context.Context -> OnSearch.Catalog -> m DOnSearch.DOnSearchReq
searchCbService context catalog = do
  providerId <- context.bpp_id & fromMaybeM (InvalidRequest "Missing bpp_id")
  providerUrl <- context.bpp_uri & fromMaybeM (InvalidRequest "Missing bpp_uri")
  -- do we need throw an error when we have more than one provider?
  let (provider :| _) = catalog.bpp_providers
  let items = provider.items
  (estimatesInfo, quotesInfo) <- partitionEithers <$> traverse (buildEstimateOrQuoteInfo provider) items
  let providerInfo =
        DOnSearch.ProviderInfo
          { providerId = providerId,
            name = provider.descriptor.name,
            url = providerUrl,
            mobileNumber = "", ----------TODO----------Need to remove it or make it maybe in db
            ridesCompleted = 0 ----------TODO----------Need to remove it or make it maybe in db
          }
  let paymentMethodsInfo = [] ----------TODO----------Need to remove it or make it maybe in db
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
  -- runTransaction $
  void $
    OnSearchEvent.create $
      OnSearchEvent {..}

logOnSearchEventV2 :: EsqDBFlow m r => Spec.OnSearchReq -> m ()
logOnSearchEventV2 req = do
  let context = req.onSearchReqContext
  createdAt <- getCurrentTime
  id <- generateGUID
  bppId <- Utils.getContextBapId context
  messageId <- Utils.getMessageIdText context
  (errorCode, errorMessage, errorType) <- case req.onSearchReqError of
    Just err -> do
      let errorCode = err.errorCode
      let errorMessage = err.errorMessage
      let errorType = err.errorMessage
      return (errorCode, errorMessage, errorType)
    Nothing ->
      return (Nothing, Nothing, Nothing)
  void $
    OnSearchEvent.create $
      OnSearchEvent {..}

buildEstimateOrQuoteInfo ::
  (MonadThrow m, Log m) =>
  OnSearch.Provider ->
  OnSearch.Item ->
  m (Either DOnSearch.EstimateInfo DOnSearch.QuoteInfo)
buildEstimateOrQuoteInfo provider item = do
  fulfillment <- find (\fulf -> fulf.id == item.fulfillment_id) provider.fulfillments & fromMaybeM (InvalidRequest "Missing fulfillment")
  let itemId = item.id
  let vehicleVariant = castVehicleVariant fulfillment.vehicle.category
      estimatedFare = roundToIntegral item.price.value
      estimatedTotalFare = roundToIntegral item.price.offered_value
      descriptions = []
      nightShiftInfo = buildNightShiftInfo =<< item.tags
      waitingCharges = buildWaitingChargeInfo <$> item.tags
      driversLocation = provider.locations
      specialLocationTag = buildSpecialLocationTag =<< item.tags
  validatePrices estimatedFare estimatedTotalFare
  let totalFareRange =
        DEstimate.FareRange
          { minFare = roundToIntegral item.price.minimum_value,
            maxFare = max (roundToIntegral item.price.maximum_value) (roundToIntegral item.price.minimum_value)
          }
  validateFareRange estimatedTotalFare totalFareRange

  -- if we get here, the discount >= 0, estimatedFare >= estimatedTotalFare
  let discount = if estimatedTotalFare == estimatedFare then Nothing else Just $ estimatedFare - estimatedTotalFare
  case fulfillment._type of
    "RIDE" -> do
      estimateBreakupList <- buildEstimateBreakUpList item
      pure $ Left DOnSearch.EstimateInfo {bppEstimateId = Id fulfillment.id, ..}
    _ -> do
      -- "RIDE_OTP"
      quoteDetails <- DOnSearch.OneWaySpecialZoneDetails <$> buildOneWaySpecialZoneQuoteDetails fulfillment
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
  OnSearch.FulfillmentInfo ->
  m DOnSearch.OneWaySpecialZoneQuoteDetails
buildOneWaySpecialZoneQuoteDetails fulfillment = do
  pure
    DOnSearch.OneWaySpecialZoneQuoteDetails
      { quoteId = fulfillment.id
      }

-- FIXME remove round by using Kilometers and Hours in spec
buildRentalQuoteDetails ::
  (MonadThrow m, Log m) =>
  OnSearch.Item ->
  m DOnSearch.RentalQuoteDetails
buildRentalQuoteDetails _ = do
  baseDistance <- Nothing & fromMaybeM (InvalidRequest "Missing base_distance in rental search item")
  baseDuration <- Nothing & fromMaybeM (InvalidRequest "Missing base_duration in rental search item")
  pure DOnSearch.RentalQuoteDetails {..}

validateFareRange :: (MonadThrow m, Log m) => Money -> DEstimate.FareRange -> m ()
validateFareRange totalFare DEstimate.FareRange {..} = do
  when (minFare < 0) $ throwError $ InvalidRequest "Minimum discounted price is less than zero"
  when (maxFare < 0) $ throwError $ InvalidRequest "Maximum discounted price is less than zero"
  when (totalFare > maxFare || totalFare < minFare) $ throwError $ InvalidRequest "Discounted price outside of range"

buildEstimateBreakUpItem ::
  (MonadThrow m, Log m) =>
  Text ->
  OnSearch.Tag ->
  m DOnSearch.EstimateBreakupInfo
buildEstimateBreakUpItem currency tag = do
  tagValue <- (readMaybe . T.unpack =<< tag.value) & fromMaybeM (InvalidRequest "Missing fare breakup item")
  title <- tag.code & fromMaybeM (InvalidRequest "Missing fare breakup item")
  pure
    DOnSearch.EstimateBreakupInfo
      { title = title,
        price =
          DOnSearch.BreakupPriceInfo
            { currency = currency,
              value = Money tagValue
            }
      }

buildEstimateBreakUpList :: (MonadThrow m, Log m) => OnSearch.Item -> m [DOnSearch.EstimateBreakupInfo]
buildEstimateBreakUpList item = do
  (OnSearch.TG tagGroups) <- item.tags & fromMaybeM (InvalidRequest "Missing fare breakup item")

  tagGroup <- find (\tagGroup -> tagGroup.code == "fare_breakup") tagGroups & fromMaybeM (InvalidRequest "Missing fare breakup")
  mapM (buildEstimateBreakUpItem item.price.currency) tagGroup.list

buildNightShiftInfo ::
  OnSearch.TagGroups ->
  Maybe DOnSearch.NightShiftInfo
buildNightShiftInfo itemTags = do
  nightShiftCharge <- getNightShiftCharge itemTags
  oldNightShiftCharge <- getOldNightShiftCharge itemTags
  nightShiftStart <- getNightShiftStart itemTags
  nightShiftEnd <- getNightShiftEnd itemTags
  Just $
    DOnSearch.NightShiftInfo
      { oldNightShiftCharge = realToFrac oldNightShiftCharge,
        ..
      }

buildWaitingChargeInfo' :: OnSearch.TagGroups -> Maybe Money
buildWaitingChargeInfo' tagGroups = do
  tagValue <- getTag "rate_card" "waiting_charge_per_min" tagGroups
  waitingChargeValue <- readMaybe $ T.unpack tagValue
  Just $ Money waitingChargeValue

buildWaitingChargeInfo :: OnSearch.TagGroups -> DOnSearch.WaitingChargesInfo
buildWaitingChargeInfo tags = do
  DOnSearch.WaitingChargesInfo
    { waitingChargePerMin = buildWaitingChargeInfo' tags
    }

buildSpecialLocationTag :: OnSearch.TagGroups -> Maybe Text
buildSpecialLocationTag = getTag "general_info" "special_location_tag"

getNightShiftCharge :: OnSearch.TagGroups -> Maybe Money
getNightShiftCharge tagGroups = do
  tagValue <- getTag "rate_card" "night_shift_charge" tagGroups
  nightShiftCharge <- readMaybe $ T.unpack tagValue
  Just $ Money nightShiftCharge

getOldNightShiftCharge :: OnSearch.TagGroups -> Maybe DecimalValue
getOldNightShiftCharge tagGroups = do
  tagValue <- getTag "rate_card" "old_night_shift_charge" tagGroups
  DecimalValue.valueFromString tagValue

buildDistanceToNearestDriver :: OnSearch.TagGroups -> Maybe DecimalValue
buildDistanceToNearestDriver tagGroups = do
  tagValue <- getTag "general_info" "distance_to_nearest_driver" tagGroups
  distanceToNearestDriver <- readMaybe $ T.unpack tagValue
  Just $ realToFrac $ int2Double distanceToNearestDriver

getNightShiftStart :: OnSearch.TagGroups -> Maybe TimeOfDay
getNightShiftStart tagGroups = do
  tagValue <- getTag "rate_card" "night_shift_start" tagGroups
  readMaybe $ T.unpack tagValue

getNightShiftEnd :: OnSearch.TagGroups -> Maybe TimeOfDay
getNightShiftEnd tagGroups = do
  tagValue <- getTag "rate_card" "night_shift_end" tagGroups
  readMaybe $ T.unpack tagValue
