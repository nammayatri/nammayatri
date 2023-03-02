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
import qualified Beckn.Types.Core.Taxi.API.OnSearch as OnSearch
import qualified Beckn.Types.Core.Taxi.OnSearch as OnSearch
import Beckn.Types.Core.Taxi.OnSearch.Item (BreakupItem (..))
import qualified Domain.Action.Beckn.OnSearch as DOnSearch
import qualified Domain.Types.Estimate as DEstimate
import Domain.Types.OnSearchEvent
import qualified Domain.Types.VehicleVariant as VehVar
import EulerHS.Prelude hiding (id, state, unpack)
import Kernel.Prelude
import Kernel.Product.Validation.Context (validateContext)
import Kernel.Storage.Esqueleto (runTransaction)
import qualified Kernel.Types.Beckn.Context as Context
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
  (estimatesInfo, quotesInfo) <- partitionEithers <$> traverse buildEstimateOrQuoteInfo items
  let providerInfo =
        DOnSearch.ProviderInfo
          { providerId = providerId,
            name = provider.descriptor.name,
            url = providerUrl,
            mobileNumber = provider.contacts,
            ridesCompleted = provider.tags.rides_completed
          }
  pure
    DOnSearch.DOnSearchReq
      { requestId = Id context.message_id,
        ..
      }

logOnSearchEvent :: forall m r. EsqDBFlow m r => OnSearch.OnSearchReq -> m ()
logOnSearchEvent (BecknCallbackReq context (leftToMaybe -> mbErr)) = do
  createdAt <- getCurrentTime
  id <- generateGUID
  bppId <- context.bpp_id & fromMaybeM (InvalidRequest "Missing context.bpp_id")
  let messageId = context.message_id
  let errorType = show . (._type) <$> mbErr
  let errorCode = (.code) <$> mbErr
  let errorMessage = (.message) =<< mbErr
  runTransaction $
    OnSearchEvent.create @m $
      OnSearchEvent {..}

buildEstimateOrQuoteInfo ::
  (MonadThrow m, Log m) =>
  OnSearch.Item ->
  m (Either DOnSearch.EstimateInfo DOnSearch.QuoteInfo)
buildEstimateOrQuoteInfo item = do
  let itemCode = item.descriptor.code
      vehicleVariant = castVehicleVariant itemCode.vehicleVariant
      estimatedFare = roundToIntegral item.price.value
      estimatedTotalFare = roundToIntegral item.price.offered_value
      estimateBreakupList = buildEstimateBreakUpList <$> item.price.value_breakup
      descriptions = item.quote_terms
      nightShiftRate = buildNightShiftRate <$> item.tags
      waitingCharges = buildWaitingChargeInfo <$> item.tags
      driversLocation = fromMaybe [] $ item.tags <&> (.drivers_location)
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
    OnSearch.DRIVER_OFFER_ESTIMATE -> pure $ Left DOnSearch.EstimateInfo {..}
    OnSearch.DRIVER_OFFER -> throwError $ InvalidRequest "DRIVER_OFFER supported in on_select, use DRIVER_OFFER_ESTIMATE"
  where
    castVehicleVariant = \case
      OnSearch.SEDAN -> VehVar.SEDAN
      OnSearch.SUV -> VehVar.SUV
      OnSearch.HATCHBACK -> VehVar.HATCHBACK
      OnSearch.AUTO_RICKSHAW -> VehVar.AUTO_RICKSHAW

buildOneWayQuoteDetails ::
  (MonadThrow m, Log m) =>
  OnSearch.Item ->
  m DOnSearch.OneWayQuoteDetails
buildOneWayQuoteDetails item = do
  distanceToNearestDriver <-
    (item.tags >>= (.distance_to_nearest_driver))
      & fromMaybeM (InvalidRequest "Trip type is ONE_WAY, but distanceToNearestDriver is Nothing")
  pure
    DOnSearch.OneWayQuoteDetails
      { distanceToNearestDriver = realToFrac distanceToNearestDriver
      }

--FIXME remove round by using Kilometers and Hours in spec
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

buildNightShiftRate ::
  OnSearch.ItemTags ->
  DOnSearch.NightShiftInfo
buildNightShiftRate itemTags = do
  DOnSearch.NightShiftInfo
    { nightShiftMultiplier = realToFrac <$> itemTags.night_shift_multiplier,
      nightShiftStart = itemTags.night_shift_start,
      nightShiftEnd = itemTags.night_shift_end
    }

buildWaitingChargeInfo ::
  OnSearch.ItemTags ->
  DOnSearch.WaitingChargesInfo
buildWaitingChargeInfo itemTags = do
  DOnSearch.WaitingChargesInfo
    { waitingChargePerMin = itemTags.waiting_charge_per_min,
      waitingTimeEstimatedThreshold = itemTags.waiting_time_estimated_threshold
    }
