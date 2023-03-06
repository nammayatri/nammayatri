{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.OnUpdate (buildOnUpdateReq) where

import Beckn.ACL.Common (validatePrices)
import qualified Beckn.Types.Core.Taxi.OnUpdate as OnUpdate
import qualified Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.BookingCancelledEvent as OnUpdate
import qualified Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.EstimateRepetitionEvent as EstRepUpd
import qualified Domain.Action.Beckn.OnUpdate as DOnUpdate
import qualified Domain.Types.BookingCancellationReason as SBCR
import qualified Domain.Types.Estimate as DEstimate
import qualified Domain.Types.VehicleVariant as VehVar
import EulerHS.Prelude hiding (state)
import Kernel.Prelude (roundToIntegral)
import Kernel.Product.Validation.Context (validateContext)
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Beckn.ReqTypes
import Kernel.Types.Id
import Kernel.Utils.Common
import Tools.Error (GenericError (InvalidRequest))

buildOnUpdateReq ::
  ( HasFlowEnv m r '["coreVersion" ::: Text],
    EsqDBFlow m r
  ) =>
  BecknCallbackReq OnUpdate.OnUpdateMessage ->
  m (Maybe DOnUpdate.OnUpdateReq)
buildOnUpdateReq req = do
  validateContext Context.ON_UPDATE $ req.context
  transactionId <- req.context.transaction_id & fromMaybeM (InvalidRequest "transaction_id is not present.")
  handleError req.contents $ \message -> do
    parseEvent transactionId message.order

handleError ::
  (MonadFlow m) =>
  Either Error OnUpdate.OnUpdateMessage ->
  (OnUpdate.OnUpdateMessage -> m DOnUpdate.OnUpdateReq) ->
  m (Maybe DOnUpdate.OnUpdateReq)
handleError etr action =
  case etr of
    Right msg -> do
      Just <$> action msg
    Left err -> do
      logTagError "on_init req" $ "on_init error: " <> show err
      pure Nothing

parseEvent :: (MonadFlow m) => Text -> OnUpdate.OnUpdateEvent -> m DOnUpdate.OnUpdateReq
parseEvent _ (OnUpdate.RideAssigned taEvent) =
  return $
    DOnUpdate.RideAssignedReq
      { bppBookingId = Id taEvent.id,
        bppRideId = Id taEvent.fulfillment.id,
        otp = taEvent.fulfillment.start.authorization.token,
        driverName = taEvent.fulfillment.agent.name,
        driverMobileNumber = taEvent.fulfillment.agent.phone,
        driverRating = realToFrac <$> taEvent.fulfillment.agent.rating,
        driverRegisteredAt = taEvent.fulfillment.agent.tags.registered_at,
        vehicleNumber = taEvent.fulfillment.vehicle.registration,
        vehicleColor = taEvent.fulfillment.vehicle.color,
        vehicleModel = taEvent.fulfillment.vehicle.model
      }
parseEvent _ (OnUpdate.RideStarted rsEvent) =
  return $
    DOnUpdate.RideStartedReq
      { bppBookingId = Id rsEvent.id,
        bppRideId = Id rsEvent.fulfillment.id
      }
parseEvent _ (OnUpdate.RideCompleted rcEvent) = do
  return $
    DOnUpdate.RideCompletedReq
      { bppBookingId = Id rcEvent.id,
        bppRideId = Id rcEvent.fulfillment.id,
        fare = roundToIntegral rcEvent.quote.price.value,
        totalFare = roundToIntegral rcEvent.quote.price.computed_value,
        chargeableDistance = realToFrac rcEvent.fulfillment.chargeable_distance,
        fareBreakups = mkOnUpdateFareBreakup <$> rcEvent.quote.breakup
      }
  where
    mkOnUpdateFareBreakup breakup =
      DOnUpdate.OnUpdateFareBreakup
        { amount = realToFrac breakup.price.value,
          description = breakup.title
        }
parseEvent _ (OnUpdate.BookingCancelled tcEvent) =
  return $
    DOnUpdate.BookingCancelledReq
      { bppBookingId = Id $ tcEvent.id,
        cancellationSource = castCancellationSource tcEvent.cancellation_reason
      }
parseEvent _ (OnUpdate.BookingReallocation rbrEvent) =
  return $
    DOnUpdate.BookingReallocationReq
      { bppBookingId = Id $ rbrEvent.id,
        bppRideId = Id rbrEvent.fulfillment.id,
        reallocationSource = castCancellationSource rbrEvent.reallocation_reason
      }
parseEvent _ (OnUpdate.DriverArrived daEvent) =
  return $
    DOnUpdate.DriverArrivedReq
      { bppBookingId = Id daEvent.id,
        bppRideId = Id daEvent.fulfillment.id,
        arrivalTime = daEvent.arrival_time
      }
parseEvent transactionId (OnUpdate.EstimateRepetition erEvent) = do
  estimateInfo <- buildEstimateInfo erEvent.item
  return $
    DOnUpdate.EstimateRepetitionReq
      { searchRequestId = Id transactionId,
        estimateInfo = estimateInfo,
        bppBookingId = Id $ erEvent.id,
        bppRideId = Id erEvent.fulfillment.id,
        cancellationSource = castCancellationSource erEvent.cancellation_reason
      }

buildEstimateInfo ::
  (MonadThrow m, Log m) =>
  EstRepUpd.Item ->
  m DOnUpdate.EstimateRepetitionEstimateInfo
buildEstimateInfo item = do
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
    EstRepUpd.DRIVER_OFFER_ESTIMATE -> pure DOnUpdate.EstimateRepetitionEstimateInfo {..}
    _ -> throwError $ InvalidRequest "category_id is not supported, use DRIVER_OFFER_ESTIMATE"
  where
    castVehicleVariant = \case
      EstRepUpd.SEDAN -> VehVar.SEDAN
      EstRepUpd.SUV -> VehVar.SUV
      EstRepUpd.HATCHBACK -> VehVar.HATCHBACK
      EstRepUpd.AUTO_RICKSHAW -> VehVar.AUTO_RICKSHAW

    validateFareRange totalFare DEstimate.FareRange {..} = do
      when (minFare < 0) $ throwError $ InvalidRequest "Minimum discounted price is less than zero"
      when (maxFare < 0) $ throwError $ InvalidRequest "Maximum discounted price is less than zero"
      when (maxFare < minFare) $ throwError $ InvalidRequest "Maximum discounted price is less than minimum discounted price"
      when (totalFare > maxFare || totalFare < minFare) $ throwError $ InvalidRequest "Discounted price outside of range"

    buildEstimateBreakUpList EstRepUpd.BreakupItem {..} = do
      DOnUpdate.EstimateBreakupInfo
        { title = title,
          price =
            DOnUpdate.BreakupPriceInfo
              { currency = price.currency,
                value = roundToIntegral price.value
              }
        }

    buildNightShiftRate itemTags = do
      DOnUpdate.NightShiftInfo
        { nightShiftMultiplier = realToFrac <$> itemTags.night_shift_multiplier,
          nightShiftStart = itemTags.night_shift_start,
          nightShiftEnd = itemTags.night_shift_end
        }

    buildWaitingChargeInfo itemTags = do
      DOnUpdate.WaitingChargesInfo
        { waitingChargePerMin = itemTags.waiting_charge_per_min,
          waitingTimeEstimatedThreshold = itemTags.waiting_time_estimated_threshold
        }

castCancellationSource :: OnUpdate.CancellationSource -> SBCR.CancellationSource
castCancellationSource = \case
  OnUpdate.ByUser -> SBCR.ByUser
  OnUpdate.ByDriver -> SBCR.ByDriver
  OnUpdate.ByMerchant -> SBCR.ByMerchant
  OnUpdate.ByAllocator -> SBCR.ByAllocator
  OnUpdate.ByApplication -> SBCR.ByApplication
