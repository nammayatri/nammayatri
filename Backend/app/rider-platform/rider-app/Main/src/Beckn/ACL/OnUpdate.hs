{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.OnUpdate (buildOnUpdateReq) where

import Beckn.ACL.Common (getTag)
import qualified Beckn.Types.Core.Taxi.OnUpdate as OnUpdate
import qualified Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.BookingCancelledEvent as OnUpdate
import qualified Data.Text as T
import qualified Domain.Action.Beckn.OnUpdate as DOnUpdate
import qualified Domain.Types.BookingCancellationReason as SBCR
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
      logTagError "on_update req" $ "on_update error: " <> show err
      pure Nothing

parseEvent :: (MonadFlow m) => Text -> OnUpdate.OnUpdateEvent -> m DOnUpdate.OnUpdateReq
parseEvent _ (OnUpdate.RideAssigned taEvent) = do
  vehicle <- fromMaybeM (InvalidRequest "vehicle is not present in RideAssigned Event.") $ taEvent.fulfillment.vehicle
  agent <- fromMaybeM (InvalidRequest "agent is not present in RideAssigned Event.") $ taEvent.fulfillment.agent
  -- let agentTagGroup = find (\tagGroup -> tagGroup.code == "driver_details") agent.tags
  registeredAt :: UTCTime <-
    fromMaybeM (InvalidRequest "registered_at is not present.") $
      readMaybe . T.unpack
        =<< getTag "driver_details" "registered_at" agent.tags
  -- =<< find (\tag -> tag.code == Just "registered_at") . (.list)
  -- =<< agentTagGroup
  let rating :: Maybe HighPrecMeters =
        readMaybe . T.unpack
          -- =<< (.value)
          =<< getTag "driver_details" "rating" agent.tags
  -- =<< find (\tag -> tag.code == Just "rating") . (.list)
  -- =<< agentTagGroup
  return $
    DOnUpdate.RideAssignedReq
      { bppBookingId = Id taEvent.id,
        bppRideId = Id taEvent.fulfillment.id,
        otp = taEvent.fulfillment.start.authorization.token,
        driverName = agent.name,
        driverMobileNumber = agent.phone,
        driverMobileCountryCode = Just "+91", -----------TODO needs to be added in agent Tags------------
        driverRating = realToFrac <$> rating,
        driverRegisteredAt = registeredAt,
        vehicleNumber = vehicle.registration,
        vehicleColor = vehicle.color,
        vehicleModel = vehicle.model
      }
parseEvent _ (OnUpdate.RideStarted rsEvent) = do
  return $
    DOnUpdate.RideStartedReq
      { bppBookingId = Id rsEvent.id,
        bppRideId = Id rsEvent.fulfillment.id
      }
parseEvent _ (OnUpdate.RideCompleted rcEvent) = do
  let tagsGroup = rcEvent.fulfillment.tags
  -- distanceInfoGroup = find (\tagGroup -> tagGroup.code == "ride_distance_details") tagsGroup -- maybe don't have tags and all in our domain types, get rid of them in formJSON toJSON itself similar to agentTags above?
  chargeableDistance :: HighPrecMeters <-
    fromMaybeM (InvalidRequest "chargeable_distance is not present.") $
      readMaybe . T.unpack
        =<< getTag "ride_distance_details" "chargeable_distance" tagsGroup
  -- =<< find (\tag -> tag.code == Just "chargeable_distance") . (.list)
  -- =<< distanceInfoGroup
  traveledDistance :: HighPrecMeters <-
    fromMaybeM (InvalidRequest "traveled_distance is not present.") $
      readMaybe . T.unpack
        =<< getTag "ride_distance_details" "traveled_distance" tagsGroup
  -- =<< find (\tag -> tag.code == Just "traveled_distance") . (.list)
  -- =<< distanceInfoGroup
  return $
    DOnUpdate.RideCompletedReq
      { bppBookingId = Id rcEvent.id,
        bppRideId = Id rcEvent.fulfillment.id,
        fare = roundToIntegral rcEvent.quote.price.value,
        totalFare = roundToIntegral rcEvent.quote.price.computed_value,
        chargeableDistance = chargeableDistance,
        traveledDistance = traveledDistance,
        fareBreakups = mkOnUpdateFareBreakup <$> rcEvent.quote.breakup,
        paymentUrl = rcEvent.payment >>= (.uri)
      }
  where
    mkOnUpdateFareBreakup breakup =
      DOnUpdate.OnUpdateFareBreakup
        { amount = realToFrac breakup.price.value,
          description = breakup.title
        }
parseEvent _ (OnUpdate.BookingCancelled tcEvent) = do
  return $
    DOnUpdate.BookingCancelledReq
      { bppBookingId = Id $ tcEvent.id,
        cancellationSource = castCancellationSource tcEvent.cancellation_reason
      }
parseEvent _ (OnUpdate.BookingReallocation rbrEvent) = do
  return $
    DOnUpdate.BookingReallocationReq
      { bppBookingId = Id $ rbrEvent.id,
        bppRideId = Id rbrEvent.fulfillment.id,
        reallocationSource = castCancellationSource rbrEvent.reallocation_reason
      }
parseEvent _ (OnUpdate.DriverArrived daEvent) = do
  let tagsGroup = daEvent.fulfillment.tags
  -- driverArrivalGroup = find (\tagGroup -> tagGroup.code == "driver_arrived_info") tagsGroup
  arrival_time <-
    fromMaybeM (InvalidRequest "arrival_time is not present.") $
      readMaybe . T.unpack
        =<< getTag "driver_arrived_info" "arrival_time" tagsGroup
  -- =<< find (\tag -> tag.code == Just "arrival_time") . (.list)
  -- =<< driverArrivalGroup
  return $
    DOnUpdate.DriverArrivedReq
      { bppBookingId = Id daEvent.id,
        bppRideId = Id daEvent.fulfillment.id,
        arrivalTime = arrival_time
      }
parseEvent _ (OnUpdate.NewMessage daEvent) = do
  let tagsGroup = daEvent.fulfillment.tags
  -- driverArrivalGroup = find (\tagGroup -> tagGroup.code == "driver_new_message") tagsGroup
  message :: Text <-
    fromMaybeM (InvalidRequest "message is not present.") $
      getTag "driver_new_message" "message" tagsGroup
  -- =<< find (\tag -> tag.code == Just "message") . (.list)
  -- =<< driverArrivalGroup
  return $
    DOnUpdate.NewMessageReq
      { bppBookingId = Id daEvent.id,
        bppRideId = Id daEvent.fulfillment.id,
        message = message
      }
parseEvent transactionId (OnUpdate.EstimateRepetition erEvent) = do
  let tagsGroup = erEvent.fulfillment.tags
  -- previousCancellationReasonsGroup = find (\tagGroup -> tagGroup.code == "previous_cancellation_reasons") tagsGroup
  cancellationReason <-
    fromMaybeM (InvalidRequest "cancellation_reason is not present.") $
      readMaybe . T.unpack
        =<< getTag "previous_cancellation_reasons" "cancellation_reason" tagsGroup
  -- =<< find (\tag -> tag.code == Just "cancellation_reason") . (.list)
  -- =<< previousCancellationReasonsGroup
  return $
    DOnUpdate.EstimateRepetitionReq
      { searchRequestId = Id transactionId,
        bppEstimateId = Id erEvent.item.id,
        bppBookingId = Id $ erEvent.id,
        bppRideId = Id erEvent.fulfillment.id,
        cancellationSource = castCancellationSource cancellationReason
      }

castCancellationSource :: OnUpdate.CancellationSource -> SBCR.CancellationSource
castCancellationSource = \case
  OnUpdate.ByUser -> SBCR.ByUser
  OnUpdate.ByDriver -> SBCR.ByDriver
  OnUpdate.ByMerchant -> SBCR.ByMerchant
  OnUpdate.ByAllocator -> SBCR.ByAllocator
  OnUpdate.ByApplication -> SBCR.ByApplication
