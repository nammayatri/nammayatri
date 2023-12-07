{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.OnUpdate (buildOnUpdateReq) where

import Beckn.ACL.Common (getTagV2)
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
  BecknCallbackReq OnUpdate.OnUpdateMessageV2 ->
  m (Maybe DOnUpdate.OnUpdateReq)
buildOnUpdateReq req = do
  validateContext Context.ON_UPDATE $ req.context
  transactionId <- req.context.transaction_id & fromMaybeM (InvalidRequest "transaction_id is not present.")
  handleError req.contents $ \message -> do
    parseEvent transactionId message.order

handleError ::
  (MonadFlow m) =>
  Either Error OnUpdate.OnUpdateMessageV2 ->
  (OnUpdate.OnUpdateMessageV2 -> m DOnUpdate.OnUpdateReq) ->
  m (Maybe DOnUpdate.OnUpdateReq)
handleError etr action =
  case etr of
    Right msg -> do
      Just <$> action msg
    Left err -> do
      logTagError "on_update req" $ "on_update error: " <> show err
      pure Nothing

parseEvent :: (MonadFlow m) => Text -> OnUpdate.OnUpdateEventV2 -> m DOnUpdate.OnUpdateReq
parseEvent _ (OnUpdate.RideAssignedV2 taEvent) = do
  vehicle <- fromMaybeM (InvalidRequest "vehicle is not present in RideAssigned Event.") $ taEvent.fulfillment.vehicle
  agent <- fromMaybeM (InvalidRequest "agent is not present in RideAssigned Event.") $ taEvent.fulfillment.agent
  agentContact <- fromMaybeM (InvalidRequest "agent contact is not present in RideAssigned Event.") $ agent._contact
  let agentPhone = show agentContact.phone
  agentPerson <- fromMaybeM (InvalidRequest "agent person is not present.") $ agent.person
  tagsGroup <- fromMaybeM (InvalidRequest "agent tags is not present in RideAssigned Event.") agentPerson.tags
  registeredAt :: UTCTime <-
    fromMaybeM (InvalidRequest "registered_at is not present.") $
      readMaybe . T.unpack
        =<< getTagV2 "driver_details" "registered_at" tagsGroup
  let rating :: Maybe HighPrecMeters =
        readMaybe . T.unpack
          =<< getTagV2 "driver_details" "rating" tagsGroup
  authorization <- fromMaybeM (InvalidRequest "authorization is not present in RideAssigned Event.") $ taEvent.fulfillment.start.authorization
  return $
    DOnUpdate.RideAssignedReq
      { bppBookingId = Id taEvent.id,
        bppRideId = Id taEvent.fulfillment.id,
        otp = authorization.token,
        driverName = agentPerson.name,
        driverMobileNumber = agentPhone,
        driverMobileCountryCode = Just "+91", -----------TODO needs to be added in agent Tags------------
        driverRating = realToFrac <$> rating,
        driverImage = agentPerson.image >>= (.url),
        driverRegisteredAt = registeredAt,
        vehicleNumber = vehicle.registration,
        vehicleColor = vehicle.color,
        vehicleModel = vehicle.model
      }
parseEvent _ (OnUpdate.RideStartedV2 rsEvent) = do
  return $
    DOnUpdate.RideStartedReq
      { bppBookingId = Id rsEvent.id,
        bppRideId = Id rsEvent.fulfillment.id
      }
parseEvent _ (OnUpdate.RideCompletedV2 rcEvent) = do
  tagsGroup <- fromMaybeM (InvalidRequest "agent tags is not present in RideCompleted Event.") rcEvent.fulfillment.tags
  chargeableDistance :: HighPrecMeters <-
    fromMaybeM (InvalidRequest "chargeable_distance is not present.") $
      readMaybe . T.unpack
        =<< getTagV2 "ride_distance_details" "chargeable_distance" tagsGroup
  traveledDistance :: HighPrecMeters <-
    fromMaybeM (InvalidRequest "traveled_distance is not present.") $
      readMaybe . T.unpack
        =<< getTagV2 "ride_distance_details" "traveled_distance" tagsGroup
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
parseEvent _ (OnUpdate.BookingCancelledV2 tcEvent) = do
  return $
    DOnUpdate.BookingCancelledReq
      { bppBookingId = Id $ tcEvent.id,
        cancellationSource = castCancellationSource tcEvent.cancellation_reason
      }
parseEvent _ (OnUpdate.BookingReallocationV2 rbrEvent) = do
  return $
    DOnUpdate.BookingReallocationReq
      { bppBookingId = Id $ rbrEvent.id,
        bppRideId = Id rbrEvent.fulfillment.id,
        reallocationSource = castCancellationSource rbrEvent.reallocation_reason
      }
parseEvent _ (OnUpdate.DriverArrivedV2 daEvent) = do
  tagsGroup <- fromMaybeM (InvalidRequest "agent tags is not present in DriverArrived Event.") daEvent.fulfillment.tags
  let arrival_time =
        readMaybe . T.unpack
          =<< getTagV2 "driver_arrived_info" "arrival_time" tagsGroup

  return $
    DOnUpdate.DriverArrivedReq
      { bppBookingId = Id daEvent.id,
        bppRideId = Id daEvent.fulfillment.id,
        arrivalTime = arrival_time
      }
parseEvent _ (OnUpdate.NewMessageV2 daEvent) = do
  tagsGroup <- fromMaybeM (InvalidRequest "agent tags is not present in NewMessage Event.") daEvent.fulfillment.tags
  message :: Text <-
    fromMaybeM (InvalidRequest "message is not present.") $
      getTagV2 "driver_new_message" "message" tagsGroup
  return $
    DOnUpdate.NewMessageReq
      { bppBookingId = Id daEvent.id,
        bppRideId = Id daEvent.fulfillment.id,
        message = message
      }
parseEvent transactionId (OnUpdate.EstimateRepetitionV2 erEvent) = do
  tagsGroup <- fromMaybeM (InvalidRequest "agent tags is not present in EstimateRepetition Event.") erEvent.fulfillment.tags
  cancellationReason <-
    fromMaybeM (InvalidRequest "cancellation_reason is not present.") $
      readMaybe . T.unpack
        =<< getTagV2 "previous_cancellation_reasons" "cancellation_reason" tagsGroup
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
