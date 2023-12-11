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
import qualified Beckn.ACL.Common as Common
import qualified Beckn.Types.Core.Taxi.OnUpdate as OnUpdate
import qualified Data.Text as T
import qualified Domain.Action.Beckn.OnUpdate as DOnUpdate
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
  agentPhone <- fromMaybeM (InvalidRequest "agent phoneNumber is not present in RideAssigned Event.") $ agent.phone
  tagsGroup <- fromMaybeM (InvalidRequest "agent tags is not present in RideAssigned Event.") agent.tags
  registeredAt :: UTCTime <-
    fromMaybeM (InvalidRequest "registered_at is not present.") $
      readMaybe . T.unpack
        =<< getTag "driver_details" "registered_at" tagsGroup
  let rating :: Maybe HighPrecMeters =
        readMaybe . T.unpack
          =<< getTag "driver_details" "rating" tagsGroup
      mbIsDriverBirthDay = getTag "driver_details" "is_driver_birthday" tagsGroup
      isDriverBirthDay = case T.toLower <$> mbIsDriverBirthDay of
        Just "true" -> True
        _ -> False
  authorization <- fromMaybeM (InvalidRequest "authorization is not present in RideAssigned Event.") $ taEvent.fulfillment.start.authorization
  return $
    DOnUpdate.RideAssignedReq
      { bppBookingId = Id taEvent.id,
        bppRideId = Id taEvent.fulfillment.id,
        otp = authorization.token,
        driverName = agent.name,
        driverMobileNumber = agentPhone,
        driverMobileCountryCode = Just "+91", -----------TODO needs to be added in agent Tags------------
        driverRating = realToFrac <$> rating,
        driverImage = agent.image,
        driverRegisteredAt = registeredAt,
        isDriverBirthDay,
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
  tagsGroup <- fromMaybeM (InvalidRequest "agent tags is not present in RideCompleted Event.") rcEvent.fulfillment.tags
  chargeableDistance :: HighPrecMeters <-
    fromMaybeM (InvalidRequest "chargeable_distance is not present.") $
      readMaybe . T.unpack
        =<< getTag "ride_distance_details" "chargeable_distance" tagsGroup
  traveledDistance :: HighPrecMeters <-
    fromMaybeM (InvalidRequest "traveled_distance is not present.") $
      readMaybe . T.unpack
        =<< getTag "ride_distance_details" "traveled_distance" tagsGroup
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
        cancellationSource = Common.castCancellationSource tcEvent.cancellation_reason
      }
parseEvent _ (OnUpdate.BookingReallocation rbrEvent) = do
  return $
    DOnUpdate.BookingReallocationReq
      { bppBookingId = Id $ rbrEvent.id,
        bppRideId = Id rbrEvent.fulfillment.id,
        reallocationSource = Common.castCancellationSource rbrEvent.reallocation_reason
      }
parseEvent _ (OnUpdate.DriverArrived daEvent) = do
  tagsGroup <- fromMaybeM (InvalidRequest "agent tags is not present in DriverArrived Event.") daEvent.fulfillment.tags
  let arrival_time =
        readMaybe . T.unpack
          =<< getTag "driver_arrived_info" "arrival_time" tagsGroup

  return $
    DOnUpdate.DriverArrivedReq
      { bppBookingId = Id daEvent.id,
        bppRideId = Id daEvent.fulfillment.id,
        arrivalTime = arrival_time
      }
parseEvent _ (OnUpdate.NewMessage daEvent) = do
  tagsGroup <- fromMaybeM (InvalidRequest "agent tags is not present in NewMessage Event.") daEvent.fulfillment.tags
  message :: Text <-
    fromMaybeM (InvalidRequest "message is not present.") $
      getTag "driver_new_message" "message" tagsGroup
  return $
    DOnUpdate.NewMessageReq
      { bppBookingId = Id daEvent.id,
        bppRideId = Id daEvent.fulfillment.id,
        message = message
      }
parseEvent _ (OnUpdate.SafetyAlert saEvent) = do
  tagsGroup <- fromMaybeM (InvalidRequest "agent tags is not present in SafetyAlert Event.") saEvent.fulfillment.tags
  deviation :: Text <-
    fromMaybeM (InvalidRequest "deviation is not present.") $
      getTag "safety_alert" "deviation" tagsGroup
  return $
    DOnUpdate.SafetyAlertReq
      { bppBookingId = Id saEvent.id,
        bppRideId = Id saEvent.fulfillment.id,
        reason = deviation,
        code = "deviation"
      }
