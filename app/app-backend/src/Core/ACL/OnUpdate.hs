module Core.ACL.OnUpdate (buildOnUpdateReq) where

import Beckn.Product.Validation.Context (validateContext)
import qualified Beckn.Types.Core.Context as Context
import Beckn.Types.Core.ReqTypes
import qualified Beckn.Types.Core.Taxi.OnUpdate as OnUpdate
import qualified Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.RideBookingCancelledEvent as OnUpdate
import Beckn.Types.Id
import qualified Domain.Action.Beckn.OnUpdate as DOnUpdate
import qualified Domain.Types.RideBookingCancellationReason as SBCR
import EulerHS.Prelude hiding (state)
import Types.Error
import Utils.Common

buildOnUpdateReq ::
  ( HasFlowEnv m r ["coreVersion" ::: Text, "domainVersion" ::: Text],
    EsqDBFlow m r
  ) =>
  BecknCallbackReq OnUpdate.OnUpdateMessage ->
  m (Maybe DOnUpdate.OnUpdateReq)
buildOnUpdateReq req = do
  validateContext Context.ON_UPDATE $ req.context
  handleError req.contents $ \message -> do
    parseEvent message.order

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

parseEvent :: (MonadFlow m) => OnUpdate.OnUpdateEvent -> m DOnUpdate.OnUpdateReq
parseEvent (OnUpdate.RideAssigned taEvent) =
  return $
    DOnUpdate.RideAssignedReq
      { bppBookingId = Id taEvent.id,
        bppRideId = undefined,
        otp = taEvent.fulfillment.start.authorization.token,
        driverName = taEvent.fulfillment.agent.name,
        driverMobileNumber = taEvent.fulfillment.agent.phone,
        driverRating = realToFrac <$> taEvent.fulfillment.agent.rating,
        driverRegisteredAt = taEvent.fulfillment.agent.tags.registered_at,
        vehicleNumber = taEvent.fulfillment.vehicle.registration,
        vehicleColor = taEvent.fulfillment.vehicle.color,
        vehicleModel = taEvent.fulfillment.vehicle.model
      }
parseEvent (OnUpdate.RideStarted rsEvent) =
  return $
    DOnUpdate.RideStartedReq
      { bppBookingId = Id rsEvent.id,
        bppRideId = undefined
      }
parseEvent (OnUpdate.RideCompleted rcEvent) = do
  fareBreakup <- safeHead rcEvent.quote.breakup & fromMaybeM (InvalidRequest "quote.breakup is empty.") -- first one must be for fare.
  return $
    DOnUpdate.RideCompletedReq
      { bppBookingId = Id rcEvent.id,
        bppRideId = undefined,
        fare = realToFrac fareBreakup.price.value,
        totalFare = realToFrac rcEvent.quote.price.value,
        chargeableDistance = undefined
      }
parseEvent (OnUpdate.RideBookingCancelled tcEvent) =
  return $
    DOnUpdate.BookingCancelledReq
      { bppBookingId = Id $ tcEvent.id,
        cancellationSource = castCancellationSource tcEvent.cancellation_reason
      }
parseEvent (OnUpdate.RideBookingReallocation rbrEvent) =
  return $
    DOnUpdate.BookingReallocationReq
      { bppBookingId = Id $ rbrEvent.id,
        bppRideId = undefined,
        cancellationSource = castCancellationSource undefined
      }

castCancellationSource :: OnUpdate.CancellationSource -> SBCR.CancellationSource
castCancellationSource = \case
  OnUpdate.ByUser -> SBCR.ByUser
  OnUpdate.ByDriver -> SBCR.ByDriver
  OnUpdate.ByOrganization -> SBCR.ByOrganization
  OnUpdate.ByAllocator -> SBCR.ByAllocator
