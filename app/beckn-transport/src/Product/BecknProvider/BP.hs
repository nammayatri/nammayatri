module Product.BecknProvider.BP
  ( sendRideAssignedUpdateToBAP,
    sendRideStartedUpdateToBAP,
    sendRideCompletedUpdateToBAP,
    sendRideBookingCanceledUpdateToBAP,
    sendRideBookingReallocationUpdateToBAP,
    buildRideReq,
  )
where

import Beckn.Types.Common
import qualified Beckn.Types.Core.Taxi.OnUpdate as OnUpdate
import Beckn.Types.Id
import qualified Domain.Types.Organization as SOrg
import qualified Domain.Types.Person as SP
import qualified Domain.Types.Ride as SRide
import qualified Domain.Types.RideBooking as SRB
import qualified Domain.Types.RideRequest as SRideRequest
import qualified Domain.Types.SearchRequest as SSR
import EulerHS.Prelude
import ExternalAPI.Flow (callOnUpdate)
import qualified Storage.Queries.Organization as QOrg
import qualified Storage.Queries.Person as Person
import qualified Storage.Queries.Vehicle as Vehicle
import Tools.Metrics (CoreMetrics)
import Types.Error
import Utils.Common

sendRideAssignedUpdateToBAP ::
  ( EsqDBFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    CoreMetrics m
  ) =>
  SRB.RideBooking ->
  SRide.Ride ->
  m ()
sendRideAssignedUpdateToBAP rideBooking ride = do
  transporter <-
    QOrg.findById rideBooking.providerId
      >>= fromMaybeM OrgNotFound
  buildRideAssignedUpdatePayload ride
    >>= sendUpdateEvent transporter rideBooking.requestId

sendRideStartedUpdateToBAP ::
  ( EsqDBFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    CoreMetrics m
  ) =>
  SRB.RideBooking ->
  SRide.Ride ->
  m ()
sendRideStartedUpdateToBAP rideBooking ride = do
  transporter <-
    QOrg.findById rideBooking.providerId
      >>= fromMaybeM OrgNotFound
  makeRideStartedUpdatePayload ride
    & sendUpdateEvent transporter rideBooking.requestId

sendRideCompletedUpdateToBAP ::
  ( EsqDBFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    CoreMetrics m
  ) =>
  SRB.RideBooking ->
  SRide.Ride ->
  m ()
sendRideCompletedUpdateToBAP rideBooking ride = do
  transporter <-
    QOrg.findById rideBooking.providerId
      >>= fromMaybeM OrgNotFound
  buildRideCompletedUpdatePayload ride
    >>= sendUpdateEvent transporter rideBooking.requestId

sendRideBookingCanceledUpdateToBAP ::
  ( EsqDBFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    CoreMetrics m
  ) =>
  SRB.RideBooking ->
  SOrg.Organization ->
  OnUpdate.CancellationSource ->
  m ()
sendRideBookingCanceledUpdateToBAP rideBooking transporter cancellationSource = do
  let message = OnUpdate.RideBookingCancelled $ OnUpdate.RideBookingCancelledEvent rideBooking.id.getId cancellationSource
  sendUpdateEvent transporter rideBooking.requestId message

sendRideBookingReallocationUpdateToBAP ::
  ( EsqDBFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    CoreMetrics m
  ) =>
  SRB.RideBooking ->
  Id SRide.Ride ->
  SOrg.Organization ->
  OnUpdate.CancellationSource ->
  m ()
sendRideBookingReallocationUpdateToBAP rideBooking rideId transporter cancellationSource = do
  let message = OnUpdate.RideBookingReallocation $ OnUpdate.RideBookingReallocationEvent rideBooking.id.getId rideId.getId cancellationSource
  sendUpdateEvent transporter rideBooking.requestId message

sendUpdateEvent ::
  ( EsqDBFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    CoreMetrics m
  ) =>
  SOrg.Organization ->
  Id SSR.SearchRequest ->
  OnUpdate.OnUpdateEvent ->
  m ()
sendUpdateEvent transporter requestId =
  void . callOnUpdate transporter requestId . OnUpdate.OnUpdateMessage

buildRideAssignedUpdatePayload ::
  (EsqDBFlow m r, EncFlow m r) =>
  SRide.Ride ->
  m OnUpdate.OnUpdateEvent
buildRideAssignedUpdatePayload ride = do
  driver <-
    Person.findById ride.driverId
      >>= fromMaybeM PersonNotFound
  veh <- Vehicle.findById ride.vehicleId >>= fromMaybeM VehicleNotFound
  mobileNumber <- (SP.getPersonNumber driver) >>= fromMaybeM (InternalError "Driver mobile number is not present.")
  name <- (SP.getPersonFullName driver) >>= fromMaybeM (PersonFieldNotPresent "firstName")
  let agent =
        OnUpdate.Agent
          { name = name,
            phone = mobileNumber,
            rating = realToFrac <$> driver.rating,
            registered_at = driver.createdAt
          }
      vehicle =
        OnUpdate.Vehicle
          { model = veh.model,
            variant = show veh.variant,
            color = veh.color,
            registration = veh.registrationNo
          }
  return $
    OnUpdate.RideAssigned
      OnUpdate.RideAssignedEvent
        { order_id = ride.bookingId.getId,
          ride_id = ride.id.getId,
          otp = ride.otp,
          ..
        }

makeRideStartedUpdatePayload ::
  SRide.Ride ->
  OnUpdate.OnUpdateEvent
makeRideStartedUpdatePayload ride = do
  OnUpdate.RideStarted
    OnUpdate.RideStartedEvent
      { order_id = ride.bookingId.getId,
        ride_id = ride.id.getId,
        ..
      }

buildRideCompletedUpdatePayload ::
  MonadFlow m =>
  SRide.Ride ->
  m OnUpdate.OnUpdateEvent
buildRideCompletedUpdatePayload ride = do
  fare <- OnUpdate.Price . realToFrac <$> ride.fare & fromMaybeM (InternalError "Ride fare is not present.")
  totalFare <- OnUpdate.Price . realToFrac <$> ride.totalFare & fromMaybeM (InternalError "Total ride fare is not present.")
  chargeableDistance <- fmap realToFrac ride.chargeableDistance & fromMaybeM (InternalError "Chargeable ride distance is not present.")
  return $
    OnUpdate.RideCompleted
      OnUpdate.RideCompletedEvent
        { order_id = ride.bookingId.getId,
          ride_id = ride.id.getId,
          chargeable_distance = chargeableDistance,
          total_fare = totalFare,
          ..
        }

buildRideReq ::
  MonadFlow m =>
  Id SRB.RideBooking ->
  ShortId SOrg.Organization ->
  SRideRequest.RideRequestType ->
  UTCTime ->
  m SRideRequest.RideRequest
buildRideReq rideId shortOrgId rideRequestType now = do
  guid <- generateGUID
  pure
    SRideRequest.RideRequest
      { id = Id guid,
        rideBookingId = rideId,
        shortOrgId = shortOrgId,
        createdAt = now,
        _type = rideRequestType,
        info = Nothing
      }
