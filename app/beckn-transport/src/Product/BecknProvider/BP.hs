module Product.BecknProvider.BP
  ( sendRideAssignedUpdateToBAP,
    sendRideStartedUpdateToBAP,
    sendRideCompletedUpdateToBAP,
    sendRideBookingCanceledUpdateToBAP,
    buildRideReq,
  )
where

import Beckn.External.Encryption (decrypt)
import Beckn.Types.Common
import qualified Beckn.Types.Core.Taxi.OnUpdate as OnUpdate
import Beckn.Types.Id
import EulerHS.Prelude
import ExternalAPI.Flow (callOnUpdate)
import qualified Storage.Queries.Organization as QOrg
import qualified Storage.Queries.Person as Person
import qualified Storage.Queries.Vehicle as Vehicle
import Types.Error
import Types.Metrics (CoreMetrics)
import qualified Types.Storage.Organization as SOrg
import qualified Types.Storage.Ride as SRide
import qualified Types.Storage.RideBooking as SRB
import qualified Types.Storage.RideRequest as SRideRequest
import qualified Types.Storage.SearchRequest as SSR
import Utils.Common

sendRideAssignedUpdateToBAP ::
  ( DBFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    CoreMetrics m
  ) =>
  SRB.RideBooking ->
  SRide.Ride ->
  m ()
sendRideAssignedUpdateToBAP rideBooking ride = do
  transporter <-
    QOrg.findOrganizationById rideBooking.providerId
      >>= fromMaybeM OrgNotFound
  buildRideAssignedUpdatePayload ride
    >>= sendUpdateEvent transporter rideBooking.requestId

sendRideStartedUpdateToBAP ::
  ( DBFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    CoreMetrics m
  ) =>
  SRB.RideBooking ->
  SRide.Ride ->
  m ()
sendRideStartedUpdateToBAP rideBooking ride = do
  transporter <-
    QOrg.findOrganizationById rideBooking.providerId
      >>= fromMaybeM OrgNotFound
  buildRideStartedUpdatePayload ride
    >>= sendUpdateEvent transporter rideBooking.requestId

sendRideCompletedUpdateToBAP ::
  ( DBFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    CoreMetrics m
  ) =>
  SRB.RideBooking ->
  SRide.Ride ->
  m ()
sendRideCompletedUpdateToBAP rideBooking ride = do
  transporter <-
    QOrg.findOrganizationById rideBooking.providerId
      >>= fromMaybeM OrgNotFound
  buildRideCompletedUpdatePayload ride
    >>= sendUpdateEvent transporter rideBooking.requestId

sendRideBookingCanceledUpdateToBAP ::
  ( DBFlow m r,
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

sendUpdateEvent ::
  ( DBFlow m r,
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
  (DBFlow m r, EncFlow m r) =>
  SRide.Ride ->
  m OnUpdate.OnUpdateEvent
buildRideAssignedUpdatePayload ride = do
  driver <-
    Person.findPersonById ride.driverId
      >>= fromMaybeM PersonNotFound
  decDriver <- decrypt driver
  veh <- Vehicle.findVehicleById ride.vehicleId >>= fromMaybeM VehicleNotFound
  mobileNumber <- decDriver.mobileCountryCode <> decDriver.mobileNumber & fromMaybeM (InternalError "Driver mobile number is not present.")
  firstName <- decDriver.firstName & fromMaybeM (PersonFieldNotPresent "firstName")
  let middleName = decDriver.middleName
      lastName = decDriver.lastName
  let name = firstName <> maybe "" (" " <>) middleName <> maybe "" (" " <>) lastName
  let vehicleNumber = veh.registrationNo
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
            registration = vehicleNumber
          }
  return $
    OnUpdate.RideAssigned
      OnUpdate.RideAssignedEvent
        { order_id = ride.bookingId.getId,
          ride_id = ride.id.getId,
          otp = ride.otp,
          ..
        }

buildRideStartedUpdatePayload ::
  (DBFlow m r, EncFlow m r) =>
  SRide.Ride ->
  m OnUpdate.OnUpdateEvent
buildRideStartedUpdatePayload ride = do
  return $
    OnUpdate.RideStarted
      OnUpdate.RideStartedEvent
        { order_id = ride.bookingId.getId,
          ride_id = ride.id.getId,
          ..
        }

buildRideCompletedUpdatePayload ::
  (DBFlow m r, EncFlow m r) =>
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
