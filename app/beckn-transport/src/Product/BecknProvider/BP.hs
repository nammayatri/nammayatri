module Product.BecknProvider.BP
  ( sendTripAssignedUpdateToBAP,
    sendRideStartedUpdateToBAP,
    sendRideCompletedUpdateToBAP,
    sendCancelToBAP,
    buildRideReq,
  )
where

import Beckn.External.Encryption (decrypt)
import Beckn.Types.Common
import qualified Beckn.Types.Core.Migration1.API.OnCancel as API
import qualified Beckn.Types.Core.Migration1.API.OnUpdate as API
import qualified Beckn.Types.Core.Migration1.OnCancel as OnCancel
import qualified Beckn.Types.Core.Migration1.OnUpdate as OnUpdate
import Beckn.Types.Id
import Data.Time (UTCTime)
import EulerHS.Prelude
import qualified ExternalAPI.Flow as ExternalAPI
import qualified Storage.Queries.Organization as QOrg
import qualified Storage.Queries.Person as Person
import qualified Storage.Queries.Vehicle as Vehicle
import Types.Error
import Types.Metrics (CoreMetrics)
import qualified Types.Storage.Organization as SOrg
import qualified Types.Storage.Ride as SRide
import qualified Types.Storage.RideBooking as SRB
import qualified Types.Storage.RideRequest as SRideRequest
import Utils.Common

sendTripAssignedUpdateToBAP ::
  ( DBFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    CoreMetrics m
  ) =>
  SRB.RideBooking ->
  SRide.Ride ->
  m ()
sendTripAssignedUpdateToBAP rideBooking ride = do
  transporter <-
    QOrg.findOrganizationById rideBooking.providerId
      >>= fromMaybeM OrgNotFound
  buildTripAssignedUpdatePayload ride transporter
    >>= ExternalAPI.callBAP "on_update" API.onUpdateAPI transporter rideBooking.requestId . Right . OnUpdate.OnUpdateMessage

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
    >>= ExternalAPI.callBAP "on_update" API.onUpdateAPI transporter rideBooking.requestId . Right . OnUpdate.OnUpdateMessage

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
    >>= ExternalAPI.callBAP "on_update" API.onUpdateAPI transporter rideBooking.requestId . Right . OnUpdate.OnUpdateMessage

sendCancelToBAP ::
  ( DBFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    CoreMetrics m
  ) =>
  SRB.RideBooking ->
  SOrg.Organization ->
  OnCancel.CancellationSource ->
  m ()
sendCancelToBAP rideBooking transporter cancellationSource = do
  let message = OnCancel.OnCancelMessage (OnCancel.Order rideBooking.id.getId) cancellationSource
  ExternalAPI.callBAP "on_cancel" API.onCancelAPI transporter rideBooking.requestId $ Right message

buildTripAssignedUpdatePayload ::
  (DBFlow m r, EncFlow m r) =>
  SRide.Ride ->
  SOrg.Organization ->
  m OnUpdate.RideOrder
buildTripAssignedUpdatePayload ride org = do
  driver <-
    Person.findPersonById ride.driverId
      >>= fromMaybeM PersonNotFound
  decDriver <- decrypt driver
  veh <- Vehicle.findVehicleById ride.vehicleId >>= fromMaybeM VehicleNotFound
  mobileNumber <- decDriver.mobileCountryCode <> decDriver.mobileNumber & fromMaybeM (InternalError "Driver mobile number is not present.")
  let vehicleNumber = veh.registrationNo
  vehicleColor <- veh.color & fromMaybeM (InternalError "Vehicle color is not present.")
  vehicleModel <- veh.model & fromMaybeM (InternalError "Vehicle model is not present.")
  vehicleVariant <- show <$> veh.variant & fromMaybeM (InternalError "Vehicle variant is not present.")
  let agent =
        OnUpdate.Agent
          { name = org.name,
            phone = mobileNumber,
            rating = driver.rating,
            registered_at = driver.createdAt
          }
      vehicle =
        OnUpdate.Vehicle
          { model = vehicleModel,
            variant = vehicleVariant,
            color = vehicleColor,
            registration = vehicleNumber
          }
      fulfillment =
        OnUpdate.TripAssignedFulfillment
          { id = ride.id.getId,
            otp = ride.otp,
            ..
          }
  return $
    OnUpdate.TripAssigned
      OnUpdate.TripAssignedOrder
        { id = ride.bookingId.getId,
          updated_at = ride.updatedAt,
          ..
        }

buildRideStartedUpdatePayload ::
  (DBFlow m r, EncFlow m r) =>
  SRide.Ride ->
  m OnUpdate.RideOrder
buildRideStartedUpdatePayload ride = do
  let fulfillment =
        OnUpdate.RideStartedFulfillment
          { id = ride.id.getId
          }
  return $
    OnUpdate.RideStarted
      OnUpdate.RideStartedOrder
        { id = ride.bookingId.getId,
          updated_at = ride.updatedAt,
          ..
        }

buildRideCompletedUpdatePayload ::
  (DBFlow m r, EncFlow m r) =>
  SRide.Ride ->
  m OnUpdate.RideOrder
buildRideCompletedUpdatePayload ride = do
  totalFare <- realToFrac <$> ride.totalFare & fromMaybeM (InternalError "Total ride fare is not present.")
  chargeableDistance <- ride.chargeableDistance & fromMaybeM (InternalError "Chargeable ride distance is not present.")
  let fulfillment =
        OnUpdate.RideCompletedFulfillment
          { id = ride.id.getId,
            chargeable_distance = chargeableDistance
          }
      payment = OnUpdate.Payment $ OnUpdate.Params totalFare
  return $
    OnUpdate.RideCompleted
      OnUpdate.RideCompletedOrder
        { id = ride.bookingId.getId,
          updated_at = ride.updatedAt,
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