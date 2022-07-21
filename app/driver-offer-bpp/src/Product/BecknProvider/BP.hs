module Product.BecknProvider.BP
  ( sendRideAssignedUpdateToBAP,
    sendRideStartedUpdateToBAP,
    sendRideCompletedUpdateToBAP,
    sendBookingCancelledUpdateToBAP,
  )
where

import Beckn.Types.Common
import qualified Core.ACL.OnUpdate as ACL
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.BookingCancellationReason as SRBCR
import qualified Domain.Types.FareParams as Fare
import qualified Domain.Types.Organization as SOrg
import qualified Domain.Types.Ride as SRide
import EulerHS.Prelude
import ExternalAPI.Flow (callOnUpdate)
import qualified Storage.Queries.Organization as QOrg
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Vehicle as QVeh
import Tools.Metrics (CoreMetrics)
import Types.Error
import Utils.Common

sendRideAssignedUpdateToBAP ::
  ( EsqDBFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    CoreMetrics m
  ) =>
  SRB.Booking ->
  SRide.Ride ->
  m ()
sendRideAssignedUpdateToBAP booking ride = do
  transporter <-
    QOrg.findById booking.providerId
      >>= fromMaybeM (OrgNotFound booking.providerId.getId)
  driver <- QPerson.findById ride.driverId >>= fromMaybeM (PersonNotFound ride.driverId.getId)
  vehicle <- QVeh.findById ride.driverId >>= fromMaybeM (VehicleNotFound ride.driverId.getId)
  let rideAssignedBuildReq = ACL.RideAssignedBuildReq {..}
  rideAssignedMsg <- ACL.buildOnUpdateMessage rideAssignedBuildReq
  void $ callOnUpdate transporter booking rideAssignedMsg

sendRideStartedUpdateToBAP ::
  ( EsqDBFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    CoreMetrics m
  ) =>
  SRB.Booking ->
  SRide.Ride ->
  m ()
sendRideStartedUpdateToBAP booking ride = do
  transporter <-
    QOrg.findById booking.providerId
      >>= fromMaybeM (OrgNotFound booking.providerId.getId)
  let rideStartedBuildReq = ACL.RideStartedBuildReq {..}
  rideStartedMsg <- ACL.buildOnUpdateMessage rideStartedBuildReq
  void $ callOnUpdate transporter booking rideStartedMsg

sendRideCompletedUpdateToBAP ::
  ( EsqDBFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    CoreMetrics m
  ) =>
  SRB.Booking ->
  SRide.Ride ->
  Fare.FareParameters ->
  m ()
sendRideCompletedUpdateToBAP booking ride fareParams = do
  transporter <-
    QOrg.findById booking.providerId
      >>= fromMaybeM (OrgNotFound booking.providerId.getId)
  let rideCompletedBuildReq = ACL.RideCompletedBuildReq {ride, fareParams}
  rideCompletedMsg <- ACL.buildOnUpdateMessage rideCompletedBuildReq
  void $ callOnUpdate transporter booking rideCompletedMsg

sendBookingCancelledUpdateToBAP ::
  ( EsqDBFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    CoreMetrics m
  ) =>
  SRB.Booking ->
  SOrg.Organization ->
  SRBCR.CancellationSource ->
  m ()
sendBookingCancelledUpdateToBAP booking transporter cancellationSource = do
  let bookingCancelledBuildReq = ACL.BookingCancelledBuildReq {..}
  bookingCancelledMsg <- ACL.buildOnUpdateMessage bookingCancelledBuildReq
  void $ callOnUpdate transporter booking bookingCancelledMsg
