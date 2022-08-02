module Product.BecknProvider.BP
  ( sendRideAssignedUpdateToBAP,
    sendRideStartedUpdateToBAP,
    sendRideCompletedUpdateToBAP,
    sendBookingCancelledUpdateToBAP,
    sendBookingReallocationUpdateToBAP,
    buildRideReq,
  )
where

import Beckn.Types.Common
import Beckn.Types.Id
import qualified Core.ACL.OnUpdate as ACL
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.BookingCancellationReason as SRBCR
import qualified Domain.Types.FarePolicy.FareBreakup as DFareBreakup
import qualified Domain.Types.Organization as SOrg
import qualified Domain.Types.Ride as SRide
import qualified Domain.Types.RideRequest as SRideRequest
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
  [DFareBreakup.FareBreakup] ->
  m ()
sendRideCompletedUpdateToBAP booking ride fareBreakups = do
  transporter <-
    QOrg.findById booking.providerId
      >>= fromMaybeM (OrgNotFound booking.providerId.getId)
  let rideCompletedBuildReq = ACL.RideCompletedBuildReq {..}
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

sendBookingReallocationUpdateToBAP ::
  ( EsqDBFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    CoreMetrics m
  ) =>
  SRB.Booking ->
  Id SRide.Ride ->
  SOrg.Organization ->
  m ()
sendBookingReallocationUpdateToBAP booking rideId transporter = do
  let bookingReallocationBuildReq = ACL.BookingReallocationBuildReq {..}
  bookingReallocationMsg <- ACL.buildOnUpdateMessage bookingReallocationBuildReq
  void $ callOnUpdate transporter booking bookingReallocationMsg

buildRideReq ::
  MonadFlow m =>
  Id SRB.Booking ->
  ShortId SOrg.Organization ->
  SRideRequest.RideRequestType ->
  UTCTime ->
  m SRideRequest.RideRequest
buildRideReq rideId shortOrgId rideRequestType now = do
  guid <- generateGUID
  pure
    SRideRequest.RideRequest
      { id = Id guid,
        bookingId = rideId,
        shortOrgId = shortOrgId,
        createdAt = now,
        _type = rideRequestType,
        info = Nothing
      }
