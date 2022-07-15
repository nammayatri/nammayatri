module Product.BecknProvider.BP
  ( sendRideAssignedUpdateToBAP,
  {-
    sendRideStartedUpdateToBAP,
    sendRideCompletedUpdateToBAP,
    sendRideBookingCancelledUpdateToBAP,
    sendRideBookingReallocationUpdateToBAP,
    buildRideReq,
    -}
  )
where

import Beckn.Types.Common
--import Beckn.Types.Id
import qualified Core.ACL.OnUpdate as ACL
--import qualified Domain.Types.FareBreakup as DFareBreakup
--import qualified Domain.Types.Organization as SOrg
import qualified Domain.Types.Ride as SRide
import qualified Domain.Types.RideBooking as SRB
--import qualified Domain.Types.RideBookingCancellationReason as SRBCR
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
  SRB.RideBooking ->
  SRide.Ride ->
  m ()
sendRideAssignedUpdateToBAP rideBooking ride = do
  transporter <-
    QOrg.findById rideBooking.providerId
      >>= fromMaybeM (OrgNotFound rideBooking.providerId.getId)
  driver <- QPerson.findById ride.driverId >>= fromMaybeM (PersonNotFound ride.driverId.getId)
  vehicle <- QVeh.findById ride.vehicleId >>= fromMaybeM (VehicleNotFound ride.vehicleId.getId)
  let rideAssignedBuildReq = ACL.RideAssignedBuildReq {ride = ride, ..}
  rideAssignedMsg <- ACL.buildOnUpdateMessage rideAssignedBuildReq
  void $ callOnUpdate transporter rideBooking rideAssignedMsg

{-
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
      >>= fromMaybeM (OrgNotFound rideBooking.providerId.getId)
  let rideStartedBuildReq = ACL.RideStartedBuildReq {..}
  rideStartedMsg <- ACL.buildOnUpdateMessage rideStartedBuildReq
  void $ callOnUpdate transporter rideBooking rideStartedMsg

sendRideCompletedUpdateToBAP ::
  ( EsqDBFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    CoreMetrics m
  ) =>
  SRB.RideBooking ->
  SRide.Ride ->
  [DFareBreakup.FareBreakup] ->
  m ()
sendRideCompletedUpdateToBAP rideBooking ride fareBreakups = do
  transporter <-
    QOrg.findById rideBooking.providerId
      >>= fromMaybeM (OrgNotFound rideBooking.providerId.getId)
  let rideCompletedBuildReq = ACL.RideCompletedBuildReq {..}
  rideCompletedMsg <- ACL.buildOnUpdateMessage rideCompletedBuildReq
  void $ callOnUpdate transporter rideBooking rideCompletedMsg

sendRideBookingCancelledUpdateToBAP ::
  ( EsqDBFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    CoreMetrics m
  ) =>
  SRB.RideBooking ->
  SOrg.Organization ->
  SRBCR.CancellationSource ->
  m ()
sendRideBookingCancelledUpdateToBAP booking transporter cancellationSource = do
  let bookingCancelledBuildReq = ACL.BookingCancelledBuildReq {..}
  bookingCancelledMsg <- ACL.buildOnUpdateMessage bookingCancelledBuildReq
  void $ callOnUpdate transporter booking bookingCancelledMsg

sendRideBookingReallocationUpdateToBAP ::
  ( EsqDBFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    CoreMetrics m
  ) =>
  SRB.RideBooking ->
  Id SRide.Ride ->
  SOrg.Organization ->
  m ()
sendRideBookingReallocationUpdateToBAP booking rideId transporter = do
  let bookingReallocationBuildReq = ACL.BookingReallocationBuildReq {..}
  bookingReallocationMsg <- ACL.buildOnUpdateMessage bookingReallocationBuildReq
  void $ callOnUpdate transporter booking bookingReallocationMsg

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
      -}
