module SharedLogic.CallBAP
  ( sendRideAssignedUpdateToBAP,
    sendRideStartedUpdateToBAP,
    sendRideCompletedUpdateToBAP,
    sendBookingCancelledUpdateToBAP,
    sendDriverOffer,
  )
where

import Beckn.Types.Common
import qualified Core.ACL.OnSelect as ACL
import qualified Core.ACL.OnUpdate as ACL
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.BookingCancellationReason as SRBCR
import qualified Domain.Types.DriverQuote as DDQ
import qualified Domain.Types.FareParams as Fare
import qualified Domain.Types.Organization as SOrg
import qualified Domain.Types.Ride as SRide
import qualified Domain.Types.SearchRequest as DSR
import EulerHS.Prelude
import ExternalAPI.Flow (callOnSelect, callOnUpdate)
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
  Money ->
  m ()
sendRideCompletedUpdateToBAP booking ride fareParams finalFare = do
  transporter <-
    QOrg.findById booking.providerId
      >>= fromMaybeM (OrgNotFound booking.providerId.getId)
  let rideCompletedBuildReq = ACL.RideCompletedBuildReq {ride, fareParams, finalFare}
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

sendDriverOffer ::
  ( HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    CoreMetrics m,
    HasPrettyLogger m r
  ) =>
  SOrg.Organization ->
  DSR.SearchRequest ->
  DDQ.DriverQuote ->
  m ()
sendDriverOffer transporter searchReq driverQuote = do
  callOnSelect transporter searchReq =<< (buildOnSelectReq transporter searchReq [driverQuote] <&> ACL.mkOnSelectMessage)
  where
    buildOnSelectReq ::
      (MonadTime m, HasPrettyLogger m r) =>
      SOrg.Organization ->
      DSR.SearchRequest ->
      [DDQ.DriverQuote] ->
      m ACL.DOnSelectReq
    buildOnSelectReq org searchRequest quotes = do
      now <- getCurrentTime
      logPretty DEBUG "on_select: searchRequest" searchRequest
      logPretty DEBUG "on_select: quotes" quotes
      let transporterInfo =
            ACL.TransporterInfo
              { shortId = org.shortId,
                name = org.name,
                contacts = fromMaybe "" org.mobileNumber,
                ridesInProgress = 0, -- FIXME
                ridesCompleted = 0, -- FIXME
                ridesConfirmed = 0 -- FIXME
              }
      pure $
        ACL.DOnSelectReq
          { transporterInfo,
            quotes,
            now,
            searchRequest
          }
