module Product.BecknProvider.Cancel where

import qualified Beckn.Storage.Esqueleto as Esq
import qualified Beckn.Storage.Queries.BecknRequest as QBR
import Beckn.Types.Core.Ack
import qualified Beckn.Types.Core.Taxi.API.Cancel as Cancel
import Beckn.Types.Id
import Beckn.Utils.Servant.SignatureAuth (SignatureAuthResult (..))
import qualified Core.ACL.Cancel as ACL
import Data.Aeson (encode)
import qualified Domain.Action.Beckn.Cancel as DCancel
import qualified Domain.Types.Organization as Organization
import Environment
import EulerHS.Prelude
import Utils.Common

cancel ::
  Id Organization.Organization ->
  SignatureAuthResult ->
  Cancel.CancelReq ->
  FlowHandler AckResponse
cancel transporterId subscriber@(SignatureAuthResult signPayload _) req =
  withFlowHandlerBecknAPI . withTransactionIdLogTag req $ do
    logTagInfo "Cancel API Flow" "Reached"
    Esq.runTransaction $ do
      QBR.logBecknRequest (show $ encode req) (show $ signPayload.signature)
    dConfirmReq <- ACL.buildCancelReq req
    DCancel.cancel transporterId subscriber dConfirmReq
    return Ack

-- cancelRide ::
--   ( EsqDBFlow m r,
--     EncFlow m r,
--     HasFlowEnv m r '["nwAddress" ::: BaseUrl],
--     HasFlowEnv m r '["defaultRadiusOfSearch" ::: Meters, "driverPositionInfoExpiry" ::: Maybe Seconds],
--     HasGoogleMaps m r,
--     FCMFlow m r,
--     CoreMetrics m
--   ) =>
--   Id SRide.Ride ->
--   SBCR.RideBookingCancellationReason ->
--   m ()
-- cancelRide rideId bookingCReason = do
--   ride <- QRide.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
--   rideBooking <- QRB.findById ride.bookingId >>= fromMaybeM (RideBookingNotFound ride.bookingId.getId)
--   let transporterId = rideBooking.providerId
--   transporter <-
--     Organization.findById transporterId
--       >>= fromMaybeM (OrgNotFound transporterId.getId)
--   if isCancelledByDriver
--     then do
--       let fareProductType = SRB.getFareProductType rideBooking.rideBookingDetails
--       driverPool <- recalculateDriverPool rideBooking.fromLocationId rideBooking.id rideBooking.providerId rideBooking.vehicleVariant fareProductType
--       Esq.runTransaction $ traverse_ (QBE.logDriverInPoolEvent SB.ON_REALLOCATION (Just rideBooking.id)) driverPool
--       reallocateRideTransaction transporter.shortId rideBooking.id ride bookingCReason
--     else cancelRideTransaction rideBooking.id ride bookingCReason
--   logTagInfo ("rideId-" <> getId rideId) ("Cancellation reason " <> show bookingCReason.source)
--   fork "cancelRide - Notify BAP" $ do
--     if isCancelledByDriver
--       then BP.sendRideBookingReallocationUpdateToBAP rideBooking ride.id transporter
--       else BP.sendRideBookingCancelledUpdateToBAP rideBooking transporter bookingCReason.source
--   notifyDriverOnCancel rideBooking ride bookingCReason
--   where
--     isCancelledByDriver = bookingCReason.source == SBCR.ByDriver

-- notifyDriverOnCancel ::
--   ( EsqDBFlow m r,
--     CoreMetrics m,
--     FCMFlow m r
--   ) =>
--   SRB.RideBooking ->
--   SRide.Ride ->
--   SBCR.RideBookingCancellationReason ->
--   m ()
-- notifyDriverOnCancel rideBooking ride cancellationReason =
--   fork "cancelRide - Notify driver" $ do
--     driver <- Person.findById ride.driverId >>= fromMaybeM (PersonNotFound ride.driverId.getId)
--     Notify.notifyOnCancel rideBooking driver.id driver.deviceToken cancellationReason.source

-- cancelRideTransaction ::
--   EsqDBFlow m r =>
--   Id SRB.RideBooking ->
--   SRide.Ride ->
--   SBCR.RideBookingCancellationReason ->
--   m ()
-- cancelRideTransaction rideBookingId ride bookingCReason = Esq.runTransaction $ do
--   updateDriverInfo ride.driverId
--   QRide.updateStatus ride.id SRide.CANCELLED
--   QRB.updateStatus rideBookingId SRB.CANCELLED
--   QBCR.create bookingCReason
--   where
--     updateDriverInfo personId = do
--       let driverId = cast personId
--       DriverInformation.updateOnRide driverId False
--       when (bookingCReason.source == SBCR.ByDriver) $ QDriverStats.updateIdleTime driverId

-- reallocateRideTransaction ::
--   EsqDBFlow m r =>
--   ShortId Organization.Organization ->
--   Id SRB.RideBooking ->
--   SRide.Ride ->
--   SBCR.RideBookingCancellationReason ->
--   m ()
-- reallocateRideTransaction orgShortId rideBookingId ride bookingCReason = do
--   now <- getCurrentTime
--   rideRequest <-
--     BP.buildRideReq
--       rideBookingId
--       orgShortId
--       SRideRequest.ALLOCATION
--       now
--   Esq.runTransaction $ do
--     QRB.updateStatus rideBookingId SRB.AWAITING_REASSIGNMENT
--     QRB.increaseReallocationsCounter rideBookingId
--     QRide.updateStatus ride.id SRide.CANCELLED
--     updateDriverInfo
--     QBCR.create bookingCReason
--     RideRequest.create rideRequest
--   where
--     updateDriverInfo = do
--       let driverId = cast ride.driverId
--       DriverInformation.updateOnRide driverId False
--       QDriverStats.updateIdleTime driverId
