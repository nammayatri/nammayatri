module Product.RideAPI.CancelRide (cancelRide) where

import Beckn.External.GoogleMaps.Types
import qualified Beckn.Storage.Esqueleto as Esq
import qualified Beckn.Types.APISuccess as APISuccess
import Beckn.Types.Id
import qualified Domain.Types.BookingCancellationReason as SBCR
import qualified Domain.Types.Person as SP
import qualified Domain.Types.Ride as SRide
import qualified Domain.Types.RideBooking as SRB
import Environment
import EulerHS.Prelude hiding (id)
import qualified Product.BecknProvider.BP as BP
import qualified Product.RideAPI.Handlers.CancelRide as Handler
import qualified Storage.Queries.BookingCancellationReason as QBCR
import qualified Storage.Queries.DriverInformation as DriverInformation
import qualified Storage.Queries.DriverStats as QDriverStats
import qualified Storage.Queries.Organization as QOrg
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RideBooking as QRB
import Tools.Metrics
import Types.API.Ride (CancelRideReq)
import Types.Error
import Utils.Common
import qualified Utils.Notifications as Notify

cancelRide :: Id SP.Person -> Id SRide.Ride -> CancelRideReq -> FlowHandler APISuccess.APISuccess
cancelRide personId rideId req = withFlowHandlerAPI $ do
  Handler.cancelRideHandler handle personId rideId req
  where
    handle =
      Handler.ServiceHandle
        { findRideById = QRide.findById,
          findById = QPerson.findById,
          cancelRide = cancelRideImpl
        }

-------------------------------------------------------------------------------------------------
--------------------------TODO: Move later-------------------------------------------------------
-------------------------------------------------------------------------------------------------

cancelRideImpl ::
  ( EsqDBFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasFlowEnv m r '["defaultRadiusOfSearch" ::: Meters, "driverPositionInfoExpiry" ::: Maybe Seconds],
    HasGoogleMaps m r,
    FCMFlow m r,
    CoreMetrics m
  ) =>
  Id SRide.Ride ->
  SBCR.BookingCancellationReason ->
  m ()
cancelRideImpl rideId bookingCReason = do
  ride <- QRide.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  rideBooking <- QRB.findById ride.bookingId >>= fromMaybeM (RideBookingNotFound ride.bookingId.getId)
  let transporterId = rideBooking.providerId
  transporter <-
    QOrg.findById transporterId
      >>= fromMaybeM (OrgNotFound transporterId.getId)
  cancelRideTransaction rideBooking.id ride bookingCReason
  logTagInfo ("rideId-" <> getId rideId) ("Cancellation reason " <> show bookingCReason.source)
  fork "cancelRide - Notify BAP" $ do
    BP.sendBookingCancelledUpdateToBAP rideBooking transporter bookingCReason.source
  fork "cancelRide - Notify driver" $ do
    driver <- QPerson.findById ride.driverId >>= fromMaybeM (PersonNotFound ride.driverId.getId)
    Notify.notifyOnCancel rideBooking driver.id driver.deviceToken bookingCReason.source

cancelRideTransaction ::
  EsqDBFlow m r =>
  Id SRB.RideBooking ->
  SRide.Ride ->
  SBCR.BookingCancellationReason ->
  m ()
cancelRideTransaction rideBookingId ride bookingCReason = Esq.runTransaction $ do
  updateDriverInfo ride.driverId
  QRide.updateStatus ride.id SRide.CANCELLED
  QRB.updateStatus rideBookingId SRB.CANCELLED
  QBCR.create bookingCReason
  where
    updateDriverInfo personId = do
      let driverId = cast personId
      DriverInformation.updateOnRide driverId False
      when (bookingCReason.source == SBCR.ByDriver) $ QDriverStats.updateIdleTime driverId
