module Domain.Action.UI.Ride.CancelRide.Internal (cancelRide) where

import Beckn.External.GoogleMaps.Types (HasGoogleMaps)
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Storage.Hedis
import Beckn.Types.App
import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Utils.Common
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.BookingCancellationReason as SBCR
import qualified Domain.Types.BusinessEvent as SB
import qualified Domain.Types.Organization as Organization
import qualified Domain.Types.Ride as SRide
import qualified Domain.Types.RideRequest as SRideRequest
import EulerHS.Prelude
import qualified SharedLogic.CallBAP as BP
import SharedLogic.DriverPool (recalculateDriverPool)
import qualified Storage.CachedQueries.Organization as Organization
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.BookingCancellationReason as QBCR
import qualified Storage.Queries.BusinessEvent as QBE
import qualified Storage.Queries.DriverInformation as DriverInformation
import qualified Storage.Queries.DriverStats as QDriverStats
import qualified Storage.Queries.Person as Person
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RideRequest as RideRequest
import Tools.Error
import Tools.Metrics (CoreMetrics)
import qualified Tools.Notifications as Notify

cancelRide ::
  ( EsqDBFlow m r,
    HedisFlow m r,
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
cancelRide rideId bookingCReason = do
  ride <- QRide.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  booking <- QRB.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
  let transporterId = booking.providerId
  transporter <-
    Organization.findById transporterId
      >>= fromMaybeM (OrgNotFound transporterId.getId)
  if isCancelledByDriver
    then do
      driverPool <- recalculateDriverPool booking
      Esq.runTransaction $ traverse_ (QBE.logDriverInPoolEvent SB.ON_REALLOCATION (Just booking.id)) driverPool
      reallocateRideTransaction transporter.shortId booking.id ride bookingCReason
    else cancelRideTransaction booking.id ride bookingCReason
  logTagInfo ("rideId-" <> getId rideId) ("Cancellation reason " <> show bookingCReason.source)
  fork "cancelRide - Notify BAP" $ do
    if isCancelledByDriver
      then BP.sendBookingReallocationUpdateToBAP booking ride.id transporter
      else BP.sendBookingCancelledUpdateToBAP booking transporter bookingCReason.source
  fork "cancelRide - Notify driver" $ do
    driver <- Person.findById ride.driverId >>= fromMaybeM (PersonNotFound ride.driverId.getId)
    Notify.notifyOnCancel booking driver.id driver.deviceToken bookingCReason.source
  where
    isCancelledByDriver = bookingCReason.source == SBCR.ByDriver

cancelRideTransaction ::
  EsqDBFlow m r =>
  Id SRB.Booking ->
  SRide.Ride ->
  SBCR.BookingCancellationReason ->
  m ()
cancelRideTransaction bookingId ride bookingCReason = Esq.runTransaction $ do
  updateDriverInfo ride.driverId
  QRide.updateStatus ride.id SRide.CANCELLED
  QRB.updateStatus bookingId SRB.CANCELLED
  QBCR.create bookingCReason
  where
    updateDriverInfo personId = do
      let driverId = cast personId
      DriverInformation.updateOnRide driverId False
      when (bookingCReason.source == SBCR.ByDriver) $ QDriverStats.updateIdleTime driverId

reallocateRideTransaction ::
  EsqDBFlow m r =>
  ShortId Organization.Organization ->
  Id SRB.Booking ->
  SRide.Ride ->
  SBCR.BookingCancellationReason ->
  m ()
reallocateRideTransaction orgShortId bookingId ride bookingCReason = do
  rideRequest <- buildRideReq orgShortId
  Esq.runTransaction $ do
    QRB.updateStatus bookingId SRB.AWAITING_REASSIGNMENT
    QRB.increaseReallocationsCounter bookingId
    QRide.updateStatus ride.id SRide.CANCELLED
    updateDriverInfo
    QBCR.create bookingCReason
    RideRequest.create rideRequest
  where
    updateDriverInfo = do
      let driverId = cast ride.driverId
      DriverInformation.updateOnRide driverId False
      QDriverStats.updateIdleTime driverId
    buildRideReq shortOrgId = do
      guid <- generateGUID
      now <- getCurrentTime
      pure
        SRideRequest.RideRequest
          { id = Id guid,
            bookingId = bookingId,
            shortOrgId = shortOrgId,
            createdAt = now,
            _type = SRideRequest.ALLOCATION,
            info = Nothing
          }
