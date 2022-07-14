module Domain.Action.UI.Ride.CancelRide.Internal (cancelRide) where

import Beckn.External.GoogleMaps.Types (HasGoogleMaps)
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Types.App
import Beckn.Types.Common
import Beckn.Types.Id
import qualified Domain.Types.BusinessEvent as SB
import qualified Domain.Types.Organization as Organization
import qualified Domain.Types.Ride as SRide
import qualified Domain.Types.RideBooking as SRB
import qualified Domain.Types.RideBookingCancellationReason as SBCR
import qualified Domain.Types.RideRequest as SRideRequest
import EulerHS.Prelude
import qualified Product.BecknProvider.BP as BP
import SharedLogic.DriverPool (recalculateDriverPool)
import qualified Storage.Queries.BusinessEvent as QBE
import qualified Storage.Queries.DriverInformation as DriverInformation
import qualified Storage.Queries.DriverStats as QDriverStats
import qualified Storage.Queries.Organization as Organization
import qualified Storage.Queries.Person as Person
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RideBooking as QRB
import qualified Storage.Queries.RideBookingCancellationReason as QBCR
import qualified Storage.Queries.RideRequest as RideRequest
import Tools.Metrics (CoreMetrics)
import Types.Error
import Utils.Common
import qualified Utils.Notifications as Notify

cancelRide ::
  ( EsqDBFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasFlowEnv m r '["defaultRadiusOfSearch" ::: Meters, "driverPositionInfoExpiry" ::: Maybe Seconds],
    HasGoogleMaps m r,
    FCMFlow m r,
    CoreMetrics m
  ) =>
  Id SRide.Ride ->
  SBCR.RideBookingCancellationReason ->
  m ()
cancelRide rideId bookingCReason = do
  ride <- QRide.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  rideBooking <- QRB.findById ride.bookingId >>= fromMaybeM (RideBookingNotFound ride.bookingId.getId)
  let transporterId = rideBooking.providerId
  transporter <-
    Organization.findById transporterId
      >>= fromMaybeM (OrgNotFound transporterId.getId)
  if isCancelledByDriver
    then do
      let fareProductType = SRB.getFareProductType rideBooking.rideBookingDetails
      driverPool <- recalculateDriverPool rideBooking.fromLocationId rideBooking.id rideBooking.providerId rideBooking.vehicleVariant fareProductType
      Esq.runTransaction $ traverse_ (QBE.logDriverInPoolEvent SB.ON_REALLOCATION (Just rideBooking.id)) driverPool
      reallocateRideTransaction transporter.shortId rideBooking.id ride bookingCReason
    else cancelRideTransaction rideBooking.id ride bookingCReason
  logTagInfo ("rideId-" <> getId rideId) ("Cancellation reason " <> show bookingCReason.source)
  fork "cancelRide - Notify BAP" $ do
    if isCancelledByDriver
      then BP.sendRideBookingReallocationUpdateToBAP rideBooking ride.id transporter
      else BP.sendRideBookingCancelledUpdateToBAP rideBooking transporter bookingCReason.source
  fork "cancelRide - Notify driver" $ do
    driver <- Person.findById ride.driverId >>= fromMaybeM (PersonNotFound ride.driverId.getId)
    Notify.notifyOnCancel rideBooking driver.id driver.deviceToken bookingCReason.source
  where
    isCancelledByDriver = bookingCReason.source == SBCR.ByDriver

cancelRideTransaction ::
  EsqDBFlow m r =>
  Id SRB.RideBooking ->
  SRide.Ride ->
  SBCR.RideBookingCancellationReason ->
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

reallocateRideTransaction ::
  EsqDBFlow m r =>
  ShortId Organization.Organization ->
  Id SRB.RideBooking ->
  SRide.Ride ->
  SBCR.RideBookingCancellationReason ->
  m ()
reallocateRideTransaction orgShortId rideBookingId ride bookingCReason = do
  now <- getCurrentTime
  rideRequest <-
    BP.buildRideReq
      rideBookingId
      orgShortId
      SRideRequest.ALLOCATION
      now
  Esq.runTransaction $ do
    QRB.updateStatus rideBookingId SRB.AWAITING_REASSIGNMENT
    QRB.increaseReallocationsCounter rideBookingId
    QRide.updateStatus ride.id SRide.CANCELLED
    updateDriverInfo
    QBCR.create bookingCReason
    RideRequest.create rideRequest
  where
    updateDriverInfo = do
      let driverId = cast ride.driverId
      DriverInformation.updateOnRide driverId False
      QDriverStats.updateIdleTime driverId
