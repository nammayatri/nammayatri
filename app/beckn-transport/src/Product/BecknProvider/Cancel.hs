module Product.BecknProvider.Cancel where

import App.Types (FlowHandler)
import Beckn.Product.Validation.Context
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Common
import Beckn.Types.Core.Ack
import qualified Beckn.Types.Core.Taxi.API.Cancel as Cancel
import qualified Beckn.Types.Core.Taxi.Cancel.Req as ReqCancel
import Beckn.Types.Id
import Beckn.Utils.Servant.SignatureAuth (SignatureAuthResult (..))
import EulerHS.Prelude
import qualified Product.BecknProvider.BP as BP
import SharedLogic.DriverPool (recalculateDriverPool)
import qualified Storage.Queries.DriverInformation as DriverInformation
import qualified Storage.Queries.DriverStats as QDriverStats
import qualified Storage.Queries.Organization as Organization
import qualified Storage.Queries.Person as Person
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RideBooking as QRB
import qualified Storage.Queries.RideBookingCancellationReason as QBCR
import qualified Storage.Queries.RideRequest as RideRequest
import qualified Storage.Queries.SearchRequest as SearchRequest
import Types.Error
import Types.Metrics (CoreMetrics)
import qualified Types.Storage.Organization as Organization
import qualified Types.Storage.Ride as SRide
import qualified Types.Storage.RideBooking as SRB
import qualified Types.Storage.RideBookingCancellationReason as SBCR
import qualified Types.Storage.RideRequest as SRideRequest
import Utils.Common
import qualified Utils.Notifications as Notify

cancel ::
  Id Organization.Organization ->
  SignatureAuthResult ->
  Cancel.CancelReq ->
  FlowHandler AckResponse
cancel transporterId _ req = withFlowHandlerBecknAPI $
  withTransactionIdLogTag req $ do
    let context = req.context
    validateContext context
    let bookingId = req.message.order_id
    transporterOrg <-
      Organization.findOrganizationById transporterId
        >>= fromMaybeM OrgNotFound
    rideBooking <- QRB.findById (Id bookingId) >>= fromMaybeM RideBookingDoesNotExist
    now <- getCurrentTime
    RideRequest.createFlow =<< BP.buildRideReq (rideBooking.id) (transporterOrg.shortId) SRideRequest.CANCELLATION now
    return Ack

cancelRide ::
  ( DBFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasFlowEnv m r '["defaultRadiusOfSearch" ::: Meters, "driverPositionInfoExpiry" ::: Maybe Seconds],
    FCMFlow m r,
    CoreMetrics m
  ) =>
  Id SRide.Ride ->
  SBCR.RideBookingCancellationReason ->
  m ()
cancelRide rideId bookingCReason = do
  ride <- QRide.findById rideId >>= fromMaybeM RideDoesNotExist
  rideBooking <- QRB.findById ride.bookingId >>= fromMaybeM RideBookingNotFound
  let transporterId = rideBooking.providerId
  transporter <-
    Organization.findOrganizationById transporterId
      >>= fromMaybeM OrgNotFound
  if isCancelledByDriver
    then do
      void $ recalculateDriverPool rideBooking.fromLocationId rideBooking.id rideBooking.providerId rideBooking.vehicleVariant
      reallocateRideTransaction transporter.shortId rideBooking.id ride bookingCReason
    else cancelRideTransaction rideBooking.id ride bookingCReason
  logTagInfo ("rideId-" <> getId rideId) ("Cancellation reason " <> show bookingCReason.source)
  fork "cancelRide - Notify BAP" $ do
    if isCancelledByDriver
      then BP.sendRideBookingReallocationUpdateToBAP rideBooking ride.id transporter bookingCReason.source
      else BP.sendRideBookingCanceledUpdateToBAP rideBooking transporter bookingCReason.source
  notifyDriverOnCancel rideBooking ride bookingCReason
  where
    isCancelledByDriver = bookingCReason.source == ReqCancel.ByDriver

notifyDriverOnCancel ::
  ( DBFlow m r,
    CoreMetrics m,
    FCMFlow m r
  ) =>
  SRB.RideBooking ->
  SRide.Ride ->
  SBCR.RideBookingCancellationReason ->
  m ()
notifyDriverOnCancel rideBooking ride cancellationReason =
  fork "cancelRide - Notify driver" $ do
    searchRequest <- SearchRequest.findById (rideBooking.requestId) >>= fromMaybeM SearchRequestNotFound
    driver <- Person.findPersonById ride.driverId >>= fromMaybeM PersonNotFound
    Notify.notifyOnCancel searchRequest driver.id driver.deviceToken cancellationReason.source

cancelRideTransaction ::
  DBFlow m r =>
  Id SRB.RideBooking ->
  SRide.Ride ->
  SBCR.RideBookingCancellationReason ->
  m ()
cancelRideTransaction rideBookingId ride bookingCReason = DB.runSqlDBTransaction $ do
  updateDriverInfo ride.driverId
  QRide.updateStatus ride.id SRide.CANCELLED
  QRB.updateStatus rideBookingId SRB.CANCELLED
  QBCR.create bookingCReason
  where
    updateDriverInfo personId = do
      let driverId = cast personId
      DriverInformation.updateOnRide driverId False
      when (bookingCReason.source == ReqCancel.ByDriver) $ QDriverStats.updateIdleTime driverId

reallocateRideTransaction ::
  DBFlow m r =>
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
  DB.runSqlDBTransaction $ do
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
