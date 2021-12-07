module Product.BecknProvider.Cancel where

import App.Types (FlowHandler)
import Beckn.Product.Validation.Context
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Common
import Beckn.Types.Core.Ack
import qualified Beckn.Types.Core.Cabs.API.Cancel as Cancel
import qualified Beckn.Types.Core.Cabs.Cancel.CancellationSource as Cancel
import Beckn.Types.Id
import Beckn.Utils.Servant.SignatureAuth (SignatureAuthResult (..))
import EulerHS.Prelude
import qualified Product.BecknProvider.BP as BP
import qualified Storage.Queries.DriverInformation as DriverInformation
import qualified Storage.Queries.DriverStats as QDriverStats
import qualified Storage.Queries.Organization as Organization
import qualified Storage.Queries.Person as Person
import qualified Storage.Queries.Quote as Quote
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RideBooking as QRB
import qualified Storage.Queries.RideRequest as RideRequest
import qualified Storage.Queries.SearchRequest as SearchRequest
import Types.Error
import Types.Metrics (CoreMetrics)
import qualified Types.Storage.Organization as Organization
import qualified Types.Storage.Ride as SRide
import qualified Types.Storage.RideBooking as SRB
import qualified Types.Storage.RideCancellationReason as SRCR
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
    let quoteId = req.message.order_id
    transporterOrg <-
      Organization.findOrganizationById transporterId
        >>= fromMaybeM OrgNotFound
    quote <- Quote.findById (Id quoteId) >>= fromMaybeM QuoteDoesNotExist
    rideBooking <- QRB.findByQuoteId (quote.id) >>= fromMaybeM RideNotFound
    now <- getCurrentTime
    RideRequest.createFlow =<< BP.buildRideReq (rideBooking.id) (transporterOrg.shortId) SRideRequest.CANCELLATION now
    return Ack

cancelRide ::
  ( DBFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    FCMFlow m r,
    CoreMetrics m
  ) =>
  Id SRide.Ride ->
  SRCR.RideCancellationReason ->
  m ()
cancelRide rideId rideCReason = do
  ride <- QRide.findById rideId >>= fromMaybeM RideDoesNotExist
  rideBooking <- QRB.findById ride.bookingId >>= fromMaybeM RideBookingNotFound
  cancelRideTransaction rideBooking ride rideCReason
  logTagInfo ("rideId-" <> getId rideId) ("Cancellation reason " <> show rideCReason.source)
  fork "cancelRide - Notify BAP" $ do
    let transporterId = rideBooking.providerId
    transporter <-
      Organization.findOrganizationById transporterId
        >>= fromMaybeM OrgNotFound
    BP.sendCancelToBAP rideBooking transporter rideCReason.source
    searchRequest <- SearchRequest.findById (rideBooking.requestId) >>= fromMaybeM SearchRequestNotFound
    driver <- Person.findPersonById ride.driverId >>= fromMaybeM PersonNotFound
    Notify.notifyOnCancel searchRequest driver.id driver.deviceToken rideCReason.source

cancelRideTransaction ::
  DBFlow m r =>
  SRB.RideBooking ->
  SRide.Ride ->
  SRCR.RideCancellationReason ->
  m ()
cancelRideTransaction rideBooking ride rideCReason = DB.runSqlDBTransaction $ do
  updateDriverInfo ride.driverId
  QRide.updateStatus ride.id SRide.CANCELLED
  QRB.updateStatus rideBooking.id SRB.CANCELLED
  where
    updateDriverInfo personId = do
      let driverId = cast personId
      DriverInformation.updateOnRide driverId False
      when (rideCReason.source == Cancel.ByDriver) $ QDriverStats.updateIdleTime driverId
