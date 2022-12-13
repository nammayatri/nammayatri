module Domain.Action.UI.Ride.StartRide
  ( ServiceHandle (..),
    DriverStartRideReq (..),
    DashboardStartRideReq (..),
    StartRideReq (..),
    buildStartRideHandle,
    driverStartRide,
    dashboardStartRide,
    startRideHandler,
  )
where

import Beckn.External.Maps.HasCoordinates
import Beckn.External.Maps.Types
import qualified Beckn.Storage.Hedis as Redis
import Beckn.Tools.Metrics.CoreMetrics
import qualified Beckn.Types.APISuccess as APISuccess
import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Utils.Common
import Beckn.Utils.SlidingWindowLimiter
import qualified Domain.Action.UI.Ride.StartRide.Internal as SInternal
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.DriverLocation as DDrLoc
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as SRide
import Environment (Flow)
import EulerHS.Prelude
import qualified Lib.LocationUpdates as LocUpd
import SharedLogic.CallBAP (sendRideStartedUpdateToBAP)
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.DriverLocation as QDrLoc
import qualified Storage.Queries.Ride as QRide
import Tools.Error

data ServiceHandle m = ServiceHandle
  { findBookingById :: Id SRB.Booking -> m (Maybe SRB.Booking),
    findLocationByDriverId :: Id DP.Person -> m (Maybe DDrLoc.DriverLocation),
    startRideAndUpdateLocation :: Id DP.Person -> Id SRide.Ride -> Id SRB.Booking -> LatLong -> m (),
    notifyBAPRideStarted :: SRB.Booking -> SRide.Ride -> m (),
    rateLimitStartRide :: Id DP.Person -> Id SRide.Ride -> m (),
    initializeDistanceCalculation :: Id SRide.Ride -> Id DP.Person -> LatLong -> m ()
  }

data StartRideReq = DriverReq DriverStartRideReq | DashboardReq DashboardStartRideReq

data DriverStartRideReq = DriverStartRideReq
  { rideOtp :: Text,
    point :: LatLong,
    requestor :: DP.Person
  }

data DashboardStartRideReq = DashboardStartRideReq
  { point :: Maybe LatLong,
    merchantId :: Id DM.Merchant
  }

buildStartRideHandle :: Id DM.Merchant -> Flow (ServiceHandle Flow)
buildStartRideHandle merchantId = do
  defaultRideInterpolationHandler <- LocUpd.buildRideInterpolationHandler merchantId False
  pure
    ServiceHandle
      { findBookingById = QRB.findById,
        findLocationByDriverId = QDrLoc.findById,
        startRideAndUpdateLocation = SInternal.startRideTransaction,
        notifyBAPRideStarted = sendRideStartedUpdateToBAP,
        rateLimitStartRide = \personId' rideId' -> checkSlidingWindowLimit (getId personId' <> "_" <> getId rideId'),
        initializeDistanceCalculation = LocUpd.initializeDistanceCalculation defaultRideInterpolationHandler
      }

driverStartRide ::
  ( EsqDBFlow m r,
    Redis.HedisFlow m r,
    CoreMetrics m,
    MonadFlow m
  ) =>
  ServiceHandle m ->
  Id SRide.Ride ->
  DriverStartRideReq ->
  m APISuccess.APISuccess
driverStartRide handle rideId = startRide handle rideId . DriverReq

dashboardStartRide ::
  ( EsqDBFlow m r,
    Redis.HedisFlow m r,
    CoreMetrics m,
    MonadFlow m
  ) =>
  ServiceHandle m ->
  Id SRide.Ride ->
  DashboardStartRideReq ->
  m APISuccess.APISuccess
dashboardStartRide handle rideId = startRide handle rideId . DashboardReq

startRide ::
  ( EsqDBFlow m r,
    Redis.HedisFlow m r,
    CoreMetrics m,
    MonadFlow m
  ) =>
  ServiceHandle m ->
  Id SRide.Ride ->
  StartRideReq ->
  m APISuccess.APISuccess
startRide handle@ServiceHandle {..} rideId req = do
  ride <- QRide.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  let driverId = ride.driverId
  LocUpd.whenWithLocationUpdatesLock driverId $ startRideHandler handle ride req
  pure APISuccess.Success

startRideHandler :: (MonadThrow m, Log m) => ServiceHandle m -> SRide.Ride -> StartRideReq -> m ()
startRideHandler ServiceHandle {..} ride req = do
  let driverId = ride.driverId
  rateLimitStartRide driverId ride.id -- do we need it for dashboard?
  booking <- findBookingById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)

  case req of
    DriverReq driverReq -> do
      let requestor = driverReq.requestor
      case requestor.role of
        DP.DRIVER -> unless (requestor.id == driverId) $ throwError NotAnExecutor
        _ -> throwError AccessDenied
    DashboardReq dashboardReq -> do
      unless (booking.providerId == dashboardReq.merchantId) $ throwError (RideDoesNotExist ride.id.getId)

  unless (isValidRideStatus (ride.status)) $ throwError $ RideInvalidStatus "This ride cannot be started"

  point <- case req of
    DriverReq driverReq -> do
      when (driverReq.rideOtp /= ride.otp) $ throwError IncorrectOTP
      logTagInfo "driver -> startRide : " ("DriverId " <> getId driverId <> ", RideId " <> getId ride.id)
      pure driverReq.point
    DashboardReq dashboardReq -> do
      logTagInfo "dashboard -> startRide : " ("DriverId " <> getId driverId <> ", RideId " <> getId ride.id)
      case dashboardReq.point of
        Just point -> pure point
        Nothing -> do
          driverLocation <- findLocationByDriverId driverId >>= fromMaybeM LocationNotFound
          pure $ getCoordinates driverLocation

  startRideAndUpdateLocation driverId ride.id booking.id point
  initializeDistanceCalculation ride.id driverId point
  notifyBAPRideStarted booking ride
  where
    isValidRideStatus status = status == SRide.NEW
