module Domain.Action.UI.Ride.StartRide
  ( ServiceHandle (..),
    DriverStartRideReq (..),
    DashboardStartRideReq (..),
    buildStartRideHandle,
    driverStartRide,
    dashboardStartRide,
  )
where

import Beckn.External.Maps.HasCoordinates
import Beckn.External.Maps.Types
import qualified Beckn.Types.APISuccess as APISuccess
import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Utils.Common
import Beckn.Utils.SlidingWindowLimiter (checkSlidingWindowLimit)
import qualified Domain.Action.UI.Ride.StartRide.Internal as SInternal
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.DriverLocation as DDrLoc
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import Environment (Flow)
import EulerHS.Prelude
import qualified Lib.LocationUpdates as LocUpd
import SharedLogic.CallBAP (sendRideStartedUpdateToBAP)
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.DriverLocation as QDrLoc
import qualified Storage.Queries.Ride as QRide
import Tools.Error

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

data ServiceHandle m = ServiceHandle
  { findRideById :: Id DRide.Ride -> m (Maybe DRide.Ride),
    findBookingById :: Id SRB.Booking -> m (Maybe SRB.Booking),
    findLocationByDriverId :: Id DP.Person -> m (Maybe DDrLoc.DriverLocation),
    startRideAndUpdateLocation :: Id DP.Person -> Id DRide.Ride -> Id SRB.Booking -> LatLong -> m (),
    notifyBAPRideStarted :: SRB.Booking -> DRide.Ride -> m (),
    rateLimitStartRide :: Id DP.Person -> Id DRide.Ride -> m (),
    initializeDistanceCalculation :: Id DRide.Ride -> Id DP.Person -> LatLong -> m (),
    whenWithLocationUpdatesLock :: Id DP.Person -> m () -> m ()
  }

buildStartRideHandle :: Id DM.Merchant -> Flow (ServiceHandle Flow)
buildStartRideHandle merchantId = do
  defaultRideInterpolationHandler <- LocUpd.buildRideInterpolationHandler merchantId False
  pure
    ServiceHandle
      { findRideById = QRide.findById,
        findBookingById = QRB.findById,
        findLocationByDriverId = QDrLoc.findById,
        startRideAndUpdateLocation = SInternal.startRideTransaction,
        notifyBAPRideStarted = sendRideStartedUpdateToBAP,
        rateLimitStartRide = \personId' rideId' -> checkSlidingWindowLimit (getId personId' <> "_" <> getId rideId'),
        initializeDistanceCalculation = LocUpd.initializeDistanceCalculation defaultRideInterpolationHandler,
        whenWithLocationUpdatesLock = LocUpd.whenWithLocationUpdatesLock
      }

driverStartRide ::
  (MonadThrow m, Log m) =>
  ServiceHandle m ->
  Id DRide.Ride ->
  DriverStartRideReq ->
  m APISuccess.APISuccess
driverStartRide handle rideId = startRide handle rideId . DriverReq

dashboardStartRide ::
  (MonadThrow m, Log m) =>
  ServiceHandle m ->
  Id DRide.Ride ->
  DashboardStartRideReq ->
  m APISuccess.APISuccess
dashboardStartRide handle rideId = startRide handle rideId . DashboardReq

startRide ::
  (MonadThrow m, Log m) =>
  ServiceHandle m ->
  Id DRide.Ride ->
  StartRideReq ->
  m APISuccess.APISuccess
startRide ServiceHandle {..} rideId req = do
  ride <- findRideById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
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

  whenWithLocationUpdatesLock driverId $ do
    startRideAndUpdateLocation driverId ride.id booking.id point
    initializeDistanceCalculation ride.id driverId point
    notifyBAPRideStarted booking ride
  pure APISuccess.Success
  where
    isValidRideStatus status = status == DRide.NEW
