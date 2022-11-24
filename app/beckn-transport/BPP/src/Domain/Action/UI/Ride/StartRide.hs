module Domain.Action.UI.Ride.StartRide
  ( ServiceHandle (..),
    StartRideReq (..),
    buildStartRideHandle,
    startRideHandler,
    startRide,
  )
where

import Beckn.External.Maps.Types
import Beckn.Prelude (ToSchema)
import qualified Beckn.Storage.Hedis as Redis
import Beckn.Tools.Metrics.CoreMetrics
import qualified Beckn.Types.APISuccess as APISuccess
import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Utils.Common
import Beckn.Utils.SlidingWindowLimiter
import qualified Domain.Action.UI.Ride.StartRide.Internal as SInternal
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.Person as Person
import qualified Domain.Types.Ride as SRide
import Environment (Flow)
import EulerHS.Prelude
import qualified Lib.LocationUpdates as LocUpd
import SharedLogic.CallBAP (sendRideStartedUpdateToBAP)
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Ride as QRide
import Tools.Error

data ServiceHandle m = ServiceHandle
  { requestor :: Person.Person,
    findBookingById :: Id SRB.Booking -> m (Maybe SRB.Booking),
    startRideAndUpdateLocation :: Id SRide.Ride -> Id SRB.Booking -> LatLong -> m (),
    notifyBAPRideStarted :: SRB.Booking -> SRide.Ride -> m (),
    rateLimitStartRide :: Id SRide.Ride -> m (),
    initializeDistanceCalculation :: LatLong -> m ()
  }

data StartRideReq = StartRideReq
  { rideOtp :: Text,
    point :: LatLong
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

buildStartRideHandle ::
  Id Person.Person ->
  Id SRide.Ride ->
  Flow (ServiceHandle Flow)
buildStartRideHandle requestorId rideId = do
  requestor <-
    QP.findById requestorId
      >>= fromMaybeM (PersonNotFound requestorId.getId)
  orgId <- requestor.merchantId & fromMaybeM (PersonFieldNotPresent "merchantId")
  defaultRideInterpolationHandler <- LocUpd.buildRideInterpolationHandler orgId False
  return $
    ServiceHandle
      { requestor,
        findBookingById = QRB.findById,
        startRideAndUpdateLocation = SInternal.startRideTransaction requestorId,
        notifyBAPRideStarted = sendRideStartedUpdateToBAP,
        rateLimitStartRide = \rideId' -> checkSlidingWindowLimit (getId requestorId <> "_" <> getId rideId'),
        initializeDistanceCalculation = LocUpd.initializeDistanceCalculation defaultRideInterpolationHandler rideId requestorId
      }

startRide :: (EsqDBFlow m r, MonadThrow m, Log m, Redis.HedisFlow m r, CoreMetrics m, MonadFlow m, MonadTime m) => ServiceHandle m -> Id SRide.Ride -> StartRideReq -> m APISuccess.APISuccess
startRide handle@ServiceHandle {..} rideId req = do
  ride <- QRide.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  let driverId = ride.driverId
  LocUpd.whenWithLocationUpdatesLock driverId $ startRideHandler handle ride req
  pure APISuccess.Success

startRideHandler :: (MonadThrow m, Log m) => ServiceHandle m -> SRide.Ride -> StartRideReq -> m ()
startRideHandler ServiceHandle {..} ride req = do
  rateLimitStartRide ride.id
  let driverId = ride.driverId
  case requestor.role of
    Person.DRIVER -> do
      unless (driverId == requestor.id) $ throwError NotAnExecutor
    _ -> throwError AccessDenied
  unless (isValidRideStatus (ride.status)) $ throwError $ RideInvalidStatus "This ride cannot be started"
  booking <- findBookingById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
  let inAppOtp = ride.otp
  when (req.rideOtp /= inAppOtp) $ throwError IncorrectOTP
  logTagInfo "startRide" ("DriverId " <> getId driverId <> ", RideId " <> getId ride.id)
  startRideAndUpdateLocation ride.id booking.id req.point
  initializeDistanceCalculation req.point
  notifyBAPRideStarted booking ride
  where
    isValidRideStatus status = status == SRide.NEW
