module Domain.Action.UI.Ride.StartRide
  ( StartRideReq (..),
    ServiceHandle (..),
    buildStartRideHandle,
    startRideHandler,
  )
where

import Beckn.External.Maps.Types
import qualified Beckn.Storage.Hedis as Redis
import Beckn.Tools.Metrics.CoreMetrics
import qualified Beckn.Types.APISuccess as APISuccess
import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Utils.Common
import Beckn.Utils.SlidingWindowLimiter (checkSlidingWindowLimit)
import Data.OpenApi
import qualified Domain.Action.UI.Ride.StartRide.Internal as RideStart
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

data StartRideReq = StartRideReq
  { rideOtp :: Text,
    point :: LatLong
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data ServiceHandle m = ServiceHandle
  { requestor :: Person.Person,
    findBookingById :: Id SRB.Booking -> m (Maybe SRB.Booking),
    findRideById :: Id SRide.Ride -> m (Maybe SRide.Ride),
    startRideAndUpdateLocation :: Id SRide.Ride -> Id SRB.Booking -> LatLong -> m (),
    notifyBAPRideStarted :: SRB.Booking -> SRide.Ride -> m (),
    rateLimitStartRide :: m (),
    initializeDistanceCalculation :: LatLong -> m ()
  }

buildStartRideHandle ::
  Id Person.Person ->
  Id SRide.Ride ->
  Flow (ServiceHandle Flow)
buildStartRideHandle requestorId rideId = do
  requestor <-
    QP.findById requestorId
      >>= fromMaybeM (PersonNotFound requestorId.getId)
  orgId <- requestor.merchantId & fromMaybeM (PersonFieldNotPresent "merchantId")
  defaultRideInterpolationHandler <- LocUpd.buildRideInterpolationHandler orgId
  return $
    ServiceHandle
      { requestor,
        findBookingById = QRB.findById,
        findRideById = QRide.findById,
        startRideAndUpdateLocation = RideStart.startRideTransaction (cast requestorId),
        notifyBAPRideStarted = sendRideStartedUpdateToBAP,
        rateLimitStartRide = checkSlidingWindowLimit (getId requestorId <> "_" <> getId rideId),
        initializeDistanceCalculation = LocUpd.initializeDistanceCalculation defaultRideInterpolationHandler rideId requestorId
      }

startRideHandler :: (MonadThrow m, Log m, Redis.HedisFlow m r, CoreMetrics m, MonadFlow m, MonadTime m) => ServiceHandle m -> Id SRide.Ride -> StartRideReq -> m APISuccess.APISuccess
startRideHandler ServiceHandle {..} rideId req = do
  rateLimitStartRide
  ride <- findRideById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  let driverId = ride.driverId
  let inAppOtp = ride.otp
  when (req.rideOtp /= inAppOtp) $ throwError IncorrectOTP
  logTagInfo "startRide" ("DriverId " <> getId requestor.id <> ", RideId " <> getId rideId)
  case requestor.role of
    Person.DRIVER -> do
      let rideDriver = ride.driverId
      unless (rideDriver == requestor.id) $ throwError NotAnExecutor
    _ -> throwError AccessDenied
  unless (isValidRideStatus (ride.status)) $ throwError $ RideInvalidStatus "This ride cannot be started"
  booking <- findBookingById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
  redisLockDriverId <- Redis.tryLockRedis (lockKey driverId) 60
  if redisLockDriverId
    then do
      logDebug $ "DriverId: " <> show driverId <> " Locked"
      startRideAndUpdateLocation ride.id booking.id req.point
      initializeDistanceCalculation req.point
      notifyBAPRideStarted booking ride
      Redis.unlockRedis (lockKey driverId)
      logDebug $ "DriverId: " <> show driverId <> " Unlocked"
    else logDebug $ "DriverId: " <> getId driverId <> " unable to get lock"
  pure APISuccess.Success
  where
    isValidRideStatus status = status == SRide.NEW
    lockKey driverId = LocUpd.makeLockKey driverId
