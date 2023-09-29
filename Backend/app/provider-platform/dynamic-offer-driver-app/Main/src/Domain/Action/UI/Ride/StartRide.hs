{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.Ride.StartRide
  ( ServiceHandle (..),
    DriverStartRideReq (..),
    DashboardStartRideReq (..),
    buildStartRideHandle,
    driverStartRide,
    dashboardStartRide,
    makeStartRideIdKey,
  )
where

import qualified Domain.Action.UI.Ride.StartRide.Internal as SInternal
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import Environment (Flow)
import EulerHS.Prelude
import Kernel.External.Maps.HasCoordinates
import Kernel.External.Maps.Types
import Kernel.Storage.Esqueleto.Config (EsqLocDBFlow)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Tools.Metrics.CoreMetrics
import qualified Kernel.Types.APISuccess as APISuccess
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.DatastoreLatencyCalculator
import Kernel.Utils.SlidingWindowLimiter (checkSlidingWindowLimit)
import qualified Lib.LocationUpdates as LocUpd
import SharedLogic.CallBAP (sendRideStartedUpdateToBAP)
import qualified SharedLogic.External.LocationTrackingService.Flow as LF
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import Storage.CachedQueries.Driver.GoHomeRequest as CQDGR
import Storage.CachedQueries.Merchant.TransporterConfig as QTC
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.DriverInformation as QDI
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
    startRideAndUpdateLocation :: Id DP.Person -> DRide.Ride -> Id SRB.Booking -> LatLong -> Id DM.Merchant -> m (),
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
        startRideAndUpdateLocation = SInternal.startRideTransaction,
        notifyBAPRideStarted = sendRideStartedUpdateToBAP,
        rateLimitStartRide = \personId' rideId' -> checkSlidingWindowLimit (getId personId' <> "_" <> getId rideId'),
        initializeDistanceCalculation = LocUpd.initializeDistanceCalculation defaultRideInterpolationHandler,
        whenWithLocationUpdatesLock = LocUpd.whenWithLocationUpdatesLock
      }

type StartRideFlow m r = (MonadThrow m, Log m, EsqLocDBFlow m r, CacheFlow m r, EsqDBFlow m r, MonadTime m, CoreMetrics m, MonadReader r m, HasField "enableAPILatencyLogging" r Bool, HasField "enableAPIPrometheusMetricLogging" r Bool, LT.HasLocationService m r)

driverStartRide ::
  (StartRideFlow m r) =>
  ServiceHandle m ->
  Id DRide.Ride ->
  DriverStartRideReq ->
  m APISuccess.APISuccess
driverStartRide handle rideId req =
  withLogTag ("requestorId-" <> req.requestor.id.getId)
    . startRide handle rideId
    $ DriverReq req

dashboardStartRide ::
  (StartRideFlow m r) =>
  ServiceHandle m ->
  Id DRide.Ride ->
  DashboardStartRideReq ->
  m APISuccess.APISuccess
dashboardStartRide handle rideId req =
  withLogTag ("merchantId-" <> req.merchantId.getId)
    . startRide handle rideId
    $ DashboardReq req

startRide ::
  (StartRideFlow m r) =>
  ServiceHandle m ->
  Id DRide.Ride ->
  StartRideReq ->
  m APISuccess.APISuccess
startRide ServiceHandle {..} rideId req = withLogTag ("rideId-" <> rideId.getId) $ do
  ride <- findRideById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  let driverId = ride.driverId
  let driverKey = makeStartRideIdKey driverId
  Redis.setExp driverKey ride.id 60
  rateLimitStartRide driverId ride.id -- do we need it for dashboard?
  booking <- findBookingById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
  driverInfo <- QDI.findById (cast driverId) >>= fromMaybeM (PersonNotFound driverId.getId)
  openMarketAllow <-
    maybe
      (pure False)
      ( \merchantId -> do
          transporterConfig <- QTC.findByMerchantId merchantId >>= fromMaybeM (TransporterConfigNotFound (getId merchantId))
          pure $ transporterConfig.openMarketUnBlocked
      )
      driverInfo.merchantId
  unless (driverInfo.subscribed || openMarketAllow) $ throwError DriverUnsubscribed
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
          driverLocation <- do
            driverLocations <- LF.driversLocation [driverId]
            listToMaybe driverLocations & fromMaybeM LocationNotFound
          pure $ getCoordinates driverLocation
  whenWithLocationUpdatesLock driverId $ do
    withTimeAPI "startRide" "startRideAndUpdateLocation" $ startRideAndUpdateLocation driverId ride booking.id point booking.providerId
    ltsRes <- LF.rideStart rideId point.lat point.lon booking.providerId driverId
    logTagInfo "ltsRes" (show ltsRes)
    withTimeAPI "startRide" "initializeDistanceCalculation" $ initializeDistanceCalculation ride.id driverId point
    withTimeAPI "startRide" "notifyBAPRideStarted" $ notifyBAPRideStarted booking ride
  CQDGR.setDriverGoHomeIsOnRide driverId booking.providerId
  pure APISuccess.Success
  where
    isValidRideStatus status = status == DRide.NEW

makeStartRideIdKey :: Id DP.Person -> Text
makeStartRideIdKey driverId = "StartRideKey:PersonId-" <> driverId.getId
