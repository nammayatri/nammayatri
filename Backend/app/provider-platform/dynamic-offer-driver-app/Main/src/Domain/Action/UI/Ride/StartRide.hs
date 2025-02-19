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

import Data.Maybe (listToMaybe)
import qualified Data.Text as Text
import qualified Domain.Action.UI.Ride.StartRide.Internal as SInternal
import qualified Domain.Types as DTC
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.RideRelatedNotificationConfig as DRN
import Environment (Flow)
import EulerHS.Prelude
import Kernel.External.Maps.HasCoordinates
import Kernel.External.Maps.Types
import Kernel.External.Types (SchedulerFlow, ServiceFlow)
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
import SharedLogic.Ride (calculateEstimatedEndTimeRange)
import qualified SharedLogic.ScheduledNotifications as SN
import Storage.Cac.TransporterConfig as SCTC
import qualified Storage.CachedQueries.RideRelatedNotificationConfig as CRN
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.Ride as QRide
import Tools.Error
import qualified Tools.Notifications as Notify
import Utils.Common.Cac.KeyNameConstants

data StartRideReq = DriverReq DriverStartRideReq | DashboardReq DashboardStartRideReq

data DriverStartRideReq = DriverStartRideReq
  { rideOtp :: Text,
    point :: LatLong,
    requestor :: DP.Person,
    odometer :: Maybe DRide.OdometerReading
  }

data DashboardStartRideReq = DashboardStartRideReq
  { point :: Maybe LatLong,
    merchantId :: Id DM.Merchant,
    merchantOperatingCityId :: Id DMOC.MerchantOperatingCity,
    odometer :: Maybe DRide.OdometerReading
  }

data ServiceHandle m = ServiceHandle
  { findRideById :: Id DRide.Ride -> m (Maybe DRide.Ride),
    findBookingById :: Id SRB.Booking -> m (Maybe SRB.Booking),
    startRideAndUpdateLocation :: Id DP.Person -> DRide.Ride -> Id SRB.Booking -> LatLong -> Id DM.Merchant -> Maybe DRide.OdometerReading -> m (),
    notifyBAPRideStarted :: SRB.Booking -> DRide.Ride -> Maybe LatLong -> m (),
    rateLimitStartRide :: Id DP.Person -> Id DRide.Ride -> m (),
    initializeDistanceCalculation :: Id DRide.Ride -> Id DP.Person -> LatLong -> m (),
    whenWithLocationUpdatesLock :: Id DP.Person -> m () -> m ()
  }

buildStartRideHandle :: Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Flow (ServiceHandle Flow)
buildStartRideHandle merchantId merchantOpCityId = do
  defaultRideInterpolationHandler <- LocUpd.buildRideInterpolationHandler merchantId merchantOpCityId False
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

type StartRideFlow m r = (MonadThrow m, Log m, CacheFlow m r, EsqDBFlow m r, MonadTime m, CoreMetrics m, MonadReader r m, HasField "enableAPILatencyLogging" r Bool, HasField "enableAPIPrometheusMetricLogging" r Bool, LT.HasLocationService m r, ServiceFlow m r, HasFlowEnv m r '["maxNotificationShards" ::: Int])

driverStartRide ::
  (StartRideFlow m r, SchedulerFlow r) =>
  ServiceHandle m ->
  Id DRide.Ride ->
  DriverStartRideReq ->
  m APISuccess.APISuccess
driverStartRide handle rideId req =
  withLogTag ("requestorId-" <> req.requestor.id.getId)
    . startRide handle rideId
    $ DriverReq req

dashboardStartRide ::
  (StartRideFlow m r, SchedulerFlow r) =>
  ServiceHandle m ->
  Id DRide.Ride ->
  DashboardStartRideReq ->
  m APISuccess.APISuccess
dashboardStartRide handle rideId req =
  withLogTag ("merchantId-" <> req.merchantId.getId)
    . startRide handle rideId
    $ DashboardReq req

startRide ::
  (StartRideFlow m r, SchedulerFlow r) =>
  ServiceHandle m ->
  Id DRide.Ride ->
  StartRideReq ->
  m APISuccess.APISuccess
startRide ServiceHandle {..} rideId req = withLogTag ("rideId-" <> rideId.getId) $ do
  ride <- findRideById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  let driverId = ride.driverId
  driverInfo <- QDI.findById (cast driverId) >>= fromMaybeM (PersonNotFound driverId.getId)
  booking <- findBookingById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
  transporterConfig <- SCTC.findByMerchantOpCityId ride.merchantOperatingCityId (Just (TransactionId (Id booking.transactionId))) >>= fromMaybeM (TransporterConfigNotFound (getId ride.merchantOperatingCityId))
  (openMarketAllow, includeDriverCurrentlyOnRide) <-
    maybe
      (pure (False, False))
      ( \_ -> do
          pure (transporterConfig.openMarketUnBlocked, transporterConfig.includeDriverCurrentlyOnRide)
      )
      driverInfo.merchantId
  when (includeDriverCurrentlyOnRide && driverInfo.hasAdvanceBooking) do throwError $ CurrentRideInprogress driverId.getId
  let driverKey = makeStartRideIdKey driverId
  Redis.setExp driverKey ride.id 60
  rateLimitStartRide driverId ride.id -- do we need it for dashboard?
  unless (driverInfo.subscribed || openMarketAllow) $ throwError DriverUnsubscribed
  case req of
    DriverReq driverReq -> do
      let requestor = driverReq.requestor
      case requestor.role of
        DP.DRIVER -> unless (requestor.id == driverId) $ throwError NotAnExecutor
        _ -> throwError AccessDenied
    DashboardReq dashboardReq -> do
      unless (booking.providerId == dashboardReq.merchantId && booking.merchantOperatingCityId == dashboardReq.merchantOperatingCityId) $ throwError (RideDoesNotExist ride.id.getId)

  unless (isValidRideStatus (ride.status)) $ throwError $ RideInvalidStatus ("This ride cannot be started" <> Text.pack (show ride.status))

  (point, odometer) <- case req of
    DriverReq driverReq -> do
      when (DTC.isOdometerReadingsRequired booking.tripCategory && isNothing driverReq.odometer) $ throwError $ OdometerReadingRequired (show booking.tripCategory)
      when (not (fromMaybe False ride.enableOtpLessRide) && driverReq.rideOtp /= ride.otp) $ throwError IncorrectOTP
      logTagInfo "driver -> startRide : " ("DriverId " <> getId driverId <> ", RideId " <> getId ride.id)
      pure (driverReq.point, driverReq.odometer)
    DashboardReq dashboardReq -> do
      when (DTC.isOdometerReadingsRequired booking.tripCategory && isNothing dashboardReq.odometer) $ throwError $ OdometerReadingRequired (show booking.tripCategory)
      logTagInfo "dashboard -> startRide : " ("DriverId " <> getId driverId <> ", RideId " <> getId ride.id)
      case dashboardReq.point of
        Just point -> pure (point, dashboardReq.odometer)
        Nothing -> do
          driverLocation <- do
            driverLocations <- LF.driversLocation [driverId]
            listToMaybe driverLocations & fromMaybeM LocationNotFound
          pure (getCoordinates driverLocation, dashboardReq.odometer)
  now <- getCurrentTime
  -- create first entry of eta here
  let estimatedEndTimeRange = booking.estimatedDuration >>= \estDuration -> calculateEstimatedEndTimeRange now estDuration transporterConfig.arrivalTimeBufferOfVehicle booking.vehicleServiceTier
  when (isJust estimatedEndTimeRange) $ QRide.updateEstimatedEndTimeRange estimatedEndTimeRange ride.id
  updatedRide <-
    if DTC.isEndOtpRequired booking.tripCategory
      then do
        endOtp <- Just <$> generateOTPCode
        QRide.updateEndRideOtp ride.id endOtp
        return $ ride {DRide.endOtp = endOtp, DRide.startOdometerReading = odometer, DRide.tripStartTime = Just now, DRide.estimatedEndTimeRange = estimatedEndTimeRange}
      else pure ride {DRide.tripStartTime = Just now, DRide.estimatedEndTimeRange = estimatedEndTimeRange}

  whenWithLocationUpdatesLock driverId $ do
    withTimeAPI "startRide" "startRideAndUpdateLocation" $ startRideAndUpdateLocation driverId updatedRide booking.id point booking.providerId odometer
    withTimeAPI "startRide" "initializeDistanceCalculation" $ initializeDistanceCalculation updatedRide.id driverId point

  fork "notify customer for ride start" $ notifyBAPRideStarted booking updatedRide (Just point)
  fork "startRide - Notify driver" $ Notify.notifyOnRideStarted ride booking

  rideRelatedNotificationConfigList <- CRN.findAllByMerchantOperatingCityIdAndTimeDiffEventInRideFlow booking.merchantOperatingCityId DRN.START_TIME booking.configInExperimentVersions
  forM_ rideRelatedNotificationConfigList (SN.pushReminderUpdatesInScheduler booking updatedRide now driverId)

  pure APISuccess.Success
  where
    isValidRideStatus status = status == DRide.NEW

makeStartRideIdKey :: Id DP.Person -> Text
makeStartRideIdKey driverId = "StartRideKey:PersonId-" <> driverId.getId
