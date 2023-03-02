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
  )
where

import qualified Domain.Action.UI.Ride.StartRide.Internal as SInternal
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.DriverLocation as DDrLoc
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import Environment (Flow)
import EulerHS.Prelude
import Kernel.External.Maps.HasCoordinates
import Kernel.External.Maps.Types
import qualified Kernel.Types.APISuccess as APISuccess
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.SlidingWindowLimiter (checkSlidingWindowLimit)
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
      { findRideById = flip QRide.findById (Proxy @Flow),
        findBookingById = flip QRB.findById (Proxy @Flow),
        findLocationByDriverId = flip QDrLoc.findById (Proxy @Flow),
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
