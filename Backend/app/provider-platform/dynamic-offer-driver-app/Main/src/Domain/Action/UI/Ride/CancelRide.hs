{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.Ride.CancelRide
  ( CancelRideReq (..),
    ServiceHandle (..),
    cancelRideHandle,
    driverCancelRideHandler,
    dashboardCancelRideHandler,
  )
where

import qualified Domain.Action.UI.Ride.CancelRide.Internal as CInternal
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.BookingCancellationReason as DBCR
import Domain.Types.CancellationReason (CancellationReasonCode (..))
import qualified Domain.Types.DriverLocation as DDriverLocation
import Domain.Types.Merchant
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import Environment
import qualified Kernel.Beam.Functions as B
import Kernel.External.Maps
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics.Types (CoreMetrics)
import qualified Kernel.Types.APISuccess as APISuccess
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.DriverLocation as QDrLoc
import qualified SharedLogic.External.LocationTrackingService.Flow as LF
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QRide
import Tools.Error
import qualified Tools.Maps as Maps

type MonadHandler m = (MonadThrow m, Log m, MonadGuid m)

data ServiceHandle m = ServiceHandle
  { findRideById :: Id DRide.Ride -> m (Maybe DRide.Ride),
    findById :: Id DP.Person -> m (Maybe DP.Person),
    findDriverLocationId :: Id Merchant -> Id DP.Person -> m (Maybe DDriverLocation.DriverLocation),
    cancelRide :: Id DRide.Ride -> DBCR.BookingCancellationReason -> m (),
    findBookingByIdInReplica :: Id SRB.Booking -> m (Maybe SRB.Booking),
    pickUpDistance :: Id DM.Merchant -> LatLong -> LatLong -> m Meters
  }

cancelRideHandle :: ServiceHandle Flow
cancelRideHandle =
  ServiceHandle
    { findRideById = QRide.findById,
      findById = QPerson.findById,
      cancelRide = CInternal.cancelRideImpl,
      findDriverLocationId = QDrLoc.findById,
      findBookingByIdInReplica = B.runInReplica . QRB.findById,
      pickUpDistance = driverDistanceToPickup
    }

data CancelRideReq = CancelRideReq
  { reasonCode :: CancellationReasonCode,
    additionalInfo :: Maybe Text
  }

data RequestorId = PersonRequestorId (Id DP.Person) | DashboardRequestorId (Id DM.Merchant)

driverCancelRideHandler :: (MonadHandler m, HasFlowEnv m r '["ltsCfg" ::: LT.LocationTrackingeServiceConfig], HasField "enableLocationTrackingService" r Bool, CoreMetrics m) => ServiceHandle m -> Id DP.Person -> Id DRide.Ride -> CancelRideReq -> m APISuccess.APISuccess
driverCancelRideHandler shandle personId rideId req =
  withLogTag ("rideId-" <> rideId.getId) $
    cancelRideHandler shandle (PersonRequestorId personId) rideId req

dashboardCancelRideHandler :: (MonadHandler m, HasFlowEnv m r '["ltsCfg" ::: LT.LocationTrackingeServiceConfig], HasField "enableLocationTrackingService" r Bool, CoreMetrics m) => ServiceHandle m -> Id DM.Merchant -> Id DRide.Ride -> CancelRideReq -> m APISuccess.APISuccess
dashboardCancelRideHandler shandle merchantId rideId req =
  withLogTag ("merchantId-" <> merchantId.getId) $
    cancelRideHandler shandle (DashboardRequestorId merchantId) rideId req

cancelRideHandler :: (MonadHandler m, HasFlowEnv m r '["ltsCfg" ::: LT.LocationTrackingeServiceConfig], HasField "enableLocationTrackingService" r Bool, CoreMetrics m) => ServiceHandle m -> RequestorId -> Id DRide.Ride -> CancelRideReq -> m APISuccess.APISuccess
cancelRideHandler ServiceHandle {..} requestorId rideId req = withLogTag ("rideId-" <> rideId.getId) do
  ride <- findRideById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  unless (isValidRide ride) $ throwError $ RideInvalidStatus "This ride cannot be canceled"
  let driverId = ride.driverId

  rideCancelationReason <- case requestorId of
    PersonRequestorId personId -> do
      authPerson <-
        findById personId
          >>= fromMaybeM (PersonNotFound personId.getId)
      driver <- findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
      case authPerson.role of
        DP.ADMIN -> do
          unless (authPerson.merchantId == driver.merchantId) $ throwError (RideDoesNotExist rideId.getId)
          logTagInfo "admin -> cancelRide : " ("DriverId " <> getId driverId <> ", RideId " <> getId ride.id)
          buildRideCancelationReason Nothing Nothing Nothing DBCR.ByMerchant ride (Just driver.merchantId)
        DP.DRIVER -> do
          unless (authPerson.id == driverId) $ throwError NotAnExecutor
          logTagInfo "driver -> cancelRide : " ("DriverId " <> getId driverId <> ", RideId " <> getId ride.id)
          enableLocationTrackingService <- asks (.enableLocationTrackingService)
          mbLocation <- do
            if enableLocationTrackingService
              then do
                ltsCfg <- asks (.ltsCfg)
                driverLocations <- LF.driversLocation ltsCfg [driverId]
                return $ LF.findByDriverId driverLocations driverId
              else findDriverLocationId driver.merchantId driverId
          booking <- findBookingByIdInReplica ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
          disToPickup <- forM mbLocation $ \location -> do
            pickUpDistance booking.providerId (getCoordinates location) (getCoordinates booking.fromLocation)
          let currentDriverLocation = getCoordinates <$> mbLocation
          buildRideCancelationReason currentDriverLocation disToPickup (Just driverId) DBCR.ByDriver ride (Just driver.merchantId)
    DashboardRequestorId reqMerchantId -> do
      driver <- findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
      unless (driver.merchantId == reqMerchantId) $ throwError (RideDoesNotExist rideId.getId)
      logTagInfo "dashboard -> cancelRide : " ("DriverId " <> getId driverId <> ", RideId " <> getId ride.id)
      buildRideCancelationReason Nothing Nothing Nothing DBCR.ByMerchant ride (Just driver.merchantId) -- is it correct DBCR.ByMerchant?
  cancelRide rideId rideCancelationReason
  pure APISuccess.Success
  where
    isValidRide ride =
      ride.status == DRide.NEW
    buildRideCancelationReason currentDriverLocation disToPickup mbDriverId source ride merchantId = do
      let CancelRideReq {..} = req
      return $
        DBCR.BookingCancellationReason
          { bookingId = ride.bookingId,
            rideId = Just ride.id,
            merchantId = merchantId,
            source = source,
            reasonCode = Just reasonCode,
            driverId = mbDriverId,
            driverCancellationLocation = currentDriverLocation,
            driverDistToPickup = disToPickup,
            ..
          }

driverDistanceToPickup ::
  ( EncFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    Maps.HasCoordinates tripStartPos,
    Maps.HasCoordinates tripEndPos
  ) =>
  Id Merchant ->
  tripStartPos ->
  tripEndPos ->
  m Meters
driverDistanceToPickup merchantId tripStartPos tripEndPos = do
  distRes <-
    Maps.getDistanceForCancelRide merchantId $
      Maps.GetDistanceReq
        { origin = tripStartPos,
          destination = tripEndPos,
          travelMode = Just Maps.CAR
        }
  return $ distRes.distance
