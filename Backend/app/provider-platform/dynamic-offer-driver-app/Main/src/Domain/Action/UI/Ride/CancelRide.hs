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
    CancelRideResp (..),
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
import qualified Domain.Types.Driver.GoHomeFeature.DriverGoHomeRequest as DDGR
import Domain.Types.Merchant
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
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
import qualified Lib.DriverCoins.Coins as DC
import qualified Lib.DriverCoins.Types as DCT
import qualified SharedLogic.External.LocationTrackingService.Flow as LF
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import qualified Storage.CachedQueries.Driver.GoHomeRequest as CQDGR
import qualified Storage.CachedQueries.GoHomeConfig as CQGHC
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.Driver.GoHomeFeature.DriverGoHomeRequest as QDGR
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QRide
import Tools.Error
import qualified Tools.Maps as Maps

type MonadHandler m = (MonadThrow m, Log m, MonadGuid m)

data ServiceHandle m = ServiceHandle
  { findRideById :: Id DRide.Ride -> m (Maybe DRide.Ride),
    findById :: Id DP.Person -> m (Maybe DP.Person),
    cancelRide :: Id DRide.Ride -> DBCR.BookingCancellationReason -> m (),
    findBookingByIdInReplica :: Id SRB.Booking -> m (Maybe SRB.Booking),
    pickUpDistance :: Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> LatLong -> LatLong -> m Meters
  }

cancelRideHandle :: ServiceHandle Flow
cancelRideHandle =
  ServiceHandle
    { findRideById = QRide.findById,
      findById = QPerson.findById,
      cancelRide = CInternal.cancelRideImpl,
      findBookingByIdInReplica = B.runInReplica . QRB.findById,
      pickUpDistance = driverDistanceToPickup
    }

data CancelRideReq = CancelRideReq
  { reasonCode :: CancellationReasonCode,
    additionalInfo :: Maybe Text
  }

data CancelRideResp = CancelRideResp
  { result :: Text,
    goHomeCancellationCount :: Maybe Int,
    isGoHomeDisabled :: Maybe Bool
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data RequestorId = PersonRequestorId (Id DP.Person) | DashboardRequestorId (Id DM.Merchant)

driverCancelRideHandler ::
  ( MonadHandler m,
    CacheFlow m r,
    MonadFlow m,
    EsqDBFlow m r,
    LT.HasLocationService m r,
    CoreMetrics m,
    HasField
      "minTripDistanceForReferralCfg"
      r
      (Maybe HighPrecMeters)
  ) =>
  ServiceHandle m ->
  Id DP.Person ->
  Id DRide.Ride ->
  CancelRideReq ->
  m CancelRideResp
driverCancelRideHandler shandle personId rideId req =
  withLogTag ("rideId-" <> rideId.getId) $
    cancelRideHandler shandle (PersonRequestorId personId) rideId req

dashboardCancelRideHandler ::
  ( MonadHandler m,
    CacheFlow m r,
    MonadFlow m,
    EsqDBFlow m r,
    LT.HasLocationService m r,
    CoreMetrics m,
    HasField
      "minTripDistanceForReferralCfg"
      r
      (Maybe HighPrecMeters)
  ) =>
  ServiceHandle m ->
  Id DM.Merchant ->
  Id DRide.Ride ->
  CancelRideReq ->
  m APISuccess.APISuccess
dashboardCancelRideHandler shandle merchantId rideId req =
  withLogTag ("merchantId-" <> merchantId.getId) $ do
    void $ cancelRideImpl shandle (DashboardRequestorId merchantId) rideId req
    return APISuccess.Success

cancelRideHandler ::
  ( MonadHandler m,
    CacheFlow m r,
    MonadFlow m,
    EsqDBFlow m r,
    LT.HasLocationService m r,
    CoreMetrics m,
    HasField
      "minTripDistanceForReferralCfg"
      r
      (Maybe HighPrecMeters)
  ) =>
  ServiceHandle m ->
  RequestorId ->
  Id DRide.Ride ->
  CancelRideReq ->
  m CancelRideResp
cancelRideHandler sh requestorId rideId req = withLogTag ("rideId-" <> rideId.getId) do
  (cancellationCnt, isGoToDisabled) <- cancelRideImpl sh requestorId rideId req
  pure $ buildCancelRideResp cancellationCnt isGoToDisabled
  where
    buildCancelRideResp cancelCnt isGoToDisabled =
      CancelRideResp
        { result = "success",
          goHomeCancellationCount = cancelCnt,
          isGoHomeDisabled = isGoToDisabled
        }

cancelRideImpl ::
  ( MonadHandler m,
    CacheFlow m r,
    MonadFlow m,
    EsqDBFlow m r,
    LT.HasLocationService m r,
    CoreMetrics m,
    HasField
      "minTripDistanceForReferralCfg"
      r
      (Maybe HighPrecMeters)
  ) =>
  ServiceHandle m ->
  RequestorId ->
  Id DRide.Ride ->
  CancelRideReq ->
  m (Maybe Int, Maybe Bool)
cancelRideImpl ServiceHandle {..} requestorId rideId req = do
  ride <- findRideById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  unless (isValidRide ride) $ throwError $ RideInvalidStatus "This ride cannot be canceled"
  let driverId = ride.driverId
  booking <- findBookingByIdInReplica ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
  driver <- findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  (rideCancelationReason, cancellationCnt, isGoToDisabled) <- case requestorId of
    PersonRequestorId personId -> do
      authPerson <-
        findById personId
          >>= fromMaybeM (PersonNotFound personId.getId)
      (rideCancellationReason, mbCancellationCnt, isGoToDisabled) <- case authPerson.role of
        DP.ADMIN -> do
          unless (authPerson.merchantId == driver.merchantId) $ throwError (RideDoesNotExist rideId.getId)
          logTagInfo "admin -> cancelRide : " ("DriverId " <> getId driverId <> ", RideId " <> getId ride.id)
          buildRideCancelationReason Nothing Nothing Nothing DBCR.ByMerchant ride (Just driver.merchantId) >>= \res -> return (res, Nothing, Nothing)
        DP.DRIVER -> do
          unless (authPerson.id == driverId) $ throwError NotAnExecutor
          goHomeConfig <- CQGHC.findByMerchantOpCityId booking.merchantOperatingCityId
          dghInfo <- CQDGR.getDriverGoHomeRequestInfo driverId booking.merchantOperatingCityId (Just goHomeConfig)
          (cancellationCount, isGoToDisabled) <-
            if dghInfo.status == Just DDGR.ACTIVE
              then do
                dghReqId <- fromMaybeM (InternalError "Status active but goHomeRequestId not found") dghInfo.driverGoHomeRequestId
                driverGoHomeReq <- QDGR.findById dghReqId >>= fromMaybeM (InternalError "DriverGoHomeRequestId present but DriverGoHome Request Entry not found")
                let cancelCnt = driverGoHomeReq.numCancellation + 1
                QDGR.updateCancellationCount driverGoHomeReq.id cancelCnt
                when (cancelCnt == goHomeConfig.cancellationCnt) $
                  CQDGR.deactivateDriverGoHomeRequest booking.merchantOperatingCityId driverId DDGR.SUCCESS dghInfo (Just False)
                return (Just cancelCnt, Just $ cancelCnt == goHomeConfig.cancellationCnt)
              else do
                return (Nothing, Nothing)
          logTagInfo "driver -> cancelRide : " ("DriverId " <> getId driverId <> ", RideId " <> getId ride.id)
          mbLocation <- do
            driverLocations <- LF.driversLocation [driverId]
            return $ listToMaybe driverLocations
          disToPickup <- forM mbLocation $ \location -> do
            pickUpDistance booking.providerId booking.merchantOperatingCityId (getCoordinates location) (getCoordinates booking.fromLocation)
          let currentDriverLocation = getCoordinates <$> mbLocation
          logDebug "RideCancelled Coin Event"
          fork "DriverRideCancelledCoin Event : " $ DC.driverCoinsEvent driverId driver.merchantId booking.merchantOperatingCityId (DCT.Cancellation ride.createdAt booking.distanceToPickup disToPickup)
          buildRideCancelationReason currentDriverLocation disToPickup (Just driverId) DBCR.ByDriver ride (Just driver.merchantId) >>= \res -> return (res, cancellationCount, isGoToDisabled)
      return (rideCancellationReason, mbCancellationCnt, isGoToDisabled)
    DashboardRequestorId reqMerchantId -> do
      unless (driver.merchantId == reqMerchantId) $ throwError (RideDoesNotExist rideId.getId)
      logTagInfo "dashboard -> cancelRide : " ("DriverId " <> getId driverId <> ", RideId " <> getId ride.id)
      buildRideCancelationReason Nothing Nothing Nothing DBCR.ByMerchant ride (Just driver.merchantId) >>= \res -> return (res, Nothing, Nothing) -- is it correct DBCR.ByMerchant?
  cancelRide rideId rideCancelationReason
  pure (cancellationCnt, isGoToDisabled)
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
  Id DMOC.MerchantOperatingCity ->
  tripStartPos ->
  tripEndPos ->
  m Meters
driverDistanceToPickup merchantId merchantOpCityId tripStartPos tripEndPos = do
  distRes <-
    Maps.getDistanceForCancelRide merchantId merchantOpCityId $
      Maps.GetDistanceReq
        { origin = tripStartPos,
          destination = tripEndPos,
          travelMode = Just Maps.CAR
        }
  return $ distRes.distance
