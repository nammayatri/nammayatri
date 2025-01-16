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
    RequestorId (..),
    cancelRideHandle,
    driverCancelRideHandler,
    dashboardCancelRideHandler,
    driverDistanceToPickup,
    cancelRideImpl,
  )
where

import qualified Data.HashMap.Strict as HM
import qualified Data.HashMap.Strict as HMS
import qualified Data.Map as M
import qualified Data.Text as Text
import qualified Domain.Action.UI.Ride.CancelRide.Internal as CInternal
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.BookingCancellationReason as DBCR
import Domain.Types.CancellationReason (CancellationReasonCode (..))
import qualified Domain.Types.DriverGoHomeRequest as DDGR
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import Environment
import qualified Kernel.Beam.Functions as B
import Kernel.External.Maps
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types (KafkaProducerTools)
import qualified Kernel.Types.APISuccess as APISuccess
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.DriverCoins.Coins as DC
import qualified Lib.DriverCoins.Types as DCT
import Lib.Scheduler (SchedulerType)
import Lib.SessionizerMetrics.Types.Event
import qualified SharedLogic.External.LocationTrackingService.Flow as LF
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import SharedLogic.GoogleTranslate (TranslateFlow)
import qualified Storage.Cac.GoHomeConfig as CGHC
import qualified Storage.CachedQueries.Driver.GoHomeRequest as CQDGR
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.DriverGoHomeRequest as QDGR
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QRide
import Tools.Error
import qualified Tools.Maps as Maps
import qualified Tools.Metrics as Metrics
import TransactionLogs.Types
import Utils.Common.Cac.KeyNameConstants

data ServiceHandle m = ServiceHandle
  { findRideById :: Id DRide.Ride -> m (Maybe DRide.Ride),
    findById :: Id DP.Person -> m (Maybe DP.Person),
    cancelRide :: Id DRide.Ride -> DRide.RideEndedBy -> DBCR.BookingCancellationReason -> Bool -> Maybe Bool -> m (),
    findBookingByIdInReplica :: Id SRB.Booking -> m (Maybe SRB.Booking),
    pickUpDistance :: SRB.Booking -> LatLong -> LatLong -> m Meters
  }

cancelRideHandle ::
  ( MonadFlow m,
    EncFlow m r,
    EsqDBReplicaFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    HasField "searchRequestExpirationSeconds" r NominalDiffTime,
    HasField "jobInfoMap" r (M.Map Text Bool),
    HasField "maxShards" r Int,
    HasField "schedulerSetName" r Text,
    HasField "schedulerType" r SchedulerType,
    Metrics.HasSendSearchRequestToDriverMetrics m r,
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    HasHttpClientOptions r c,
    HasLongDurationRetryCfg r c,
    HasField "singleBatchProcessingTempDelay" r NominalDiffTime,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasFlowEnv m r '["ondcTokenHashMap" ::: HMS.HashMap KeyConfig TokenConfig],
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    TranslateFlow m r,
    LT.HasLocationService m r,
    HasFlowEnv m r '["maxNotificationShards" ::: Int],
    HasShortDurationRetryCfg r c,
    Redis.HedisFlow m r,
    EventStreamFlow m r,
    MonadFlow m,
    Metrics.HasCoreMetrics r
  ) =>
  ServiceHandle m
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
    additionalInfo :: Maybe Text,
    doCancellationRateBasedBlocking :: Maybe Bool
  }

data CancelRideResp = CancelRideResp
  { result :: Text,
    goHomeCancellationCount :: Maybe Int,
    isGoHomeDisabled :: Maybe Bool
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data RequestorId = PersonRequestorId (Id DP.Person) | DashboardRequestorId (Id DM.Merchant, Id DMOC.MerchantOperatingCity) | ApplicationRequestorId Text | MerchantRequestorId (Id DM.Merchant, Id DMOC.MerchantOperatingCity)

driverCancelRideHandler ::
  ServiceHandle Flow ->
  Id DP.Person ->
  Id DRide.Ride ->
  CancelRideReq ->
  Flow CancelRideResp
driverCancelRideHandler shandle personId rideId req =
  withLogTag ("rideId-" <> rideId.getId) $
    cancelRideHandler shandle (PersonRequestorId personId) rideId req

dashboardCancelRideHandler ::
  ServiceHandle Flow ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Id DRide.Ride ->
  CancelRideReq ->
  Flow APISuccess.APISuccess
dashboardCancelRideHandler shandle merchantId merchantOpCityId rideId req =
  withLogTag ("merchantId-" <> merchantId.getId) $ do
    void $ cancelRideImpl shandle (DashboardRequestorId (merchantId, merchantOpCityId)) rideId req False
    return APISuccess.Success

cancelRideHandler ::
  ServiceHandle Flow ->
  RequestorId ->
  Id DRide.Ride ->
  CancelRideReq ->
  Flow CancelRideResp
cancelRideHandler sh requestorId rideId req = withLogTag ("rideId-" <> rideId.getId) do
  (cancellationCnt, isGoToDisabled) <- cancelRideImpl sh requestorId rideId req False
  pure $ buildCancelRideResp cancellationCnt isGoToDisabled
  where
    buildCancelRideResp cancelCnt isGoToDisabled =
      CancelRideResp
        { result = "success",
          goHomeCancellationCount = cancelCnt,
          isGoHomeDisabled = isGoToDisabled
        }

cancelRideImpl ::
  ( EsqDBFlow m r,
    CacheFlow m r,
    EsqDBReplicaFlow m r,
    LT.HasLocationService m r,
    DC.EventFlow m r
  ) =>
  ServiceHandle m ->
  RequestorId ->
  Id DRide.Ride ->
  CancelRideReq ->
  Bool ->
  m (Maybe Int, Maybe Bool)
cancelRideImpl ServiceHandle {..} requestorId rideId req isForceReallocation = do
  ride <- findRideById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  unless (isValidRide ride) $ throwError $ RideInvalidStatus ("This ride cannot be canceled" <> Text.pack (show ride.status))
  let driverId = ride.driverId
  booking <- findBookingByIdInReplica ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
  driver <- findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  (rideCancelationReason, cancellationCnt, isGoToDisabled, rideEndedBy) <- case requestorId of
    PersonRequestorId personId -> do
      authPerson <-
        findById personId
          >>= fromMaybeM (PersonNotFound personId.getId)
      (rideCancellationReason, mbCancellationCnt, isGoToDisabled, rideEndedBy) <- case authPerson.role of
        DP.ADMIN -> do
          unless (authPerson.merchantId == driver.merchantId) $ throwError (RideDoesNotExist rideId.getId)
          logTagInfo "admin -> cancelRide : " ("DriverId " <> getId driverId <> ", RideId " <> getId ride.id)
          buildRideCancelationReason Nothing Nothing Nothing DBCR.ByMerchant ride (Just driver.merchantId) >>= \res -> return (res, Nothing, Nothing, DRide.CallBased)
        _ -> do
          unless (authPerson.id == driverId) $ throwError NotAnExecutor
          goHomeConfig <- CGHC.findByMerchantOpCityId booking.merchantOperatingCityId (Just (TransactionId (Id booking.transactionId)))
          dghInfo <- CQDGR.getDriverGoHomeRequestInfo driverId booking.merchantOperatingCityId (Just goHomeConfig)
          (cancellationCount, isGoToDisabled) <-
            if dghInfo.status == Just DDGR.ACTIVE
              then do
                dghReqId <- fromMaybeM (InternalError "Status active but goHomeRequestId not found") dghInfo.driverGoHomeRequestId
                driverGoHomeReq <- QDGR.findById dghReqId >>= fromMaybeM (InternalError "DriverGoHomeRequestId present but DriverGoHome Request Entry not found")
                let cancelCnt = driverGoHomeReq.numCancellation + 1
                QDGR.updateCancellationCount cancelCnt driverGoHomeReq.id
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
            pickUpDistance booking (getCoordinates location) (getCoordinates booking.fromLocation)
          -- Temporary for debug issue with huge values
          let disToPickupThreshold = 1000000 --1000km can be max valid distance
          updatedDisToPickup :: Maybe Meters <- case disToPickup of
            Just dis -> if abs (toInteger dis) > disToPickupThreshold then logWarning ("Invalid disToPickup received:" <> show disToPickup) >> return Nothing else return (Just dis)
            Nothing -> return Nothing

          let currentDriverLocation = getCoordinates <$> mbLocation
          logDebug "RideCancelled Coin Event by driver"
          fork "DriverRideCancelledCoin Event : " $ do
            DC.driverCoinsEvent driverId driver.merchantId booking.merchantOperatingCityId (DCT.Cancellation ride.createdAt booking.distanceToPickup disToPickup) (Just ride.id.getId) ride.vehicleVariant
          buildRideCancelationReason currentDriverLocation updatedDisToPickup (Just driverId) DBCR.ByDriver ride (Just driver.merchantId) >>= \res -> return (res, cancellationCount, isGoToDisabled, DRide.Driver)
      return (rideCancellationReason, mbCancellationCnt, isGoToDisabled, rideEndedBy)
    DashboardRequestorId (reqMerchantId, _) -> do
      unless (driver.merchantId == reqMerchantId) $ throwError (RideDoesNotExist rideId.getId)
      logTagInfo "dashboard -> cancelRide : " ("DriverId " <> getId driverId <> ", RideId " <> getId ride.id)
      buildRideCancelationReason Nothing Nothing Nothing DBCR.ByMerchant ride (Just driver.merchantId) >>= \res -> return (res, Nothing, Nothing, DRide.Dashboard) -- is it correct DBCR.ByMerchant?
    ApplicationRequestorId jobId -> do
      logTagInfo "Allocator -> cancelRide : " ("DriverId " <> getId driverId <> ", RideId " <> getId ride.id <> "JobId " <> jobId)
      buildRideCancelationReason Nothing Nothing Nothing DBCR.ByApplication ride (Just driver.merchantId) >>= \res -> return (res, Nothing, Nothing, DRide.Allocator)
    MerchantRequestorId (reqMerchantId, mocId) -> do
      unless (driver.merchantId == reqMerchantId && mocId == driver.merchantOperatingCityId) $ throwError (RideDoesNotExist rideId.getId)
      logTagInfo "Allocator : ByMerchant -> cancelRide : " ("DriverId " <> getId driverId <> ", RideId " <> getId ride.id)
      buildRideCancelationReason Nothing Nothing Nothing DBCR.ByMerchant ride (Just driver.merchantId) >>= \res -> return (res, Nothing, Nothing, DRide.Allocator)

  cancelRide rideId rideEndedBy rideCancelationReason isForceReallocation req.doCancellationRateBasedBlocking
  pure (cancellationCnt, isGoToDisabled)
  where
    isValidRide ride = ride.status `elem` [DRide.NEW, DRide.UPCOMING]
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
            distanceUnit = ride.distanceUnit,
            merchantOperatingCityId = Just ride.merchantOperatingCityId,
            ..
          }

driverDistanceToPickup ::
  ( EncFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    Maps.HasCoordinates tripStartPos,
    Maps.HasCoordinates tripEndPos
  ) =>
  SRB.Booking ->
  tripStartPos ->
  tripEndPos ->
  m Meters
driverDistanceToPickup booking tripStartPos tripEndPos = do
  distRes <-
    Maps.getDistanceForCancelRide booking.providerId booking.merchantOperatingCityId $
      Maps.GetDistanceReq
        { origin = tripStartPos,
          destination = tripEndPos,
          travelMode = Just Maps.CAR,
          distanceUnit = booking.distanceUnit,
          sourceDestinationMapping = Nothing
        }
  return $ distRes.distance
