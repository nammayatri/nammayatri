{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TypeApplications #-}

module Domain.Action.UI.Ride.CancelRide.Internal (cancelRideImpl) where

import qualified Data.Map as M
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.BookingCancellationReason as SBCR
import qualified Domain.Types.FarePolicy as DFP
import qualified Domain.Types.Merchant as DMerc
import Domain.Types.Merchant.DriverPoolConfig (DriverPoolConfig)
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.SearchRequest as DSR
import qualified Domain.Types.SearchTry as DST
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Storage.Esqueleto.Config (EsqLocDBFlow, EsqLocRepDBFlow)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.DriverScore as DS
import qualified Lib.DriverScore.Types as DST
import Lib.Scheduler
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import Lib.SessionizerMetrics.Types.Event
import SharedLogic.Allocator
import SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers
import qualified SharedLogic.CallBAP as BP
import qualified SharedLogic.DriverLocation as DLoc
import SharedLogic.DriverMode as DMode
import SharedLogic.DriverPool
import qualified SharedLogic.DriverPool as DP
import qualified SharedLogic.External.LocationTrackingService.Flow as LF
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import SharedLogic.FareCalculator
import SharedLogic.FarePolicy
import SharedLogic.GoogleTranslate (TranslateFlow)
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.TransporterConfig as QTC
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.BookingCancellationReason as QBCR
import qualified Storage.Queries.Driver.DriverFlowStatus as QDFS
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.DriverQuote as QDQ
import qualified Storage.Queries.DriverStats as QDriverStats
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.SearchRequest as QSR
import qualified Storage.Queries.SearchTry as QST
import Tools.Error
import Tools.Event
import Tools.Metrics
import qualified Tools.Notifications as Notify

cancelRideImpl ::
  ( EsqDBFlow m r,
    EsqLocDBFlow m r,
    EsqLocRepDBFlow m r,
    TranslateFlow m r,
    Esq.EsqDBReplicaFlow m r,
    HasSendSearchRequestToDriverMetrics m r,
    HasHttpClientOptions r c,
    HasLongDurationRetryCfg r c,
    HasShortDurationRetryCfg r c,
    HasField "maxShards" r Int,
    HasField "schedulerSetName" r Text,
    CacheFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    EventStreamFlow m r,
    HasField "searchRequestExpirationSeconds" r NominalDiffTime,
    HasField "jobInfoMap" r (M.Map Text Bool),
    HasField "schedulerType" r SchedulerType,
    LT.HasLocationService m r
  ) =>
  Id DRide.Ride ->
  SBCR.BookingCancellationReason ->
  m ()
cancelRideImpl rideId bookingCReason = do
  ride <- QRide.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  booking <- QRB.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
  let merchantId = booking.providerId
  merchant <-
    CQM.findById merchantId
      >>= fromMaybeM (MerchantNotFound merchantId.getId)
  cancelRideTransaction booking.id ride bookingCReason merchantId
  logTagInfo ("rideId-" <> getId rideId) ("Cancellation reason " <> show bookingCReason.source)

  fork "cancelRide - Notify driver" $ do
    driver <- QPerson.findById ride.driverId >>= fromMaybeM (PersonNotFound ride.driverId.getId)
    triggerRideCancelledEvent RideEventData {ride = ride{status = DRide.CANCELLED}, personId = driver.id, merchantId = merchantId}
    triggerBookingCancelledEvent BookingEventData {booking = booking{status = SRB.CANCELLED}, personId = driver.id, merchantId = merchantId}
    enableLocationTrackingService <- asks (.enableLocationTrackingService)
    when enableLocationTrackingService $
      void $ LF.rideDetails ride.id DRide.CANCELLED merchantId ride.driverId booking.fromLocation.lat booking.fromLocation.lon

    when (bookingCReason.source == SBCR.ByDriver) $
      DS.driverScoreEventHandler DST.OnDriverCancellation {merchantId = merchantId, driverId = driver.id, rideFare = Just booking.estimatedFare}
    Notify.notifyOnCancel merchantId booking driver.id driver.deviceToken bookingCReason.source

  fork "cancelRide - Notify BAP" $ do
    driverQuote <- QDQ.findById (Id booking.quoteId) >>= fromMaybeM (QuoteNotFound booking.quoteId)
    searchTry <- QST.findById driverQuote.searchTryId >>= fromMaybeM (SearchTryNotFound driverQuote.searchTryId.getId)
    searchReq <- QSR.findById searchTry.requestId >>= fromMaybeM (SearchRequestNotFound searchTry.requestId.getId)
    transpConf <- QTC.findByMerchantId merchant.id >>= fromMaybeM (TransporterConfigNotFound merchant.id.getId)
    let searchRepeatLimit = transpConf.searchRepeatLimit
    now <- getCurrentTime
    farePolicy <- getFarePolicy searchReq.providerId searchTry.vehicleVariant searchReq.area
    let isRepeatSearch =
          searchTry.searchRepeatCounter < searchRepeatLimit
            && bookingCReason.source == SBCR.ByDriver
            && maybe True (\nsBounds -> isJust booking.fareParams.nightShiftCharge == isNightShift nsBounds now) farePolicy.nightShiftBounds
    if isRepeatSearch
      then do
        blockListedDriverList <- addDriverToSearchCancelledList searchReq.id ride
        driverPoolCfg <- getDriverPoolConfig merchant.id searchReq.estimatedDistance
        logDebug $ "BlockListed Drivers-" <> show blockListedDriverList
        driverPool <- calculateDriverPool DP.Estimate driverPoolCfg (Just searchTry.vehicleVariant) searchReq.fromLocation merchant.id True Nothing
        let newDriverPool = filter (\dpr -> cast dpr.driverId `notElem` blockListedDriverList) driverPool
        if not (null newDriverPool)
          then repeatSearch merchant farePolicy searchReq searchTry booking ride SBCR.ByDriver now driverPoolCfg
          else BP.sendBookingCancelledUpdateToBAP booking merchant bookingCReason.source
      else BP.sendBookingCancelledUpdateToBAP booking merchant bookingCReason.source
  where
    addDriverToSearchCancelledList searchReqId ride = do
      let keyForDriverCancelledList = DP.mkBlockListedDriversKey searchReqId
      cacheBlockListedDrivers keyForDriverCancelledList ride.driverId
      Redis.withCrossAppRedis $ Redis.getList keyForDriverCancelledList

    cacheBlockListedDrivers key driverId = do
      searchRequestExpirationSeconds <- asks (.searchRequestExpirationSeconds)
      Redis.withCrossAppRedis $ Redis.rPushExp key [driverId] (round searchRequestExpirationSeconds)

cancelRideTransaction ::
  ( EsqDBFlow m r,
    CacheFlow m r,
    Esq.EsqDBReplicaFlow m r,
    EsqLocDBFlow m r,
    EsqLocRepDBFlow m r,
    HasField "enableLocationTrackingService" r Bool
  ) =>
  Id SRB.Booking ->
  DRide.Ride ->
  SBCR.BookingCancellationReason ->
  Id DMerc.Merchant ->
  m ()
cancelRideTransaction bookingId ride bookingCReason merchantId = do
  let driverId = cast ride.driverId
  driverInfo <- QDI.findById (cast ride.driverId) >>= fromMaybeM (PersonNotFound ride.driverId.getId)
  void $ DLoc.updateOnRide merchantId ride.driverId False
  void $ QRide.updateStatus ride.id DRide.CANCELLED
  QBCR.upsert bookingCReason
  void $ QRB.updateStatus bookingId SRB.CANCELLED
  void $ QDFS.updateStatus ride.driverId $ DMode.getDriverStatus driverInfo.mode driverInfo.active
  when (bookingCReason.source == SBCR.ByDriver) $ QDriverStats.updateIdleTime driverId

repeatSearch ::
  ( EsqDBFlow m r,
    EsqLocDBFlow m r,
    EsqLocRepDBFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    TranslateFlow m r,
    Esq.EsqDBReplicaFlow m r,
    HasSendSearchRequestToDriverMetrics m r,
    HasHttpClientOptions r c,
    HasField "maxShards" r Int,
    HasField "schedulerSetName" r Text,
    HasField "schedulerType" r SchedulerType,
    HasField "jobInfoMap" r (M.Map Text Bool),
    HasShortDurationRetryCfg r c,
    CacheFlow m r,
    LT.HasLocationService m r
  ) =>
  DMerc.Merchant ->
  DFP.FullFarePolicy ->
  DSR.SearchRequest ->
  DST.SearchTry ->
  SRB.Booking ->
  DRide.Ride ->
  SBCR.CancellationSource ->
  UTCTime ->
  DriverPoolConfig ->
  m ()
repeatSearch merchant farePolicy searchReq searchTry booking ride cancellationSource now driverPoolConfig = do
  newSearchTry <- buildSearchTry searchTry

  _ <- QST.create newSearchTry

  let driverExtraFeeBounds = DFP.findDriverExtraFeeBoundsByDistance searchReq.estimatedDistance <$> farePolicy.driverExtraFeeBounds
  (res, _) <-
    sendSearchRequestToDrivers'
      driverPoolConfig
      searchReq
      newSearchTry
      merchant
      driverExtraFeeBounds

  case res of
    ReSchedule _ -> do
      let inTime = fromIntegral driverPoolConfig.singleBatchProcessTime
      maxShards <- asks (.maxShards)
      createJobIn @_ @'SendSearchRequestToDriver inTime maxShards $
        SendSearchRequestToDriverJobData
          { searchTryId = newSearchTry.id,
            estimatedRideDistance = searchReq.estimatedDistance,
            driverExtraFeeBounds = driverExtraFeeBounds
          }
    _ -> return ()

  BP.sendEstimateRepetitionUpdateToBAP booking ride searchTry.estimateId cancellationSource
  where
    buildSearchTry ::
      ( MonadTime m,
        MonadGuid m
      ) =>
      DST.SearchTry ->
      m DST.SearchTry
    buildSearchTry DST.SearchTry {..} = do
      id_ <- Id <$> generateGUID
      let validTill_ = 120 `addUTCTime` validTill
      pure
        DST.SearchTry
          { id = id_,
            startTime = now,
            validTill = validTill_,
            status = DST.ACTIVE,
            searchRepeatCounter = searchRepeatCounter + 1,
            searchRepeatType = DST.REALLOCATION,
            updatedAt = now,
            createdAt = now,
            ..
          }
