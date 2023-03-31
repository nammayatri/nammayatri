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

import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.BookingCancellationReason as SBCR
import qualified Domain.Types.Driver.DriverFlowStatus as DDFS
import qualified Domain.Types.FareParameters as DFParams (FarePolicyType (..))
import qualified Domain.Types.FarePolicy as DFP
import qualified Domain.Types.Merchant as DMerc
import Domain.Types.Merchant.DriverPoolConfig (DriverPoolConfig)
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.SearchRequest as DSR
import qualified Domain.Types.SearchRequest as DSearchReq
import qualified Domain.Types.SearchRequest.SearchReqLocation as DLoc
import qualified Domain.Types.SlabFarePolicy as DSFP
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.DriverScore as DS
import qualified Lib.DriverScore.Types as DST
import Lib.Scheduler
import Lib.Scheduler.JobStorageType.DB.Queries (createJobIn)
import SharedLogic.Allocator
import SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers
import qualified SharedLogic.CallBAP as BP
import qualified SharedLogic.DriverLocation as DLoc
import SharedLogic.DriverPool
import SharedLogic.Estimate
import SharedLogic.FareCalculator
import SharedLogic.GoogleTranslate (TranslateFlow)
import qualified SharedLogic.Ride as SRide
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.DriverInformation as CDI
import qualified Storage.CachedQueries.FarePolicy as QFP
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.TransporterConfig as QTC
import qualified Storage.CachedQueries.SlabFarePolicy as QSFP
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.BookingCancellationReason as QBCR
import qualified Storage.Queries.Driver.DriverFlowStatus as QDFS
import qualified Storage.Queries.DriverQuote as QDQ
import qualified Storage.Queries.DriverStats as QDriverStats
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.SearchRequest as QSR
import Tools.Error
import Tools.Metrics
import qualified Tools.Notifications as Notify

cancelRideImpl ::
  ( EsqDBFlow m r,
    TranslateFlow m r,
    Esq.EsqDBReplicaFlow m r,
    HasSendSearchRequestToDriverMetrics m r,
    HasHttpClientOptions r c,
    HasLongDurationRetryCfg r c,
    HasShortDurationRetryCfg r c,
    HasField "maxShards" r Int,
    HasCacheConfig r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl]
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
  cancelRideTransaction booking.id ride bookingCReason
  logTagInfo ("rideId-" <> getId rideId) ("Cancellation reason " <> show bookingCReason.source)

  fork "cancelRide - Notify driver" $ do
    driver <- QPerson.findById ride.driverId >>= fromMaybeM (PersonNotFound ride.driverId.getId)
    when (bookingCReason.source == SBCR.ByDriver) $
      DS.driverScoreEventHandler DST.OnDriverCancellation {merchantId = merchantId, driverId = driver.id}
    Notify.notifyOnCancel merchantId booking driver.id driver.deviceToken bookingCReason.source

  fork "cancelRide - Notify BAP" $ do
    driverQuote <- QDQ.findById (Id booking.quoteId) >>= fromMaybeM (QuoteNotFound booking.quoteId)
    searchReq <- QSR.findById driverQuote.searchRequestId >>= fromMaybeM (SearchRequestNotFound driverQuote.searchRequestId.getId)
    transpConf <- QTC.findByMerchantId merchant.id >>= fromMaybeM (TransporterConfigNotFound merchant.id.getId)
    let searchRepeatLimit = transpConf.searchRepeatLimit
    driverPoolCfg <- getDriverPoolConfig merchant.id searchReq.estimatedDistance
    driverPool <- calculateDriverPool Estimate driverPoolCfg (Just searchReq.vehicleVariant) searchReq.fromLocation merchant.id True Nothing
    now <- getCurrentTime
    case merchant.farePolicyType of
      DFParams.NORMAL -> do
        farePolicy <- QFP.findByMerchantIdAndVariant searchReq.providerId searchReq.vehicleVariant (Just searchReq.estimatedDistance) >>= fromMaybeM NoFarePolicy
        let isRepeatSearch = isRepeatSearchFun farePolicy.nightShiftStart farePolicy.nightShiftEnd now booking.fareParams.nightCoefIncluded searchReq.searchRepeatCounter searchRepeatLimit
        cancelBooking isRepeatSearch ride merchant driverPoolCfg driverPool (Left farePolicy) now booking searchReq
      DFParams.SLAB -> do
        slabFarePolicy <- QSFP.findByMerchantIdAndVariant searchReq.providerId searchReq.vehicleVariant >>= fromMaybeM NoFarePolicy
        let isRepeatSearch = isRepeatSearchFun slabFarePolicy.nightShiftStart slabFarePolicy.nightShiftEnd now booking.fareParams.nightCoefIncluded searchReq.searchRepeatCounter searchRepeatLimit
        cancelBooking isRepeatSearch ride merchant driverPoolCfg driverPool (Right slabFarePolicy) now booking searchReq
  where
    isRepeatSearchFun nightShiftStart nightShiftEnd now nightCoefIncluded searchRepeatCounter searchRepeatLimit =
      searchRepeatCounter < searchRepeatLimit
        && bookingCReason.source == SBCR.ByDriver
        && (isNightShift nightShiftStart nightShiftEnd now == nightCoefIncluded)

    cancelBooking isRepeatSearch ride merchant driverPoolCfg driverPool farePolicy now booking searchReq = do
      case (isRepeatSearch, driverPool) of
        (True, a : xs) -> repeatSearch merchant farePolicy searchReq booking ride SBCR.ByDriver now driverPoolCfg (a :| xs)
        _ -> BP.sendBookingCancelledUpdateToBAP booking merchant bookingCReason.source

cancelRideTransaction ::
  ( EsqDBFlow m r,
    CacheFlow m r,
    Esq.EsqDBReplicaFlow m r
  ) =>
  Id SRB.Booking ->
  DRide.Ride ->
  SBCR.BookingCancellationReason ->
  m ()
cancelRideTransaction bookingId ride bookingCReason = do
  let driverId = cast ride.driverId
  DLoc.updateOnRide driverId False
  driverInfo <- CDI.findById (cast ride.driverId) >>= fromMaybeM (PersonNotFound ride.driverId.getId)
  Esq.runTransaction $ do
    when (bookingCReason.source == SBCR.ByDriver) $ QDriverStats.updateIdleTime driverId
    QRide.updateStatus ride.id DRide.CANCELLED
    QRB.updateStatus bookingId SRB.CANCELLED
    QBCR.upsert bookingCReason
    if driverInfo.active
      then QDFS.updateStatus ride.driverId DDFS.ACTIVE
      else QDFS.updateStatus ride.driverId DDFS.IDLE
  SRide.clearCache ride.driverId

repeatSearch ::
  ( EsqDBFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    TranslateFlow m r,
    Esq.EsqDBReplicaFlow m r,
    HasSendSearchRequestToDriverMetrics m r,
    HasHttpClientOptions r c,
    HasField "maxShards" r Int,
    HasShortDurationRetryCfg r c,
    CacheFlow m r
  ) =>
  DMerc.Merchant ->
  Either DFP.FarePolicy DSFP.SlabFarePolicy ->
  DSR.SearchRequest ->
  SRB.Booking ->
  DRide.Ride ->
  SBCR.CancellationSource ->
  UTCTime ->
  DriverPoolConfig ->
  NonEmpty DriverPoolResult ->
  m ()
repeatSearch merchant eitherFarePolicy searchReq booking ride cancellationSource now driverPoolConfig driverPool = do
  newSearchReq <- buildSearchRequest searchReq
  (estimateItem, driverExtraFare) <- case eitherFarePolicy of
    Left normalFarePolicy ->
      (,normalFarePolicy.driverExtraFee)
        <$> buildEstimate merchant newSearchReq.startTime newSearchReq.estimatedDistance (normalFarePolicy, driverPool)
    Right slabFarePolicy -> do
      (,DFP.ExtraFee {minFee = 0, maxFee = 0})
        <$> buildEstimateFromSlabFarePolicy merchant newSearchReq.startTime newSearchReq.estimatedDistance (slabFarePolicy, driverPool)

  fareParams <- calculateFare searchReq.providerId eitherFarePolicy searchReq.estimatedDistance now Nothing

  let baseFare = fareSum fareParams
  Esq.runTransaction $ do
    QSR.create newSearchReq

  res <-
    sendSearchRequestToDrivers'
      driverPoolConfig
      newSearchReq
      merchant
      baseFare
      driverExtraFare.minFee
      driverExtraFare.maxFee

  case res of
    ReSchedule _ -> do
      let inTime = fromIntegral driverPoolConfig.singleBatchProcessTime
      maxShards <- asks (.maxShards)
      Esq.runTransaction $ do
        createJobIn @_ @'SendSearchRequestToDriver inTime maxShards $
          SendSearchRequestToDriverJobData
            { requestId = newSearchReq.id,
              baseFare = baseFare,
              estimatedRideDistance = newSearchReq.estimatedDistance,
              driverMinExtraFee = driverExtraFare.minFee,
              driverMaxExtraFee = driverExtraFare.maxFee
            }
    _ -> return ()

  BP.sendEstimateRepetitionUpdateToBAP booking ride estimateItem cancellationSource
  where
    buildSearchRequest ::
      ( MonadTime m,
        MonadGuid m,
        MonadReader r m
      ) =>
      DSearchReq.SearchRequest ->
      m DSearchReq.SearchRequest
    buildSearchRequest DSearchReq.SearchRequest {..} = do
      id_ <- Id <$> generateGUID
      let validTill_ = 120 `addUTCTime` validTill
      from <- buildSearchReqLocation fromLocation
      to <- buildSearchReqLocation toLocation
      pure
        DSearchReq.SearchRequest
          { id = id_,
            startTime = now,
            validTill = validTill_,
            fromLocation = from,
            toLocation = to,
            status = DSearchReq.ACTIVE,
            searchRepeatCounter = searchRepeatCounter + 1,
            updatedAt = now,
            createdAt = now,
            ..
          }

    buildSearchReqLocation ::
      ( MonadTime m,
        MonadGuid m
      ) =>
      DLoc.SearchReqLocation ->
      m DLoc.SearchReqLocation
    buildSearchReqLocation DLoc.SearchReqLocation {..} = do
      newId <- Id <$> generateGUID
      pure
        DLoc.SearchReqLocation
          { id = newId,
            createdAt = now,
            updatedAt = now,
            ..
          }
