{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.SearchTry where

import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import qualified Domain.Types.Common as DTC
import Domain.Types.DriverPoolConfig
import qualified Domain.Types.FarePolicy as DFP
import qualified Domain.Types.FarePolicy as DFarePolicy
import Domain.Types.GoHomeConfig (GoHomeConfig)
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.SearchRequest as DSR
import Domain.Types.SearchRequestForDriver
import qualified Domain.Types.SearchTry as DST
import qualified Domain.Types.ServiceTierType as DVST
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Scheduler
import Lib.Scheduler.JobStorageType.SchedulerType as JC
import qualified Lib.Types.SpecialLocation as SL
import SharedLogic.Allocator
import qualified SharedLogic.Booking as SBooking
import SharedLogic.DriverPool (getDriverPoolConfig)
import SharedLogic.DriverPool.Types (PoolType)
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import SharedLogic.FareCalculator
import SharedLogic.FarePolicy
import SharedLogic.GoogleTranslate (TranslateFlow)
import qualified Storage.CachedQueries.GoHomeConfig as CQGHC
import qualified Storage.CachedQueries.VehicleServiceTier as CQVST
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.DriverQuote as QDQ
import qualified Storage.Queries.SearchTry as QST
import Tools.Error
import qualified Tools.Metrics as Metrics

getNextScheduleTime ::
  ( MonadFlow m,
    Metrics.CoreMetrics m,
    CacheFlow m r,
    MonadReader r m
  ) =>
  DriverPoolConfig ->
  DSR.SearchRequest ->
  m (Maybe NominalDiffTime)
getNextScheduleTime driverPoolConfig searchRequest = do
  mbScheduleTryTimes <- getKey
  let scheduleTryTimes =
        case mbScheduleTryTimes of
          Just scheduleTryTimes' -> scheduleTryTimes'
          Nothing -> (secondsToNominalDiffTime . Seconds) <$> driverPoolConfig.scheduleTryTimes
  case scheduleTryTimes of
    [] -> return Nothing
    (scheduleTryTime : rest) -> do
      setKey rest
      now <- getCurrentTime
      return $ Just $ searchRequest.startTime `diffUTCTime` (scheduleTryTime `addUTCTime` now)
  where
    scheduleSearchKey = "ScheduleSearch-" <> searchRequest.id.getId
    setKey scheduleTryTimes = Redis.withCrossAppRedis $ Redis.setExp scheduleSearchKey scheduleTryTimes 3600
    getKey = Redis.withCrossAppRedis $ Redis.safeGet scheduleSearchKey

initiateDriverSearchBatch ::
  ( EncFlow m r,
    TranslateFlow m r,
    EsqDBReplicaFlow m r,
    Metrics.HasSendSearchRequestToDriverMetrics m r,
    CacheFlow m r,
    EsqDBFlow m r,
    Log m,
    LT.HasLocationService m r,
    HasFlowEnv m r '["maxNotificationShards" ::: Int],
    HasField "maxShards" r Int,
    HasField "schedulerSetName" r Text,
    HasField "schedulerType" r SchedulerType,
    HasField "jobInfoMap" r (M.Map Text Bool),
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasHttpClientOptions r c,
    HasLongDurationRetryCfg r c,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl]
  ) =>
  (DriverPoolConfig -> DSR.SearchRequest -> DST.SearchTry -> DM.Merchant -> Maybe DFP.DriverExtraFeeBounds -> Maybe Money -> GoHomeConfig -> m (ExecutionResult, PoolType, Maybe Seconds)) ->
  DM.Merchant ->
  DSR.SearchRequest ->
  DTC.TripCategory ->
  DVST.ServiceTierType ->
  Text ->
  Maybe Money ->
  Text ->
  Bool ->
  m ()
initiateDriverSearchBatch sendSearchRequestToDrivers merchant searchReq tripCategory serviceTier estOrQuoteId customerExtraFee messageId isRepeatSearch = do
  farePolicy <- getFarePolicyByEstOrQuoteId searchReq.merchantOperatingCityId tripCategory serviceTier searchReq.area estOrQuoteId (Just searchReq.transactionId) (Just "transactionId")
  searchTry <- createNewSearchTry farePolicy
  driverPoolConfig <- getDriverPoolConfig searchReq.merchantOperatingCityId searchTry.vehicleServiceTier searchTry.tripCategory (fromMaybe SL.Default searchReq.area) searchReq.estimatedDistance (Just searchReq.transactionId) (Just "transactionId")
  goHomeCfg <- CQGHC.findByMerchantOpCityId searchReq.merchantOperatingCityId (Just searchReq.transactionId) (Just "transactionId")
  let driverExtraFeeBounds = DFarePolicy.findDriverExtraFeeBoundsByDistance (fromMaybe 0 searchReq.estimatedDistance) <$> farePolicy.driverExtraFeeBounds
  let driverPickUpCharges = extractDriverPickupCharges farePolicy.farePolicyDetails
  if not searchTry.isScheduled
    then do
      (res, _, mbNewScheduleTimeIn) <- sendSearchRequestToDrivers driverPoolConfig searchReq searchTry merchant driverExtraFeeBounds driverPickUpCharges goHomeCfg
      let inTime = maybe (fromIntegral driverPoolConfig.singleBatchProcessTime) fromIntegral mbNewScheduleTimeIn
      case res of
        ReSchedule _ -> scheduleBatching searchTry driverExtraFeeBounds driverPickUpCharges inTime
        _ -> return ()
    else do
      mbScheduleTime <- getNextScheduleTime driverPoolConfig searchReq
      case mbScheduleTime of
        Just scheduleTime -> scheduleBatching searchTry driverExtraFeeBounds driverPickUpCharges scheduleTime
        Nothing -> do
          booking <- QRB.findByQuoteId estOrQuoteId >>= fromMaybeM (BookingDoesNotExist estOrQuoteId)
          QST.updateStatus searchTry.id DST.CANCELLED
          SBooking.cancelBooking booking Nothing merchant
  where
    scheduleBatching searchTry driverExtraFeeBounds driverPickUpCharges inTime = do
      maxShards <- asks (.maxShards)
      JC.createJobIn @_ @'SendSearchRequestToDriver inTime maxShards $
        SendSearchRequestToDriverJobData
          { searchTryId = searchTry.id,
            estimatedRideDistance = searchReq.estimatedDistance,
            driverExtraFeeBounds = driverExtraFeeBounds,
            driverPickUpCharges = driverPickUpCharges
          }

    createNewSearchTry farePolicy = do
      mbLastSearchTry <- QST.findLastByRequestId searchReq.id
      fareParams <-
        calculateFareParameters
          CalculateFareParametersParams
            { farePolicy = farePolicy,
              actualDistance = searchReq.estimatedDistance,
              rideTime = searchReq.startTime,
              waitingTime = Nothing,
              actualRideDuration = Nothing,
              avgSpeedOfVehicle = Nothing,
              driverSelectedFare = Nothing,
              customerExtraFee = customerExtraFee,
              nightShiftCharge = Nothing,
              customerCancellationDues = searchReq.customerCancellationDues,
              tollCharges = searchReq.tollCharges,
              nightShiftOverlapChecking = DTC.isRentalTrip farePolicy.tripCategory,
              estimatedDistance = searchReq.estimatedDistance,
              estimatedRideDuration = searchReq.estimatedDuration,
              timeDiffFromUtc = Nothing,
              ..
            }
      let estimatedFare = fareSum fareParams
          pureEstimatedFare = pureFareSum fareParams
      searchTry <- case mbLastSearchTry of
        Nothing -> do
          searchTry <- buildSearchTry merchant.id searchReq estOrQuoteId estimatedFare 0 DST.INITIAL tripCategory customerExtraFee messageId serviceTier
          _ <- QST.create searchTry
          return searchTry
        Just oldSearchTry -> do
          let searchRepeatType
                | isRepeatSearch = DST.REALLOCATION
                | oldSearchTry.status == DST.ACTIVE = DST.CANCELLED_AND_RETRIED
                | otherwise = DST.RETRIED
          unless (pureEstimatedFare == oldSearchTry.baseFare - fromMaybe 0 oldSearchTry.customerExtraFee) $
            throwError SearchTryEstimatedFareChanged
          searchTry <- buildSearchTry merchant.id searchReq estOrQuoteId estimatedFare (oldSearchTry.searchRepeatCounter + 1) searchRepeatType tripCategory customerExtraFee messageId serviceTier
          when (oldSearchTry.status == DST.ACTIVE) $ do
            QST.updateStatus oldSearchTry.id DST.CANCELLED
            void $ QDQ.setInactiveBySTId oldSearchTry.id
          _ <- QST.create searchTry
          return searchTry

      logDebug $
        "search try id=" <> show searchTry.id
          <> "; estimated distance = "
          <> show searchReq.estimatedDistance
          <> "; estimated base fare:"
          <> show estimatedFare
      return searchTry

buildSearchTry ::
  ( MonadFlow m,
    CacheFlow m r,
    Metrics.CoreMetrics m,
    EsqDBFlow m r
  ) =>
  Id DM.Merchant ->
  DSR.SearchRequest ->
  Text ->
  Money ->
  Int ->
  DST.SearchRepeatType ->
  DTC.TripCategory ->
  Maybe Money ->
  Text ->
  DVST.ServiceTierType ->
  m DST.SearchTry
buildSearchTry merchantId searchReq estOrQuoteId baseFare searchRepeatCounter searchRepeatType tripCategory customerExtraFee messageId serviceTier = do
  now <- getCurrentTime
  id_ <- Id <$> generateGUID
  vehicleServiceTierItem <- CQVST.findByServiceTierTypeAndCityId serviceTier searchReq.merchantOperatingCityId >>= fromMaybeM (VehicleServiceTierNotFound (show serviceTier))
  pure $
    DST.SearchTry
      { id = id_,
        vehicleServiceTier = serviceTier,
        vehicleServiceTierName = vehicleServiceTierItem.name,
        requestId = searchReq.id,
        estimateId = estOrQuoteId,
        merchantId = Just merchantId,
        merchantOperatingCityId = searchReq.merchantOperatingCityId,
        messageId = messageId,
        startTime = searchReq.startTime,
        isScheduled = searchReq.isScheduled,
        validTill = searchReq.validTill,
        status = DST.ACTIVE,
        createdAt = now,
        updatedAt = now,
        ..
      }
