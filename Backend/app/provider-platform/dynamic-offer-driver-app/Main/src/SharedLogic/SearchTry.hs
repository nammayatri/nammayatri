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
import qualified Domain.Types.SearchTry as DST
import qualified Domain.Types.Vehicle as DVeh
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Scheduler
import Lib.Scheduler.JobStorageType.SchedulerType as JC
import SharedLogic.Allocator
import qualified SharedLogic.Booking as SBooking
import SharedLogic.DriverPool (getDriverPoolConfig)
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import SharedLogic.FareCalculator
import SharedLogic.FarePolicy
import SharedLogic.GoogleTranslate (TranslateFlow)
import qualified Storage.CachedQueries.GoHomeConfig as CQGHC
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
    HasField "isBecknSpecVersion2" r Bool,
    HasHttpClientOptions r c,
    HasLongDurationRetryCfg r c,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl]
  ) =>
  (DriverPoolConfig -> DSR.SearchRequest -> DST.SearchTry -> DM.Merchant -> Maybe DFP.DriverExtraFeeBounds -> GoHomeConfig -> m (ExecutionResult, Bool)) ->
  DM.Merchant ->
  DSR.SearchRequest ->
  DTC.TripCategory ->
  DVeh.Variant ->
  Text ->
  Maybe Money ->
  Text ->
  m ()
initiateDriverSearchBatch sendSearchRequestToDrivers merchant searchReq tripCategory vehicleVariant estOrQuoteId customerExtraFee messageId = do
  farePolicy <- getFarePolicyByEstOrQuoteId searchReq.merchantOperatingCityId tripCategory vehicleVariant searchReq.area estOrQuoteId
  searchTry <- createNewSearchTry farePolicy searchReq.customerCancellationDues
  driverPoolConfig <- getDriverPoolConfig searchReq.merchantOperatingCityId searchTry.vehicleVariant searchTry.tripCategory searchReq.estimatedDistance
  goHomeCfg <- CQGHC.findByMerchantOpCityId searchReq.merchantOperatingCityId
  let driverExtraFeeBounds = DFarePolicy.findDriverExtraFeeBoundsByDistance (fromMaybe 0 searchReq.estimatedDistance) <$> farePolicy.driverExtraFeeBounds
  if not searchTry.isScheduled
    then do
      (res, isGoHomeBatch) <- sendSearchRequestToDrivers driverPoolConfig searchReq searchTry merchant driverExtraFeeBounds goHomeCfg
      let inTime = fromIntegral (if isGoHomeBatch then goHomeCfg.goHomeBatchDelay else driverPoolConfig.singleBatchProcessTime)
      case res of
        ReSchedule _ -> scheduleBatching searchTry driverExtraFeeBounds inTime
        _ -> return ()
    else do
      mbScheduleTime <- getNextScheduleTime driverPoolConfig searchReq
      case mbScheduleTime of
        Just scheduleTime -> scheduleBatching searchTry driverExtraFeeBounds scheduleTime
        Nothing -> do
          booking <- QRB.findByQuoteId estOrQuoteId >>= fromMaybeM (BookingDoesNotExist estOrQuoteId)
          QST.updateStatus searchTry.id DST.CANCELLED
          SBooking.cancelBooking booking Nothing merchant
  where
    scheduleBatching searchTry driverExtraFeeBounds inTime = do
      maxShards <- asks (.maxShards)
      JC.createJobIn @_ @'SendSearchRequestToDriver inTime maxShards $
        SendSearchRequestToDriverJobData
          { searchTryId = searchTry.id,
            estimatedRideDistance = searchReq.estimatedDistance,
            driverExtraFeeBounds = driverExtraFeeBounds
          }

    createNewSearchTry farePolicy customerCancellationDues = do
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
              customerCancellationDues = customerCancellationDues,
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
          searchTry <- buildSearchTry merchant.id searchReq estOrQuoteId estimatedFare 0 DST.INITIAL tripCategory customerExtraFee messageId vehicleVariant
          _ <- QST.create searchTry
          return searchTry
        Just oldSearchTry -> do
          let searchRepeatType = if oldSearchTry.status == DST.ACTIVE then DST.CANCELLED_AND_RETRIED else DST.RETRIED
          unless (pureEstimatedFare == oldSearchTry.baseFare - fromMaybe 0 oldSearchTry.customerExtraFee) $
            throwError SearchTryEstimatedFareChanged
          searchTry <- buildSearchTry merchant.id searchReq estOrQuoteId estimatedFare (oldSearchTry.searchRepeatCounter + 1) searchRepeatType tripCategory customerExtraFee messageId vehicleVariant
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
  ( MonadTime m,
    MonadGuid m,
    MonadReader r m
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
  DVeh.Variant ->
  m DST.SearchTry
buildSearchTry merchantId searchReq estOrQuoteId baseFare searchRepeatCounter searchRepeatType tripCategory customerExtraFee messageId vehicleVariant = do
  now <- getCurrentTime
  id_ <- Id <$> generateGUID
  pure
    DST.SearchTry
      { id = id_,
        vehicleVariant,
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
