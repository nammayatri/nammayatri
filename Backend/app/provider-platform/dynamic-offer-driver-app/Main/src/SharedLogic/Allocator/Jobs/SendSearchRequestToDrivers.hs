{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers where

import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import Domain.Types.Booking (BookingStatus (..))
import Domain.Types.Common as DTC
import Domain.Types.DriverPoolConfig
import Domain.Types.GoHomeConfig (GoHomeConfig)
import Domain.Types.SearchTry (SearchTry)
import qualified Kernel.Beam.Functions as B
import Kernel.Prelude hiding (handle)
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Scheduler
import qualified Lib.Types.SpecialLocation as SL
import SharedLogic.Allocator (AllocatorJobType (..))
import SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Handle
import qualified SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Handle.Internal as I
import qualified SharedLogic.Booking as SBooking
import SharedLogic.DriverPool
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import SharedLogic.GoogleTranslate (TranslateFlow)
import qualified SharedLogic.SearchTry as SST
import qualified Storage.CachedQueries.GoHomeConfig as CQGHC
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.Estimate as QEst
import qualified Storage.Queries.Quote as QQuote
import qualified Storage.Queries.SearchRequest as QSR
import qualified Storage.Queries.SearchTry as QST
import Tools.Error
import qualified Tools.Metrics as Metrics

sendSearchRequestToDrivers ::
  ( EncFlow m r,
    TranslateFlow m r,
    EsqDBReplicaFlow m r,
    Metrics.HasSendSearchRequestToDriverMetrics m r,
    CacheFlow m r,
    EsqDBFlow m r,
    Log m,
    MonadFlow m,
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
  Job 'SendSearchRequestToDriver ->
  m ExecutionResult
sendSearchRequestToDrivers Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) $ do
  let jobData = jobInfo.jobData
  let searchTryId = jobData.searchTryId
  searchTry <- B.runInReplica $ QST.findById searchTryId >>= fromMaybeM (SearchTryNotFound searchTryId.getId)
  searchReq <- B.runInReplica $ QSR.findById searchTry.requestId >>= fromMaybeM (SearchRequestNotFound searchTry.requestId.getId)
  merchant <- CQM.findById searchReq.providerId >>= fromMaybeM (MerchantNotFound (searchReq.providerId.getId))
  driverPoolConfig <- getDriverPoolConfig searchReq.merchantOperatingCityId searchTry.vehicleServiceTier searchTry.tripCategory (fromMaybe SL.Default searchReq.area) jobData.estimatedRideDistance (Just searchReq.transactionId) (Just "transactionId")
  goHomeCfg <- CQGHC.findByMerchantOpCityId searchReq.merchantOperatingCityId (Just searchReq.transactionId) (Just "transactionId")
  tripQuoteDetails <- do
    let estimateIds = if length searchTry.estimateIds == 0 then [searchTry.estimateId] else searchTry.estimateIds
    estimateIds `forM` \estimateId -> do
      if DTC.isDynamicOfferTrip searchTry.tripCategory
        then do
          estimate <- B.runInReplica $ QEst.findById (Id estimateId) >>= fromMaybeM (EstimateNotFound estimateId)
          SST.buildTripQuoteDetail searchReq estimate.tripCategory estimate.vehicleServiceTier estimate.vehicleServiceTierName (estimate.minFare + fromMaybe 0 searchTry.customerExtraFee) (Just 0) (Just $ estimate.maxFare - estimate.minFare) estimate.driverPickUpCharge estimate.id.getId
        else do
          quote <- B.runInReplica $ QQuote.findById (Id estimateId) >>= fromMaybeM (QuoteNotFound estimateId)
          SST.buildTripQuoteDetail searchReq quote.tripCategory quote.vehicleServiceTier quote.vehicleServiceTierName (quote.estimatedFare + fromMaybe 0 searchTry.customerExtraFee) quote.driverMinFee quote.driverMaxFee quote.driverPickUpCharge quote.id.getId
  let driverSearchBatchInput =
        DriverSearchBatchInput
          { sendSearchRequestToDrivers = sendSearchRequestToDrivers',
            merchant,
            searchReq,
            tripQuoteDetails,
            customerExtraFee = searchTry.customerExtraFee,
            messageId = searchTry.messageId,
            isRepeatSearch = False
          }
  (res, _, _) <- sendSearchRequestToDrivers' driverPoolConfig searchTry driverSearchBatchInput goHomeCfg
  return res

sendSearchRequestToDrivers' ::
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
  DriverPoolConfig ->
  SearchTry ->
  DriverSearchBatchInput m ->
  GoHomeConfig ->
  m (ExecutionResult, PoolType, Maybe Seconds)
sendSearchRequestToDrivers' driverPoolConfig searchTry driverSearchBatchInput goHomeCfg = do
  -- In case of static offer flow we will have booking created before driver ride request is sent
  mbBooking <- if DTC.isDynamicOfferTrip searchTry.tripCategory then pure Nothing else QRB.findByQuoteId searchTry.estimateId
  handler (handle mbBooking) goHomeCfg
  where
    handle mbBooking =
      Handle
        { isBatchNumExceedLimit = I.isBatchNumExceedLimit driverPoolConfig searchTry.id,
          isReceivedMaxDriverQuotes = I.isReceivedMaxDriverQuotes driverPoolConfig searchTry.id,
          getNextDriverPoolBatch = I.getNextDriverPoolBatch driverPoolConfig driverSearchBatchInput.searchReq searchTry driverSearchBatchInput.tripQuoteDetails,
          sendSearchRequestToDrivers = I.sendSearchRequestToDrivers driverSearchBatchInput.tripQuoteDetails driverSearchBatchInput.searchReq searchTry driverPoolConfig,
          getRescheduleTime = I.getRescheduleTime driverPoolConfig.singleBatchProcessTime,
          setBatchDurationLock = I.setBatchDurationLock searchTry.id driverPoolConfig.singleBatchProcessTime,
          createRescheduleTime = I.createRescheduleTime driverPoolConfig.singleBatchProcessTime,
          metrics =
            MetricsHandle
              { incrementTaskCounter = Metrics.incrementTaskCounter driverSearchBatchInput.merchant.name,
                incrementFailedTaskCounter = Metrics.incrementFailedTaskCounter driverSearchBatchInput.merchant.name,
                putTaskDuration = Metrics.putTaskDuration driverSearchBatchInput.merchant.name
              },
          isSearchTryValid = I.isSearchTryValid searchTry.id,
          initiateDriverSearchBatch = SST.initiateDriverSearchBatch driverSearchBatchInput,
          isScheduledBooking = searchTry.isScheduled,
          cancelSearchTry = I.cancelSearchTry searchTry.id,
          isBookingValid = do
            case mbBooking of
              Just booking -> booking.status `notElem` [COMPLETED, CANCELLED]
              Nothing -> True,
          cancelBookingIfApplies = do
            whenJust mbBooking $ \booking -> do
              SBooking.cancelBooking booking Nothing driverSearchBatchInput.merchant
        }
