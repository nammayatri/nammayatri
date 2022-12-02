module SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers where

import Beckn.Prelude hiding (handle)
import Beckn.Storage.Esqueleto (EsqDBReplicaFlow)
import Beckn.Storage.Hedis (HedisFlow)
import Beckn.Types.Error
import qualified Beckn.Types.SlidingWindowCounters as SWC
import Beckn.Utils.Common
import Domain.Types.Merchant (Merchant)
import Domain.Types.SearchRequest (SearchRequest)
import Lib.Scheduler
import SharedLogic.Allocator (JobType, SendSearchRequestToDriverJobData (..))
import SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Config (HasSendSearchRequestJobConfig)
import SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Handle
import qualified SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Handle.Internal as I
import SharedLogic.DriverPool.Config (HasDriverPoolConfig)
import SharedLogic.GoogleTranslate (TranslateFlow)
import Storage.CachedQueries.CacheConfig (HasCacheConfig)
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.Queries.SearchRequest as QSR
import qualified Tools.Metrics as Metrics

sendSearchRequestToDrivers ::
  ( EncFlow m r,
    TranslateFlow m r,
    EsqDBReplicaFlow m r,
    Metrics.HasSendSearchRequestToDriverMetrics m r,
    Metrics.CoreMetrics m,
    HasSendSearchRequestJobConfig r,
    HasDriverPoolConfig r,
    HasCacheConfig r,
    HedisFlow m r,
    EsqDBFlow m r,
    Log m,
    SWC.HasWindowOptions r
  ) =>
  Job JobType SendSearchRequestToDriverJobData ->
  m ExecutionResult
sendSearchRequestToDrivers Job {id, jobData} = withLogTag ("JobId-" <> id.getId) do
  let searchReqId = jobData.requestId
  searchReq <- QSR.findById searchReqId >>= fromMaybeM (SearchRequestNotFound searchReqId.getId)
  merchant <- CQM.findById searchReq.providerId >>= fromMaybeM (MerchantNotFound (searchReq.providerId.getId))
  sendSearchRequestToDrivers' searchReq merchant jobData.baseFare jobData.driverMinExtraFee jobData.driverMaxExtraFee

sendSearchRequestToDrivers' ::
  ( EncFlow m r,
    TranslateFlow m r,
    EsqDBReplicaFlow m r,
    Metrics.HasSendSearchRequestToDriverMetrics m r,
    Metrics.CoreMetrics m,
    HasSendSearchRequestJobConfig r,
    HasDriverPoolConfig r,
    HasCacheConfig r,
    HedisFlow m r,
    EsqDBFlow m r,
    Log m,
    SWC.HasWindowOptions r
  ) =>
  SearchRequest ->
  Merchant ->
  Money ->
  Money ->
  Money ->
  m ExecutionResult
sendSearchRequestToDrivers' searchReq merchant baseFare driverMinExtraCharge driverMaxExtraCharge = do
  handler handle
  where
    handle =
      Handle
        { isBatchNumExceedLimit = I.isBatchNumExceedLimit searchReq.id,
          isRideAlreadyAssigned = I.isRideAlreadyAssigned searchReq.id,
          isReceivedMaxDriverQuotes = I.isReceivedMaxDriverQuotes searchReq.id,
          getNextDriverPoolBatch = I.getNextDriverPoolBatch searchReq,
          cleanupDriverPoolBatches = I.cleanupDriverPoolBatches searchReq.id,
          sendSearchRequestToDrivers = I.sendSearchRequestToDrivers searchReq baseFare driverMinExtraCharge driverMaxExtraCharge,
          getRescheduleTime = I.getRescheduleTime,
          setBatchDurationLock = I.setBatchDurationLock searchReq.id,
          createRescheduleTime = I.createRescheduleTime,
          metrics =
            MetricsHandle
              { incrementTaskCounter = Metrics.incrementTaskCounter merchant.name,
                incrementFailedTaskCounter = Metrics.incrementFailedTaskCounter merchant.name,
                putTaskDuration = Metrics.putTaskDuration merchant.name
              }
        }
