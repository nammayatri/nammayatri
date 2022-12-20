module Jobs.SendSearchRequestToDrivers where

import Beckn.Prelude hiding (handle)
import Beckn.Types.Error
import Beckn.Utils.Common
import Environment
import Jobs.SendSearchRequestToDrivers.Handle
import qualified Jobs.SendSearchRequestToDrivers.Handle.Internal as I
import Lib.Scheduler
import SharedLogic.Allocator (SendSearchRequestToDriverJobData (..))
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.Queries.SearchRequest as QSR
import qualified Tools.Metrics as Metrics

sendSearchRequestToDrivers :: Job JobType SendSearchRequestToDriverJobData -> Flow ExecutionResult
sendSearchRequestToDrivers Job {id, jobData} = withLogTag ("JobId-" <> id.getId) do
  let searchReqId = jobData.requestId
  searchReq <- QSR.findById searchReqId >>= fromMaybeM (SearchRequestNotFound searchReqId.getId)
  merchant <- CQM.findById searchReq.providerId >>= fromMaybeM (MerchantNotFound (searchReq.providerId.getId))
  handler (handle searchReq merchant)
  where
    handle searchReq merchant =
      Handle
        { isBatchNumExceedLimit = I.isBatchNumExceedLimit searchReq.id,
          isRideAlreadyAssigned = I.isRideAlreadyAssigned searchReq.id,
          isReceivedMaxDriverQuotes = I.isReceivedMaxDriverQuotes searchReq.id,
          getNextDriverPoolBatch = I.getNextDriverPoolBatch searchReq,
          cleanupDriverPoolBatches = I.cleanupDriverPoolBatches searchReq.id,
          sendSearchRequestToDrivers = I.sendSearchRequestToDrivers searchReq jobData.baseFare,
          getRescheduleTime = I.getRescheduleTime,
          metrics =
            MetricsHandle
              { incrementTaskCounter = Metrics.incrementTaskCounter merchant.name,
                incrementFailedTaskCounter = Metrics.incrementFailedTaskCounter merchant.name,
                putTaskDuration = Metrics.putTaskDuration merchant.name
              }
        }
