module Jobs.SendSearchRequestToDrivers where

import Beckn.Prelude hiding (handle)
import Beckn.Types.Error
import Beckn.Utils.Common
import Environment
import Jobs.SendSearchRequestToDrivers.Handle
import qualified Jobs.SendSearchRequestToDrivers.Handle.Internal as I
import Lib.Scheduler
import SharedLogic.Allocator (SendSearchRequestToDriverJobData (..))
import qualified Storage.Queries.SearchRequest as QSR

sendSearchRequestToDrivers :: Job JobType SendSearchRequestToDriverJobData -> Flow ExecutionResult
sendSearchRequestToDrivers Job {jobData} = do
  let searchReqId = jobData.requestId
  searchReq <- QSR.findById searchReqId >>= fromMaybeM (SearchRequestNotFound searchReqId.getId)
  handler (handle searchReq)
  where
    handle searchReq =
      Handle
        { isRideAlreadyAssigned = I.isRideAlreadyAssigned searchReq.id,
          getNextDriverPoolBatch = I.getNextDriverPoolBatch searchReq.id,
          prepareDriverPoolBatches = I.prepareDriverPoolBatches searchReq,
          incrementPoolRadiusStep = I.incrementPoolRadiusStep searchReq,
          sendSearchRequestToDrivers = I.sendSearchRequestToDrivers searchReq jobData.baseFare,
          getRescheduleTime = I.getRescheduleTime
        }
