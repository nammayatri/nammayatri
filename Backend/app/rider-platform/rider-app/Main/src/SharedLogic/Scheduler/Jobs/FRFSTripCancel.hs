module SharedLogic.Scheduler.Jobs.FRFSTripCancel (frfsTripCancelJob) where

import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Scheduler
import qualified SharedLogic.FRFSTripCancel as FRFSTripCancel
import SharedLogic.JobScheduler

frfsTripCancelJob :: FRFSTripCancel.TripCancelJobFlow m r c => Job 'FRFSTripCancel -> m ExecutionResult
frfsTripCancelJob Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) $ do
  withTryCatch "frfsTripCancelJob" (FRFSTripCancel.runTripCancel (Id jobInfo.jobData.entityValueLogId)) >>= \case
    Right () -> pure Complete
    Left err -> do
      logError $ "FRFS trip cancel job failed, retrying: " <> show err
      pure Retry
