module SharedLogic.Allocator.Jobs.Payout.PayoutStatusCheck (payoutStatusCheckJob) where

import qualified Domain.Action.UI.Payout as UIPayout
import Kernel.External.Types (SchedulerFlow)
import Kernel.Prelude
import Kernel.Utils.Common
import qualified Lib.Payment.Payout.StatusCheck as PSC
import Lib.Scheduler
import SharedLogic.Allocator
import qualified SharedLogic.PayoutStatusCheck as SPSC
import Storage.Beam.SchedulerJob ()

payoutStatusCheckJob ::
  ( UIPayout.PayoutSettlementFlow m r,
    SchedulerFlow r,
    HasField "blackListedJobs" r [Text]
  ) =>
  Job 'PayoutStatusCheck ->
  m ExecutionResult
payoutStatusCheckJob Job {id, jobInfo} =
  withLogTag ("JobId-" <> id.getId) $
    PSC.runPayoutStatusCheckJob
      PSC.Handle
        { getConfig = SPSC.getPayoutStatusCheckConfig,
          refreshWithSettlement = UIPayout.refreshPayoutOrderWithSettlement,
          scheduleNextCheck = SPSC.scheduleNextPayoutStatusCheck
        }
      jobInfo.jobData
