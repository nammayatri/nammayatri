module SharedLogic.Scheduler.Jobs.ExecuteCashRideCashbackPayout where

import Kernel.External.Types (SchedulerFlow, ServiceFlow)
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer)
import Kernel.Utils.Common
import qualified Lib.Finance.Core.Types as Finance
import qualified Lib.Finance.Storage.Beam.BeamFlow as FinanceBeamFlow
import Lib.Scheduler
import SharedLogic.Finance.CashbackPayout (runCashbackPayout)
import SharedLogic.JobScheduler
import Storage.Beam.Payment ()

executeCashRideCashbackPayoutJob ::
  ( EncFlow m r,
    CacheFlow m r,
    MonadFlow m,
    ServiceFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    SchedulerFlow r,
    HasShortDurationRetryCfg r c,
    HasKafkaProducer r,
    HasFlowEnv m r '["selfBaseUrl" ::: BaseUrl],
    FinanceBeamFlow.BeamFlow m r,
    Finance.HasActorInfo m r,
    HasField "blackListedJobs" r [Text]
  ) =>
  Job 'ExecuteCashRideCashbackPayout ->
  m ExecutionResult
executeCashRideCashbackPayoutJob Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) do
  let personId = jobInfo.jobData.personId
  runCashbackPayout personId
  pure Complete
