module Jobs.Daily where

import Kernel.External.Types (SchedulerFlow)
import Kernel.Prelude
import Kernel.Storage.ClickhouseV2 as CH
import Kernel.Storage.Esqueleto.Config
import Kernel.Utils.Common
import Lib.Scheduler
import Lib.Yudhishthira.Types

runDailyJob ::
  ( EncFlow m r,
    CacheFlow m r,
    MonadFlow m,
    CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    SchedulerFlow r
  ) =>
  Job 'Daily ->
  m ExecutionResult
runDailyJob Job {id, jobInfo = _} = withLogTag ("JobId-" <> id.getId) do
  logInfo "Running Daily Job"
  ReSchedule <$> (addUTCTime 120 <$> getCurrentTime) -- day difference
