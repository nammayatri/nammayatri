module Jobs.Monthly where

import "dynamic-offer-driver-app" Domain.Action.Dashboard.Management.NammaTag (kaalChakraHandle)
import Kernel.External.Types (SchedulerFlow)
import Kernel.Prelude
import Kernel.Storage.ClickhouseV2 as CH
import Kernel.Storage.Esqueleto.Config
import Kernel.Utils.Common
import Lib.Scheduler
import qualified Lib.Yudhishthira.Event.KaalChakra as Event
import Lib.Yudhishthira.Storage.Beam.BeamFlow (BeamFlow)
import Lib.Yudhishthira.Types

runMonthlyJob ::
  ( EncFlow m r,
    CacheFlow m r,
    MonadFlow m,
    CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    SchedulerFlow r,
    BeamFlow m r
  ) =>
  Job 'Monthly ->
  m ExecutionResult
runMonthlyJob Job {id, jobInfo = _} = withLogTag ("JobId-" <> id.getId) do
  logInfo "Running Monthly Job"
  Event.kaalChakraEvent kaalChakraHandle Monthly
  ReSchedule <$> (addUTCTime 2592000 <$> getCurrentTime) -- month difference
