module Jobs.Quarterly where

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

runQuarterlyJob ::
  ( EncFlow m r,
    CacheFlow m r,
    MonadFlow m,
    CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    SchedulerFlow r,
    BeamFlow m r
  ) =>
  Job 'Quarterly ->
  m ExecutionResult
runQuarterlyJob Job {id, jobInfo = _} = withLogTag ("JobId-" <> id.getId) do
  logInfo "Running Quarterly Job"
  Event.kaalChakraEvent kaalChakraHandle Quarterly
  ReSchedule <$> (addUTCTime 7776000 <$> getCurrentTime) -- quarter difference
