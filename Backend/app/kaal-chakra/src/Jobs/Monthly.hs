module Jobs.Monthly where

import "dynamic-offer-driver-app" Domain.Action.Dashboard.Management.NammaTag.Handle (kaalChakraHandle)
import Environment
import qualified Jobs.Common as Common
import Kernel.Prelude
import Kernel.Utils.Common
import Lib.Scheduler
import qualified Lib.Yudhishthira.Event.KaalChakra as Event
import Lib.Yudhishthira.Types
import Storage.Beam.SchedulerJob ()
import "dynamic-offer-driver-app" Storage.Beam.Yudhishthira ()
import qualified Utils.Time as Time

runMonthlyJob ::
  Job 'Monthly ->
  Flow ExecutionResult
runMonthlyJob Job {id, scheduledAt} = withLogTag ("JobId-" <> id.getId) do
  logInfo "Running Monthly Job"
  req <- Common.buildRunKaalChakraJobReq Monthly
  void $ Event.kaalChakraEvent kaalChakraHandle req
  pure $ ReSchedule $ Time.incrementMonth scheduledAt
