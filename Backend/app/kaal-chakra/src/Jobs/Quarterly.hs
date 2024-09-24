module Jobs.Quarterly where

import "dynamic-offer-driver-app" Domain.Action.Dashboard.Management.NammaTag (kaalChakraHandle)
import Environment
import qualified Jobs.Common as Common
import Kernel.Prelude
import Kernel.Utils.Common
import Lib.Scheduler
import qualified Lib.Yudhishthira.Event.KaalChakra as Event
import Lib.Yudhishthira.Types
import Storage.Beam.SchedulerJob ()
import qualified Utils.Time as Time

runQuarterlyJob ::
  Job 'Quarterly ->
  Flow ExecutionResult
runQuarterlyJob Job {id, scheduledAt} = withLogTag ("JobId-" <> id.getId) do
  logInfo "Running Quarterly Job"
  req <- Common.buildRunKaalChakraJobReq Quarterly
  void $ Event.kaalChakraEvent kaalChakraHandle req
  pure $ ReSchedule $ Time.incrementQuarter scheduledAt
