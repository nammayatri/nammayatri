{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Lib.Scheduler.App
  ( runSchedulerService,
  )
where

import Kernel.Prelude hiding (mask, throwIO)
import qualified Kernel.Tools.Metrics.Init as Metrics
import qualified Kernel.Utils.Logging as Log
import Kernel.Utils.Servant.Server
import Lib.Scheduler.Environment
import Lib.Scheduler.Handler (SchedulerHandle, handler)
import Servant (Context (EmptyContext))
import System.Exit
import UnliftIO

runSchedulerService ::
  SchedulerConfig ->
  SchedulerHandle t ->
  IO ()
runSchedulerService schedulerCfg handle_ = do
  schedulerEnv <- buildSchedulerEnv schedulerCfg

  when (schedulerEnv.tasksPerIteration <= 0) $ do
    hPutStrLn stderr ("tasksPerIteration should be greater than 0" :: Text)
    exitFailure

  Metrics.serve schedulerCfg.metricsPort
  let serverStartAction = Log.withLogTag ("scheduler-instance: " <> schedulerEnv.schedulerInstanceName) $ handler handle_
  withAsync (runSchedulerM schedulerEnv serverStartAction) $ \schedulerAction ->
    runServerGeneric
      schedulerEnv
      (Proxy @HealthCheckAPI)
      healthCheck
      identity
      identity
      EmptyContext
      (const identity)
      (\_ -> cancel schedulerAction)
      runSchedulerM

-- TODO: explore what is going on here
-- To which thread do signals come?
