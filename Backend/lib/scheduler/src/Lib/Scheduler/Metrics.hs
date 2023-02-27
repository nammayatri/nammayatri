{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Lib.Scheduler.Metrics where

import Kernel.Prelude
import qualified Prometheus as P

newtype SchedulerMetrics = SchedulerMetrics
  { durationHistogram :: P.Histogram
  }

setupSchedulerMetrics :: MonadIO m => m SchedulerMetrics
setupSchedulerMetrics = do
  let histInfo = P.Info "job_execution_duration" "Duration of the job execution"
  durationHistogram <- P.register $ P.histogram histInfo P.defaultBuckets
  pure $ SchedulerMetrics {..}

observeJobExecDuration :: (MonadIO m, MonadReader r m, HasField "metrics" r SchedulerMetrics) => Double -> m ()
observeJobExecDuration duration = do
  metrics <- asks (.metrics)
  liftIO $ P.observe metrics.durationHistogram duration
