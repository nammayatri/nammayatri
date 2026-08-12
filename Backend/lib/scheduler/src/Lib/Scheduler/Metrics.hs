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
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics, incrementSchedulerJobLifecycleCounter)
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

-- | Points in a scheduler job's life that we count, emitted as the @status@
-- label of @scheduler_job_lifecycle_counter@.
--
-- Every job contributes exactly one 'JobCreated', at most one 'JobPicked' per
-- execution attempt, and exactly one of the remaining constructors per attempt.
-- 'JobRescheduled' and 'JobRetried' are not terminal -- the job will be picked
-- again -- so @created - (completed + failed + retry_exhausted)@ is the number
-- of jobs still in flight.
--
-- 'JobDequeued' counts jobs read off the redis stream, before the per-job lock
-- is taken; 'JobPicked' counts the ones that went on to actually execute. The
-- gap between them is jobs lost to lock contention or blacklisting. Together
-- with @scheduler_producer_stage_counter@ these trace the full path:
-- created -> picked_from_set -> inserted_to_stream -> dequeued -> picked.
data JobLifecycleStatus
  = JobCreated
  | JobDequeued
  | JobPicked
  | JobCompleted
  | JobFailed
  | JobRescheduled
  | JobRetried
  | JobRetryExhausted
  | JobDuplicate
  deriving (Eq, Show)

jobLifecycleStatusLabel :: JobLifecycleStatus -> Text
jobLifecycleStatusLabel = \case
  JobCreated -> "created"
  JobDequeued -> "dequeued"
  JobPicked -> "picked"
  JobCompleted -> "completed"
  JobFailed -> "failed"
  JobRescheduled -> "rescheduled"
  JobRetried -> "retried"
  JobRetryExhausted -> "retry_exhausted"
  JobDuplicate -> "duplicate"

-- | Record a job lifecycle transition.
--
-- @jobType@ must be the bare job type (e.g. @SendSearchRequestToDriver@). Do
-- not pre-apply 'show' to it: the older @stream_jobs_counter@ call sites do,
-- which is why their @job_type@ label values carry embedded quotes.
incrementJobLifecycleCounter :: CoreMetrics m => Text -> JobLifecycleStatus -> m ()
incrementJobLifecycleCounter jobType status =
  incrementSchedulerJobLifecycleCounter jobType (jobLifecycleStatusLabel status)
