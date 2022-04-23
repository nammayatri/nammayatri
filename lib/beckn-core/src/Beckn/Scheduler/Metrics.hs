module Beckn.Scheduler.Metrics where

import Beckn.Prelude
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
