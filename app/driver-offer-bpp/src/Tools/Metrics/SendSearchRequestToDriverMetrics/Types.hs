module Tools.Metrics.SendSearchRequestToDriverMetrics.Types
  ( HasSendSearchRequestToDriverMetrics,
    SendSearchRequestToDriverMetricsContainer (..),
    module CoreMetrics,
    registerSendSearchRequestToDriverMetricsContainer,
  )
where

import Beckn.Tools.Metrics.CoreMetrics as CoreMetrics
import Beckn.Utils.Common
import EulerHS.Prelude
import Prometheus as P

type HasSendSearchRequestToDriverMetrics m r = (HasFlowEnv m r '["ssrMetrics" ::: SendSearchRequestToDriverMetricsContainer])

type TaskCounterMetric = P.Vector P.Label1 P.Counter

type TaskDurationMetric = P.Vector P.Label1 P.Histogram

type FailedTaskCounterMetric = P.Vector P.Label1 P.Counter

data SendSearchRequestToDriverMetricsContainer = SendSearchRequestToDriverMetricsContainer
  { taskCounter :: TaskCounterMetric,
    taskDuration :: TaskDurationMetric,
    failedTaskCounter :: FailedTaskCounterMetric
  }

registerSendSearchRequestToDriverMetricsContainer :: IO SendSearchRequestToDriverMetricsContainer
registerSendSearchRequestToDriverMetricsContainer = do
  taskCounter <- registerTaskCounter
  taskDuration <- registerTaskDurationMetric
  failedTaskCounter <- registerFailedTaskCounter
  return $ SendSearchRequestToDriverMetricsContainer {..}

registerTaskCounter :: IO TaskCounterMetric
registerTaskCounter = P.register . P.vector "agency_name" . P.counter $ P.Info "BTM_task_count" ""

registerFailedTaskCounter :: IO FailedTaskCounterMetric
registerFailedTaskCounter = P.register . P.vector "agency_name" . P.counter $ P.Info "BTM_failed_task_count" ""

registerTaskDurationMetric :: IO TaskDurationMetric
registerTaskDurationMetric = P.register . P.vector "agency_name" . P.histogram (P.Info "BTM_task_duration" "") $ P.linearBuckets 0 0.1 20
