{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Tools.Metrics.AllocatorMetrics.Types
  ( HasAllocatorMetrics,
    AllocatorMetricsContainer (..),
    module CoreMetrics,
    registerAllocatorMetricsContainer,
  )
where

import EulerHS.Prelude
import Kernel.Tools.Metrics.CoreMetrics as CoreMetrics
import Kernel.Utils.Common
import Prometheus as P

type HasAllocatorMetrics m r = (HasFlowEnv m r '["btmMetrics" ::: AllocatorMetricsContainer])

type TaskCounterMetric = P.Vector P.Label1 P.Counter

type TaskDurationMetric = P.Vector P.Label1 P.Histogram

type FailedTaskCounterMetric = P.Vector P.Label1 P.Counter

data AllocatorMetricsContainer = AllocatorMetricsContainer
  { taskCounter :: TaskCounterMetric,
    taskDuration :: TaskDurationMetric,
    failedTaskCounter :: FailedTaskCounterMetric
  }

registerAllocatorMetricsContainer :: IO AllocatorMetricsContainer
registerAllocatorMetricsContainer = do
  taskCounter <- registerTaskCounter
  taskDuration <- registerTaskDurationMetric
  failedTaskCounter <- registerFailedTaskCounter
  return $ AllocatorMetricsContainer {..}

registerTaskCounter :: IO TaskCounterMetric
registerTaskCounter = P.register . P.vector "agency_name" . P.counter $ P.Info "BTM_task_count" ""

registerFailedTaskCounter :: IO FailedTaskCounterMetric
registerFailedTaskCounter = P.register . P.vector "agency_name" . P.counter $ P.Info "BTM_failed_task_count" ""

registerTaskDurationMetric :: IO TaskDurationMetric
registerTaskDurationMetric = P.register . P.vector "agency_name" . P.histogram (P.Info "BTM_task_duration" "") $ P.linearBuckets 0 0.1 20
