module Tools.Metrics.AllocatorMetrics
  ( module Tools.Metrics.AllocatorMetrics,
    module Reexport,
  )
where

import Beckn.Types.Common (Milliseconds)
import qualified EulerHS.Language as L
import EulerHS.Prelude
import GHC.Records.Extra
import Prometheus as P
import Tools.Metrics.AllocatorMetrics.Types as Reexport

incrementTaskCounter :: HasAllocatorMetrics m r => m ()
incrementTaskCounter = do
  bmContainer <- asks (.btmMetrics)
  incrementTaskCounter' bmContainer

incrementFailedTaskCounter :: HasAllocatorMetrics m r => m ()
incrementFailedTaskCounter = do
  bmContainer <- asks (.btmMetrics)
  incrementFailedTaskCounter' bmContainer

putTaskDuration :: HasAllocatorMetrics m r => Milliseconds -> m ()
putTaskDuration duration = do
  bmContainer <- asks (.btmMetrics)
  putTaskDuration' bmContainer duration

incrementTaskCounter' :: L.MonadFlow m => AllocatorMetricsContainer -> m ()
incrementTaskCounter' bmContainer = do
  let taskCounter = bmContainer.taskCounter
  L.runIO $ P.incCounter taskCounter

incrementFailedTaskCounter' :: L.MonadFlow m => AllocatorMetricsContainer -> m ()
incrementFailedTaskCounter' bmContainer = do
  let failedTaskCounter = bmContainer.failedTaskCounter
  L.runIO $ P.incCounter failedTaskCounter

putTaskDuration' :: L.MonadFlow m => AllocatorMetricsContainer -> Milliseconds -> m ()
putTaskDuration' bmContainer duration = do
  let taskDuration = bmContainer.taskDuration
  L.runIO $ P.observe taskDuration . (/ 1000) . fromIntegral $ duration
