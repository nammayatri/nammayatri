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

incrementTaskCounter :: HasAllocatorMetrics m r => Text -> m ()
incrementTaskCounter agencyName = do
  bmContainer <- asks (.btmMetrics)
  incrementTaskCounter' bmContainer agencyName

incrementFailedTaskCounter :: HasAllocatorMetrics m r => Text -> m ()
incrementFailedTaskCounter agencyName = do
  bmContainer <- asks (.btmMetrics)
  incrementFailedTaskCounter' bmContainer agencyName

putTaskDuration :: HasAllocatorMetrics m r => Text -> Milliseconds -> m ()
putTaskDuration agencyName duration = do
  bmContainer <- asks (.btmMetrics)
  putTaskDuration' bmContainer agencyName duration

incrementTaskCounter' :: L.MonadFlow m => AllocatorMetricsContainer -> Text -> m ()
incrementTaskCounter' bmContainer agencyName = do
  let taskCounter = bmContainer.taskCounter
  L.runIO $ P.withLabel taskCounter agencyName P.incCounter

incrementFailedTaskCounter' :: L.MonadFlow m => AllocatorMetricsContainer -> Text -> m ()
incrementFailedTaskCounter' bmContainer agencyName = do
  let failedTaskCounter = bmContainer.failedTaskCounter
  L.runIO $ P.withLabel failedTaskCounter agencyName P.incCounter

putTaskDuration' :: L.MonadFlow m => AllocatorMetricsContainer -> Text -> Milliseconds -> m ()
putTaskDuration' bmContainer agencyName duration = do
  let taskDuration = bmContainer.taskDuration
  L.runIO $ P.withLabel taskDuration agencyName (`P.observe` ((/ 1000) . fromIntegral $ duration))
