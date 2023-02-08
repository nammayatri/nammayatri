module Tools.Metrics.SendSearchRequestToDriverMetrics
  ( module Tools.Metrics.SendSearchRequestToDriverMetrics,
    module Reexport,
  )
where

import qualified EulerHS.Language as L
import EulerHS.Prelude
import GHC.Records.Extra
import Kernel.Types.Common (Milliseconds)
import Prometheus as P
import Tools.Metrics.SendSearchRequestToDriverMetrics.Types as Reexport

incrementTaskCounter :: HasSendSearchRequestToDriverMetrics m r => Text -> m ()
incrementTaskCounter agencyName = do
  bmContainer <- asks (.ssrMetrics)
  incrementTaskCounter' bmContainer agencyName

incrementFailedTaskCounter :: HasSendSearchRequestToDriverMetrics m r => Text -> m ()
incrementFailedTaskCounter agencyName = do
  bmContainer <- asks (.ssrMetrics)
  incrementFailedTaskCounter' bmContainer agencyName

putTaskDuration :: HasSendSearchRequestToDriverMetrics m r => Text -> Milliseconds -> m ()
putTaskDuration agencyName duration = do
  bmContainer <- asks (.ssrMetrics)
  putTaskDuration' bmContainer agencyName duration

incrementTaskCounter' :: L.MonadFlow m => SendSearchRequestToDriverMetricsContainer -> Text -> m ()
incrementTaskCounter' bmContainer agencyName = do
  let taskCounter = bmContainer.taskCounter
  L.runIO $ P.withLabel taskCounter agencyName P.incCounter

incrementFailedTaskCounter' :: L.MonadFlow m => SendSearchRequestToDriverMetricsContainer -> Text -> m ()
incrementFailedTaskCounter' bmContainer agencyName = do
  let failedTaskCounter = bmContainer.failedTaskCounter
  L.runIO $ P.withLabel failedTaskCounter agencyName P.incCounter

putTaskDuration' :: L.MonadFlow m => SendSearchRequestToDriverMetricsContainer -> Text -> Milliseconds -> m ()
putTaskDuration' bmContainer agencyName duration = do
  let taskDuration = bmContainer.taskDuration
  L.runIO $ P.withLabel taskDuration agencyName (`P.observe` ((/ 1000) . fromIntegral $ duration))
