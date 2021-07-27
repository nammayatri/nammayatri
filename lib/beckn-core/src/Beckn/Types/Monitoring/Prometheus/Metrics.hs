module Beckn.Types.Monitoring.Prometheus.Metrics
  ( HasCoreMetrics,
    CoreMetrics (..),
    CoreMetricsContainer (..),
    registerCoreMetricsContainer,
  )
where

import Beckn.Types.Error.BaseError.HTTPError
import Beckn.Types.Time (Milliseconds)
import EulerHS.Prelude as E
import GHC.Records.Extra
import Prometheus as P
import Servant.Client (BaseUrl, ClientError)

type RequestLatencyMetric = P.Vector P.Label3 P.Histogram

type ErrorCounterMetric = P.Vector P.Label2 P.Counter

type URLCallRetriesMetric = P.Vector P.Label2 P.Histogram

type HasCoreMetrics r =
  ( HasField "coreMetrics" r CoreMetricsContainer
  )

class CoreMetrics m where
  addRequestLatency ::
    Text ->
    Text ->
    Milliseconds ->
    Either ClientError a ->
    m ()
  incrementErrorCounter :: IsHTTPException e => e -> m ()
  addUrlCallRetries :: BaseUrl -> Int -> Int -> m ()

data CoreMetricsContainer = CoreMetricsContainer
  { requestLatency :: RequestLatencyMetric,
    errorCounter :: ErrorCounterMetric,
    urlCallRetriesCounter :: URLCallRetriesMetric
  }

registerCoreMetricsContainer :: IO CoreMetricsContainer
registerCoreMetricsContainer = do
  requestLatency <- registerRequestLatencyMetric
  errorCounter <- registerErrorCounterMetric
  urlCallRetriesCounter <- registerURLCallRetriesMetric
  return CoreMetricsContainer {..}

registerRequestLatencyMetric :: IO RequestLatencyMetric
registerRequestLatencyMetric =
  P.register $
    P.vector ("host", "service", "status") $
      P.histogram info P.defaultBuckets
  where
    info = P.Info "external_request_duration" ""

registerErrorCounterMetric :: IO ErrorCounterMetric
registerErrorCounterMetric =
  P.register $
    P.vector ("HttpCode", "ErrorCode") $
      P.counter info
  where
    info = P.Info "error_counter" ""

registerURLCallRetriesMetric :: IO URLCallRetriesMetric
registerURLCallRetriesMetric =
  P.register $
    P.vector ("URL", "Max_retry_count") $
      P.histogram info P.defaultBuckets
  where
    info = P.Info "url_call_retries_counter" ""
