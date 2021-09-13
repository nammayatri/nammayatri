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

type URLCallRetriesMetric = P.Vector P.Label2 P.Counter

type URLCallRetryFailuresMetric = P.Vector P.Label1 P.Counter

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
  addUrlCallRetries :: BaseUrl -> Int -> m ()
  addUrlCallRetryFailures :: BaseUrl -> m ()

data CoreMetricsContainer = CoreMetricsContainer
  { requestLatency :: RequestLatencyMetric,
    errorCounter :: ErrorCounterMetric,
    urlCallRetries :: URLCallRetriesMetric,
    urlCallRetryFailures :: URLCallRetryFailuresMetric
  }

registerCoreMetricsContainer :: IO CoreMetricsContainer
registerCoreMetricsContainer = do
  requestLatency <- registerRequestLatencyMetric
  errorCounter <- registerErrorCounterMetric
  urlCallRetries <- registerURLCallRetriesMetric
  urlCallRetryFailures <- registerURLCallRetryFailuresMetric

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
    P.vector ("URL", "RetryCount") $
      P.counter info
  where
    info = P.Info "url_call_retries_counter" ""

registerURLCallRetryFailuresMetric :: IO URLCallRetryFailuresMetric
registerURLCallRetryFailuresMetric =
  P.register $
    P.vector "URL" $
      P.counter info
  where
    info = P.Info "url_call_retry_failures_counter" ""
