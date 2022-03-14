{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Beckn.Tools.Metrics.CoreMetrics
  ( module Beckn.Tools.Metrics.CoreMetrics,
    module Reexport,
  )
where

import Beckn.Tools.Metrics.CoreMetrics.Types as Reexport
import Beckn.Types.Error (GenericError (InternalError))
import Beckn.Types.Error.BaseError.HTTPError (BaseException (..), HTTPException (..), IsBaseError (toMessage), IsHTTPError (toErrorCode, toHttpCode), IsHTTPException)
import Beckn.Types.Time (Milliseconds, getMilliseconds)
import Beckn.Utils.Servant.BaseUrl
import Data.Text as DT
import qualified EulerHS.Language as L
import EulerHS.Prelude as E
import GHC.Records.Extra
import Prometheus as P
import Servant.Client (BaseUrl, ClientError (..), ResponseF (..))

incrementErrorCounterImplementation ::
  ( HasCoreMetrics r,
    L.MonadFlow m,
    MonadReader r m
  ) =>
  Text ->
  SomeException ->
  m ()
incrementErrorCounterImplementation errorContext err = do
  cmContainer <- asks (.coreMetrics)
  incrementErrorCounterImplementation' cmContainer errorContext err

addUrlCallRetriesImplementation ::
  ( HasCoreMetrics r,
    L.MonadFlow m,
    MonadReader r m
  ) =>
  BaseUrl ->
  Int ->
  m ()
addUrlCallRetriesImplementation url retryCount = do
  cmContainer <- asks (.coreMetrics)
  addUrlCallRetriesImplementation' cmContainer url retryCount

addUrlCallFailuresImplementation ::
  ( HasCoreMetrics r,
    L.MonadFlow m,
    MonadReader r m
  ) =>
  BaseUrl ->
  m ()
addUrlCallFailuresImplementation url = do
  cmContainer <- asks (.coreMetrics)
  addUrlCallFailuresImplementation' cmContainer url

addRequestLatencyImplementation ::
  ( HasCoreMetrics r,
    L.MonadFlow m,
    MonadReader r m
  ) =>
  Text ->
  Text ->
  Milliseconds ->
  Either ClientError a ->
  m ()
addRequestLatencyImplementation host serviceName dur status = do
  cmContainer <- asks (.coreMetrics)
  addRequestLatencyImplementation' cmContainer host serviceName dur status

addRequestLatencyImplementation' ::
  L.MonadFlow m =>
  CoreMetricsContainer ->
  Text ->
  Text ->
  Milliseconds ->
  Either ClientError a ->
  m ()
addRequestLatencyImplementation' cmContainer host serviceName dur status = do
  let requestLatencyMetric = cmContainer.requestLatency
  L.runIO $
    P.withLabel
      requestLatencyMetric
      (host, serviceName, status')
      (`P.observe` ((/ 1000) . fromIntegral $ getMilliseconds dur))
  where
    status' =
      case status of
        Right _ -> "200"
        Left (FailureResponse _ (Response code _ _ _)) -> show code
        Left (DecodeFailure _ (Response code _ _ _)) -> show code
        Left (InvalidContentTypeHeader (Response code _ _ _)) -> show code
        Left (UnsupportedContentType _ (Response code _ _ _)) -> show code
        Left (ConnectionError _) -> "Connection error"

incrementErrorCounterImplementation' :: L.MonadFlow m => CoreMetricsContainer -> Text -> SomeException -> m ()
incrementErrorCounterImplementation' cmContainers errorContext exc
  | Just (HTTPException err) <- fromException exc = incCounter' err
  | Just (BaseException err) <- fromException exc = incCounter' . InternalError . fromMaybe (show err) $ toMessage err
  | otherwise = incCounter' . InternalError $ show exc
  where
    errorCounterMetric = cmContainers.errorCounter

    incCounter' :: (L.MonadFlow m, IsHTTPException e) => e -> m ()
    incCounter' err =
      L.runIO $
        P.withLabel
          errorCounterMetric
          (show $ toHttpCode err, errorContext, toErrorCode err)
          P.incCounter

addUrlCallRetriesImplementation' :: L.MonadFlow m => CoreMetricsContainer -> BaseUrl -> Int -> m ()
addUrlCallRetriesImplementation' cmContainers url retryCount = do
  let urlCallRetriesMetric = cmContainers.urlCallRetries
  L.runIO $
    P.withLabel
      urlCallRetriesMetric
      (showBaseUrlText url, show retryCount)
      P.incCounter

addUrlCallFailuresImplementation' :: L.MonadFlow m => CoreMetricsContainer -> BaseUrl -> m ()
addUrlCallFailuresImplementation' cmContainers url = do
  let urlCallRetriesMetric = cmContainers.urlCallRetryFailures
  L.runIO $
    P.withLabel
      urlCallRetriesMetric
      (showBaseUrlText url)
      P.incCounter
