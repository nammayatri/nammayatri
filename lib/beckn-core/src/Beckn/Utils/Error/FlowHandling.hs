{-# LANGUAGE TypeApplications #-}

module Beckn.Utils.Error.FlowHandling
  ( withFlowHandlerAPI,
    withFlowHandlerBecknAPI,
    withFlowHandlerBecknAPI',
    apiHandler,
    becknApiHandler,
    someExceptionToBecknApiError,
  )
where

import Beckn.Types.App
import Beckn.Types.Common
import Beckn.Types.Core.Ack
import Beckn.Types.Error.API as Err
import Beckn.Types.Error.APIError
import Beckn.Types.Error.BecknAPIError
import Beckn.Types.Monitoring.Prometheus.Metrics (HasCoreMetrics)
import Beckn.Utils.Logging
import Beckn.Utils.Monitoring.Prometheus.Metrics (incrementErrorCounter)
import Control.Concurrent.STM (isEmptyTMVar)
import Control.Monad.Reader
import qualified Data.Aeson as A
import qualified EulerHS.Language as L
import EulerHS.Prelude
import GHC.Records
import Network.HTTP.Types (Header, hContentType)
import Network.HTTP.Types.Header (HeaderName)
import Servant (ServerError (..))

withFlowHandler :: FlowR r a -> FlowHandlerR r a
withFlowHandler flow = do
  (EnvR flowRt appEnv) <- ask
  liftIO . runFlowR flowRt appEnv $ flow

withFlowHandlerAPI :: (HasCoreMetrics r, HasField "isShuttingDown" r (TMVar ())) => FlowR r a -> FlowHandlerR r a
withFlowHandlerAPI = withFlowHandler . apiHandler . handleIfUp

withFlowHandlerBecknAPI :: (HasCoreMetrics r, HasField "isShuttingDown" r (TMVar ())) => FlowR r AckResponse -> FlowHandlerR r AckResponse
withFlowHandlerBecknAPI = withFlowHandlerBecknAPI'

withFlowHandlerBecknAPI' :: (HasCoreMetrics r, HasField "isShuttingDown" r (TMVar ())) => FlowR r a -> FlowHandlerR r a
withFlowHandlerBecknAPI' = withFlowHandler . becknApiHandler . handleIfUp

handleIfUp ::
  (HasField "isShuttingDown" r (TMVar ()), HasCoreMetrics r) =>
  FlowR r a ->
  FlowR r a
handleIfUp flow = do
  appEnv <- ask
  let shutdown = getField @"isShuttingDown" appEnv
  shouldRun <- L.runIO $ atomically $ isEmptyTMVar shutdown
  if shouldRun
    then flow
    else throwApiError ServerUnavailable

apiHandler :: (L.MonadFlow m, MonadReader r m, MonadCatch m, Log m, HasCoreMetrics r) => m a -> m a
apiHandler = (`catch` someExceptionToApiErrorThrow)

becknApiHandler :: (L.MonadFlow m, MonadReader r m, MonadCatch m, Log m, HasCoreMetrics r) => m a -> m a
becknApiHandler = (`catch` someExceptionToBecknApiErrorThrow)

someExceptionToApiErrorThrow :: (L.MonadFlow m, MonadReader r m, MonadCatch m, Log m, HasCoreMetrics r) => SomeException -> m a
someExceptionToApiErrorThrow exc
  | Just (APIException err) <- fromException exc = throwApiError err
  | otherwise = throwApiError . InternalError $ show exc

someExceptionToBecknApiErrorThrow :: (L.MonadFlow m, MonadReader r m, MonadCatch m, Log m, HasCoreMetrics r) => SomeException -> m a
someExceptionToBecknApiErrorThrow exc
  | Just (BecknAPIException err) <- fromException exc = throwBecknApiError err
  | Just (APIException err) <- fromException exc =
    throwIsApiError becknAPIErrorFromAPIError err
  | otherwise =
    throwIsApiError becknAPIErrorFromAPIError . InternalError $ show exc

someExceptionToBecknApiError :: SomeException -> BecknAPIError
someExceptionToBecknApiError exc
  | Just (BecknAPIException err) <- fromException exc = toBecknAPIError err
  | Just (APIException err) <- fromException exc = becknAPIErrorFromAPIError err
  | otherwise = becknAPIErrorFromAPIError . InternalError $ show exc

pushMetrics ::
  ( L.MonadFlow m,
    MonadReader r m,
    MonadCatch m,
    Log m,
    HasCoreMetrics r,
    IsAPIException e
  ) =>
  e ->
  m ()
pushMetrics err = do
  errCounterMetric <- getField @"metricsErrorCounter" <$> ask
  incrementErrorCounter errCounterMetric err

throwApiError ::
  ( L.MonadFlow m,
    MonadReader r m,
    Log m,
    MonadThrow m,
    IsAPIError e,
    Exception e,
    HasCoreMetrics r
  ) =>
  e ->
  m a
throwApiError = throwIsApiError toAPIError

throwBecknApiError ::
  ( L.MonadFlow m,
    MonadReader r m,
    Log m,
    MonadThrow m,
    IsBecknAPIError e,
    Exception e,
    HasCoreMetrics r
  ) =>
  e ->
  m a
throwBecknApiError = throwIsApiError toBecknAPIError

throwIsApiError ::
  ( L.MonadFlow m,
    MonadReader r m,
    ToJSON j,
    Log m,
    MonadThrow m,
    IsAPIError e,
    Exception e,
    HasCoreMetrics r
  ) =>
  (e -> j) ->
  e ->
  m b
throwIsApiError toJsonError err = do
  logError $ toLogMessageAPIError err
  pushMetrics err
  throwServantError (toHttpCode err) (toCustomHeaders err) (toJsonError err)

throwServantError ::
  (ToJSON a, Log m, MonadThrow m) =>
  HttpCode ->
  [Header] ->
  a ->
  m b
throwServantError httpCode customHeaders jsonError = withLogTag "HTTP_ERROR" $ do
  let body = A.encode jsonError
  let serverErr = toServerError httpCode
  throwM
    serverErr
      { errBody = body,
        errHeaders = jsonHeader : customHeaders ++ errHeaders serverErr
      }
  where
    jsonHeader :: (HeaderName, ByteString)
    jsonHeader = (hContentType, "application/json;charset=utf-8")

becknAPIErrorFromAPIError :: IsAPIError e => e -> BecknAPIError
becknAPIErrorFromAPIError e =
  let _type =
        if isInternalError (toHttpCode e)
          then INTERNAL_ERROR
          else DOMAIN_ERROR
   in BecknAPIError
        Error
          { _type,
            code = toErrorCode e,
            path = Nothing,
            message = toMessageIfNotInternal e
          }
