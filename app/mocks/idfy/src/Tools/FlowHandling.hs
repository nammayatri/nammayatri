{-# LANGUAGE TypeApplications #-}

module Tools.FlowHandling where

import Kernel.Prelude
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.App
import Kernel.Types.Common
import Kernel.Types.Error.BaseError.HTTPError hiding (Error)
import Kernel.Types.Flow
import Kernel.Utils.Error.FlowHandling hiding (apiHandler, withFlowHandlerAPI)
import Kernel.Utils.Logging
import Types.Common

toError :: Maybe Text -> Error
toError mbMessage =
  Error
    { message = fromMaybe "unknown_error" mbMessage
    }

withFlowHandlerAPI ::
  ( Metrics.CoreMetrics (FlowR r),
    HasField "isShuttingDown" r (TMVar ()),
    Log (FlowR r)
  ) =>
  FlowR r a ->
  FlowHandlerR r a
withFlowHandlerAPI = withFlowHandler . apiHandler . handleIfUp

apiHandler ::
  ( MonadCatch m,
    Log m
  ) =>
  m a ->
  m a
apiHandler = (`catch` someExceptionToAPIErrorThrow)

someExceptionToAPIErrorThrow ::
  ( MonadCatch m,
    Log m
  ) =>
  SomeException ->
  m a
someExceptionToAPIErrorThrow exc
  | Just err <- fromException @Error exc = throwHTTPError err
  | Just (BaseException err) <- fromException exc = throwHTTPError (toError . toMessage $ err)
  | otherwise = throwHTTPError (toError . Just . show $ exc)

throwHTTPError ::
  ( Log m,
    MonadThrow m
  ) =>
  Error ->
  m b
throwHTTPError err = do
  let someExc = toException err
  logError $ makeLogSomeException someExc
  throwServantError (toHttpCode err) (toCustomHeaders err) err
