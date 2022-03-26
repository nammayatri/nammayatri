{-# LANGUAGE TypeApplications #-}

module Tools.FlowHandling where

import Beckn.Prelude
import qualified Beckn.Tools.Metrics.CoreMetrics as Metrics
import Beckn.Types.App
import Beckn.Types.Common
import Beckn.Types.Error.BaseError.HTTPError
import Beckn.Types.Flow
import Beckn.Utils.Error.FlowHandling hiding (apiHandler, withFlowHandlerAPI)
import Beckn.Utils.Logging
import Data.Aeson (Value (Null))
import qualified "fmd-wrapper" ExternalAPI.Dunzo.Types as API

toError :: Maybe Text -> API.Error
toError mbMessage =
  API.Error
    { code = "internal_server_error",
      message = fromMaybe "unknown_error" mbMessage,
      details = Null
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
  | Just err <- fromException @API.Error exc = throwHTTPError err
  | Just (BaseException err) <- fromException exc = throwHTTPError (toError . toMessage $ err)
  | otherwise = throwHTTPError (toError . Just . show $ exc)

throwHTTPError ::
  ( Log m,
    MonadThrow m
  ) =>
  API.Error ->
  m b
throwHTTPError err = do
  let someExc = toException err
  logError $ makeLogSomeException someExc
  throwServantError (toHttpCode err) (toCustomHeaders err) err
