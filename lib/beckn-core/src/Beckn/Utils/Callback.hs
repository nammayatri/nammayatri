module Beckn.Utils.Callback (withBecknCallback, WithBecknCallback) where

import Beckn.Types.Common
import Beckn.Types.Core.API.Callback
import Beckn.Types.Core.Ack
import Beckn.Types.Core.Context
import Beckn.Types.Error.BecknAPIError
import Beckn.Types.Monitoring.Prometheus.Metrics (HasCoreMetrics)
import Beckn.Utils.Error.BecknAPIError
import Beckn.Utils.Error.FlowHandling
import Beckn.Utils.Flow
import EulerHS.Prelude
import qualified EulerHS.Types as ET
import Servant.Client

toCallbackReq :: Context -> a -> CallbackReq a
toCallbackReq context a =
  CallbackReq
    { contents = Right a,
      context
    }

someExceptionToCallbackReq :: Context -> SomeException -> CallbackReq a
someExceptionToCallbackReq context exc =
  let BecknAPIError err = someExceptionToBecknApiError exc
   in CallbackReq
        { contents = Left err,
          context
        }

type WithBecknCallback api callback_success r =
  ( HasCoreMetrics r,
    HasClient ET.EulerClient api,
    Client ET.EulerClient api
      ~ (CallbackReq callback_success -> ET.EulerClient AckResponse)
  ) =>
  Text ->
  Proxy api ->
  Context ->
  BaseUrl ->
  FlowR r callback_success ->
  FlowR r AckResponse

withBecknCallback ::
  Maybe ET.ManagerSelector ->
  WithBecknCallback api callback_success r
withBecknCallback auth action api context cbUrl f = do
  now <- getCurrentTime
  let cbAction = "on_" <> action
  let context' =
        context
          & #action .~ cbAction
          & #timestamp .~ now
  safeFork
    (someExceptionToCallbackReq context')
    (toCallbackReq context')
    action
    (callBecknAPI auth Nothing cbAction api cbUrl)
    f
  return Ack
