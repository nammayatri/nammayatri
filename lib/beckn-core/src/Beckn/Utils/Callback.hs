module Beckn.Utils.Callback (withBecknCallback, WithBecknCallback, withBecknCallbackMig, WithBecknCallbackMig) where

import Beckn.Types.Common
import Beckn.Types.Core.API.Callback
import Beckn.Types.Core.Ack
import Beckn.Types.Core.Context
import qualified Beckn.Types.Core.Migration.API.Types as API
import qualified Beckn.Types.Core.Migration.Context as M.Context
import Beckn.Types.Error
import Beckn.Types.Error.BecknAPIError
import Beckn.Types.Monitoring.Prometheus.Metrics
import Beckn.Utils.Common
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

someExceptionToCallbackReqMig :: M.Context.Context -> SomeException -> API.BecknCallbackReq a
someExceptionToCallbackReqMig context exc =
  let BecknAPIError err = someExceptionToBecknApiError exc
   in API.BecknCallbackReq
        { contents = Left err,
          context
        }

type WithBecknCallback api callback_success m =
  ( MonadFlow m,
    CoreMetrics m,
    HasClient ET.EulerClient api,
    Client ET.EulerClient api
      ~ (CallbackReq callback_success -> ET.EulerClient AckResponse)
  ) =>
  Text ->
  Proxy api ->
  Context ->
  BaseUrl ->
  m callback_success ->
  m AckResponse

withBecknCallback ::
  Maybe ET.ManagerSelector ->
  WithBecknCallback api callback_success m
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

type WithBecknCallbackMig api callback_success m =
  ( MonadFlow m,
    CoreMetrics m,
    HasClient ET.EulerClient api,
    Client ET.EulerClient api
      ~ (API.BecknCallbackReq callback_success -> ET.EulerClient AckResponse)
  ) =>
  M.Context.Action ->
  Proxy api ->
  M.Context.Context ->
  BaseUrl ->
  m callback_success ->
  m AckResponse

withBecknCallbackMig ::
  Maybe ET.ManagerSelector ->
  WithBecknCallbackMig api callback_success m
withBecknCallbackMig auth action api context cbUrl f = do
  now <- getCurrentTime
  cbAction <-
    M.Context.mapToCbAction action
      & fromMaybeM (InternalError $ "Beckn " <> show action <> " action doesn't have callback")
  let cbContext =
        context
          & #action .~ cbAction
          & #timestamp .~ now
  safeFork
    (someExceptionToCallbackReqMig cbContext)
    (API.BecknCallbackReq cbContext . Right)
    (show action)
    (callBecknAPI auth Nothing (show cbAction) api cbUrl)
    f
  return Ack
