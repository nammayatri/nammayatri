module Beckn.Utils.Callback (withBecknCallback, WithBecknCallback, withBecknCallbackMig, WithBecknCallbackMig) where

import Beckn.Types.Common
import Beckn.Types.Core.Ack
import qualified Beckn.Types.Core.Cabs.API.Types as API
import qualified Beckn.Types.Core.Cabs.Common.Context as Context
import qualified Beckn.Types.Core.Migration.API.Types as M.API
import qualified Beckn.Types.Core.Migration.Context as M.Context
import Beckn.Types.Error
import Beckn.Types.Error.BaseError.HTTPError.BecknAPIError
import Beckn.Types.Monitoring.Prometheus.Metrics
import Beckn.Utils.Common
import EulerHS.Prelude
import qualified EulerHS.Types as ET
import Servant.Client

toCallbackReq :: Context.Context -> a -> API.BecknCallbackReq a
toCallbackReq context a =
  API.BecknCallbackReq
    { contents = Right a,
      context
    }

someExceptionToCallbackReq :: Context.Context -> SomeException -> API.BecknCallbackReq a
someExceptionToCallbackReq context exc =
  let BecknAPIError err = someExceptionToBecknApiError exc
   in API.BecknCallbackReq
        { contents = Left err,
          context
        }

someExceptionToCallbackReqMig :: M.Context.Context -> SomeException -> M.API.BecknCallbackReq a
someExceptionToCallbackReqMig context exc =
  let BecknAPIError err = someExceptionToBecknApiError exc
   in M.API.BecknCallbackReq
        { contents = Left err,
          context
        }

type WithBecknCallback api callback_success m =
  ( MonadFlow m,
    CoreMetrics m,
    HasClient ET.EulerClient api,
    Client ET.EulerClient api
      ~ (API.BecknCallbackReq callback_success -> ET.EulerClient AckResponse)
  ) =>
  Text ->
  Proxy api ->
  Context.Context ->
  BaseUrl ->
  m callback_success ->
  m AckResponse

withBecknCallback ::
  (m () -> m ()) ->
  Maybe ET.ManagerSelector ->
  WithBecknCallback api callback_success m
withBecknCallback doWithCallback auth action api context cbUrl f = do
  now <- getCurrentTime
  let cbAction = "on_" <> action
  let context' =
        context
          & #timestamp .~ now
  safeFork
    (someExceptionToCallbackReq context')
    (toCallbackReq context')
    action
    (doWithCallback . callBecknAPI auth Nothing cbAction api cbUrl)
    f
  return Ack

type WithBecknCallbackMig api callback_success m =
  ( MonadFlow m,
    CoreMetrics m,
    HasClient ET.EulerClient api,
    Client ET.EulerClient api
      ~ (M.API.BecknCallbackReq callback_success -> ET.EulerClient AckResponse)
  ) =>
  M.Context.Action ->
  Proxy api ->
  M.Context.Context ->
  BaseUrl ->
  m callback_success ->
  m AckResponse

withBecknCallbackMig ::
  (m () -> m ()) ->
  Maybe ET.ManagerSelector ->
  WithBecknCallbackMig api callback_success m
withBecknCallbackMig doWithCallback auth action api context cbUrl f = do
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
    (M.API.BecknCallbackReq cbContext . Right)
    (show action)
    (doWithCallback . callBecknAPI auth Nothing (show cbAction) api cbUrl)
    f
  return Ack
