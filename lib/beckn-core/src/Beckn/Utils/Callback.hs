module Beckn.Utils.Callback (withBecknCallback, WithBecknCallback, withBecknCallbackMig, WithBecknCallbackMig) where

import Beckn.Types.Common
import Beckn.Types.Core.Ack
import qualified Beckn.Types.Core.Migration.API.Types as M.API
import qualified Beckn.Types.Core.Migration.Context as M.Context
import qualified Beckn.Types.Core.ReqTypes as API
import qualified Beckn.Types.Core.Taxi.Common.Context as Context
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
withBecknCallback doWithCallback auth actionName api context cbUrl action = do
  now <- getCurrentTime
  let cbAction = "on_" <> actionName
  let context' =
        context
          & #timestamp .~ now
  forkBecknCallback
    (someExceptionToCallbackReq context')
    (toCallbackReq context')
    (doWithCallback . void . callBecknAPI auth Nothing cbAction api cbUrl)
    actionName
    action
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
withBecknCallbackMig doWithCallback auth actionName api context cbUrl action = do
  now <- getCurrentTime
  cbAction <-
    M.Context.mapToCbAction actionName
      & fromMaybeM (InternalError $ "Beckn " <> show actionName <> " action doesn't have callback")
  let cbContext =
        context
          & #action .~ cbAction
          & #timestamp .~ now
  forkBecknCallback
    (someExceptionToCallbackReqMig cbContext)
    (M.API.BecknCallbackReq cbContext . Right)
    (doWithCallback . void . callBecknAPI auth Nothing (show cbAction) api cbUrl)
    (show actionName)
    action
  return Ack

forkBecknCallback ::
  (Forkable m, MonadCatch m, Log m) =>
  (SomeException -> result) ->
  (success -> result) ->
  (result -> m ()) ->
  Text ->
  m success ->
  m ()
forkBecknCallback fromError fromSuccess doWithResult actionName action =
  fork actionName $
    try action >>= \case
      Right success -> doWithResult $ fromSuccess success
      Left err -> do
        logError $ "Error executing callback action " <> actionName <> ": " <> show err
        doWithResult $ fromError err
