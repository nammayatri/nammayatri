module Beckn.Utils.Callback (withBecknCallback, WithBecknCallback, withBecknCallbackMig, WithBecknCallbackMig) where

import Beckn.Types.Common
import Beckn.Types.Core.API.Callback
import Beckn.Types.Core.Ack
import Beckn.Types.Core.Context
import qualified Beckn.Types.Core.Migration.API.Types as API
import qualified Beckn.Types.Core.Migration.Context as M.Context
import Beckn.Types.Error
import Beckn.Types.Error.BecknAPIError
import Beckn.Types.Monitoring.Prometheus.Metrics (HasCoreMetrics)
import Beckn.Utils.Common (throwError)
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

someExceptionToCallbackReqMig :: M.Context.Context -> SomeException -> API.BecknCallbackReq a
someExceptionToCallbackReqMig context exc =
  let BecknAPIError err = someExceptionToBecknApiError exc
   in API.BecknCallbackReq
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

type WithBecknCallbackMig api callback_success r =
  ( HasCoreMetrics r,
    HasClient ET.EulerClient api,
    Client ET.EulerClient api
      ~ (API.BecknCallbackReq callback_success -> ET.EulerClient AckResponse)
  ) =>
  M.Context.Action ->
  Proxy api ->
  M.Context.Context ->
  BaseUrl ->
  FlowR r callback_success ->
  FlowR r AckResponse

withBecknCallbackMig ::
  Maybe ET.ManagerSelector ->
  WithBecknCallbackMig api callback_success r
withBecknCallbackMig auth action api context cbUrl f = do
  now <- getCurrentTime
  cbAction <- mapToCbAction action
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
  where
    mapToCbAction M.Context.SEARCH = pure M.Context.ON_SEARCH
    mapToCbAction M.Context.SELECT = pure M.Context.ON_SELECT
    mapToCbAction M.Context.INIT = pure M.Context.ON_INIT
    mapToCbAction M.Context.CONFIRM = pure M.Context.ON_CONFIRM
    mapToCbAction M.Context.UPDATE = pure M.Context.ON_UPDATE
    mapToCbAction M.Context.STATUS = pure M.Context.ON_STATUS
    mapToCbAction M.Context.TRACK = pure M.Context.ON_TRACK
    mapToCbAction M.Context.CANCEL = pure M.Context.ON_CANCEL
    mapToCbAction M.Context.FEEDBACK = pure M.Context.ON_FEEDBACK
    mapToCbAction M.Context.SUPPORT = pure M.Context.ON_SUPPORT
    mapToCbAction invalidAction = throwError . InvalidRequest $ "Beckn " <> show invalidAction <> " action doesn't have callback"
