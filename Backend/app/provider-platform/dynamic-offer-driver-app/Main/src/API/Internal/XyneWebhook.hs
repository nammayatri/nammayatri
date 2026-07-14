module API.Internal.XyneWebhook
  ( API,
    handler,
    BearerAPI,
    bearerHandler,
  )
where

import qualified Domain.Action.UI.XyneWebhook
import Environment
import EulerHS.Prelude
import qualified IssueManagement.Domain.Action.UI.XyneWebhook as XyneShared
import Kernel.External.Ticket.XyneSpaces.Webhook (RawByteString, RawJson)
import qualified Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Utils.Common
import Servant

type API =
  "xyne" :> "webhook"
    :> Header "X-Xyne-Signature" Kernel.Prelude.Text
    :> ReqBody '[RawJson, OctetStream] RawByteString
    :> Post '[JSON] XyneShared.XyneWebhookAck

handler :: FlowServer API
handler = postXyneWebhook
  where
    postXyneWebhook mbSig rawBody =
      withFlowHandlerAPI $ Domain.Action.UI.XyneWebhook.postXyneWebhook mbSig rawBody

type BearerAPI =
  "xyne" :> "webhook" :> "bearer"
    :> Header "Authorization" Kernel.Prelude.Text
    :> ReqBody '[RawJson, OctetStream] RawByteString
    :> Post '[JSON] APISuccess

bearerHandler :: FlowServer BearerAPI
bearerHandler = postXyneBearerWebhook
  where
    postXyneBearerWebhook mbAuth rawBody =
      withFlowHandlerAPI $ Domain.Action.UI.XyneWebhook.postXyneBearerWebhook mbAuth rawBody
