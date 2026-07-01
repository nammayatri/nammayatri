module API.Internal.XyneWebhook
  ( API,
    handler,
  )
where

import qualified Domain.Action.UI.XyneWebhook
import Environment
import EulerHS.Prelude
import qualified IssueManagement.Domain.Action.UI.XyneWebhook as XyneShared
import Kernel.External.Ticket.XyneSpaces.Webhook (RawByteString)
import qualified Kernel.Prelude
import Kernel.Utils.Common
import Servant

type API =
  "xyne" :> "webhook"
    :> Header "X-Xyne-Signature" Kernel.Prelude.Text
    :> ReqBody '[OctetStream] RawByteString
    :> Post '[JSON] XyneShared.XyneWebhookAck

handler :: FlowServer API
handler = postXyneWebhook
  where
    postXyneWebhook mbSig rawBody =
      withFlowHandlerAPI $ Domain.Action.UI.XyneWebhook.postXyneWebhook mbSig rawBody
