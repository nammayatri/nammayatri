module API.Internal.RadarXyneWebhook
  ( API,
    handler,
  )
where

import qualified Domain.Action.UI.RadarXyneWebhook
import Environment
import EulerHS.Prelude
import qualified IssueManagement.Domain.Action.UI.XyneWebhook as XyneShared
import Kernel.External.Ticket.XyneSpaces.Webhook (RawByteString, RawJson)
import qualified Kernel.Prelude
import Kernel.Utils.Common
import Servant

-- | Inbound @DESK_REPLY@ webhook for the RADAR desk's Xyne app (Control
-- Center tickets), separate from the in-app issue desk's @/xyne/webhook@
-- because the two Xyne apps sign with different secrets.
--
-- @mocId@ names any city row of the merchant holding the app's signing
-- secret; replies for tickets of every city arrive on this one URL.
type API =
  "radar" :> "xyne" :> "webhook"
    :> QueryParam "mocId" Kernel.Prelude.Text
    :> Header "X-Xyne-Signature" Kernel.Prelude.Text
    :> ReqBody '[RawJson, OctetStream] RawByteString
    :> Post '[JSON] XyneShared.XyneWebhookAck

handler :: FlowServer API
handler = postRadarXyneWebhook
  where
    postRadarXyneWebhook mbMocId mbSig rawBody =
      withFlowHandlerAPI $ Domain.Action.UI.RadarXyneWebhook.postRadarXyneWebhook mbMocId mbSig rawBody
