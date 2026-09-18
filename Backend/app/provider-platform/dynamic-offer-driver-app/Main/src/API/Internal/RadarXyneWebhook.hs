module API.Internal.RadarXyneWebhook
  ( API,
    handler,
  )
where

import qualified Domain.Action.UI.RadarXyneWebhook
import Environment
import EulerHS.Prelude
import Kernel.External.Ticket.XyneSpaces.Webhook (RawByteString, RawJson)
import qualified Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Utils.Common
import Servant

-- | Webhook target for the RADAR desk channel's Xyne app (Control Center
-- support tickets) — signed with 'radarXyneCfg.webhookSigningSecret',
-- independent of the in-app issue-desk webhook mounted at @/xyne/webhook@.
type API =
  "radar" :> "xyne" :> "webhook"
    :> Header "X-Xyne-Signature" Kernel.Prelude.Text
    :> ReqBody '[RawJson, OctetStream] RawByteString
    :> Post '[JSON] APISuccess

handler :: FlowServer API
handler = postRadarXyneWebhook
  where
    postRadarXyneWebhook mbSig rawBody =
      withFlowHandlerAPI $ Domain.Action.UI.RadarXyneWebhook.postRadarXyneWebhook mbSig rawBody
