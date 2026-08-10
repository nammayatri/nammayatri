module API.Internal.XyneWebhook
  ( API,
    handler,
    BearerAPI,
    bearerHandler,
    IssuesAPI,
    issuesHandler,
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

-- | Bearer-token authenticated read endpoint for Xyne to page through issues
-- that changed after a @since@ cursor, so it can catch up on syncs that were
-- dropped in transit.
type IssuesAPI =
  "xyne" :> "webhook" :> "issues"
    :> Header "Authorization" Kernel.Prelude.Text
    :> QueryParam "since" Kernel.Prelude.UTCTime
    :> QueryParam "limit" Int
    :> QueryParam "offset" Int
    :> Get '[JSON] [XyneShared.XyneIssueListItem]

issuesHandler :: FlowServer IssuesAPI
issuesHandler = getXyneIssues
  where
    getXyneIssues mbAuth mbSince mbLimit mbOffset =
      withFlowHandlerAPI $ Domain.Action.UI.XyneWebhook.getXyneIssues mbSince mbLimit mbOffset mbAuth
