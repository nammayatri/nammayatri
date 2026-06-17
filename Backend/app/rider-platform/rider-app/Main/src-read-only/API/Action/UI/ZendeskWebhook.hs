{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.ZendeskWebhook
  ( API,
    handler,
  )
where

import qualified API.Types.UI.ZendeskWebhook
import qualified Domain.Action.UI.ZendeskWebhook
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API =
  ( "zendesk" :> "webhook" :> Header "Authorization" Kernel.Prelude.Text :> ReqBody ('[JSON]) API.Types.UI.ZendeskWebhook.ZendeskWebhookPayload
      :> Post
           ('[JSON])
           Kernel.Types.APISuccess.APISuccess
  )

handler :: Environment.FlowServer API
handler = postZendeskWebhook

postZendeskWebhook :: (Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> API.Types.UI.ZendeskWebhook.ZendeskWebhookPayload -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postZendeskWebhook a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.ZendeskWebhook.postZendeskWebhook a2 a1
