{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.AlertWebhook
  ( API,
    handler,
  )
where

import qualified API.Types.UI.AlertWebhook
import qualified Domain.Action.UI.AlertWebhook
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API = ("api" :> "v1" :> "alerts" :> "update" :> ReqBody '[JSON] API.Types.UI.AlertWebhook.VmAlertWebhookReq :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

handler :: Environment.FlowServer API
handler = postApiV1AlertsUpdate

postApiV1AlertsUpdate :: (API.Types.UI.AlertWebhook.VmAlertWebhookReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postApiV1AlertsUpdate a1 = withFlowHandlerAPI $ Domain.Action.UI.AlertWebhook.postApiV1AlertsUpdate a1
