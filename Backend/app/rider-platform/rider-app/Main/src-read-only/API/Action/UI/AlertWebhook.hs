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
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API =
  ( "api" :> "v1" :> "alerts" :> "update" :> QueryParam "isManual" Kernel.Prelude.Bool :> QueryParam "rca" Kernel.Prelude.Text
      :> ReqBody
           '[JSON]
           API.Types.UI.AlertWebhook.VmAlertWebhookReq
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
  )

handler :: Environment.FlowServer API
handler = postApiV1AlertsUpdate

postApiV1AlertsUpdate :: (Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> API.Types.UI.AlertWebhook.VmAlertWebhookReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postApiV1AlertsUpdate a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.AlertWebhook.postApiV1AlertsUpdate a3 a2 a1
