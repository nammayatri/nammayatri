module API.UI.Webengage.InfoBIPWebhook where

import Beckn.Types.APISuccess (APISuccess)
import Beckn.Utils.Common
import qualified Domain.Action.UI.Webengage.InfoBIPWebhook as IW
import Environment
import EulerHS.Prelude hiding (id)
import Servant

type API =
  "update"
    :> "status"
    :> ReqBody '[JSON] IW.StatusRes
    :> Post '[JSON] APISuccess

handler :: FlowServer API
handler =
  updateStatus

updateStatus :: IW.StatusRes -> FlowHandler APISuccess
updateStatus = withFlowHandlerAPI . IW.sendStatus
