{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.TriggerFCM
  ( API,
    handler,
  )
where

import qualified API.Types.UI.TriggerFCM
import qualified Control.Lens
import qualified Domain.Action.UI.TriggerFCM
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API = (TokenAuth :> "triggerFCM" :> "message" :> ReqBody '[JSON] API.Types.UI.TriggerFCM.TriggerFcmReq :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

handler :: Environment.FlowServer API
handler = postTriggerFCMMessage

postTriggerFCMMessage ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    API.Types.UI.TriggerFCM.TriggerFcmReq ->
    Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
  )
postTriggerFCMMessage a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.TriggerFCM.postTriggerFCMMessage (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1
