{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module API.Action.UI.TriggerFCM
( API,
handler )
where
import Storage.Beam.SystemConfigs ()
import EulerHS.Prelude
import Servant
import Tools.Auth
import Kernel.Utils.Common
import qualified Domain.Action.UI.TriggerFCM
import qualified Domain.Types.Person
import qualified Kernel.Prelude
import qualified Control.Lens
import qualified Domain.Types.Merchant
import qualified Environment
import qualified Kernel.Types.Id
import qualified API.Types.UI.TriggerFCM
import qualified Kernel.Types.APISuccess



type API = (TokenAuth :> "triggerFCM" :> "message" :> ReqBody ('[JSON]) API.Types.UI.TriggerFCM.TriggerFcmReq :> Post ('[JSON]) Kernel.Types.APISuccess.APISuccess)
handler :: Environment.FlowServer API
handler = postTriggerFCMMessage
postTriggerFCMMessage :: ((Kernel.Types.Id.Id Domain.Types.Person.Person,
                           Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> API.Types.UI.TriggerFCM.TriggerFcmReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postTriggerFCMMessage a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.TriggerFCM.postTriggerFCMMessage (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1



