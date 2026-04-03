{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module API.Action.UI.CallFeedback 
( API,
handler )
where
import Storage.Beam.SystemConfigs ()
import EulerHS.Prelude
import Servant
import Tools.Auth
import Kernel.Utils.Common
import qualified Domain.Action.UI.CallFeedback
import qualified Domain.Types.Person
import qualified Kernel.Prelude
import qualified Control.Lens
import qualified Domain.Types.Merchant
import qualified Environment
import qualified Kernel.Types.Id
import qualified API.Types.UI.CallFeedback
import qualified Kernel.Types.APISuccess
import qualified Domain.Types.MerchantOperatingCity



type API = (TokenAuth :> "driver" :> "call" :> "feedback" :> ReqBody ('[JSON]) API.Types.UI.CallFeedback.CallFeedbackReq :> Post ('[JSON]) Kernel.Types.APISuccess.APISuccess)
handler :: Environment.FlowServer API
handler = postDriverCallFeedback
postDriverCallFeedback :: ((Kernel.Types.Id.Id Domain.Types.Person.Person,
                            Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
                            Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity) -> API.Types.UI.CallFeedback.CallFeedbackReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverCallFeedback a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.CallFeedback.postDriverCallFeedback (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1



