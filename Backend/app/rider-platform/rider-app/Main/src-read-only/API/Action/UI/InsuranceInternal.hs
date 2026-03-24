{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module API.Action.UI.InsuranceInternal
( API,
handler )
where
import Storage.Beam.SystemConfigs ()
import EulerHS.Prelude
import Servant
import Tools.Auth
import Kernel.Utils.Common
import qualified Domain.Action.UI.InsuranceInternal
import qualified Kernel.Prelude
import qualified Environment
import qualified API.Types.UI.Insurance



type API = ("insurance" :> Capture "bppRideId" Kernel.Prelude.Text :> Header "token" Kernel.Prelude.Text :> Get ('[JSON]) API.Types.UI.Insurance.InsuranceAPIEntity)
handler :: Environment.FlowServer API
handler = getInsurance
getInsurance :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.FlowHandler API.Types.UI.Insurance.InsuranceAPIEntity)
getInsurance a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.InsuranceInternal.getInsurance a2 a1



