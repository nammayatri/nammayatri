{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module API.Action.UI.Penalty 
( API,
handler )
where
import Storage.Beam.SystemConfigs ()
import EulerHS.Prelude
import Servant
import Tools.Auth
import Kernel.Utils.Common
import qualified Domain.Action.UI.Penalty
import qualified Domain.Types.Person
import qualified Kernel.Prelude
import qualified Control.Lens
import qualified Domain.Types.Merchant
import qualified Environment
import qualified Kernel.Types.Id
import qualified API.Types.UI.Penalty
import qualified Domain.Types.MerchantOperatingCity



type API = (TokenAuth :> "penalty" :> "check" :> ReqBody ('[JSON]) API.Types.UI.Penalty.PenaltyCheckReq :> Post ('[JSON]) API.Types.UI.Penalty.PenaltyCheckRes)
handler :: Environment.FlowServer API
handler = postPenaltyCheck
postPenaltyCheck :: ((Kernel.Types.Id.Id Domain.Types.Person.Person,
                      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
                      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity) -> API.Types.UI.Penalty.PenaltyCheckReq -> Environment.FlowHandler API.Types.UI.Penalty.PenaltyCheckRes)
postPenaltyCheck a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.Penalty.postPenaltyCheck (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1



