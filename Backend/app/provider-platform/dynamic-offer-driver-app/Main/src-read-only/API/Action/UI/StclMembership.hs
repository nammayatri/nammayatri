{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module API.Action.UI.StclMembership 
( API,
handler )
where
import Storage.Beam.SystemConfigs ()
import EulerHS.Prelude
import Servant
import Tools.Auth
import Kernel.Utils.Common
import qualified Domain.Action.UI.StclMembership
import qualified Domain.Types.Person
import qualified Kernel.Prelude
import qualified Control.Lens
import qualified Domain.Types.Merchant
import qualified Environment
import qualified Kernel.Types.Id
import qualified API.Types.UI.StclMembership
import qualified Kernel.External.Payment.Interface.Types
import qualified Domain.Types.MerchantOperatingCity



type API = (TokenAuth :> "submitApplication" :> ReqBody ('[JSON]) API.Types.UI.StclMembership.MembershipApplicationReq :> Post ('[JSON])
                                                                                                                               Kernel.External.Payment.Interface.Types.CreateOrderResp :<|> TokenAuth :> "membership" :> Get ('[JSON]) API.Types.UI.StclMembership.MembershipDetailsResp)
handler :: Environment.FlowServer API
handler = postSubmitApplication :<|> getMembership
postSubmitApplication :: ((Kernel.Types.Id.Id Domain.Types.Person.Person,
                           Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
                           Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity) -> API.Types.UI.StclMembership.MembershipApplicationReq -> Environment.FlowHandler Kernel.External.Payment.Interface.Types.CreateOrderResp)
postSubmitApplication a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.StclMembership.postSubmitApplication (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1
getMembership :: ((Kernel.Types.Id.Id Domain.Types.Person.Person,
                   Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
                   Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity) -> Environment.FlowHandler API.Types.UI.StclMembership.MembershipDetailsResp)
getMembership a1 = withFlowHandlerAPI $ Domain.Action.UI.StclMembership.getMembership (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a1)



