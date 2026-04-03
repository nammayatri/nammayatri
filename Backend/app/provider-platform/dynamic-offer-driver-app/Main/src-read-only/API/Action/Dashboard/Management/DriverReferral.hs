{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module API.Action.Dashboard.Management.DriverReferral 
( API.Types.ProviderPlatform.Management.DriverReferral.API,
handler )
where
import EulerHS.Prelude
import Servant
import Tools.Auth
import Kernel.Utils.Common
import qualified Domain.Action.Dashboard.Management.DriverReferral
import qualified Domain.Types.Merchant
import qualified Environment
import qualified Kernel.Types.Id
import qualified Kernel.Types.Beckn.Context
import qualified API.Types.ProviderPlatform.Management.DriverReferral
import qualified Kernel.Types.APISuccess



handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.ProviderPlatform.Management.DriverReferral.API)
handler merchantId city = postDriverReferralReferralOpsPassword merchantId city :<|> postDriverReferralLinkReferral merchantId city
postDriverReferralReferralOpsPassword :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Management.DriverReferral.ReferralLinkPasswordUpdateAPIReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverReferralReferralOpsPassword a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.DriverReferral.postDriverReferralReferralOpsPassword a3 a2 a1
postDriverReferralLinkReferral :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Management.DriverReferral.ReferralLinkReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.DriverReferral.LinkReport)
postDriverReferralLinkReferral a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.DriverReferral.postDriverReferralLinkReferral a3 a2 a1



