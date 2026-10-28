{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.Fleet.MinimumGuarantee
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Fleet.MinimumGuarantee
import qualified Domain.Action.Dashboard.Fleet.MinimumGuarantee
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import qualified Tools.ActorInfo
import Tools.Auth
import Tools.Auth.DashboardUserAuth

type API = ("fleet" :> (GetMinimumGuaranteeInvitations :<|> PostMinimumGuaranteeOptIn :<|> PostMinimumGuaranteeOptOut))

type GetMinimumGuaranteeInvitations =
  ( DashboardUserAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      "PROVIDER_FLEET/MINIMUM_GUARANTEE/GET_MINIMUM_GUARANTEE_INVITATIONS"
      :> API.Types.ProviderPlatform.Fleet.MinimumGuarantee.GetMinimumGuaranteeInvitations
  )

type PostMinimumGuaranteeOptIn =
  ( DashboardUserAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      "PROVIDER_FLEET/MINIMUM_GUARANTEE/POST_MINIMUM_GUARANTEE_OPT_IN"
      :> API.Types.ProviderPlatform.Fleet.MinimumGuarantee.PostMinimumGuaranteeOptIn
  )

type PostMinimumGuaranteeOptOut =
  ( DashboardUserAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      "PROVIDER_FLEET/MINIMUM_GUARANTEE/POST_MINIMUM_GUARANTEE_OPT_OUT"
      :> API.Types.ProviderPlatform.Fleet.MinimumGuarantee.PostMinimumGuaranteeOptOut
  )

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getMinimumGuaranteeInvitations merchantId city :<|> postMinimumGuaranteeOptIn merchantId city :<|> postMinimumGuaranteeOptOut merchantId city

getMinimumGuaranteeInvitations :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.MinimumGuarantee.FleetMgInvitationsRes)
getMinimumGuaranteeInvitations a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a4 $ Domain.Action.Dashboard.Fleet.MinimumGuarantee.getMinimumGuaranteeInvitations a6 a5 (Tools.Auth.DashboardUserAuth.dashboardRequestorId a4) a3 a2 a1

postMinimumGuaranteeOptIn :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Fleet.MinimumGuarantee.FleetMgOptReq -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.MinimumGuarantee.FleetMgOptRes)
postMinimumGuaranteeOptIn a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.DRIVER_OFFER_BPP_MANAGEMENT "PROVIDER_FLEET/MINIMUM_GUARANTEE/POST_MINIMUM_GUARANTEE_OPT_IN" a2 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a2 $ Domain.Action.Dashboard.Fleet.MinimumGuarantee.postMinimumGuaranteeOptIn a4 a3 (Tools.Auth.DashboardUserAuth.dashboardRequestorId a2) a1
    )

postMinimumGuaranteeOptOut :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Fleet.MinimumGuarantee.FleetMgOptReq -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.MinimumGuarantee.FleetMgOptRes)
postMinimumGuaranteeOptOut a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.DRIVER_OFFER_BPP_MANAGEMENT "PROVIDER_FLEET/MINIMUM_GUARANTEE/POST_MINIMUM_GUARANTEE_OPT_OUT" a2 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a2 $ Domain.Action.Dashboard.Fleet.MinimumGuarantee.postMinimumGuaranteeOptOut a4 a3 (Tools.Auth.DashboardUserAuth.dashboardRequestorId a2) a1
    )
