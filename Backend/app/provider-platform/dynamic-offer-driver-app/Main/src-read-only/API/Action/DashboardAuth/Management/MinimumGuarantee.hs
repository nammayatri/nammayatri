{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.Management.MinimumGuarantee
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management.MinimumGuarantee
import qualified Dashboard.Common
import qualified Data.Time
import qualified Domain.Action.Dashboard.Management.MinimumGuarantee
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import qualified Tools.ActorInfo
import Tools.Auth
import Tools.Auth.DashboardUserAuth

type API = ("minimumGuarantee" :> (GetMinimumGuaranteeList :<|> PostMinimumGuaranteeCreate :<|> PostMinimumGuaranteeUpdate :<|> PostMinimumGuaranteeInvite :<|> PostMinimumGuaranteeStatus :<|> PostMinimumGuaranteeExtend :<|> GetMinimumGuaranteeEnrollments))

type GetMinimumGuaranteeList =
  ( DashboardUserAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      "PROVIDER_MANAGEMENT/MINIMUM_GUARANTEE/GET_MINIMUM_GUARANTEE_LIST"
      :> API.Types.ProviderPlatform.Management.MinimumGuarantee.GetMinimumGuaranteeList
  )

type PostMinimumGuaranteeCreate =
  ( DashboardUserAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      "PROVIDER_MANAGEMENT/MINIMUM_GUARANTEE/POST_MINIMUM_GUARANTEE_CREATE"
      :> API.Types.ProviderPlatform.Management.MinimumGuarantee.PostMinimumGuaranteeCreate
  )

type PostMinimumGuaranteeUpdate =
  ( DashboardUserAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      "PROVIDER_MANAGEMENT/MINIMUM_GUARANTEE/POST_MINIMUM_GUARANTEE_UPDATE"
      :> API.Types.ProviderPlatform.Management.MinimumGuarantee.PostMinimumGuaranteeUpdate
  )

type PostMinimumGuaranteeInvite =
  ( DashboardUserAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      "PROVIDER_MANAGEMENT/MINIMUM_GUARANTEE/POST_MINIMUM_GUARANTEE_INVITE"
      :> API.Types.ProviderPlatform.Management.MinimumGuarantee.PostMinimumGuaranteeInvite
  )

type PostMinimumGuaranteeStatus =
  ( DashboardUserAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      "PROVIDER_MANAGEMENT/MINIMUM_GUARANTEE/POST_MINIMUM_GUARANTEE_STATUS"
      :> API.Types.ProviderPlatform.Management.MinimumGuarantee.PostMinimumGuaranteeStatus
  )

type PostMinimumGuaranteeExtend =
  ( DashboardUserAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      "PROVIDER_MANAGEMENT/MINIMUM_GUARANTEE/POST_MINIMUM_GUARANTEE_EXTEND"
      :> API.Types.ProviderPlatform.Management.MinimumGuarantee.PostMinimumGuaranteeExtend
  )

type GetMinimumGuaranteeEnrollments =
  ( DashboardUserAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      "PROVIDER_MANAGEMENT/MINIMUM_GUARANTEE/GET_MINIMUM_GUARANTEE_ENROLLMENTS"
      :> API.Types.ProviderPlatform.Management.MinimumGuarantee.GetMinimumGuaranteeEnrollments
  )

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getMinimumGuaranteeList merchantId city :<|> postMinimumGuaranteeCreate merchantId city :<|> postMinimumGuaranteeUpdate merchantId city :<|> postMinimumGuaranteeInvite merchantId city :<|> postMinimumGuaranteeStatus merchantId city :<|> postMinimumGuaranteeExtend merchantId city :<|> getMinimumGuaranteeEnrollments merchantId city

getMinimumGuaranteeList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Environment.FlowHandler API.Types.ProviderPlatform.Management.MinimumGuarantee.MgCampaignListRes)
getMinimumGuaranteeList a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.Management.MinimumGuarantee.getMinimumGuaranteeList a5 a4 a2 a1

postMinimumGuaranteeCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Management.MinimumGuarantee.MgCampaignReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.MinimumGuarantee.MgCampaignRes)
postMinimumGuaranteeCreate a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.DRIVER_OFFER_BPP_MANAGEMENT "PROVIDER_MANAGEMENT/MINIMUM_GUARANTEE/POST_MINIMUM_GUARANTEE_CREATE" a2 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a2 $ Domain.Action.Dashboard.Management.MinimumGuarantee.postMinimumGuaranteeCreate a4 a3 (Tools.Auth.DashboardUserAuth.dashboardRequestorId a2) a1
    )

postMinimumGuaranteeUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.MgCampaign -> API.Types.ProviderPlatform.Management.MinimumGuarantee.MgCampaignReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMinimumGuaranteeUpdate a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.DRIVER_OFFER_BPP_MANAGEMENT "PROVIDER_MANAGEMENT/MINIMUM_GUARANTEE/POST_MINIMUM_GUARANTEE_UPDATE" a3 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.Management.MinimumGuarantee.postMinimumGuaranteeUpdate a5 a4 a2 a1
    )

postMinimumGuaranteeInvite :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.MgCampaign -> API.Types.ProviderPlatform.Management.MinimumGuarantee.MgInviteReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.MinimumGuarantee.MgInviteRes)
postMinimumGuaranteeInvite a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.DRIVER_OFFER_BPP_MANAGEMENT "PROVIDER_MANAGEMENT/MINIMUM_GUARANTEE/POST_MINIMUM_GUARANTEE_INVITE" a3 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.Management.MinimumGuarantee.postMinimumGuaranteeInvite a5 a4 a2 a1
    )

postMinimumGuaranteeStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.MgCampaign -> API.Types.ProviderPlatform.Management.MinimumGuarantee.MgCampaignStatusReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMinimumGuaranteeStatus a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.DRIVER_OFFER_BPP_MANAGEMENT "PROVIDER_MANAGEMENT/MINIMUM_GUARANTEE/POST_MINIMUM_GUARANTEE_STATUS" a3 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.Management.MinimumGuarantee.postMinimumGuaranteeStatus a5 a4 a2 a1
    )

postMinimumGuaranteeExtend :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.MgCampaign -> API.Types.ProviderPlatform.Management.MinimumGuarantee.MgCampaignExtendReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMinimumGuaranteeExtend a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.DRIVER_OFFER_BPP_MANAGEMENT "PROVIDER_MANAGEMENT/MINIMUM_GUARANTEE/POST_MINIMUM_GUARANTEE_EXTEND" a3 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.Management.MinimumGuarantee.postMinimumGuaranteeExtend a5 a4 a2 a1
    )

getMinimumGuaranteeEnrollments :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.MgCampaign -> Kernel.Prelude.Maybe Data.Time.Day -> Environment.FlowHandler API.Types.ProviderPlatform.Management.MinimumGuarantee.MgEnrollmentListRes)
getMinimumGuaranteeEnrollments a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.Management.MinimumGuarantee.getMinimumGuaranteeEnrollments a5 a4 a2 a1
