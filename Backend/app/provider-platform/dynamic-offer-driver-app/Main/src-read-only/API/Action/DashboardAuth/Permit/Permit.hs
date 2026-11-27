{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.Permit.Permit
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Permit.Permit
import qualified Data.Time
import qualified Domain.Action.Dashboard.Permit.Permit
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Permit.Api.Types
import qualified Lib.Permit.Domain.Types.PermitApplication
import qualified Lib.Permit.Domain.Types.PermitApplicationDocument
import qualified Lib.Permit.Types
import Servant
import qualified Tools.ActorInfo
import Tools.Auth
import Tools.Auth.DashboardUserAuth

type API = ("permit" :> (GetPermitApplicationList :<|> GetPermitApplication :<|> GetPermitApplicationDocument :<|> PostPermitApplicationDecision :<|> PostPermitApplicationDcDecision :<|> PostPermitPassVerify))

type GetPermitApplicationList =
  ( DashboardUserAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      "PROVIDER_PERMIT/PERMIT/GET_PERMIT_APPLICATION_LIST"
      :> API.Types.ProviderPlatform.Permit.Permit.GetPermitApplicationList
  )

type GetPermitApplication = (DashboardUserAuth 'DRIVER_OFFER_BPP_MANAGEMENT "PROVIDER_PERMIT/PERMIT/GET_PERMIT_APPLICATION" :> API.Types.ProviderPlatform.Permit.Permit.GetPermitApplication)

type GetPermitApplicationDocument =
  ( DashboardUserAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      "PROVIDER_PERMIT/PERMIT/GET_PERMIT_APPLICATION_DOCUMENT"
      :> API.Types.ProviderPlatform.Permit.Permit.GetPermitApplicationDocument
  )

type PostPermitApplicationDecision =
  ( DashboardUserAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      "PROVIDER_PERMIT/PERMIT/POST_PERMIT_APPLICATION_DECISION"
      :> API.Types.ProviderPlatform.Permit.Permit.PostPermitApplicationDecision
  )

type PostPermitApplicationDcDecision =
  ( DashboardUserAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      "PROVIDER_PERMIT/PERMIT/POST_PERMIT_APPLICATION_DC_DECISION"
      :> API.Types.ProviderPlatform.Permit.Permit.PostPermitApplicationDcDecision
  )

type PostPermitPassVerify = (DashboardUserAuth 'DRIVER_OFFER_BPP_MANAGEMENT "PROVIDER_PERMIT/PERMIT/POST_PERMIT_PASS_VERIFY" :> API.Types.ProviderPlatform.Permit.Permit.PostPermitPassVerify)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getPermitApplicationList merchantId city :<|> getPermitApplication merchantId city :<|> getPermitApplicationDocument merchantId city :<|> postPermitApplicationDecision merchantId city :<|> postPermitApplicationDcDecision merchantId city :<|> postPermitPassVerify merchantId city

getPermitApplicationList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe Lib.Permit.Domain.Types.PermitApplication.PermitStatus -> Kernel.Prelude.Maybe Data.Time.Day -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Lib.Permit.Types.PermitType -> Environment.FlowHandler Lib.Permit.Api.Types.PermitApplicationListRes)
getPermitApplicationList a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a6 $ Domain.Action.Dashboard.Permit.Permit.getPermitApplicationList a8 a7 a5 a4 a3 a2 a1

getPermitApplication :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Lib.Permit.Domain.Types.PermitApplication.PermitApplication -> Environment.FlowHandler Lib.Permit.Api.Types.PermitApplicationDetailsRes)
getPermitApplication a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a2 $ Domain.Action.Dashboard.Permit.Permit.getPermitApplication a4 a3 a1

getPermitApplicationDocument :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Lib.Permit.Domain.Types.PermitApplication.PermitApplication -> Lib.Permit.Domain.Types.PermitApplicationDocument.PermitDocumentType -> Environment.FlowHandler Lib.Permit.Api.Types.PermitDocumentRes)
getPermitApplicationDocument a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.Permit.Permit.getPermitApplicationDocument a5 a4 a2 a1 (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3) (Tools.Auth.DashboardUserAuth.dashboardRequestorName a3)

postPermitApplicationDecision :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Lib.Permit.Domain.Types.PermitApplication.PermitApplication -> Lib.Permit.Api.Types.PermitDecisionReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postPermitApplicationDecision a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.DRIVER_OFFER_BPP_MANAGEMENT "PROVIDER_PERMIT/PERMIT/POST_PERMIT_APPLICATION_DECISION" a3 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.Permit.Permit.postPermitApplicationDecision a5 a4 a2 (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3) (Tools.Auth.DashboardUserAuth.dashboardRequestorName a3) a1
    )

postPermitApplicationDcDecision :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Lib.Permit.Domain.Types.PermitApplication.PermitApplication -> Lib.Permit.Api.Types.PermitDecisionReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postPermitApplicationDcDecision a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.DRIVER_OFFER_BPP_MANAGEMENT "PROVIDER_PERMIT/PERMIT/POST_PERMIT_APPLICATION_DC_DECISION" a3 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.Permit.Permit.postPermitApplicationDcDecision a5 a4 a2 (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3) (Tools.Auth.DashboardUserAuth.dashboardRequestorName a3) a1
    )

postPermitPassVerify :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Lib.Permit.Api.Types.PermitVerifyPassReq -> Environment.FlowHandler Lib.Permit.Api.Types.PermitVerifyPassRes)
postPermitPassVerify a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.DRIVER_OFFER_BPP_MANAGEMENT "PROVIDER_PERMIT/PERMIT/POST_PERMIT_PASS_VERIFY" a2 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a2 $ Domain.Action.Dashboard.Permit.Permit.postPermitPassVerify a4 a3 (Tools.Auth.DashboardUserAuth.dashboardRequestorId a2) (Tools.Auth.DashboardUserAuth.dashboardRequestorName a2) a1
    )
