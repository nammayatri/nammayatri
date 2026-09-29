{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.Management.PolicyDocument
  ( API,
    handler,
  )
where

import qualified API.Types.RiderPlatform.Management.PolicyDocument
import qualified Domain.Action.Dashboard.PolicyDocument
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

type API = ("policyDocument" :> (PostPolicyDocumentCreate :<|> PostPolicyDocumentUpdate :<|> GetPolicyDocumentList))

type PostPolicyDocumentCreate =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_MANAGEMENT/POLICY_DOCUMENT/POST_POLICY_DOCUMENT_CREATE"
      :> API.Types.RiderPlatform.Management.PolicyDocument.PostPolicyDocumentCreate
  )

type PostPolicyDocumentUpdate =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_MANAGEMENT/POLICY_DOCUMENT/POST_POLICY_DOCUMENT_UPDATE"
      :> API.Types.RiderPlatform.Management.PolicyDocument.PostPolicyDocumentUpdate
  )

type GetPolicyDocumentList =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_MANAGEMENT/POLICY_DOCUMENT/GET_POLICY_DOCUMENT_LIST"
      :> API.Types.RiderPlatform.Management.PolicyDocument.GetPolicyDocumentList
  )

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postPolicyDocumentCreate merchantId city :<|> postPolicyDocumentUpdate merchantId city :<|> getPolicyDocumentList merchantId city

postPolicyDocumentCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.RiderPlatform.Management.PolicyDocument.PolicyCreateReq -> Environment.FlowHandler API.Types.RiderPlatform.Management.PolicyDocument.PolicyCreateResp)
postPolicyDocumentCreate a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_MANAGEMENT/POLICY_DOCUMENT/POST_POLICY_DOCUMENT_CREATE" a2 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a2 $ Domain.Action.Dashboard.PolicyDocument.postPolicyDocumentCreate a4 a3 a1
    )

postPolicyDocumentUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> API.Types.RiderPlatform.Management.PolicyDocument.PolicyUpdateReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postPolicyDocumentUpdate a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_MANAGEMENT/POLICY_DOCUMENT/POST_POLICY_DOCUMENT_UPDATE" a3 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.PolicyDocument.postPolicyDocumentUpdate a5 a4 a2 a1
    )

getPolicyDocumentList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Environment.FlowHandler API.Types.RiderPlatform.Management.PolicyDocument.PolicyListMgmtResp)
getPolicyDocumentList a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.PolicyDocument.getPolicyDocumentList a5 a4 a2 a1
