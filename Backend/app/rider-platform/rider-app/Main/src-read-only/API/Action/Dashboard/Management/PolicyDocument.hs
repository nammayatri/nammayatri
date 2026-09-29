{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.Management.PolicyDocument
  ( API.Types.RiderPlatform.Management.PolicyDocument.API,
    handler,
  )
where

import qualified API.Types.RiderPlatform.Management.PolicyDocument
import qualified Dashboard.Common
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
import Tools.Auth

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.RiderPlatform.Management.PolicyDocument.API)
handler merchantId city = postPolicyDocumentCreate merchantId city :<|> postPolicyDocumentUpdate merchantId city :<|> getPolicyDocumentList merchantId city

postPolicyDocumentCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.RiderPlatform.Management.PolicyDocument.PolicyCreateReq -> Environment.FlowHandler API.Types.RiderPlatform.Management.PolicyDocument.PolicyCreateResp)
postPolicyDocumentCreate a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.PolicyDocument.postPolicyDocumentCreate a3 a2 a1

postPolicyDocumentUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.PolicyAndComplianceDocument -> API.Types.RiderPlatform.Management.PolicyDocument.PolicyUpdateReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postPolicyDocumentUpdate a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.PolicyDocument.postPolicyDocumentUpdate a4 a3 a2 a1

getPolicyDocumentList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Environment.FlowHandler API.Types.RiderPlatform.Management.PolicyDocument.PolicyListMgmtResp)
getPolicyDocumentList a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.PolicyDocument.getPolicyDocumentList a4 a3 a2 a1
