{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.RiderPlatform.Management.PolicyDocument
  ( API,
    handler,
  )
where

import qualified API.Types.RiderPlatform.Management
import qualified API.Types.RiderPlatform.Management.PolicyDocument
import qualified Domain.Action.RiderPlatform.Management.PolicyDocument
import "rider-app" Domain.Types.AccessMatrix
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.CommonInstances ()

type API = ("policyDocument" :> (PostPolicyDocumentCreate :<|> PostPolicyDocumentUpdate :<|> GetPolicyDocumentList))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postPolicyDocumentCreate merchantId city :<|> postPolicyDocumentUpdate merchantId city :<|> getPolicyDocumentList merchantId city

type PostPolicyDocumentCreate =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_MANAGEMENT) / ('API.Types.RiderPlatform.Management.POLICY_DOCUMENT) / ('API.Types.RiderPlatform.Management.PolicyDocument.POST_POLICY_DOCUMENT_CREATE))
      :> API.Types.RiderPlatform.Management.PolicyDocument.PostPolicyDocumentCreate
  )

type PostPolicyDocumentUpdate =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_MANAGEMENT) / ('API.Types.RiderPlatform.Management.POLICY_DOCUMENT) / ('API.Types.RiderPlatform.Management.PolicyDocument.POST_POLICY_DOCUMENT_UPDATE))
      :> API.Types.RiderPlatform.Management.PolicyDocument.PostPolicyDocumentUpdate
  )

type GetPolicyDocumentList =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_MANAGEMENT) / ('API.Types.RiderPlatform.Management.POLICY_DOCUMENT) / ('API.Types.RiderPlatform.Management.PolicyDocument.GET_POLICY_DOCUMENT_LIST))
      :> API.Types.RiderPlatform.Management.PolicyDocument.GetPolicyDocumentList
  )

postPolicyDocumentCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> API.Types.RiderPlatform.Management.PolicyDocument.PolicyCreateReq -> Environment.FlowHandler API.Types.RiderPlatform.Management.PolicyDocument.PolicyCreateResp)
postPolicyDocumentCreate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.PolicyDocument.postPolicyDocumentCreate merchantShortId opCity apiTokenInfo req

postPolicyDocumentUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Text -> API.Types.RiderPlatform.Management.PolicyDocument.PolicyUpdateReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postPolicyDocumentUpdate merchantShortId opCity apiTokenInfo policyDocId req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.PolicyDocument.postPolicyDocumentUpdate merchantShortId opCity apiTokenInfo policyDocId req

getPolicyDocumentList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Environment.FlowHandler API.Types.RiderPlatform.Management.PolicyDocument.PolicyListMgmtResp)
getPolicyDocumentList merchantShortId opCity apiTokenInfo limit offset = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.PolicyDocument.getPolicyDocumentList merchantShortId opCity apiTokenInfo limit offset
