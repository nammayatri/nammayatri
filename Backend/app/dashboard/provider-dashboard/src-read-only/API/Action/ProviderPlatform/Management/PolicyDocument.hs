{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.ProviderPlatform.Management.PolicyDocument
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management
import qualified API.Types.ProviderPlatform.Management.PolicyDocument
import qualified Domain.Action.ProviderPlatform.Management.PolicyDocument
import "dynamic-offer-driver-app" Domain.Types.AccessMatrix
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude hiding (sortOn)
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common hiding (INFO)
import Servant
import Storage.Beam.CommonInstances ()

type API = ("policyDocument" :> (PostPolicyDocumentCreate :<|> PostPolicyDocumentUpdate :<|> GetPolicyDocumentList))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postPolicyDocumentCreate merchantId city :<|> postPolicyDocumentUpdate merchantId city :<|> getPolicyDocumentList merchantId city

type PostPolicyDocumentCreate =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.POLICY_DOCUMENT) / ('API.Types.ProviderPlatform.Management.PolicyDocument.POST_POLICY_DOCUMENT_CREATE))
      :> API.Types.ProviderPlatform.Management.PolicyDocument.PostPolicyDocumentCreate
  )

type PostPolicyDocumentUpdate =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.POLICY_DOCUMENT) / ('API.Types.ProviderPlatform.Management.PolicyDocument.POST_POLICY_DOCUMENT_UPDATE))
      :> API.Types.ProviderPlatform.Management.PolicyDocument.PostPolicyDocumentUpdate
  )

type GetPolicyDocumentList =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.POLICY_DOCUMENT) / ('API.Types.ProviderPlatform.Management.PolicyDocument.GET_POLICY_DOCUMENT_LIST))
      :> API.Types.ProviderPlatform.Management.PolicyDocument.GetPolicyDocumentList
  )

postPolicyDocumentCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> API.Types.ProviderPlatform.Management.PolicyDocument.PolicyCreateReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.PolicyDocument.PolicyCreateResp)
postPolicyDocumentCreate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.PolicyDocument.postPolicyDocumentCreate merchantShortId opCity apiTokenInfo req

postPolicyDocumentUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Text -> API.Types.ProviderPlatform.Management.PolicyDocument.PolicyUpdateReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postPolicyDocumentUpdate merchantShortId opCity apiTokenInfo policyDocId req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.PolicyDocument.postPolicyDocumentUpdate merchantShortId opCity apiTokenInfo policyDocId req

getPolicyDocumentList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.PolicyDocument.PolicyListMgmtResp)
getPolicyDocumentList merchantShortId opCity apiTokenInfo limit offset = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.PolicyDocument.getPolicyDocumentList merchantShortId opCity apiTokenInfo limit offset
