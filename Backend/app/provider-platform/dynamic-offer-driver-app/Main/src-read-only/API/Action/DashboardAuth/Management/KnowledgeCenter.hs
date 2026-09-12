{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.Management.KnowledgeCenter
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management.KnowledgeCenter
import qualified Domain.Action.Dashboard.Management.KnowledgeCenter
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
import Tools.Auth.DashboardUserAuth

type API = ("knowledgeCenter" :> (GetKnowledgeCenterGetDocument :<|> GetKnowledgeCenterSopList :<|> PostKnowledgeCenterSopUpload :<|> PutKnowledgeCenterSopTypeRename :<|> DeleteKnowledgeCenterSopDocument :<|> DeleteKnowledgeCenterSopType))

type GetKnowledgeCenterGetDocument =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/KNOWLEDGE_CENTER/GET_KNOWLEDGE_CENTER_GET_DOCUMENT"
      :> API.Types.ProviderPlatform.Management.KnowledgeCenter.GetKnowledgeCenterGetDocument
  )

type GetKnowledgeCenterSopList =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/KNOWLEDGE_CENTER/GET_KNOWLEDGE_CENTER_SOP_LIST"
      :> API.Types.ProviderPlatform.Management.KnowledgeCenter.GetKnowledgeCenterSopList
  )

type PostKnowledgeCenterSopUpload =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/KNOWLEDGE_CENTER/POST_KNOWLEDGE_CENTER_SOP_UPLOAD"
      :> API.Types.ProviderPlatform.Management.KnowledgeCenter.PostKnowledgeCenterSopUpload
  )

type PutKnowledgeCenterSopTypeRename =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/KNOWLEDGE_CENTER/PUT_KNOWLEDGE_CENTER_SOP_TYPE_RENAME"
      :> API.Types.ProviderPlatform.Management.KnowledgeCenter.PutKnowledgeCenterSopTypeRename
  )

type DeleteKnowledgeCenterSopDocument =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/KNOWLEDGE_CENTER/DELETE_KNOWLEDGE_CENTER_SOP_DOCUMENT"
      :> API.Types.ProviderPlatform.Management.KnowledgeCenter.DeleteKnowledgeCenterSopDocument
  )

type DeleteKnowledgeCenterSopType =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/KNOWLEDGE_CENTER/DELETE_KNOWLEDGE_CENTER_SOP_TYPE"
      :> API.Types.ProviderPlatform.Management.KnowledgeCenter.DeleteKnowledgeCenterSopType
  )

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getKnowledgeCenterGetDocument merchantId city :<|> getKnowledgeCenterSopList merchantId city :<|> postKnowledgeCenterSopUpload merchantId city :<|> putKnowledgeCenterSopTypeRename merchantId city :<|> deleteKnowledgeCenterSopDocument merchantId city :<|> deleteKnowledgeCenterSopType merchantId city

getKnowledgeCenterGetDocument :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.KnowledgeCenter.GetKnowledgeCenterDocumentResp)
getKnowledgeCenterGetDocument a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.KnowledgeCenter.getKnowledgeCenterGetDocument a4 a3 a1 (Tools.Auth.DashboardUserAuth.dashboardRequestorId a2)

getKnowledgeCenterSopList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.KnowledgeCenter.KnowledgeCenterSopListResp)
getKnowledgeCenterSopList a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.KnowledgeCenter.getKnowledgeCenterSopList a4 a3 a1 (Tools.Auth.DashboardUserAuth.dashboardRequestorId a2)

postKnowledgeCenterSopUpload :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Management.KnowledgeCenter.KnowledgeCenterUploadImageReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.KnowledgeCenter.KnowledgeCenterUploadImageResp)
postKnowledgeCenterSopUpload a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.KnowledgeCenter.postKnowledgeCenterSopUpload a4 a3 (Tools.Auth.DashboardUserAuth.dashboardRequestorId a2) a1

putKnowledgeCenterSopTypeRename :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Management.KnowledgeCenter.KnowledgeCenterRenameSopTypeReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
putKnowledgeCenterSopTypeRename a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.KnowledgeCenter.putKnowledgeCenterSopTypeRename a4 a3 (Tools.Auth.DashboardUserAuth.dashboardRequestorId a2) a1

deleteKnowledgeCenterSopDocument :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
deleteKnowledgeCenterSopDocument a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.KnowledgeCenter.deleteKnowledgeCenterSopDocument a4 a3 a1 (Tools.Auth.DashboardUserAuth.dashboardRequestorId a2)

deleteKnowledgeCenterSopType :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
deleteKnowledgeCenterSopType a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.KnowledgeCenter.deleteKnowledgeCenterSopType a4 a3 a1 (Tools.Auth.DashboardUserAuth.dashboardRequestorId a2)
