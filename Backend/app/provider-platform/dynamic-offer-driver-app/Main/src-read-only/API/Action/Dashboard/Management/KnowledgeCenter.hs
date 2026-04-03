{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module API.Action.Dashboard.Management.KnowledgeCenter 
( API.Types.ProviderPlatform.Management.KnowledgeCenter.API,
handler )
where
import EulerHS.Prelude
import Servant
import Tools.Auth
import Kernel.Utils.Common
import qualified Domain.Action.Dashboard.Management.KnowledgeCenter
import qualified Kernel.Prelude
import qualified Domain.Types.Merchant
import qualified Environment
import qualified Kernel.Types.Id
import qualified Kernel.Types.Beckn.Context
import qualified API.Types.ProviderPlatform.Management.KnowledgeCenter
import qualified Kernel.Types.APISuccess



handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.ProviderPlatform.Management.KnowledgeCenter.API)
handler merchantId city = getKnowledgeCenterGetDocument merchantId city :<|> getKnowledgeCenterSopList merchantId city :<|> postKnowledgeCenterSopUpload merchantId city :<|> putKnowledgeCenterSopTypeRename merchantId city :<|> deleteKnowledgeCenterSopDocument merchantId city :<|> deleteKnowledgeCenterSopType merchantId city
getKnowledgeCenterGetDocument :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.KnowledgeCenter.GetKnowledgeCenterDocumentResp)
getKnowledgeCenterGetDocument a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.KnowledgeCenter.getKnowledgeCenterGetDocument a4 a3 a2 a1
getKnowledgeCenterSopList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.KnowledgeCenter.KnowledgeCenterSopListResp)
getKnowledgeCenterSopList a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.KnowledgeCenter.getKnowledgeCenterSopList a4 a3 a2 a1
postKnowledgeCenterSopUpload :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> API.Types.ProviderPlatform.Management.KnowledgeCenter.KnowledgeCenterUploadImageReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.KnowledgeCenter.KnowledgeCenterUploadImageResp)
postKnowledgeCenterSopUpload a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.KnowledgeCenter.postKnowledgeCenterSopUpload a4 a3 a2 a1
putKnowledgeCenterSopTypeRename :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> API.Types.ProviderPlatform.Management.KnowledgeCenter.KnowledgeCenterRenameSopTypeReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
putKnowledgeCenterSopTypeRename a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.KnowledgeCenter.putKnowledgeCenterSopTypeRename a4 a3 a2 a1
deleteKnowledgeCenterSopDocument :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
deleteKnowledgeCenterSopDocument a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.KnowledgeCenter.deleteKnowledgeCenterSopDocument a4 a3 a2 a1
deleteKnowledgeCenterSopType :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
deleteKnowledgeCenterSopType a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.KnowledgeCenter.deleteKnowledgeCenterSopType a4 a3 a2 a1



