module Domain.Action.ProviderPlatform.Management.KnowledgeCenter
  ( getKnowledgeCenterGetDocument,
    getKnowledgeCenterSopList,
    postKnowledgeCenterSopUpload,
    putKnowledgeCenterSopTypeRename,
    deleteKnowledgeCenterSopDocument,
    deleteKnowledgeCenterSopType,
  )
where

import qualified API.Client.ProviderPlatform.Management as Client
import qualified API.Types.ProviderPlatform.Management.KnowledgeCenter
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified Domain.Types.Transaction
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import qualified SharedLogic.Transaction
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api
import Tools.Auth.Merchant

getKnowledgeCenterGetDocument :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> Environment.Flow API.Types.ProviderPlatform.Management.KnowledgeCenter.GetKnowledgeCenterDocumentResp)
getKnowledgeCenterGetDocument merchantShortId opCity apiTokenInfo knowledgeCenterId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let requestorId = apiTokenInfo.personId.getId
  Client.callManagementAPI checkedMerchantId opCity (.knowledgeCenterDSL.getKnowledgeCenterGetDocument) knowledgeCenterId requestorId

getKnowledgeCenterSopList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.Flow API.Types.ProviderPlatform.Management.KnowledgeCenter.KnowledgeCenterSopListResp)
getKnowledgeCenterSopList merchantShortId opCity apiTokenInfo sopType = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let requestorId = apiTokenInfo.personId.getId
  Client.callManagementAPI checkedMerchantId opCity (.knowledgeCenterDSL.getKnowledgeCenterSopList) sopType requestorId

postKnowledgeCenterSopUpload :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.KnowledgeCenter.KnowledgeCenterUploadImageReq -> Environment.Flow API.Types.ProviderPlatform.Management.KnowledgeCenter.KnowledgeCenterUploadImageResp)
postKnowledgeCenterSopUpload merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ do
    let requestorId = apiTokenInfo.personId.getId
    Client.callManagementAPI checkedMerchantId opCity (.knowledgeCenterDSL.postKnowledgeCenterSopUpload) requestorId req

putKnowledgeCenterSopTypeRename :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.KnowledgeCenter.KnowledgeCenterRenameSopTypeReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
putKnowledgeCenterSopTypeRename merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ do
    let requestorId = apiTokenInfo.personId.getId
    Client.callManagementAPI checkedMerchantId opCity (.knowledgeCenterDSL.putKnowledgeCenterSopTypeRename) requestorId req

deleteKnowledgeCenterSopDocument :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
deleteKnowledgeCenterSopDocument merchantShortId opCity apiTokenInfo knowledgeCenterId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing SharedLogic.Transaction.emptyRequest
  SharedLogic.Transaction.withTransactionStoring transaction $ do
    let requestorId = apiTokenInfo.personId.getId
    Client.callManagementAPI checkedMerchantId opCity (.knowledgeCenterDSL.deleteKnowledgeCenterSopDocument) knowledgeCenterId requestorId

deleteKnowledgeCenterSopType :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
deleteKnowledgeCenterSopType merchantShortId opCity apiTokenInfo sopType = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing SharedLogic.Transaction.emptyRequest
  SharedLogic.Transaction.withTransactionStoring transaction $ do
    let requestorId = apiTokenInfo.personId.getId
    Client.callManagementAPI checkedMerchantId opCity (.knowledgeCenterDSL.deleteKnowledgeCenterSopType) sopType requestorId
