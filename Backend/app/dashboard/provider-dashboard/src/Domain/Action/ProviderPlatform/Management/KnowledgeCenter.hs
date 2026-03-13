{-
  Knowledge Center: proxy to BPP dashboard APIs (list, upload image/video, rename, delete).
  API types from dashboard-helper-api; client calls BPP.
-}

module Domain.Action.ProviderPlatform.Management.KnowledgeCenter
  ( getKnowledgeCenterGetDocument,
    getKnowledgeCenterSopList,
    postKnowledgeCenterSopUpload,
    postKnowledgeCenterVideoUploadLink,
    postKnowledgeCenterVideoConfirm,
    putKnowledgeCenterSopTypeRename,
    deleteKnowledgeCenterSopDocument,
    deleteKnowledgeCenterSopType,
  )
where

import qualified API.Client.ProviderPlatform.Management as Client
import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Management.KnowledgeCenter as Common
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import qualified Domain.Types.Transaction as DT
import "lib-dashboard" Environment
import EulerHS.Prelude
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Beckn.City as City
import Kernel.Types.Id (ShortId)
import qualified SharedLogic.Transaction as T
import Storage.Beam.CommonInstances ()
import "lib-dashboard" Tools.Auth
import Tools.Auth.Merchant

getKnowledgeCenterGetDocument ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Text ->
  Flow Common.GetKnowledgeCenterDocumentResp
getKnowledgeCenterGetDocument merchantShortId opCity apiTokenInfo knowledgeCenterId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let requestorId = apiTokenInfo.personId.getId
  Client.callManagementAPI checkedMerchantId opCity (.knowledgeCenterDSL.getKnowledgeCenterGetDocument) knowledgeCenterId requestorId

postKnowledgeCenterSopUpload ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Common.KnowledgeCenterUploadImageReq ->
  Flow Common.KnowledgeCenterUploadImageResp
postKnowledgeCenterSopUpload merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- T.buildTransaction (DT.castEndpoint apiTokenInfo.userActionType) (Just DRIVER_OFFER_BPP_MANAGEMENT) (Just apiTokenInfo) Nothing Nothing (Just req)
  let requestorId = apiTokenInfo.personId.getId
  T.withResponseTransactionStoring transaction $ do
    Client.callManagementAPI checkedMerchantId opCity (.knowledgeCenterDSL.postKnowledgeCenterSopUpload) requestorId req

postKnowledgeCenterVideoUploadLink ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Common.KnowledgeCenterUploadVideoLinkReq ->
  Flow Common.KnowledgeCenterUploadVideoLinkResp
postKnowledgeCenterVideoUploadLink merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- T.buildTransaction (DT.castEndpoint apiTokenInfo.userActionType) (Just DRIVER_OFFER_BPP_MANAGEMENT) (Just apiTokenInfo) Nothing Nothing (Just req)
  let requestorId = apiTokenInfo.personId.getId
  T.withResponseTransactionStoring transaction $ do
    Client.callManagementAPI checkedMerchantId opCity (.knowledgeCenterDSL.postKnowledgeCenterVideoUploadLink) requestorId req

postKnowledgeCenterVideoConfirm ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Common.KnowledgeCenterConfirmVideoReq ->
  Flow APISuccess
postKnowledgeCenterVideoConfirm merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let requestorId = apiTokenInfo.personId.getId
  Client.callManagementAPI checkedMerchantId opCity (.knowledgeCenterDSL.postKnowledgeCenterVideoConfirm) requestorId req

putKnowledgeCenterSopTypeRename ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Common.KnowledgeCenterRenameSopTypeReq ->
  Flow APISuccess
putKnowledgeCenterSopTypeRename merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let requestorId = apiTokenInfo.personId.getId
  Client.callManagementAPI checkedMerchantId opCity (.knowledgeCenterDSL.putKnowledgeCenterSopTypeRename) requestorId req

deleteKnowledgeCenterSopDocument ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Text ->
  Flow APISuccess
deleteKnowledgeCenterSopDocument merchantShortId opCity apiTokenInfo knowledgeCenterId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let requestorId = apiTokenInfo.personId.getId
  Client.callManagementAPI checkedMerchantId opCity (.knowledgeCenterDSL.deleteKnowledgeCenterSopDocument) requestorId knowledgeCenterId

deleteKnowledgeCenterSopType ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Text ->
  Flow APISuccess
deleteKnowledgeCenterSopType merchantShortId opCity apiTokenInfo sopType = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let requestorId = apiTokenInfo.personId.getId
  Client.callManagementAPI checkedMerchantId opCity (.knowledgeCenterDSL.deleteKnowledgeCenterSopType) requestorId sopType

getKnowledgeCenterSopList ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Maybe Text ->
  Flow Common.KnowledgeCenterSopListResp
getKnowledgeCenterSopList merchantShortId opCity apiTokenInfo merchantOperatingCityId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let requestorId = apiTokenInfo.personId.getId
  Client.callManagementAPI checkedMerchantId opCity (.knowledgeCenterDSL.getKnowledgeCenterSopList) merchantOperatingCityId requestorId
