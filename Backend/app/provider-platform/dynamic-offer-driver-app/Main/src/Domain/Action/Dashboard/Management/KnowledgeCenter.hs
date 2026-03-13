{-
  Knowledge Center dashboard APIs: list, upload image, upload video (link + confirm),
  rename SOP type, delete document, delete by SOP type.
  API types (KnowledgeCenterSopListResp, etc.) are generated from
  CommonAPIs/spec/ProviderPlatform/Management/API/KnowledgeCenter.yaml.
  Run the generator to create API.Types.ProviderPlatform.Management.KnowledgeCenter.
-}

module Domain.Action.Dashboard.Management.KnowledgeCenter
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

import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Management.KnowledgeCenter as API
import qualified Domain.Types.Merchant as DM
import Environment
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import qualified SharedLogic.KnowledgeCenter as SL

getKnowledgeCenterGetDocument ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Text ->
  Flow API.GetKnowledgeCenterDocumentResp
getKnowledgeCenterGetDocument merchantShortId _ knowledgeCenterId _requestorId = do
  (fileType, imageBase64, viewLink) <- SL.knowledgeCenterGetDocument merchantShortId knowledgeCenterId
  pure $
    API.GetKnowledgeCenterDocumentResp
      { API.fileType = fileType,
        API.imageBase64 = imageBase64,
        API.viewLink = viewLink
      }

getKnowledgeCenterSopList ::
  ShortId DM.Merchant ->
  Context.City ->
  Maybe Text ->
  Text ->
  Flow API.KnowledgeCenterSopListResp
getKnowledgeCenterSopList merchantShortId opCity mbMerchantOperatingCityId _requestorId = do
  sopTypeDocs <- SL.knowledgeCenterSopList merchantShortId opCity mbMerchantOperatingCityId
  pure $
    API.KnowledgeCenterSopListResp
      { API.sopTypeDocuments =
          map
            (\(sopType, ids) -> API.SopTypeDocumentsItem {API.sopType = sopType, API.knowledgeCenterIds = ids})
            sopTypeDocs
      }

postKnowledgeCenterSopUpload ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  API.KnowledgeCenterUploadImageReq ->
  Flow API.KnowledgeCenterUploadImageResp
postKnowledgeCenterSopUpload merchantShortId opCity _requestorId req = do
  kcId <-
    SL.knowledgeCenterUploadImage
      merchantShortId
      opCity
      ((API.sopType :: API.KnowledgeCenterUploadImageReq -> Text) req)
      ((API.imageBase64 :: API.KnowledgeCenterUploadImageReq -> Text) req)
      ((API.fileExtension :: API.KnowledgeCenterUploadImageReq -> Maybe Text) req)
      ((API.merchantOperatingCityId :: API.KnowledgeCenterUploadImageReq -> Maybe Text) req)
  pure API.KnowledgeCenterUploadImageResp {API.knowledgeCenterId = kcId.getId}

postKnowledgeCenterVideoUploadLink ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  API.KnowledgeCenterUploadVideoLinkReq ->
  Flow API.KnowledgeCenterUploadVideoLinkResp
postKnowledgeCenterVideoUploadLink merchantShortId opCity _requestorId req = do
  (uploadLink, kcId) <-
    SL.knowledgeCenterVideoUploadLink
      merchantShortId
      opCity
      ((API.sopType :: API.KnowledgeCenterUploadVideoLinkReq -> Text) req)
      ((API.reqContentType :: API.KnowledgeCenterUploadVideoLinkReq -> Text) req)
      ((API.merchantOperatingCityId :: API.KnowledgeCenterUploadVideoLinkReq -> Maybe Text) req)
  pure
    API.KnowledgeCenterUploadVideoLinkResp
      { API.uploadLink = uploadLink,
        API.knowledgeCenterId = kcId.getId
      }

postKnowledgeCenterVideoConfirm ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  API.KnowledgeCenterConfirmVideoReq ->
  Flow APISuccess
postKnowledgeCenterVideoConfirm _ _ _ req =
  SL.knowledgeCenterVideoConfirm ((API.knowledgeCenterId :: API.KnowledgeCenterConfirmVideoReq -> Text) req)

putKnowledgeCenterSopTypeRename ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  API.KnowledgeCenterRenameSopTypeReq ->
  Flow APISuccess
putKnowledgeCenterSopTypeRename _ _ _ req =
  let req' = req :: API.KnowledgeCenterRenameSopTypeReq
   in SL.knowledgeCenterRenameSopType (API.newSopType req') (API.oldSopType req')

deleteKnowledgeCenterSopDocument ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Text ->
  Flow APISuccess
deleteKnowledgeCenterSopDocument _ _ _ knowledgeCenterId =
  SL.knowledgeCenterDeleteDocument knowledgeCenterId

deleteKnowledgeCenterSopType ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Text ->
  Flow APISuccess
deleteKnowledgeCenterSopType _ _ _ sopType =
  SL.knowledgeCenterDeleteBySopType sopType
