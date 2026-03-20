{-
  Knowledge Center dashboard APIs: list, upload image, rename SOP type, delete document, delete by SOP type.
  Image flow only (no video).
-}

module Domain.Action.Dashboard.Management.KnowledgeCenter
  ( getKnowledgeCenterGetDocument,
    getKnowledgeCenterSopList,
    postKnowledgeCenterSopUpload,
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
import Kernel.Utils.Logging (logDebug)
import qualified SharedLogic.KnowledgeCenter as SL

getKnowledgeCenterGetDocument ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Text ->
  Flow API.GetKnowledgeCenterDocumentResp
getKnowledgeCenterGetDocument merchantShortId opCity knowledgeCenterId _requestorId = do
  (fileType, imageBase64, viewLink, documentName) <- SL.knowledgeCenterGetDocument merchantShortId opCity knowledgeCenterId
  pure $
    API.GetKnowledgeCenterDocumentResp
      { API.fileType = fileType,
        API.imageBase64 = imageBase64,
        API.viewLink = viewLink,
        API.documentName = documentName
      }

getKnowledgeCenterSopList ::
  ShortId DM.Merchant ->
  Context.City ->
  Maybe Text ->
  Text ->
  Flow API.KnowledgeCenterSopListResp
getKnowledgeCenterSopList merchantShortId opCity mbSopType requestorId = do
  logDebug $
    "KnowledgeCenterSopList requested by: " <> requestorId
      <> " merchant: "
      <> merchantShortId.getShortId
      <> " city: "
      <> show opCity
      <> " sopType: "
      <> fromMaybe "all" mbSopType
  sopTypeDocs <- SL.knowledgeCenterSopList merchantShortId opCity Nothing mbSopType
  pure $
    API.KnowledgeCenterSopListResp
      { API.sopTypeDocuments =
          map
            ( \(sopType, docs) ->
                API.SopTypeDocumentsItem
                  { API.sopType = sopType,
                    API.documents =
                      map
                        (\(kcId, docName) -> API.KnowledgeCenterDocumentItem {API.knowledgeCenterId = kcId, API.documentName = docName})
                        docs
                  }
            )
            sopTypeDocs
      }

postKnowledgeCenterSopUpload ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  API.KnowledgeCenterUploadImageReq ->
  Flow API.KnowledgeCenterUploadImageResp
postKnowledgeCenterSopUpload merchantShortId opCity requestorId req = do
  logDebug $
    "KnowledgeCenterSopUpload requested by: " <> requestorId
      <> " merchant: "
      <> merchantShortId.getShortId
      <> " city: "
      <> show opCity
      <> " sopType: "
      <> req.sopType
      <> " documentName: "
      <> fromMaybe "" req.documentName
  mbKcId <-
    SL.knowledgeCenterUploadImage
      merchantShortId
      opCity
      req.sopType
      req.imageBase64
      req.documentName
      req.fileExtension
      Nothing
  pure API.KnowledgeCenterUploadImageResp {API.knowledgeCenterId = (.getId) <$> mbKcId}

putKnowledgeCenterSopTypeRename ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  API.KnowledgeCenterRenameSopTypeReq ->
  Flow APISuccess
putKnowledgeCenterSopTypeRename merchantShortId opCity _ req =
  SL.knowledgeCenterRenameSopType merchantShortId opCity req.newSopType req.oldSopType

deleteKnowledgeCenterSopDocument ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Text ->
  Flow APISuccess
deleteKnowledgeCenterSopDocument merchantShortId opCity knowledgeCenterId _requestorId =
  SL.knowledgeCenterDeleteDocument merchantShortId opCity knowledgeCenterId

deleteKnowledgeCenterSopType ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Text ->
  Flow APISuccess
deleteKnowledgeCenterSopType merchantShortId opCity sopType _requestorId =
  SL.knowledgeCenterDeleteBySopType merchantShortId opCity sopType
