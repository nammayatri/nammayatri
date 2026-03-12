{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Management.Endpoints.KnowledgeCenter where

import qualified AWS.S3
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import Servant
import Servant.Client

data GetKnowledgeCenterDocumentResp = GetKnowledgeCenterDocumentResp {fileType :: AWS.S3.FileType, imageBase64 :: Kernel.Prelude.Maybe Kernel.Prelude.Text, viewLink :: Kernel.Prelude.Maybe Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data KnowledgeCenterConfirmVideoReq = KnowledgeCenterConfirmVideoReq {knowledgeCenterId :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data KnowledgeCenterRenameSopTypeReq = KnowledgeCenterRenameSopTypeReq {oldSopType :: Kernel.Prelude.Text, newSopType :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data KnowledgeCenterSopListResp = KnowledgeCenterSopListResp {sopTypeDocuments :: [SopTypeDocumentsItem]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data KnowledgeCenterUploadImageReq = KnowledgeCenterUploadImageReq
  { sopType :: Kernel.Prelude.Text,
    imageBase64 :: Kernel.Prelude.Text,
    fileExtension :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    merchantOperatingCityId :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data KnowledgeCenterUploadImageResp = KnowledgeCenterUploadImageResp {knowledgeCenterId :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data KnowledgeCenterUploadVideoLinkReq = KnowledgeCenterUploadVideoLinkReq {sopType :: Kernel.Prelude.Text, reqContentType :: Kernel.Prelude.Text, merchantOperatingCityId :: Kernel.Prelude.Maybe Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data KnowledgeCenterUploadVideoLinkResp = KnowledgeCenterUploadVideoLinkResp {uploadLink :: Kernel.Prelude.Maybe Kernel.Prelude.Text, knowledgeCenterId :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SopTypeDocumentsItem = SopTypeDocumentsItem {sopType :: Kernel.Prelude.Text, knowledgeCenterIds :: [Kernel.Prelude.Text]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type API = ("knowledgeCenter" :> (GetKnowledgeCenterGetDocumentHelper :<|> GetKnowledgeCenterSopListHelper :<|> PostKnowledgeCenterSopUploadHelper :<|> PostKnowledgeCenterVideoUploadLinkHelper :<|> PostKnowledgeCenterVideoConfirmHelper :<|> PutKnowledgeCenterSopTypeRenameHelper :<|> DeleteKnowledgeCenterSopDocumentHelper :<|> DeleteKnowledgeCenterSopTypeHelper))

type GetKnowledgeCenterGetDocument = ("getDocument" :> Capture "knowledgeCenterId" Kernel.Prelude.Text :> Get ('[JSON]) GetKnowledgeCenterDocumentResp)

type GetKnowledgeCenterGetDocumentHelper =
  ( "getDocument" :> Capture "knowledgeCenterId" Kernel.Prelude.Text :> MandatoryQueryParam "requestorId" Kernel.Prelude.Text
      :> Get
           ('[JSON])
           GetKnowledgeCenterDocumentResp
  )

type GetKnowledgeCenterSopList = ("sop" :> "list" :> QueryParam "merchantOperatingCityId" Kernel.Prelude.Text :> Get ('[JSON]) KnowledgeCenterSopListResp)

type GetKnowledgeCenterSopListHelper =
  ( "sop" :> "list" :> QueryParam "merchantOperatingCityId" Kernel.Prelude.Text :> MandatoryQueryParam "requestorId" Kernel.Prelude.Text
      :> Get
           ('[JSON])
           KnowledgeCenterSopListResp
  )

type PostKnowledgeCenterSopUpload = ("sop" :> "upload" :> ReqBody ('[JSON]) KnowledgeCenterUploadImageReq :> Post ('[JSON]) KnowledgeCenterUploadImageResp)

type PostKnowledgeCenterSopUploadHelper =
  ( "sop" :> "upload" :> MandatoryQueryParam "requestorId" Kernel.Prelude.Text :> ReqBody ('[JSON]) KnowledgeCenterUploadImageReq
      :> Post
           ('[JSON])
           KnowledgeCenterUploadImageResp
  )

type PostKnowledgeCenterVideoUploadLink = ("video" :> "uploadLink" :> ReqBody ('[JSON]) KnowledgeCenterUploadVideoLinkReq :> Post ('[JSON]) KnowledgeCenterUploadVideoLinkResp)

type PostKnowledgeCenterVideoUploadLinkHelper =
  ( "video" :> "uploadLink" :> MandatoryQueryParam "requestorId" Kernel.Prelude.Text
      :> ReqBody
           ('[JSON])
           KnowledgeCenterUploadVideoLinkReq
      :> Post ('[JSON]) KnowledgeCenterUploadVideoLinkResp
  )

type PostKnowledgeCenterVideoConfirm = ("video" :> "confirm" :> ReqBody ('[JSON]) KnowledgeCenterConfirmVideoReq :> Post ('[JSON]) Kernel.Types.APISuccess.APISuccess)

type PostKnowledgeCenterVideoConfirmHelper =
  ( "video" :> "confirm" :> MandatoryQueryParam "requestorId" Kernel.Prelude.Text :> ReqBody ('[JSON]) KnowledgeCenterConfirmVideoReq
      :> Post
           ('[JSON])
           Kernel.Types.APISuccess.APISuccess
  )

type PutKnowledgeCenterSopTypeRename = ("sopType" :> "rename" :> ReqBody ('[JSON]) KnowledgeCenterRenameSopTypeReq :> Put ('[JSON]) Kernel.Types.APISuccess.APISuccess)

type PutKnowledgeCenterSopTypeRenameHelper =
  ( "sopType" :> "rename" :> MandatoryQueryParam "requestorId" Kernel.Prelude.Text :> ReqBody ('[JSON]) KnowledgeCenterRenameSopTypeReq
      :> Put
           ('[JSON])
           Kernel.Types.APISuccess.APISuccess
  )

type DeleteKnowledgeCenterSopDocument = ("sop" :> "document" :> Capture "knowledgeCenterId" Kernel.Prelude.Text :> Delete ('[JSON]) Kernel.Types.APISuccess.APISuccess)

type DeleteKnowledgeCenterSopDocumentHelper =
  ( "sop" :> "document" :> Capture "knowledgeCenterId" Kernel.Prelude.Text :> MandatoryQueryParam "requestorId" Kernel.Prelude.Text
      :> Delete
           ('[JSON])
           Kernel.Types.APISuccess.APISuccess
  )

type DeleteKnowledgeCenterSopType = ("sopType" :> MandatoryQueryParam "sopType" Kernel.Prelude.Text :> Delete ('[JSON]) Kernel.Types.APISuccess.APISuccess)

type DeleteKnowledgeCenterSopTypeHelper =
  ( "sopType" :> MandatoryQueryParam "sopType" Kernel.Prelude.Text :> MandatoryQueryParam "requestorId" Kernel.Prelude.Text
      :> Delete
           ('[JSON])
           Kernel.Types.APISuccess.APISuccess
  )

data KnowledgeCenterAPIs = KnowledgeCenterAPIs
  { getKnowledgeCenterGetDocument :: (Kernel.Prelude.Text -> Kernel.Prelude.Text -> EulerHS.Types.EulerClient GetKnowledgeCenterDocumentResp),
    getKnowledgeCenterSopList :: (Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Text -> EulerHS.Types.EulerClient KnowledgeCenterSopListResp),
    postKnowledgeCenterSopUpload :: (Kernel.Prelude.Text -> KnowledgeCenterUploadImageReq -> EulerHS.Types.EulerClient KnowledgeCenterUploadImageResp),
    postKnowledgeCenterVideoUploadLink :: (Kernel.Prelude.Text -> KnowledgeCenterUploadVideoLinkReq -> EulerHS.Types.EulerClient KnowledgeCenterUploadVideoLinkResp),
    postKnowledgeCenterVideoConfirm :: (Kernel.Prelude.Text -> KnowledgeCenterConfirmVideoReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess),
    putKnowledgeCenterSopTypeRename :: (Kernel.Prelude.Text -> KnowledgeCenterRenameSopTypeReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess),
    deleteKnowledgeCenterSopDocument :: (Kernel.Prelude.Text -> Kernel.Prelude.Text -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess),
    deleteKnowledgeCenterSopType :: (Kernel.Prelude.Text -> Kernel.Prelude.Text -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess)
  }

mkKnowledgeCenterAPIs :: (Client EulerHS.Types.EulerClient API -> KnowledgeCenterAPIs)
mkKnowledgeCenterAPIs knowledgeCenterClient = (KnowledgeCenterAPIs {..})
  where
    getKnowledgeCenterGetDocument :<|> getKnowledgeCenterSopList :<|> postKnowledgeCenterSopUpload :<|> postKnowledgeCenterVideoUploadLink :<|> postKnowledgeCenterVideoConfirm :<|> putKnowledgeCenterSopTypeRename :<|> deleteKnowledgeCenterSopDocument :<|> deleteKnowledgeCenterSopType = knowledgeCenterClient

data KnowledgeCenterUserActionType
  = GET_KNOWLEDGE_CENTER_GET_DOCUMENT
  | GET_KNOWLEDGE_CENTER_SOP_LIST
  | POST_KNOWLEDGE_CENTER_SOP_UPLOAD
  | POST_KNOWLEDGE_CENTER_VIDEO_UPLOAD_LINK
  | POST_KNOWLEDGE_CENTER_VIDEO_CONFIRM
  | PUT_KNOWLEDGE_CENTER_SOP_TYPE_RENAME
  | DELETE_KNOWLEDGE_CENTER_SOP_DOCUMENT
  | DELETE_KNOWLEDGE_CENTER_SOP_TYPE
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [(''KnowledgeCenterUserActionType)])
