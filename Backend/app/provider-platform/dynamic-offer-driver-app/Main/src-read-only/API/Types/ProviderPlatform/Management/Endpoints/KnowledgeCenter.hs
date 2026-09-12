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

data GetKnowledgeCenterDocumentResp = GetKnowledgeCenterDocumentResp
  { fileType :: AWS.S3.FileType,
    imageBase64 :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    viewLink :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    documentName :: Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data KnowledgeCenterDocumentItem = KnowledgeCenterDocumentItem {knowledgeCenterId :: Kernel.Prelude.Text, documentName :: Kernel.Prelude.Text}
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
    imageBase64 :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    documentName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    fileExtension :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data KnowledgeCenterUploadImageResp = KnowledgeCenterUploadImageResp {knowledgeCenterId :: Kernel.Prelude.Maybe Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SopTypeDocumentsItem = SopTypeDocumentsItem {sopType :: Kernel.Prelude.Text, documents :: [KnowledgeCenterDocumentItem]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type API = ("knowledgeCenter" :> (GetKnowledgeCenterGetDocumentHelper :<|> GetKnowledgeCenterSopListHelper :<|> PostKnowledgeCenterSopUploadHelper :<|> PutKnowledgeCenterSopTypeRenameHelper :<|> DeleteKnowledgeCenterSopDocumentHelper :<|> DeleteKnowledgeCenterSopTypeHelper))

type GetKnowledgeCenterGetDocument = ("getDocument" :> Capture "knowledgeCenterId" Kernel.Prelude.Text :> Get ('[JSON]) GetKnowledgeCenterDocumentResp)

type GetKnowledgeCenterGetDocumentHelper =
  ( "getDocument" :> Capture "knowledgeCenterId" Kernel.Prelude.Text :> MandatoryQueryParam "requestorId" Kernel.Prelude.Text
      :> Get
           ('[JSON])
           GetKnowledgeCenterDocumentResp
  )

type GetKnowledgeCenterSopList = ("sop" :> "list" :> QueryParam "sopType" Kernel.Prelude.Text :> Get ('[JSON]) KnowledgeCenterSopListResp)

type GetKnowledgeCenterSopListHelper =
  ( "sop" :> "list" :> QueryParam "sopType" Kernel.Prelude.Text :> MandatoryQueryParam "requestorId" Kernel.Prelude.Text
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
    putKnowledgeCenterSopTypeRename :: (Kernel.Prelude.Text -> KnowledgeCenterRenameSopTypeReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess),
    deleteKnowledgeCenterSopDocument :: (Kernel.Prelude.Text -> Kernel.Prelude.Text -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess),
    deleteKnowledgeCenterSopType :: (Kernel.Prelude.Text -> Kernel.Prelude.Text -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess)
  }

mkKnowledgeCenterAPIs :: (Client EulerHS.Types.EulerClient API -> KnowledgeCenterAPIs)
mkKnowledgeCenterAPIs knowledgeCenterClient = (KnowledgeCenterAPIs {..})
  where
    getKnowledgeCenterGetDocument :<|> getKnowledgeCenterSopList :<|> postKnowledgeCenterSopUpload :<|> putKnowledgeCenterSopTypeRename :<|> deleteKnowledgeCenterSopDocument :<|> deleteKnowledgeCenterSopType = knowledgeCenterClient

data KnowledgeCenterUserActionType
  = GET_KNOWLEDGE_CENTER_GET_DOCUMENT
  | GET_KNOWLEDGE_CENTER_SOP_LIST
  | POST_KNOWLEDGE_CENTER_SOP_UPLOAD
  | PUT_KNOWLEDGE_CENTER_SOP_TYPE_RENAME
  | DELETE_KNOWLEDGE_CENTER_SOP_DOCUMENT
  | DELETE_KNOWLEDGE_CENTER_SOP_TYPE
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [(''KnowledgeCenterUserActionType)])
