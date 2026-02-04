{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UnifiedDashboard.Management.Endpoints.MediaFileDocument where

import qualified AWS.S3
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import qualified "this" Domain.Types.Common
import qualified "this" Domain.Types.MediaFileDocument
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.HideSecrets
import qualified Kernel.Types.Id
import Servant
import Servant.Client

newtype MediaFileDocumentReq = MediaFileDocumentReq {mediaFileDocumentId :: Kernel.Types.Id.Id Domain.Types.MediaFileDocument.MediaFileDocument}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets MediaFileDocumentReq where
  hideSecrets = Kernel.Prelude.identity

data MediaFileDocumentResp = MediaFileDocumentResp
  { mediaFileLink :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    mediaFileDocumentId :: Kernel.Types.Id.Id Domain.Types.MediaFileDocument.MediaFileDocument,
    mediaFileDocumentStatus :: Domain.Types.MediaFileDocument.MediaFileDocumentStatus
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MediaFileDocumentTResp = MediaFileDocumentTResp {mediaFileDocumentId :: Kernel.Types.Id.Id Domain.Types.MediaFileDocument.MediaFileDocument, mediaFileDocumentStatus :: Domain.Types.MediaFileDocument.MediaFileDocumentStatus}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data UploadMediaFileDocumentReq = UploadMediaFileDocumentReq {mediaFileDocumentType :: Domain.Types.Common.MediaFileDocumentType, fileType :: AWS.S3.FileType, reqContentType :: Kernel.Prelude.Text, rcNumber :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data UploadMediaFileDocumentTReq = UploadMediaFileDocumentTReq {mediaFileDocumentType :: Domain.Types.Common.MediaFileDocumentType, fileType :: AWS.S3.FileType, reqContentType :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type API = ("mediaFileDocument" :> (PostMediaFileDocumentUploadLinkHelper :<|> PostMediaFileDocumentConfirmHelper :<|> PostMediaFileDocumentDeleteHelper :<|> GetMediaFileDocumentDownloadLinkHelper))

type PostMediaFileDocumentUploadLink = ("uploadLink" :> ReqBody '[JSON] UploadMediaFileDocumentReq :> Post '[JSON] MediaFileDocumentResp)

type PostMediaFileDocumentUploadLinkHelper =
  ( "uploadLink" :> MandatoryQueryParam "requestorId" Kernel.Prelude.Text :> ReqBody '[JSON] UploadMediaFileDocumentReq
      :> Post
           '[JSON]
           MediaFileDocumentResp
  )

type PostMediaFileDocumentConfirm = ("confirm" :> ReqBody '[JSON] MediaFileDocumentReq :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type PostMediaFileDocumentConfirmHelper =
  ( "confirm" :> MandatoryQueryParam "requestorId" Kernel.Prelude.Text :> ReqBody '[JSON] MediaFileDocumentReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type PostMediaFileDocumentDelete = ("delete" :> ReqBody '[JSON] MediaFileDocumentReq :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type PostMediaFileDocumentDeleteHelper =
  ( "delete" :> MandatoryQueryParam "requestorId" Kernel.Prelude.Text :> ReqBody '[JSON] MediaFileDocumentReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type GetMediaFileDocumentDownloadLink =
  ( "downloadLink" :> MandatoryQueryParam "mediaFileDocumentType" Domain.Types.Common.MediaFileDocumentType
      :> MandatoryQueryParam
           "rcNumber"
           Kernel.Prelude.Text
      :> Get '[JSON] MediaFileDocumentResp
  )

type GetMediaFileDocumentDownloadLinkHelper =
  ( "downloadLink" :> MandatoryQueryParam "mediaFileDocumentType" Domain.Types.Common.MediaFileDocumentType
      :> MandatoryQueryParam
           "rcNumber"
           Kernel.Prelude.Text
      :> MandatoryQueryParam "requestorId" Kernel.Prelude.Text
      :> Get '[JSON] MediaFileDocumentResp
  )

data MediaFileDocumentAPIs = MediaFileDocumentAPIs
  { postMediaFileDocumentUploadLink :: Kernel.Prelude.Text -> UploadMediaFileDocumentReq -> EulerHS.Types.EulerClient MediaFileDocumentResp,
    postMediaFileDocumentConfirm :: Kernel.Prelude.Text -> MediaFileDocumentReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postMediaFileDocumentDelete :: Kernel.Prelude.Text -> MediaFileDocumentReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    getMediaFileDocumentDownloadLink :: Domain.Types.Common.MediaFileDocumentType -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> EulerHS.Types.EulerClient MediaFileDocumentResp
  }

mkMediaFileDocumentAPIs :: (Client EulerHS.Types.EulerClient API -> MediaFileDocumentAPIs)
mkMediaFileDocumentAPIs mediaFileDocumentClient = (MediaFileDocumentAPIs {..})
  where
    postMediaFileDocumentUploadLink :<|> postMediaFileDocumentConfirm :<|> postMediaFileDocumentDelete :<|> getMediaFileDocumentDownloadLink = mediaFileDocumentClient

data MediaFileDocumentUserActionType
  = POST_MEDIA_FILE_DOCUMENT_UPLOAD_LINK
  | POST_MEDIA_FILE_DOCUMENT_CONFIRM
  | POST_MEDIA_FILE_DOCUMENT_DELETE
  | GET_MEDIA_FILE_DOCUMENT_DOWNLOAD_LINK
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [''MediaFileDocumentUserActionType])
