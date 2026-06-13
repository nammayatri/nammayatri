{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Management.Endpoints.MediaFileDocument where

import qualified AWS.S3
import qualified Dashboard.Common
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.HideSecrets
import qualified Kernel.Types.Id
import Servant
import Servant.Client

data ConfirmMediaFileReq = ConfirmMediaFileReq {fileId :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets ConfirmMediaFileReq where
  hideSecrets = Kernel.Prelude.identity

data ConfirmMediaFileResp = ConfirmMediaFileResp {fileId :: Kernel.Types.Id.Id Dashboard.Common.File}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DeleteMediaFileReq = DeleteMediaFileReq {fileId :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets DeleteMediaFileReq where
  hideSecrets = Kernel.Prelude.identity

data MediaFileDownloadResp = MediaFileDownloadResp {url :: Kernel.Prelude.Maybe Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MediaFileUploadLinkResp = MediaFileUploadLinkResp {uploadUrl :: Kernel.Prelude.Text, filePath :: Kernel.Prelude.Text, fileId :: Kernel.Types.Id.Id Dashboard.Common.File}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data UploadMediaFileDocumentReq = UploadMediaFileDocumentReq {fileType :: AWS.S3.FileType, reqContentType :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets UploadMediaFileDocumentReq where
  hideSecrets = Kernel.Prelude.identity

type API = ("mediaFileDocument" :> (PostMediaFileDocumentUploadLinkHelper :<|> PostMediaFileDocumentConfirmHelper :<|> PostMediaFileDocumentDeleteHelper :<|> GetMediaFileDocumentDownloadLinkHelper))

type PostMediaFileDocumentUploadLink = ("uploadLink" :> ReqBody '[JSON] UploadMediaFileDocumentReq :> Post '[JSON] MediaFileUploadLinkResp)

type PostMediaFileDocumentUploadLinkHelper =
  ( "uploadLink" :> MandatoryQueryParam "requestorId" Kernel.Prelude.Text :> ReqBody '[JSON] UploadMediaFileDocumentReq
      :> Post
           '[JSON]
           MediaFileUploadLinkResp
  )

type PostMediaFileDocumentConfirm = ("confirm" :> ReqBody '[JSON] ConfirmMediaFileReq :> Post '[JSON] ConfirmMediaFileResp)

type PostMediaFileDocumentConfirmHelper = ("confirm" :> MandatoryQueryParam "requestorId" Kernel.Prelude.Text :> ReqBody '[JSON] ConfirmMediaFileReq :> Post '[JSON] ConfirmMediaFileResp)

type PostMediaFileDocumentDelete = ("delete" :> ReqBody '[JSON] DeleteMediaFileReq :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type PostMediaFileDocumentDeleteHelper =
  ( "delete" :> MandatoryQueryParam "requestorId" Kernel.Prelude.Text :> ReqBody '[JSON] DeleteMediaFileReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type GetMediaFileDocumentDownloadLink = ("downloadLink" :> MandatoryQueryParam "fileId" Kernel.Prelude.Text :> Get '[JSON] MediaFileDownloadResp)

type GetMediaFileDocumentDownloadLinkHelper =
  ( "downloadLink" :> MandatoryQueryParam "fileId" Kernel.Prelude.Text :> MandatoryQueryParam "requestorId" Kernel.Prelude.Text
      :> Get
           '[JSON]
           MediaFileDownloadResp
  )

data MediaFileDocumentAPIs = MediaFileDocumentAPIs
  { postMediaFileDocumentUploadLink :: Kernel.Prelude.Text -> UploadMediaFileDocumentReq -> EulerHS.Types.EulerClient MediaFileUploadLinkResp,
    postMediaFileDocumentConfirm :: Kernel.Prelude.Text -> ConfirmMediaFileReq -> EulerHS.Types.EulerClient ConfirmMediaFileResp,
    postMediaFileDocumentDelete :: Kernel.Prelude.Text -> DeleteMediaFileReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    getMediaFileDocumentDownloadLink :: Kernel.Prelude.Text -> Kernel.Prelude.Text -> EulerHS.Types.EulerClient MediaFileDownloadResp
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
