module Domain.Action.Dashboard.Management.MediaFileDocument
  ( postMediaFileDocumentUploadLink,
    postMediaFileDocumentConfirm,
    postMediaFileDocumentDelete,
    getMediaFileDocumentDownloadLink,
  )
where

import qualified API.Types.ProviderPlatform.Management.MediaFileDocument
import qualified Domain.Types.Merchant
import qualified Environment
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import qualified SharedLogic.MediaFileDocument as SMFD

postMediaFileDocumentUploadLink :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> API.Types.ProviderPlatform.Management.MediaFileDocument.UploadMediaFileDocumentReq -> Environment.Flow API.Types.ProviderPlatform.Management.MediaFileDocument.MediaFileUploadLinkResp)
postMediaFileDocumentUploadLink = SMFD.mediaFileDocumentUploadLink

postMediaFileDocumentConfirm :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> API.Types.ProviderPlatform.Management.MediaFileDocument.ConfirmMediaFileReq -> Environment.Flow API.Types.ProviderPlatform.Management.MediaFileDocument.ConfirmMediaFileResp)
postMediaFileDocumentConfirm = SMFD.mediaFileDocumentConfirm

postMediaFileDocumentDelete :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> API.Types.ProviderPlatform.Management.MediaFileDocument.DeleteMediaFileReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postMediaFileDocumentDelete = SMFD.mediaFileDocumentDelete

getMediaFileDocumentDownloadLink :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> Environment.Flow API.Types.ProviderPlatform.Management.MediaFileDocument.MediaFileDownloadResp)
getMediaFileDocumentDownloadLink = SMFD.mediaFileDocumentDownloadLink
