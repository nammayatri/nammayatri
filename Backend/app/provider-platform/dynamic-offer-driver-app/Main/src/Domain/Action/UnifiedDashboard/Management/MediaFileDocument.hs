module Domain.Action.UnifiedDashboard.Management.MediaFileDocument
  ( postMediaFileDocumentUploadLink,
    postMediaFileDocumentConfirm,
    postMediaFileDocumentDelete,
    getMediaFileDocumentDownloadLink,
  )
where

import qualified API.Types.UnifiedDashboard.Management.MediaFileDocument
import qualified Domain.Types.Common
import qualified Domain.Types.Merchant
import qualified Environment
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import qualified SharedLogic.MediaFileDocument as SMFD

postMediaFileDocumentUploadLink :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> API.Types.UnifiedDashboard.Management.MediaFileDocument.UploadMediaFileDocumentReq -> Environment.Flow API.Types.UnifiedDashboard.Management.MediaFileDocument.MediaFileDocumentResp)
postMediaFileDocumentUploadLink = SMFD.mediaFileDocumentUploadLink

postMediaFileDocumentConfirm :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> API.Types.UnifiedDashboard.Management.MediaFileDocument.MediaFileDocumentReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postMediaFileDocumentConfirm = SMFD.mediaFileDocumentConfirm

postMediaFileDocumentDelete :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> API.Types.UnifiedDashboard.Management.MediaFileDocument.MediaFileDocumentReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postMediaFileDocumentDelete = SMFD.mediaFileDocumentDelete

getMediaFileDocumentDownloadLink :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Domain.Types.Common.MediaFileDocumentType -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> Environment.Flow API.Types.UnifiedDashboard.Management.MediaFileDocument.MediaFileDocumentResp)
getMediaFileDocumentDownloadLink = SMFD.mediaFileDocumentDownloadLink
