module Domain.Action.Dashboard.Management.MediaFileDocument
  ( postMediaFileDocumentUploadLink,
    postMediaFileDocumentConfirm,
    postMediaFileDocumentDelete,
    getMediaFileDocumentDownloadLink,
  )
where

import qualified API.Types.ProviderPlatform.Management.MediaFileDocument as Common
import qualified "this" API.Types.UnifiedDashboard.Management.MediaFileDocument as UCommon
import qualified "dashboard-helper-api" Dashboard.Common.MediaFileDocument as CommonMFD
import qualified Domain.Types.Common as DCommon
import qualified Domain.Types.MediaFileDocument as DMFD
import qualified Domain.Types.Merchant
import qualified Environment
import Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import qualified SharedLogic.MediaFileDocument as SMFD

postMediaFileDocumentUploadLink :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Common.UploadMediaFileDocumentReq -> Environment.Flow Common.MediaFileDocumentResp)
postMediaFileDocumentUploadLink merchantShortId opCity requestorId req = castMediaFileDocumentResp <$> SMFD.mediaFileDocumentUploadLink merchantShortId opCity requestorId (castUploadMediaFileDocumentReq req)

castUploadMediaFileDocumentReq :: Common.UploadMediaFileDocumentReq -> UCommon.UploadMediaFileDocumentReq
castUploadMediaFileDocumentReq Common.UploadMediaFileDocumentReq {..} = UCommon.UploadMediaFileDocumentReq {mediaFileDocumentType = castMediaFileDocumentType mediaFileDocumentType, ..}

castMediaFileDocumentResp :: UCommon.MediaFileDocumentResp -> Common.MediaFileDocumentResp
castMediaFileDocumentResp UCommon.MediaFileDocumentResp {..} = Common.MediaFileDocumentResp {mediaFileDocumentId = Kernel.Types.Id.cast @DMFD.MediaFileDocument @CommonMFD.MediaFileDocument mediaFileDocumentId, mediaFileDocumentStatus = castMediaFileDocumentStatus mediaFileDocumentStatus, ..}

postMediaFileDocumentConfirm :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Common.MediaFileDocumentReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postMediaFileDocumentConfirm merchantShortId opCity requestorId req = SMFD.mediaFileDocumentConfirm merchantShortId opCity requestorId (castMediaFileDocumentReq req)

castMediaFileDocumentReq :: Common.MediaFileDocumentReq -> UCommon.MediaFileDocumentReq
castMediaFileDocumentReq Common.MediaFileDocumentReq {..} = UCommon.MediaFileDocumentReq {mediaFileDocumentId = Kernel.Types.Id.cast @CommonMFD.MediaFileDocument @DMFD.MediaFileDocument mediaFileDocumentId, ..}

postMediaFileDocumentDelete :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Common.MediaFileDocumentReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postMediaFileDocumentDelete merchantShortId opCity requestorId req = SMFD.mediaFileDocumentDelete merchantShortId opCity requestorId (castMediaFileDocumentReq req)

getMediaFileDocumentDownloadLink :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> CommonMFD.MediaFileDocumentType -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> Environment.Flow Common.MediaFileDocumentResp)
getMediaFileDocumentDownloadLink merchantShortId opCity mediaFileDocumentType rcNumber requestorId = castMediaFileDocumentResp <$> SMFD.mediaFileDocumentDownloadLink merchantShortId opCity (castMediaFileDocumentType mediaFileDocumentType) rcNumber requestorId

castMediaFileDocumentType :: CommonMFD.MediaFileDocumentType -> DCommon.MediaFileDocumentType
castMediaFileDocumentType = \case
  Common.VehicleVideo -> DCommon.VehicleVideo

castMediaFileDocumentStatus :: DMFD.MediaFileDocumentStatus -> Common.MediaFileDocumentStatus
castMediaFileDocumentStatus = \case
  DMFD.PENDING -> Common.PENDING
  DMFD.DELETED -> Common.DELETED
  DMFD.FAILED -> Common.FAILED
  DMFD.CONFIRMED -> Common.CONFIRMED
  DMFD.COMPLETED -> Common.COMPLETED
