{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Provider.Management.MediaFileDocument
  ( API,
    handler,
  )
where

import qualified "dynamic-offer-driver-app" API.Types.UnifiedDashboard.Management
import qualified "dynamic-offer-driver-app" API.Types.UnifiedDashboard.Management.MediaFileDocument
import qualified Domain.Action.Provider.Management.MediaFileDocument
import qualified "dynamic-offer-driver-app" Domain.Types.Common
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude hiding (sortOn)
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common hiding (INFO)
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("mediaFileDocument" :> (PostMediaFileDocumentUploadLink :<|> PostMediaFileDocumentConfirm :<|> PostMediaFileDocumentDelete :<|> GetMediaFileDocumentDownloadLink))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postMediaFileDocumentUploadLink merchantId city :<|> postMediaFileDocumentConfirm merchantId city :<|> postMediaFileDocumentDelete merchantId city :<|> getMediaFileDocumentDownloadLink merchantId city

type PostMediaFileDocumentUploadLink =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'POST_MANAGEMENT_MEDIA_FILE_DOCUMENT_UPLOAD_LINK
      :> API.Types.UnifiedDashboard.Management.MediaFileDocument.PostMediaFileDocumentUploadLink
  )

type PostMediaFileDocumentConfirm =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'POST_MANAGEMENT_MEDIA_FILE_DOCUMENT_CONFIRM
      :> API.Types.UnifiedDashboard.Management.MediaFileDocument.PostMediaFileDocumentConfirm
  )

type PostMediaFileDocumentDelete =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'POST_MANAGEMENT_MEDIA_FILE_DOCUMENT_DELETE
      :> API.Types.UnifiedDashboard.Management.MediaFileDocument.PostMediaFileDocumentDelete
  )

type GetMediaFileDocumentDownloadLink =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'GET_MANAGEMENT_MEDIA_FILE_DOCUMENT_DOWNLOAD_LINK
      :> API.Types.UnifiedDashboard.Management.MediaFileDocument.GetMediaFileDocumentDownloadLink
  )

postMediaFileDocumentUploadLink :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.UnifiedDashboard.Management.MediaFileDocument.UploadMediaFileDocumentReq -> Environment.FlowHandler API.Types.UnifiedDashboard.Management.MediaFileDocument.MediaFileDocumentResp)
postMediaFileDocumentUploadLink merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.Provider.Management.MediaFileDocument.postMediaFileDocumentUploadLink merchantShortId opCity apiTokenInfo req

postMediaFileDocumentConfirm :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.UnifiedDashboard.Management.MediaFileDocument.MediaFileDocumentReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMediaFileDocumentConfirm merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.Provider.Management.MediaFileDocument.postMediaFileDocumentConfirm merchantShortId opCity apiTokenInfo req

postMediaFileDocumentDelete :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.UnifiedDashboard.Management.MediaFileDocument.MediaFileDocumentReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMediaFileDocumentDelete merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.Provider.Management.MediaFileDocument.postMediaFileDocumentDelete merchantShortId opCity apiTokenInfo req

getMediaFileDocumentDownloadLink :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Domain.Types.Common.MediaFileDocumentType -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.UnifiedDashboard.Management.MediaFileDocument.MediaFileDocumentResp)
getMediaFileDocumentDownloadLink merchantShortId opCity apiTokenInfo mediaFileDocumentType rcNumber = withFlowHandlerAPI' $ Domain.Action.Provider.Management.MediaFileDocument.getMediaFileDocumentDownloadLink merchantShortId opCity apiTokenInfo mediaFileDocumentType rcNumber
