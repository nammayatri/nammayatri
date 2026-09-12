{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.Management.MediaFileDocument
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management.MediaFileDocument
import qualified Domain.Action.Dashboard.Management.MediaFileDocument
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth
import Tools.Auth.DashboardUserAuth

type API = ("mediaFileDocument" :> (PostMediaFileDocumentUploadLink :<|> PostMediaFileDocumentConfirm :<|> PostMediaFileDocumentDelete :<|> GetMediaFileDocumentDownloadLink))

type PostMediaFileDocumentUploadLink =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/MEDIA_FILE_DOCUMENT/POST_MEDIA_FILE_DOCUMENT_UPLOAD_LINK"
      :> API.Types.ProviderPlatform.Management.MediaFileDocument.PostMediaFileDocumentUploadLink
  )

type PostMediaFileDocumentConfirm =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/MEDIA_FILE_DOCUMENT/POST_MEDIA_FILE_DOCUMENT_CONFIRM"
      :> API.Types.ProviderPlatform.Management.MediaFileDocument.PostMediaFileDocumentConfirm
  )

type PostMediaFileDocumentDelete =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/MEDIA_FILE_DOCUMENT/POST_MEDIA_FILE_DOCUMENT_DELETE"
      :> API.Types.ProviderPlatform.Management.MediaFileDocument.PostMediaFileDocumentDelete
  )

type GetMediaFileDocumentDownloadLink =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/MEDIA_FILE_DOCUMENT/GET_MEDIA_FILE_DOCUMENT_DOWNLOAD_LINK"
      :> API.Types.ProviderPlatform.Management.MediaFileDocument.GetMediaFileDocumentDownloadLink
  )

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postMediaFileDocumentUploadLink merchantId city :<|> postMediaFileDocumentConfirm merchantId city :<|> postMediaFileDocumentDelete merchantId city :<|> getMediaFileDocumentDownloadLink merchantId city

postMediaFileDocumentUploadLink :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Management.MediaFileDocument.UploadMediaFileDocumentReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.MediaFileDocument.MediaFileDocumentResp)
postMediaFileDocumentUploadLink a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.MediaFileDocument.postMediaFileDocumentUploadLink a4 a3 (Tools.Auth.DashboardUserAuth.dashboardRequestorId a2) a1

postMediaFileDocumentConfirm :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Management.MediaFileDocument.MediaFileDocumentReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMediaFileDocumentConfirm a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.MediaFileDocument.postMediaFileDocumentConfirm a4 a3 (Tools.Auth.DashboardUserAuth.dashboardRequestorId a2) a1

postMediaFileDocumentDelete :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Management.MediaFileDocument.MediaFileDocumentReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMediaFileDocumentDelete a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.MediaFileDocument.postMediaFileDocumentDelete a4 a3 (Tools.Auth.DashboardUserAuth.dashboardRequestorId a2) a1

getMediaFileDocumentDownloadLink :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.MediaFileDocument.MediaFileDocumentResp)
getMediaFileDocumentDownloadLink a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.MediaFileDocument.getMediaFileDocumentDownloadLink a4 a3 a1 (Tools.Auth.DashboardUserAuth.dashboardRequestorId a2)
