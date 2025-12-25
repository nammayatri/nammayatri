{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.ProviderPlatform.Management.MediaFileDocument
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management
import qualified API.Types.ProviderPlatform.Management.MediaFileDocument
import qualified Dashboard.Common.MediaFileDocument
import qualified Domain.Action.ProviderPlatform.Management.MediaFileDocument
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
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
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.MEDIA_FILE_DOCUMENT / 'API.Types.ProviderPlatform.Management.MediaFileDocument.POST_MEDIA_FILE_DOCUMENT_UPLOAD_LINK)
      :> API.Types.ProviderPlatform.Management.MediaFileDocument.PostMediaFileDocumentUploadLink
  )

type PostMediaFileDocumentConfirm =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.MEDIA_FILE_DOCUMENT / 'API.Types.ProviderPlatform.Management.MediaFileDocument.POST_MEDIA_FILE_DOCUMENT_CONFIRM)
      :> API.Types.ProviderPlatform.Management.MediaFileDocument.PostMediaFileDocumentConfirm
  )

type PostMediaFileDocumentDelete =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.MEDIA_FILE_DOCUMENT / 'API.Types.ProviderPlatform.Management.MediaFileDocument.POST_MEDIA_FILE_DOCUMENT_DELETE)
      :> API.Types.ProviderPlatform.Management.MediaFileDocument.PostMediaFileDocumentDelete
  )

type GetMediaFileDocumentDownloadLink =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.MEDIA_FILE_DOCUMENT / 'API.Types.ProviderPlatform.Management.MediaFileDocument.GET_MEDIA_FILE_DOCUMENT_DOWNLOAD_LINK)
      :> API.Types.ProviderPlatform.Management.MediaFileDocument.GetMediaFileDocumentDownloadLink
  )

postMediaFileDocumentUploadLink :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.MediaFileDocument.UploadMediaFileDocumentReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.MediaFileDocument.MediaFileDocumentResp)
postMediaFileDocumentUploadLink merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.MediaFileDocument.postMediaFileDocumentUploadLink merchantShortId opCity apiTokenInfo req

postMediaFileDocumentConfirm :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.MediaFileDocument.MediaFileDocumentReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMediaFileDocumentConfirm merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.MediaFileDocument.postMediaFileDocumentConfirm merchantShortId opCity apiTokenInfo req

postMediaFileDocumentDelete :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.MediaFileDocument.MediaFileDocumentReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMediaFileDocumentDelete merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.MediaFileDocument.postMediaFileDocumentDelete merchantShortId opCity apiTokenInfo req

getMediaFileDocumentDownloadLink :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Dashboard.Common.MediaFileDocument.MediaFileDocumentType -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.MediaFileDocument.MediaFileDocumentResp)
getMediaFileDocumentDownloadLink merchantShortId opCity apiTokenInfo mediaFileDocumentType rcNumber = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.MediaFileDocument.getMediaFileDocumentDownloadLink merchantShortId opCity apiTokenInfo mediaFileDocumentType rcNumber
