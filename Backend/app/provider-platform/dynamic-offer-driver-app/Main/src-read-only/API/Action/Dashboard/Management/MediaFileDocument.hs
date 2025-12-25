{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.Management.MediaFileDocument
  ( API.Types.ProviderPlatform.Management.MediaFileDocument.API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management.MediaFileDocument
import qualified Dashboard.Common.MediaFileDocument
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

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.ProviderPlatform.Management.MediaFileDocument.API)
handler merchantId city = postMediaFileDocumentUploadLink merchantId city :<|> postMediaFileDocumentConfirm merchantId city :<|> postMediaFileDocumentDelete merchantId city :<|> getMediaFileDocumentDownloadLink merchantId city

postMediaFileDocumentUploadLink :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> API.Types.ProviderPlatform.Management.MediaFileDocument.UploadMediaFileDocumentReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.MediaFileDocument.MediaFileDocumentResp)
postMediaFileDocumentUploadLink a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.MediaFileDocument.postMediaFileDocumentUploadLink a4 a3 a2 a1

postMediaFileDocumentConfirm :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> API.Types.ProviderPlatform.Management.MediaFileDocument.MediaFileDocumentReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMediaFileDocumentConfirm a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.MediaFileDocument.postMediaFileDocumentConfirm a4 a3 a2 a1

postMediaFileDocumentDelete :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> API.Types.ProviderPlatform.Management.MediaFileDocument.MediaFileDocumentReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMediaFileDocumentDelete a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.MediaFileDocument.postMediaFileDocumentDelete a4 a3 a2 a1

getMediaFileDocumentDownloadLink :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Dashboard.Common.MediaFileDocument.MediaFileDocumentType -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.MediaFileDocument.MediaFileDocumentResp)
getMediaFileDocumentDownloadLink a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.MediaFileDocument.getMediaFileDocumentDownloadLink a5 a4 a3 a2 a1
