module Domain.Action.Provider.Management.MediaFileDocument
  ( postMediaFileDocumentUploadLink,
    postMediaFileDocumentConfirm,
    postMediaFileDocumentDelete,
    getMediaFileDocumentDownloadLink,
  )
where

import qualified API.Client.Provider.Management
import qualified API.Types.UnifiedDashboard.Management.MediaFileDocument
import qualified Domain.Action.Management.Transaction
import qualified "dynamic-offer-driver-app" Domain.Types.Common
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api
import Tools.Auth.Merchant

postMediaFileDocumentUploadLink :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.UnifiedDashboard.Management.MediaFileDocument.UploadMediaFileDocumentReq -> Environment.Flow API.Types.UnifiedDashboard.Management.MediaFileDocument.MediaFileDocumentResp)
postMediaFileDocumentUploadLink merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- Domain.Action.Management.Transaction.buildTransaction apiTokenInfo.userActionType (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  let requestorId = apiTokenInfo.personId.getId
  Domain.Action.Management.Transaction.withResponseTransactionStoring transaction $ do
    API.Client.Provider.Management.callManagementAPI checkedMerchantId opCity (.mediaFileDocumentDSL.postMediaFileDocumentUploadLink) requestorId req

postMediaFileDocumentConfirm :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.UnifiedDashboard.Management.MediaFileDocument.MediaFileDocumentReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postMediaFileDocumentConfirm merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- Domain.Action.Management.Transaction.buildTransaction apiTokenInfo.userActionType (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  let requestorId = apiTokenInfo.personId.getId
  Domain.Action.Management.Transaction.withTransactionStoring transaction $ do
    API.Client.Provider.Management.callManagementAPI checkedMerchantId opCity (.mediaFileDocumentDSL.postMediaFileDocumentConfirm) requestorId req

postMediaFileDocumentDelete :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.UnifiedDashboard.Management.MediaFileDocument.MediaFileDocumentReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postMediaFileDocumentDelete merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- Domain.Action.Management.Transaction.buildTransaction apiTokenInfo.userActionType (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  let requestorId = apiTokenInfo.personId.getId
  Domain.Action.Management.Transaction.withTransactionStoring transaction $ do
    API.Client.Provider.Management.callManagementAPI checkedMerchantId opCity (.mediaFileDocumentDSL.postMediaFileDocumentDelete) requestorId req

getMediaFileDocumentDownloadLink :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Domain.Types.Common.MediaFileDocumentType -> Kernel.Prelude.Text -> Environment.Flow API.Types.UnifiedDashboard.Management.MediaFileDocument.MediaFileDocumentResp)
getMediaFileDocumentDownloadLink merchantShortId opCity apiTokenInfo mediaFileDocumentType rcNumber = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let requestorId = apiTokenInfo.personId.getId
  API.Client.Provider.Management.callManagementAPI checkedMerchantId opCity (.mediaFileDocumentDSL.getMediaFileDocumentDownloadLink) mediaFileDocumentType rcNumber requestorId
