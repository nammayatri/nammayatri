module Domain.Action.ProviderPlatform.Management.TdsDistribution
  ( postTdsDistributionBatch,
    postTdsDistributionBatchValidate,
    getTdsDistributionBatch,
    getTdsDistributionBatchFiles,
    postTdsDistributionBatchCancel,
    postTdsDistributionBatchConfirm,
    getTdsDistributionBatchRecords,
    postTdsDistributionBatchRetryFailed,
    getTdsDistributionBatches,
    getTdsDistributionSummary,
    getTdsDistributionPersonCertificates,
    getTdsDistributionRecordDownloadUrl,
    postTdsDistributionRecordRetry,
  )
where

import qualified API.Client.ProviderPlatform.Management
import qualified API.Types.ProviderPlatform.Management.TdsDistribution
import "dynamic-offer-driver-app" Domain.Types.AccessMatrix
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified Domain.Types.Transaction
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import qualified SharedLogic.Transaction
import Storage.Beam.CommonInstances ()
import Tools.Auth.Merchant

postTdsDistributionBatch :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> API.Types.ProviderPlatform.Management.TdsDistribution.CreateTdsBatchReq -> Environment.Flow API.Types.ProviderPlatform.Management.TdsDistribution.CreateTdsBatchResp)
postTdsDistributionBatch merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.ActionAPI apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  let requestorId = apiTokenInfo.personId.getId
      uploadedByName = apiTokenInfo.person.firstName <> " " <> apiTokenInfo.person.lastName
  SharedLogic.Transaction.withTransactionStoring transaction $
    API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.tdsDistributionDSL.postTdsDistributionBatch) requestorId (req {API.Types.ProviderPlatform.Management.TdsDistribution.uploadedByName = Kernel.Prelude.Just uploadedByName} :: API.Types.ProviderPlatform.Management.TdsDistribution.CreateTdsBatchReq)

postTdsDistributionBatchValidate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Text -> Environment.Flow API.Types.ProviderPlatform.Management.TdsDistribution.TdsBatchResp)
postTdsDistributionBatchValidate merchantShortId opCity apiTokenInfo batchId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.ActionAPI apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing SharedLogic.Transaction.emptyRequest
  SharedLogic.Transaction.withTransactionStoring transaction $ API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.tdsDistributionDSL.postTdsDistributionBatchValidate) batchId

getTdsDistributionBatch :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Text -> Environment.Flow API.Types.ProviderPlatform.Management.TdsDistribution.TdsBatchResp)
getTdsDistributionBatch merchantShortId opCity apiTokenInfo batchId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.tdsDistributionDSL.getTdsDistributionBatch) batchId

getTdsDistributionBatchFiles :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe API.Types.ProviderPlatform.Management.TdsDistribution.TdsFileFilter -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Environment.Flow API.Types.ProviderPlatform.Management.TdsDistribution.TdsFileListResp)
getTdsDistributionBatchFiles merchantShortId opCity apiTokenInfo batchId mbFilter search limit offset = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.tdsDistributionDSL.getTdsDistributionBatchFiles) batchId mbFilter search limit offset

postTdsDistributionBatchCancel :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Text -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postTdsDistributionBatchCancel merchantShortId opCity apiTokenInfo batchId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.ActionAPI apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing SharedLogic.Transaction.emptyRequest
  SharedLogic.Transaction.withTransactionStoring transaction $ API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.tdsDistributionDSL.postTdsDistributionBatchCancel) batchId

postTdsDistributionBatchConfirm :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Text -> API.Types.ProviderPlatform.Management.TdsDistribution.TdsConfirmReq -> Environment.Flow API.Types.ProviderPlatform.Management.TdsDistribution.TdsBatchResp)
postTdsDistributionBatchConfirm merchantShortId opCity apiTokenInfo batchId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.ActionAPI apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  let requestorId = apiTokenInfo.personId.getId
      confirmedByName = apiTokenInfo.person.firstName <> " " <> apiTokenInfo.person.lastName
  SharedLogic.Transaction.withTransactionStoring transaction $
    API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.tdsDistributionDSL.postTdsDistributionBatchConfirm) batchId requestorId (req {API.Types.ProviderPlatform.Management.TdsDistribution.confirmedByName = Kernel.Prelude.Just confirmedByName} :: API.Types.ProviderPlatform.Management.TdsDistribution.TdsConfirmReq)

getTdsDistributionBatchRecords :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe API.Types.ProviderPlatform.Management.TdsDistribution.TdsRecordFilter -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Environment.Flow API.Types.ProviderPlatform.Management.TdsDistribution.TdsRecordListResp)
getTdsDistributionBatchRecords merchantShortId opCity apiTokenInfo batchId recordFilter search limit offset = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.tdsDistributionDSL.getTdsDistributionBatchRecords) batchId recordFilter search limit offset

postTdsDistributionBatchRetryFailed :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Text -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postTdsDistributionBatchRetryFailed merchantShortId opCity apiTokenInfo batchId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.ActionAPI apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing SharedLogic.Transaction.emptyRequest
  SharedLogic.Transaction.withTransactionStoring transaction $
    API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.tdsDistributionDSL.postTdsDistributionBatchRetryFailed) batchId apiTokenInfo.personId.getId

getTdsDistributionBatches :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Environment.Flow API.Types.ProviderPlatform.Management.TdsDistribution.TdsBatchListResp)
getTdsDistributionBatches merchantShortId opCity apiTokenInfo limit offset = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.tdsDistributionDSL.getTdsDistributionBatches) limit offset

getTdsDistributionSummary :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Text -> Environment.Flow API.Types.ProviderPlatform.Management.TdsDistribution.TdsSummaryResp)
getTdsDistributionSummary merchantShortId opCity apiTokenInfo financialYear = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.tdsDistributionDSL.getTdsDistributionSummary) financialYear

getTdsDistributionPersonCertificates :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> Environment.Flow API.Types.ProviderPlatform.Management.TdsDistribution.TdsPersonCertificatesResp)
getTdsDistributionPersonCertificates merchantShortId opCity apiTokenInfo personId financialYear = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.tdsDistributionDSL.getTdsDistributionPersonCertificates) personId financialYear

getTdsDistributionRecordDownloadUrl :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Text -> Environment.Flow API.Types.ProviderPlatform.Management.TdsDistribution.TdsDownloadUrlResp)
getTdsDistributionRecordDownloadUrl merchantShortId opCity apiTokenInfo recordId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.tdsDistributionDSL.getTdsDistributionRecordDownloadUrl) recordId

postTdsDistributionRecordRetry :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Text -> API.Types.ProviderPlatform.Management.TdsDistribution.TdsRetryReq -> Environment.Flow API.Types.ProviderPlatform.Management.TdsDistribution.TdsRecordItem)
postTdsDistributionRecordRetry merchantShortId opCity apiTokenInfo recordId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.ActionAPI apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $
    API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.tdsDistributionDSL.postTdsDistributionRecordRetry) recordId apiTokenInfo.personId.getId req
