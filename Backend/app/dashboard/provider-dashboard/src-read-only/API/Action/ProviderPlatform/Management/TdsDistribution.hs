{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.ProviderPlatform.Management.TdsDistribution
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management
import qualified API.Types.ProviderPlatform.Management.TdsDistribution
import qualified Domain.Action.ProviderPlatform.Management.TdsDistribution
import "dynamic-offer-driver-app" Domain.Types.AccessMatrix
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

type API = ("tdsDistribution" :> (PostTdsDistributionBatch :<|> PostTdsDistributionBatchValidate :<|> GetTdsDistributionBatch :<|> GetTdsDistributionBatchFiles :<|> PostTdsDistributionBatchCancel :<|> PostTdsDistributionBatchConfirm :<|> GetTdsDistributionBatchRecords :<|> PostTdsDistributionBatchRetryFailed :<|> GetTdsDistributionBatches :<|> GetTdsDistributionSummary :<|> GetTdsDistributionPersonCertificates :<|> GetTdsDistributionRecordDownloadUrl :<|> PostTdsDistributionRecordRetry))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postTdsDistributionBatch merchantId city :<|> postTdsDistributionBatchValidate merchantId city :<|> getTdsDistributionBatch merchantId city :<|> getTdsDistributionBatchFiles merchantId city :<|> postTdsDistributionBatchCancel merchantId city :<|> postTdsDistributionBatchConfirm merchantId city :<|> getTdsDistributionBatchRecords merchantId city :<|> postTdsDistributionBatchRetryFailed merchantId city :<|> getTdsDistributionBatches merchantId city :<|> getTdsDistributionSummary merchantId city :<|> getTdsDistributionPersonCertificates merchantId city :<|> getTdsDistributionRecordDownloadUrl merchantId city :<|> postTdsDistributionRecordRetry merchantId city

type PostTdsDistributionBatch =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.TDS_DISTRIBUTION) / ('API.Types.ProviderPlatform.Management.TdsDistribution.POST_TDS_DISTRIBUTION_BATCH))
      :> API.Types.ProviderPlatform.Management.TdsDistribution.PostTdsDistributionBatch
  )

type PostTdsDistributionBatchValidate =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.TDS_DISTRIBUTION) / ('API.Types.ProviderPlatform.Management.TdsDistribution.POST_TDS_DISTRIBUTION_BATCH_VALIDATE))
      :> API.Types.ProviderPlatform.Management.TdsDistribution.PostTdsDistributionBatchValidate
  )

type GetTdsDistributionBatch =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.TDS_DISTRIBUTION) / ('API.Types.ProviderPlatform.Management.TdsDistribution.GET_TDS_DISTRIBUTION_BATCH))
      :> API.Types.ProviderPlatform.Management.TdsDistribution.GetTdsDistributionBatch
  )

type GetTdsDistributionBatchFiles =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.TDS_DISTRIBUTION) / ('API.Types.ProviderPlatform.Management.TdsDistribution.GET_TDS_DISTRIBUTION_BATCH_FILES))
      :> API.Types.ProviderPlatform.Management.TdsDistribution.GetTdsDistributionBatchFiles
  )

type PostTdsDistributionBatchCancel =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.TDS_DISTRIBUTION) / ('API.Types.ProviderPlatform.Management.TdsDistribution.POST_TDS_DISTRIBUTION_BATCH_CANCEL))
      :> API.Types.ProviderPlatform.Management.TdsDistribution.PostTdsDistributionBatchCancel
  )

type PostTdsDistributionBatchConfirm =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.TDS_DISTRIBUTION) / ('API.Types.ProviderPlatform.Management.TdsDistribution.POST_TDS_DISTRIBUTION_BATCH_CONFIRM))
      :> API.Types.ProviderPlatform.Management.TdsDistribution.PostTdsDistributionBatchConfirm
  )

type GetTdsDistributionBatchRecords =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.TDS_DISTRIBUTION) / ('API.Types.ProviderPlatform.Management.TdsDistribution.GET_TDS_DISTRIBUTION_BATCH_RECORDS))
      :> API.Types.ProviderPlatform.Management.TdsDistribution.GetTdsDistributionBatchRecords
  )

type PostTdsDistributionBatchRetryFailed =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.TDS_DISTRIBUTION) / ('API.Types.ProviderPlatform.Management.TdsDistribution.POST_TDS_DISTRIBUTION_BATCH_RETRY_FAILED))
      :> API.Types.ProviderPlatform.Management.TdsDistribution.PostTdsDistributionBatchRetryFailed
  )

type GetTdsDistributionBatches =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.TDS_DISTRIBUTION) / ('API.Types.ProviderPlatform.Management.TdsDistribution.GET_TDS_DISTRIBUTION_BATCHES))
      :> API.Types.ProviderPlatform.Management.TdsDistribution.GetTdsDistributionBatches
  )

type GetTdsDistributionSummary =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.TDS_DISTRIBUTION) / ('API.Types.ProviderPlatform.Management.TdsDistribution.GET_TDS_DISTRIBUTION_SUMMARY))
      :> API.Types.ProviderPlatform.Management.TdsDistribution.GetTdsDistributionSummary
  )

type GetTdsDistributionPersonCertificates =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.TDS_DISTRIBUTION) / ('API.Types.ProviderPlatform.Management.TdsDistribution.GET_TDS_DISTRIBUTION_PERSON_CERTIFICATES))
      :> API.Types.ProviderPlatform.Management.TdsDistribution.GetTdsDistributionPersonCertificates
  )

type GetTdsDistributionRecordDownloadUrl =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.TDS_DISTRIBUTION) / ('API.Types.ProviderPlatform.Management.TdsDistribution.GET_TDS_DISTRIBUTION_RECORD_DOWNLOAD_URL))
      :> API.Types.ProviderPlatform.Management.TdsDistribution.GetTdsDistributionRecordDownloadUrl
  )

type PostTdsDistributionRecordRetry =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.TDS_DISTRIBUTION) / ('API.Types.ProviderPlatform.Management.TdsDistribution.POST_TDS_DISTRIBUTION_RECORD_RETRY))
      :> API.Types.ProviderPlatform.Management.TdsDistribution.PostTdsDistributionRecordRetry
  )

postTdsDistributionBatch :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> API.Types.ProviderPlatform.Management.TdsDistribution.CreateTdsBatchReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.TdsDistribution.CreateTdsBatchResp)
postTdsDistributionBatch merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.TdsDistribution.postTdsDistributionBatch merchantShortId opCity apiTokenInfo req

postTdsDistributionBatchValidate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.TdsDistribution.TdsBatchResp)
postTdsDistributionBatchValidate merchantShortId opCity apiTokenInfo batchId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.TdsDistribution.postTdsDistributionBatchValidate merchantShortId opCity apiTokenInfo batchId

getTdsDistributionBatch :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.TdsDistribution.TdsBatchResp)
getTdsDistributionBatch merchantShortId opCity apiTokenInfo batchId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.TdsDistribution.getTdsDistributionBatch merchantShortId opCity apiTokenInfo batchId

getTdsDistributionBatchFiles :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (API.Types.ProviderPlatform.Management.TdsDistribution.TdsFileFilter) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.TdsDistribution.TdsFileListResp)
getTdsDistributionBatchFiles merchantShortId opCity apiTokenInfo batchId fileFilter search limit offset = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.TdsDistribution.getTdsDistributionBatchFiles merchantShortId opCity apiTokenInfo batchId fileFilter search limit offset

postTdsDistributionBatchCancel :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Text -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postTdsDistributionBatchCancel merchantShortId opCity apiTokenInfo batchId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.TdsDistribution.postTdsDistributionBatchCancel merchantShortId opCity apiTokenInfo batchId

postTdsDistributionBatchConfirm :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Text -> API.Types.ProviderPlatform.Management.TdsDistribution.TdsConfirmReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.TdsDistribution.TdsBatchResp)
postTdsDistributionBatchConfirm merchantShortId opCity apiTokenInfo batchId req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.TdsDistribution.postTdsDistributionBatchConfirm merchantShortId opCity apiTokenInfo batchId req

getTdsDistributionBatchRecords :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (API.Types.ProviderPlatform.Management.TdsDistribution.TdsRecordFilter) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.TdsDistribution.TdsRecordListResp)
getTdsDistributionBatchRecords merchantShortId opCity apiTokenInfo batchId recordFilter search limit offset = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.TdsDistribution.getTdsDistributionBatchRecords merchantShortId opCity apiTokenInfo batchId recordFilter search limit offset

postTdsDistributionBatchRetryFailed :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Text -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postTdsDistributionBatchRetryFailed merchantShortId opCity apiTokenInfo batchId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.TdsDistribution.postTdsDistributionBatchRetryFailed merchantShortId opCity apiTokenInfo batchId

getTdsDistributionBatches :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.TdsDistribution.TdsBatchListResp)
getTdsDistributionBatches merchantShortId opCity apiTokenInfo limit offset = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.TdsDistribution.getTdsDistributionBatches merchantShortId opCity apiTokenInfo limit offset

getTdsDistributionSummary :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.TdsDistribution.TdsSummaryResp)
getTdsDistributionSummary merchantShortId opCity apiTokenInfo financialYear = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.TdsDistribution.getTdsDistributionSummary merchantShortId opCity apiTokenInfo financialYear

getTdsDistributionPersonCertificates :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.TdsDistribution.TdsPersonCertificatesResp)
getTdsDistributionPersonCertificates merchantShortId opCity apiTokenInfo personId financialYear = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.TdsDistribution.getTdsDistributionPersonCertificates merchantShortId opCity apiTokenInfo personId financialYear

getTdsDistributionRecordDownloadUrl :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.TdsDistribution.TdsDownloadUrlResp)
getTdsDistributionRecordDownloadUrl merchantShortId opCity apiTokenInfo recordId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.TdsDistribution.getTdsDistributionRecordDownloadUrl merchantShortId opCity apiTokenInfo recordId

postTdsDistributionRecordRetry :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Text -> API.Types.ProviderPlatform.Management.TdsDistribution.TdsRetryReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.TdsDistribution.TdsRecordItem)
postTdsDistributionRecordRetry merchantShortId opCity apiTokenInfo recordId req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.TdsDistribution.postTdsDistributionRecordRetry merchantShortId opCity apiTokenInfo recordId req
