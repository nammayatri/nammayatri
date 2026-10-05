{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.Management.TdsDistribution
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management.TdsDistribution
import qualified Domain.Action.Dashboard.Management.TdsDistribution
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import qualified Tools.ActorInfo
import Tools.Auth
import Tools.Auth.DashboardUserAuth

type API = ("tdsDistribution" :> (PostTdsDistributionBatch :<|> PostTdsDistributionBatchValidate :<|> GetTdsDistributionBatch :<|> GetTdsDistributionBatchFiles :<|> PostTdsDistributionBatchCancel :<|> PostTdsDistributionBatchConfirm :<|> GetTdsDistributionBatchRecords :<|> PostTdsDistributionBatchRetryFailed :<|> GetTdsDistributionBatches :<|> GetTdsDistributionSummary :<|> GetTdsDistributionPersonCertificates :<|> GetTdsDistributionRecordDownloadUrl :<|> PostTdsDistributionRecordRetry))

type PostTdsDistributionBatch =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/TDS_DISTRIBUTION/POST_TDS_DISTRIBUTION_BATCH"
      :> API.Types.ProviderPlatform.Management.TdsDistribution.PostTdsDistributionBatch
  )

type PostTdsDistributionBatchValidate =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/TDS_DISTRIBUTION/POST_TDS_DISTRIBUTION_BATCH_VALIDATE"
      :> API.Types.ProviderPlatform.Management.TdsDistribution.PostTdsDistributionBatchValidate
  )

type GetTdsDistributionBatch =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/TDS_DISTRIBUTION/GET_TDS_DISTRIBUTION_BATCH"
      :> API.Types.ProviderPlatform.Management.TdsDistribution.GetTdsDistributionBatch
  )

type GetTdsDistributionBatchFiles =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/TDS_DISTRIBUTION/GET_TDS_DISTRIBUTION_BATCH_FILES"
      :> API.Types.ProviderPlatform.Management.TdsDistribution.GetTdsDistributionBatchFiles
  )

type PostTdsDistributionBatchCancel =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/TDS_DISTRIBUTION/POST_TDS_DISTRIBUTION_BATCH_CANCEL"
      :> API.Types.ProviderPlatform.Management.TdsDistribution.PostTdsDistributionBatchCancel
  )

type PostTdsDistributionBatchConfirm =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/TDS_DISTRIBUTION/POST_TDS_DISTRIBUTION_BATCH_CONFIRM"
      :> API.Types.ProviderPlatform.Management.TdsDistribution.PostTdsDistributionBatchConfirm
  )

type GetTdsDistributionBatchRecords =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/TDS_DISTRIBUTION/GET_TDS_DISTRIBUTION_BATCH_RECORDS"
      :> API.Types.ProviderPlatform.Management.TdsDistribution.GetTdsDistributionBatchRecords
  )

type PostTdsDistributionBatchRetryFailed =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/TDS_DISTRIBUTION/POST_TDS_DISTRIBUTION_BATCH_RETRY_FAILED"
      :> API.Types.ProviderPlatform.Management.TdsDistribution.PostTdsDistributionBatchRetryFailed
  )

type GetTdsDistributionBatches =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/TDS_DISTRIBUTION/GET_TDS_DISTRIBUTION_BATCHES"
      :> API.Types.ProviderPlatform.Management.TdsDistribution.GetTdsDistributionBatches
  )

type GetTdsDistributionSummary =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/TDS_DISTRIBUTION/GET_TDS_DISTRIBUTION_SUMMARY"
      :> API.Types.ProviderPlatform.Management.TdsDistribution.GetTdsDistributionSummary
  )

type GetTdsDistributionPersonCertificates =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/TDS_DISTRIBUTION/GET_TDS_DISTRIBUTION_PERSON_CERTIFICATES"
      :> API.Types.ProviderPlatform.Management.TdsDistribution.GetTdsDistributionPersonCertificates
  )

type GetTdsDistributionRecordDownloadUrl =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/TDS_DISTRIBUTION/GET_TDS_DISTRIBUTION_RECORD_DOWNLOAD_URL"
      :> API.Types.ProviderPlatform.Management.TdsDistribution.GetTdsDistributionRecordDownloadUrl
  )

type PostTdsDistributionRecordRetry =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/TDS_DISTRIBUTION/POST_TDS_DISTRIBUTION_RECORD_RETRY"
      :> API.Types.ProviderPlatform.Management.TdsDistribution.PostTdsDistributionRecordRetry
  )

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postTdsDistributionBatch merchantId city :<|> postTdsDistributionBatchValidate merchantId city :<|> getTdsDistributionBatch merchantId city :<|> getTdsDistributionBatchFiles merchantId city :<|> postTdsDistributionBatchCancel merchantId city :<|> postTdsDistributionBatchConfirm merchantId city :<|> getTdsDistributionBatchRecords merchantId city :<|> postTdsDistributionBatchRetryFailed merchantId city :<|> getTdsDistributionBatches merchantId city :<|> getTdsDistributionSummary merchantId city :<|> getTdsDistributionPersonCertificates merchantId city :<|> getTdsDistributionRecordDownloadUrl merchantId city :<|> postTdsDistributionRecordRetry merchantId city

postTdsDistributionBatch :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Management.TdsDistribution.CreateTdsBatchReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.TdsDistribution.CreateTdsBatchResp)
postTdsDistributionBatch a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.DRIVER_OFFER_BPP_MANAGEMENT "PROVIDER_MANAGEMENT/TDS_DISTRIBUTION/POST_TDS_DISTRIBUTION_BATCH" a2 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a2 $ Domain.Action.Dashboard.Management.TdsDistribution.postTdsDistributionBatch a4 a3 (Tools.Auth.DashboardUserAuth.dashboardRequestorId a2) a1
    )

postTdsDistributionBatchValidate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.TdsDistribution.TdsBatchResp)
postTdsDistributionBatchValidate a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.DRIVER_OFFER_BPP_MANAGEMENT "PROVIDER_MANAGEMENT/TDS_DISTRIBUTION/POST_TDS_DISTRIBUTION_BATCH_VALIDATE" a2 (Kernel.Prelude.Nothing :: Kernel.Prelude.Maybe ())
        Tools.ActorInfo.withDashboardUserActorInfo a2 $ Domain.Action.Dashboard.Management.TdsDistribution.postTdsDistributionBatchValidate a4 a3 a1
    )

getTdsDistributionBatch :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.TdsDistribution.TdsBatchResp)
getTdsDistributionBatch a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a2 $ Domain.Action.Dashboard.Management.TdsDistribution.getTdsDistributionBatch a4 a3 a1

getTdsDistributionBatchFiles :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (API.Types.ProviderPlatform.Management.TdsDistribution.TdsFileFilter) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.TdsDistribution.TdsFileListResp)
getTdsDistributionBatchFiles a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a6 $ Domain.Action.Dashboard.Management.TdsDistribution.getTdsDistributionBatchFiles a8 a7 a5 a4 a3 a2 a1

postTdsDistributionBatchCancel :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postTdsDistributionBatchCancel a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.DRIVER_OFFER_BPP_MANAGEMENT "PROVIDER_MANAGEMENT/TDS_DISTRIBUTION/POST_TDS_DISTRIBUTION_BATCH_CANCEL" a2 (Kernel.Prelude.Nothing :: Kernel.Prelude.Maybe ())
        Tools.ActorInfo.withDashboardUserActorInfo a2 $ Domain.Action.Dashboard.Management.TdsDistribution.postTdsDistributionBatchCancel a4 a3 a1
    )

postTdsDistributionBatchConfirm :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> API.Types.ProviderPlatform.Management.TdsDistribution.TdsConfirmReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.TdsDistribution.TdsBatchResp)
postTdsDistributionBatchConfirm a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.DRIVER_OFFER_BPP_MANAGEMENT "PROVIDER_MANAGEMENT/TDS_DISTRIBUTION/POST_TDS_DISTRIBUTION_BATCH_CONFIRM" a3 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.Management.TdsDistribution.postTdsDistributionBatchConfirm a5 a4 a2 (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3) a1
    )

getTdsDistributionBatchRecords :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (API.Types.ProviderPlatform.Management.TdsDistribution.TdsRecordFilter) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.TdsDistribution.TdsRecordListResp)
getTdsDistributionBatchRecords a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a6 $ Domain.Action.Dashboard.Management.TdsDistribution.getTdsDistributionBatchRecords a8 a7 a5 a4 a3 a2 a1

postTdsDistributionBatchRetryFailed :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postTdsDistributionBatchRetryFailed a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.DRIVER_OFFER_BPP_MANAGEMENT "PROVIDER_MANAGEMENT/TDS_DISTRIBUTION/POST_TDS_DISTRIBUTION_BATCH_RETRY_FAILED" a2 (Kernel.Prelude.Nothing :: Kernel.Prelude.Maybe ())
        Tools.ActorInfo.withDashboardUserActorInfo a2 $ Domain.Action.Dashboard.Management.TdsDistribution.postTdsDistributionBatchRetryFailed a4 a3 a1 (Tools.Auth.DashboardUserAuth.dashboardRequestorId a2)
    )

getTdsDistributionBatches :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.TdsDistribution.TdsBatchListResp)
getTdsDistributionBatches a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.Management.TdsDistribution.getTdsDistributionBatches a5 a4 a2 a1

getTdsDistributionSummary :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.TdsDistribution.TdsSummaryResp)
getTdsDistributionSummary a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a2 $ Domain.Action.Dashboard.Management.TdsDistribution.getTdsDistributionSummary a4 a3 a1

getTdsDistributionPersonCertificates :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.TdsDistribution.TdsPersonCertificatesResp)
getTdsDistributionPersonCertificates a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.Management.TdsDistribution.getTdsDistributionPersonCertificates a5 a4 a2 a1

getTdsDistributionRecordDownloadUrl :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.TdsDistribution.TdsDownloadUrlResp)
getTdsDistributionRecordDownloadUrl a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a2 $ Domain.Action.Dashboard.Management.TdsDistribution.getTdsDistributionRecordDownloadUrl a4 a3 a1

postTdsDistributionRecordRetry :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> API.Types.ProviderPlatform.Management.TdsDistribution.TdsRetryReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.TdsDistribution.TdsRecordItem)
postTdsDistributionRecordRetry a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.DRIVER_OFFER_BPP_MANAGEMENT "PROVIDER_MANAGEMENT/TDS_DISTRIBUTION/POST_TDS_DISTRIBUTION_RECORD_RETRY" a3 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.Management.TdsDistribution.postTdsDistributionRecordRetry a5 a4 a2 (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3) a1
    )
