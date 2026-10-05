{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.Management.TdsDistribution
  ( API.Types.ProviderPlatform.Management.TdsDistribution.API,
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
import Tools.Auth

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.ProviderPlatform.Management.TdsDistribution.API)
handler merchantId city = postTdsDistributionBatch merchantId city :<|> postTdsDistributionBatchValidate merchantId city :<|> getTdsDistributionBatch merchantId city :<|> getTdsDistributionBatchFiles merchantId city :<|> postTdsDistributionBatchCancel merchantId city :<|> postTdsDistributionBatchConfirm merchantId city :<|> getTdsDistributionBatchRecords merchantId city :<|> postTdsDistributionBatchRetryFailed merchantId city :<|> getTdsDistributionBatches merchantId city :<|> getTdsDistributionSummary merchantId city :<|> getTdsDistributionPersonCertificates merchantId city :<|> getTdsDistributionRecordDownloadUrl merchantId city :<|> postTdsDistributionRecordRetry merchantId city

postTdsDistributionBatch :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> API.Types.ProviderPlatform.Management.TdsDistribution.CreateTdsBatchReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.TdsDistribution.CreateTdsBatchResp)
postTdsDistributionBatch a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.TdsDistribution.postTdsDistributionBatch a4 a3 a2 a1

postTdsDistributionBatchValidate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.TdsDistribution.TdsBatchResp)
postTdsDistributionBatchValidate a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.TdsDistribution.postTdsDistributionBatchValidate a3 a2 a1

getTdsDistributionBatch :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.TdsDistribution.TdsBatchResp)
getTdsDistributionBatch a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.TdsDistribution.getTdsDistributionBatch a3 a2 a1

getTdsDistributionBatchFiles :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (API.Types.ProviderPlatform.Management.TdsDistribution.TdsFileFilter) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.TdsDistribution.TdsFileListResp)
getTdsDistributionBatchFiles a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.TdsDistribution.getTdsDistributionBatchFiles a7 a6 a5 a4 a3 a2 a1

postTdsDistributionBatchCancel :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postTdsDistributionBatchCancel a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.TdsDistribution.postTdsDistributionBatchCancel a3 a2 a1

postTdsDistributionBatchConfirm :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> API.Types.ProviderPlatform.Management.TdsDistribution.TdsConfirmReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.TdsDistribution.TdsBatchResp)
postTdsDistributionBatchConfirm a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.TdsDistribution.postTdsDistributionBatchConfirm a5 a4 a3 a2 a1

getTdsDistributionBatchRecords :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (API.Types.ProviderPlatform.Management.TdsDistribution.TdsRecordFilter) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.TdsDistribution.TdsRecordListResp)
getTdsDistributionBatchRecords a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.TdsDistribution.getTdsDistributionBatchRecords a7 a6 a5 a4 a3 a2 a1

postTdsDistributionBatchRetryFailed :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postTdsDistributionBatchRetryFailed a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.TdsDistribution.postTdsDistributionBatchRetryFailed a4 a3 a2 a1

getTdsDistributionBatches :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.TdsDistribution.TdsBatchListResp)
getTdsDistributionBatches a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.TdsDistribution.getTdsDistributionBatches a4 a3 a2 a1

getTdsDistributionSummary :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.TdsDistribution.TdsSummaryResp)
getTdsDistributionSummary a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.TdsDistribution.getTdsDistributionSummary a3 a2 a1

getTdsDistributionPersonCertificates :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.TdsDistribution.TdsPersonCertificatesResp)
getTdsDistributionPersonCertificates a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.TdsDistribution.getTdsDistributionPersonCertificates a4 a3 a2 a1

getTdsDistributionRecordDownloadUrl :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.TdsDistribution.TdsDownloadUrlResp)
getTdsDistributionRecordDownloadUrl a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.TdsDistribution.getTdsDistributionRecordDownloadUrl a3 a2 a1

postTdsDistributionRecordRetry :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> API.Types.ProviderPlatform.Management.TdsDistribution.TdsRetryReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.TdsDistribution.TdsRecordItem)
postTdsDistributionRecordRetry a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.TdsDistribution.postTdsDistributionRecordRetry a5 a4 a3 a2 a1
