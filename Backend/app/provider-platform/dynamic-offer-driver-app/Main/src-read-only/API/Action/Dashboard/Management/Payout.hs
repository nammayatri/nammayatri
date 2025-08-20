{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.Management.Payout
  ( API.Types.ProviderPlatform.Management.Payout.API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management.Payout
import qualified Dashboard.Common
import qualified Domain.Action.Dashboard.Management.Payout
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

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.ProviderPlatform.Management.Payout.API)
handler merchantId city = getPayoutPayoutReferralHistory merchantId city :<|> getPayoutPayoutHistory merchantId city :<|> postPayoutPayoutVerifyFraudStatus merchantId city :<|> postPayoutPayoutRetryFailed merchantId city :<|> postPayoutPayoutRetryAllWithStatus merchantId city :<|> postPayoutPayoutPendingPayout merchantId city :<|> postPayoutPayoutDeleteVPA merchantId city :<|> postPayoutPayoutDriversSetBlockState merchantId city :<|> postPayoutPayoutUpdateVPA merchantId city :<|> postPayoutPayoutRefundRegistrationAmount merchantId city

getPayoutPayoutReferralHistory :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Driver) -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Payout.PayoutReferralHistoryRes)
getPayoutPayoutReferralHistory a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Payout.getPayoutPayoutReferralHistory a10 a9 a8 a7 a6 a5 a4 a3 a2 a1

getPayoutPayoutHistory :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Driver) -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Payout.PayoutHistoryRes)
getPayoutPayoutHistory a9 a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Payout.getPayoutPayoutHistory a9 a8 a7 a6 a5 a4 a3 a2 a1

postPayoutPayoutVerifyFraudStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Management.Payout.UpdateFraudStatusReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postPayoutPayoutVerifyFraudStatus a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Payout.postPayoutPayoutVerifyFraudStatus a3 a2 a1

postPayoutPayoutRetryFailed :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Management.Payout.FailedRetryPayoutReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postPayoutPayoutRetryFailed a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Payout.postPayoutPayoutRetryFailed a3 a2 a1

postPayoutPayoutRetryAllWithStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Management.Payout.RetryPayoutsReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postPayoutPayoutRetryAllWithStatus a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Payout.postPayoutPayoutRetryAllWithStatus a3 a2 a1

postPayoutPayoutPendingPayout :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Management.Payout.PendingPayoutReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postPayoutPayoutPendingPayout a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Payout.postPayoutPayoutPendingPayout a3 a2 a1

postPayoutPayoutDeleteVPA :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Management.Payout.DeleteVpaReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postPayoutPayoutDeleteVPA a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Payout.postPayoutPayoutDeleteVPA a3 a2 a1

postPayoutPayoutDriversSetBlockState :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Management.Payout.SetDriversBlockStateReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postPayoutPayoutDriversSetBlockState a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Payout.postPayoutPayoutDriversSetBlockState a3 a2 a1

postPayoutPayoutUpdateVPA :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Management.Payout.UpdateVpaReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postPayoutPayoutUpdateVPA a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Payout.postPayoutPayoutUpdateVPA a3 a2 a1

postPayoutPayoutRefundRegistrationAmount :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Management.Payout.RefundRegAmountReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postPayoutPayoutRefundRegistrationAmount a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Payout.postPayoutPayoutRefundRegistrationAmount a3 a2 a1
