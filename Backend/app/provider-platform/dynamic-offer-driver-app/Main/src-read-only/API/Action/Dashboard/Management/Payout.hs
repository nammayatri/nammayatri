{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.Management.Payout
  ( API.Types.ProviderPlatform.Management.Payout.API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management.Payout
import qualified Domain.Action.Dashboard.Management.Payout
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified "payment" Lib.Payment.API.Payout.Types
import qualified "payment" Lib.Payment.Domain.Types.PayoutRequest
import Servant
import Tools.Auth

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.ProviderPlatform.Management.Payout.API)
handler merchantId city = getPayoutPayout merchantId city :<|> postPayoutPayoutRetry merchantId city :<|> postPayoutPayoutCancel merchantId city :<|> postPayoutPayoutCash merchantId city :<|> postPayoutPayoutVpaDelete merchantId city :<|> postPayoutPayoutVpaUpdate merchantId city :<|> postPayoutPayoutVpaRefundRegistration merchantId city

getPayoutPayout :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Lib.Payment.Domain.Types.PayoutRequest.PayoutRequest -> Environment.FlowHandler Lib.Payment.API.Payout.Types.PayoutRequestResp)
getPayoutPayout a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Payout.getPayoutPayout a3 a2 a1

postPayoutPayoutRetry :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Lib.Payment.Domain.Types.PayoutRequest.PayoutRequest -> Environment.FlowHandler Lib.Payment.API.Payout.Types.PayoutSuccess)
postPayoutPayoutRetry a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Payout.postPayoutPayoutRetry a3 a2 a1

postPayoutPayoutCancel :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Lib.Payment.Domain.Types.PayoutRequest.PayoutRequest -> Lib.Payment.API.Payout.Types.PayoutCancelReq -> Environment.FlowHandler Lib.Payment.API.Payout.Types.PayoutSuccess)
postPayoutPayoutCancel a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Payout.postPayoutPayoutCancel a4 a3 a2 a1

postPayoutPayoutCash :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Lib.Payment.Domain.Types.PayoutRequest.PayoutRequest -> Lib.Payment.API.Payout.Types.PayoutCashUpdateReq -> Environment.FlowHandler Lib.Payment.API.Payout.Types.PayoutSuccess)
postPayoutPayoutCash a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Payout.postPayoutPayoutCash a4 a3 a2 a1

postPayoutPayoutVpaDelete :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Lib.Payment.API.Payout.Types.DeleteVpaReq -> Environment.FlowHandler Lib.Payment.API.Payout.Types.PayoutSuccess)
postPayoutPayoutVpaDelete a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Payout.postPayoutPayoutVpaDelete a3 a2 a1

postPayoutPayoutVpaUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Lib.Payment.API.Payout.Types.UpdateVpaReq -> Environment.FlowHandler Lib.Payment.API.Payout.Types.PayoutSuccess)
postPayoutPayoutVpaUpdate a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Payout.postPayoutPayoutVpaUpdate a3 a2 a1

postPayoutPayoutVpaRefundRegistration :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Lib.Payment.API.Payout.Types.RefundRegAmountReq -> Environment.FlowHandler Lib.Payment.API.Payout.Types.PayoutSuccess)
postPayoutPayoutVpaRefundRegistration a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Payout.postPayoutPayoutVpaRefundRegistration a3 a2 a1
