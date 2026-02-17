{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.AppManagement.DriverWallet
  ( API.Types.Dashboard.AppManagement.DriverWallet.API,
    handler,
  )
where

import qualified API.Types.Dashboard.AppManagement.DriverWallet
import qualified "this" API.Types.UI.DriverWallet
import qualified Domain.Action.Dashboard.AppManagement.DriverWallet
import qualified "this" Domain.Action.UI.Plan
import qualified Domain.Types.Merchant
import qualified "this" Domain.Types.Person
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified "payment" Lib.Payment.Domain.Types.PayoutRequest
import Servant
import Tools.Auth

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.Dashboard.AppManagement.DriverWallet.API)
handler merchantId city = getDriverWalletWalletTransactions merchantId city :<|> postDriverWalletWalletPayout merchantId city :<|> postDriverWalletWalletTopup merchantId city :<|> getDriverWalletWalletPayoutHistory merchantId city

getDriverWalletWalletTransactions :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Person.Driver -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Environment.FlowHandler API.Types.UI.DriverWallet.WalletSummaryResponse)
getDriverWalletWalletTransactions a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.DriverWallet.getDriverWalletWalletTransactions a5 a4 a3 a2 a1

postDriverWalletWalletPayout :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Person.Driver -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverWalletWalletPayout a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.DriverWallet.postDriverWalletWalletPayout a3 a2 a1

postDriverWalletWalletTopup :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Person.Driver -> API.Types.UI.DriverWallet.TopUpRequest -> Environment.FlowHandler Domain.Action.UI.Plan.PlanSubscribeRes)
postDriverWalletWalletTopup a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.DriverWallet.postDriverWalletWalletTopup a4 a3 a2 a1

getDriverWalletWalletPayoutHistory :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Person.Driver -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe ([Lib.Payment.Domain.Types.PayoutRequest.PayoutRequestStatus]) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Environment.FlowHandler API.Types.UI.DriverWallet.PayoutHistoryResponse)
getDriverWalletWalletPayoutHistory a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.DriverWallet.getDriverWalletWalletPayoutHistory a8 a7 a6 a5 a4 a3 a2 a1
