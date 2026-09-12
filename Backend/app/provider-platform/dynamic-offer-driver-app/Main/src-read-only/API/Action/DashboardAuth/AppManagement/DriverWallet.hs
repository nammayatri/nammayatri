{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.AppManagement.DriverWallet
  ( API,
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
import Tools.Auth.DashboardUserAuth

type API = ("driverWallet" :> (GetDriverWalletWalletBalance :<|> GetDriverWalletWalletTransactions :<|> PostDriverWalletWalletPayout :<|> PostDriverWalletWalletTopup :<|> PostDriverWalletWalletAirportCashRecharge :<|> GetDriverWalletWalletPayoutHistory :<|> GetDriverWalletWalletTransactionHistory))

type GetDriverWalletWalletBalance =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_APP_MANAGEMENT/DRIVER_WALLET/GET_DRIVER_WALLET_WALLET_BALANCE"
      :> API.Types.Dashboard.AppManagement.DriverWallet.GetDriverWalletWalletBalance
  )

type GetDriverWalletWalletTransactions =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_APP_MANAGEMENT/DRIVER_WALLET/GET_DRIVER_WALLET_WALLET_TRANSACTIONS"
      :> API.Types.Dashboard.AppManagement.DriverWallet.GetDriverWalletWalletTransactions
  )

type PostDriverWalletWalletPayout =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_APP_MANAGEMENT/DRIVER_WALLET/POST_DRIVER_WALLET_WALLET_PAYOUT"
      :> API.Types.Dashboard.AppManagement.DriverWallet.PostDriverWalletWalletPayout
  )

type PostDriverWalletWalletTopup =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_APP_MANAGEMENT/DRIVER_WALLET/POST_DRIVER_WALLET_WALLET_TOPUP"
      :> API.Types.Dashboard.AppManagement.DriverWallet.PostDriverWalletWalletTopup
  )

type PostDriverWalletWalletAirportCashRecharge =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_APP_MANAGEMENT/DRIVER_WALLET/POST_DRIVER_WALLET_WALLET_AIRPORT_CASH_RECHARGE"
      :> API.Types.Dashboard.AppManagement.DriverWallet.PostDriverWalletWalletAirportCashRecharge
  )

type GetDriverWalletWalletPayoutHistory =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_APP_MANAGEMENT/DRIVER_WALLET/GET_DRIVER_WALLET_WALLET_PAYOUT_HISTORY"
      :> API.Types.Dashboard.AppManagement.DriverWallet.GetDriverWalletWalletPayoutHistory
  )

type GetDriverWalletWalletTransactionHistory =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_APP_MANAGEMENT/DRIVER_WALLET/GET_DRIVER_WALLET_WALLET_TRANSACTION_HISTORY"
      :> API.Types.Dashboard.AppManagement.DriverWallet.GetDriverWalletWalletTransactionHistory
  )

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getDriverWalletWalletBalance merchantId city :<|> getDriverWalletWalletTransactions merchantId city :<|> postDriverWalletWalletPayout merchantId city :<|> postDriverWalletWalletTopup merchantId city :<|> postDriverWalletWalletAirportCashRecharge merchantId city :<|> getDriverWalletWalletPayoutHistory merchantId city :<|> getDriverWalletWalletTransactionHistory merchantId city

getDriverWalletWalletBalance :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.Person.Driver -> Environment.FlowHandler API.Types.UI.DriverWallet.WalletBalanceResponse)
getDriverWalletWalletBalance a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.DriverWallet.getDriverWalletWalletBalance a4 a3 a1

getDriverWalletWalletTransactions :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.Person.Driver -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (API.Types.UI.DriverWallet.AggregationLevel) -> Environment.FlowHandler API.Types.UI.DriverWallet.WalletSummaryResponse)
getDriverWalletWalletTransactions a7 a6 _a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.DriverWallet.getDriverWalletWalletTransactions a7 a6 a4 a3 a2 a1

postDriverWalletWalletPayout :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.Person.Driver -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverWalletWalletPayout a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.DriverWallet.postDriverWalletWalletPayout a4 a3 a1 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a2))

postDriverWalletWalletTopup :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.Person.Driver -> API.Types.UI.DriverWallet.TopUpRequest -> Environment.FlowHandler Domain.Action.UI.Plan.PlanSubscribeRes)
postDriverWalletWalletTopup a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.DriverWallet.postDriverWalletWalletTopup a5 a4 a2 a1

postDriverWalletWalletAirportCashRecharge :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.Person.Driver -> API.Types.Dashboard.AppManagement.DriverWallet.AirportCashRechargeRequest -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverWalletWalletAirportCashRecharge a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.DriverWallet.postDriverWalletWalletAirportCashRecharge a5 a4 a2 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3)) a1

getDriverWalletWalletPayoutHistory :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.Person.Driver -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe ([Lib.Payment.Domain.Types.PayoutRequest.PayoutRequestStatus]) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Environment.FlowHandler API.Types.UI.DriverWallet.PayoutHistoryResponse)
getDriverWalletWalletPayoutHistory a9 a8 _a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.DriverWallet.getDriverWalletWalletPayoutHistory a9 a8 a6 a5 a4 a3 a2 a1

getDriverWalletWalletTransactionHistory :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.Person.Driver -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Environment.FlowHandler API.Types.UI.DriverWallet.WalletTransactionHistoryResponse)
getDriverWalletWalletTransactionHistory a8 a7 _a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.DriverWallet.getDriverWalletWalletTransactionHistory a8 a7 a5 a4 a3 a2 a1
