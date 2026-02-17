{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.ProviderPlatform.AppManagement.DriverWallet
  ( API,
    handler,
  )
where

import qualified "dynamic-offer-driver-app" API.Types.Dashboard.AppManagement
import qualified "dynamic-offer-driver-app" API.Types.Dashboard.AppManagement.DriverWallet
import qualified "dynamic-offer-driver-app" API.Types.UI.DriverWallet
import qualified Domain.Action.ProviderPlatform.AppManagement.DriverWallet
import qualified "dynamic-offer-driver-app" Domain.Action.UI.Plan
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "dynamic-offer-driver-app" Domain.Types.Person
import qualified "lib-dashboard" Environment
import EulerHS.Prelude hiding (sortOn)
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common hiding (INFO)
import qualified "payment" Lib.Payment.Domain.Types.PayoutRequest
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("driverWallet" :> (GetDriverWalletWalletTransactions :<|> PostDriverWalletWalletPayout :<|> PostDriverWalletWalletTopup :<|> GetDriverWalletWalletPayoutHistory))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getDriverWalletWalletTransactions merchantId city :<|> postDriverWalletWalletPayout merchantId city :<|> postDriverWalletWalletTopup merchantId city :<|> getDriverWalletWalletPayoutHistory merchantId city

type GetDriverWalletWalletTransactions =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.DRIVER_WALLET) / ('API.Types.Dashboard.AppManagement.DriverWallet.GET_DRIVER_WALLET_WALLET_TRANSACTIONS))
      :> API.Types.Dashboard.AppManagement.DriverWallet.GetDriverWalletWalletTransactions
  )

type PostDriverWalletWalletPayout =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.DRIVER_WALLET) / ('API.Types.Dashboard.AppManagement.DriverWallet.POST_DRIVER_WALLET_WALLET_PAYOUT))
      :> API.Types.Dashboard.AppManagement.DriverWallet.PostDriverWalletWalletPayout
  )

type PostDriverWalletWalletTopup =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.DRIVER_WALLET) / ('API.Types.Dashboard.AppManagement.DriverWallet.POST_DRIVER_WALLET_WALLET_TOPUP))
      :> API.Types.Dashboard.AppManagement.DriverWallet.PostDriverWalletWalletTopup
  )

type GetDriverWalletWalletPayoutHistory =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.DRIVER_WALLET) / ('API.Types.Dashboard.AppManagement.DriverWallet.GET_DRIVER_WALLET_WALLET_PAYOUT_HISTORY))
      :> API.Types.Dashboard.AppManagement.DriverWallet.GetDriverWalletWalletPayoutHistory
  )

getDriverWalletWalletTransactions :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Person.Driver -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Environment.FlowHandler API.Types.UI.DriverWallet.WalletSummaryResponse)
getDriverWalletWalletTransactions merchantShortId opCity apiTokenInfo driverId fromDate toDate = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.AppManagement.DriverWallet.getDriverWalletWalletTransactions merchantShortId opCity apiTokenInfo driverId fromDate toDate

postDriverWalletWalletPayout :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Person.Driver -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverWalletWalletPayout merchantShortId opCity apiTokenInfo driverId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.AppManagement.DriverWallet.postDriverWalletWalletPayout merchantShortId opCity apiTokenInfo driverId

postDriverWalletWalletTopup :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Person.Driver -> API.Types.UI.DriverWallet.TopUpRequest -> Environment.FlowHandler Domain.Action.UI.Plan.PlanSubscribeRes)
postDriverWalletWalletTopup merchantShortId opCity apiTokenInfo driverId req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.AppManagement.DriverWallet.postDriverWalletWalletTopup merchantShortId opCity apiTokenInfo driverId req

getDriverWalletWalletPayoutHistory :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Person.Driver -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe ([Lib.Payment.Domain.Types.PayoutRequest.PayoutRequestStatus]) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Environment.FlowHandler API.Types.UI.DriverWallet.PayoutHistoryResponse)
getDriverWalletWalletPayoutHistory merchantShortId opCity apiTokenInfo driverId from to status limit offset = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.AppManagement.DriverWallet.getDriverWalletWalletPayoutHistory merchantShortId opCity apiTokenInfo driverId from to status limit offset
