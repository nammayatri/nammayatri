{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.ProviderPlatform.AppManagement.DriverWallet
  ( getDriverWalletWalletTransactions,
    postDriverWalletWalletPayout,
    postDriverWalletWalletTopup,
    getDriverWalletWalletPayoutHistory,
  )
where

import qualified API.Client.ProviderPlatform.AppManagement
import qualified "dynamic-offer-driver-app" API.Types.UI.DriverWallet
import qualified "dynamic-offer-driver-app" Domain.Action.UI.Plan
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "dynamic-offer-driver-app" Domain.Types.Person
import qualified Domain.Types.Transaction
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified "payment" Lib.Payment.Domain.Types.PayoutRequest
import qualified SharedLogic.Transaction
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api
import Tools.Auth.Merchant

getDriverWalletWalletTransactions :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Person.Driver -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Environment.Flow API.Types.UI.DriverWallet.WalletSummaryResponse)
getDriverWalletWalletTransactions merchantShortId opCity apiTokenInfo driverId fromDate toDate = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.ProviderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.driverWalletDSL.getDriverWalletWalletTransactions) driverId fromDate toDate

postDriverWalletWalletPayout :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Person.Driver -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postDriverWalletWalletPayout merchantShortId opCity apiTokenInfo driverId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) (Kernel.Prelude.Just $ Kernel.Types.Id.cast driverId) Kernel.Prelude.Nothing SharedLogic.Transaction.emptyRequest
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.ProviderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.driverWalletDSL.postDriverWalletWalletPayout) driverId)

postDriverWalletWalletTopup :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Person.Driver -> API.Types.UI.DriverWallet.TopUpRequest -> Environment.Flow Domain.Action.UI.Plan.PlanSubscribeRes)
postDriverWalletWalletTopup merchantShortId opCity apiTokenInfo driverId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) (Kernel.Prelude.Just $ Kernel.Types.Id.cast driverId) Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.ProviderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.driverWalletDSL.postDriverWalletWalletTopup) driverId req)

getDriverWalletWalletPayoutHistory :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Person.Driver -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe ([Lib.Payment.Domain.Types.PayoutRequest.PayoutRequestStatus]) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Environment.Flow API.Types.UI.DriverWallet.PayoutHistoryResponse)
getDriverWalletWalletPayoutHistory merchantShortId opCity apiTokenInfo driverId from to status limit offset = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.ProviderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.driverWalletDSL.getDriverWalletWalletPayoutHistory) driverId from to status limit offset
