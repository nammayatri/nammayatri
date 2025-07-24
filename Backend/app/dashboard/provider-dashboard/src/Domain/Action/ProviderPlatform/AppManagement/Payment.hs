module Domain.Action.ProviderPlatform.AppManagement.Payment
  ( createPaymentOrder,
    getPaymentOrder,
    getPaymentOrderStatus,
  )
where

import qualified API.Client.ProviderPlatform.AppManagement
import qualified Dashboard.Common
import qualified "dynamic-offer-driver-app" Domain.Action.UI.Payment
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified Domain.Types.Transaction
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.External.Payment.Juspay.Types.CreateOrder
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import qualified Lib.Payment.Domain.Types.PaymentOrder
import qualified SharedLogic.Transaction
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api
import Tools.Auth.Merchant

createPaymentOrder ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ApiTokenInfo ->
  Kernel.Types.Id.Id Dashboard.Common.Person ->
  Kernel.Prelude.Text ->
  Environment.Flow Kernel.External.Payment.Juspay.Types.CreateOrder.CreateOrderResp
createPaymentOrder merchantShortId opCity apiTokenInfo requestorId invoiceId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing SharedLogic.Transaction.emptyRequest
  SharedLogic.Transaction.withTransactionStoring transaction $
    API.Client.ProviderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.paymentDSL.createPaymentOrder) requestorId invoiceId

getPaymentOrder ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ApiTokenInfo ->
  Kernel.Prelude.Text ->
  Environment.Flow Lib.Payment.Domain.Types.PaymentOrder.PaymentOrderAPIEntity
getPaymentOrder merchantShortId opCity apiTokenInfo orderId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let requestorId = Kernel.Types.Id.cast apiTokenInfo.personId
  API.Client.ProviderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.paymentDSL.getPaymentOrder) orderId requestorId

getPaymentOrderStatus ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ApiTokenInfo ->
  Kernel.Prelude.Text ->
  Environment.Flow Domain.Action.UI.Payment.PaymentStatusResp
getPaymentOrderStatus merchantShortId opCity apiTokenInfo orderId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let requestorId = Kernel.Types.Id.cast apiTokenInfo.personId
  API.Client.ProviderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.paymentDSL.getPaymentOrderStatus) orderId requestorId
