{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Tools.Payment where

import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MerchantServiceConfig as DMSC
import qualified Domain.Types.MerchantServiceUsageConfig as DMSUC
import qualified Kernel.External.Payment.Interface as Payment
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Cac.MerchantServiceUsageConfig as QOMC
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as CQMSC

createOrder :: ServiceFlow m r => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> DMSC.ServiceName -> Payment.CreateOrderReq -> m Payment.CreateOrderResp
createOrder = runWithServiceConfigAndName Payment.createOrder

orderStatus :: ServiceFlow m r => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> DMSC.ServiceName -> Payment.OrderStatusReq -> m Payment.OrderStatusResp
orderStatus = runWithServiceConfigAndName Payment.orderStatus

offerList :: ServiceFlow m r => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> DMSC.ServiceName -> Payment.OfferListReq -> m Payment.OfferListResp
offerList = runWithServiceConfigAndName Payment.offerList

offerApply :: ServiceFlow m r => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> DMSC.ServiceName -> Payment.OfferApplyReq -> m Payment.OfferApplyResp
offerApply = runWithServiceConfigAndName Payment.offerApply

mandateRevoke :: ServiceFlow m r => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> DMSC.ServiceName -> Payment.MandateRevokeReq -> m Payment.MandateRevokeRes
mandateRevoke = runWithServiceConfigAndName Payment.mandateRevoke

mandateNotification :: (ServiceFlow m r) => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> DMSC.ServiceName -> Payment.MandateNotificationReq -> m Payment.MandateNotificationRes
mandateNotification = runWithServiceConfigAndName Payment.mandateNotification

mandateNotificationStatus :: (ServiceFlow m r) => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> DMSC.ServiceName -> Payment.NotificationStatusReq -> m Payment.NotificationStatusResp
mandateNotificationStatus = runWithServiceConfigAndName Payment.mandateNotificationStatus

mandateExecution :: ServiceFlow m r => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> DMSC.ServiceName -> Payment.MandateExecutionReq -> m Payment.MandateExecutionRes
mandateExecution = runWithServiceConfigAndName Payment.mandateExecution

verifyVpa :: ServiceFlow m r => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> DMSC.ServiceName -> Payment.VerifyVPAReq -> m Payment.VerifyVPAResp
verifyVpa = runWithServiceConfigAndName Payment.verifyVPA

runWithServiceConfigAndName ::
  ServiceFlow m r =>
  (Payment.PaymentServiceConfig -> req -> m resp) ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  DMSC.ServiceName ->
  req ->
  m resp
runWithServiceConfigAndName func merchantId merchantOperatingCity serviceName req = do
  merchantServiceConfig <-
    CQMSC.findByServiceAndCity serviceName merchantOperatingCity
      >>= fromMaybeM (MerchantServiceConfigNotFound merchantId.getId "Payment" (show Payment.Juspay))
  case merchantServiceConfig.serviceConfig of
    DMSC.PaymentServiceConfig vsc -> func vsc req
    DMSC.RentalPaymentServiceConfig vsc -> func vsc req
    DMSC.CautioPaymentServiceConfig vsc -> func vsc req
    _ -> throwError $ InternalError "Unknown Service Config"

createIndividualConnectAccount ::
  ServiceFlow m r =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Payment.IndividualConnectAccountReq ->
  m Payment.IndividualConnectAccountResp
createIndividualConnectAccount = runWithServiceConfig Payment.createIndividualConnectAccount (.createBankAccount)

retryAccountLink ::
  ServiceFlow m r =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Payment.AccountId ->
  m Payment.RetryAccountLink
retryAccountLink = runWithServiceConfig Payment.retryAccountLink (.retryBankAccountLink)

getAccount ::
  ServiceFlow m r =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Payment.AccountId ->
  m Payment.ConnectAccountResp
getAccount = runWithServiceConfig Payment.getAccount (.getBankAccount)

runWithServiceConfig ::
  ServiceFlow m r =>
  (Payment.PaymentServiceConfig -> req -> m resp) ->
  (DMSUC.MerchantServiceUsageConfig -> Payment.PaymentService) ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  req ->
  m resp
runWithServiceConfig func getCfg _merchantId merchantOpCityId req = do
  orgPaymentsConfig <- QOMC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantOpCityId.getId)
  orgPaymentServiceConfig <-
    CQMSC.findByServiceAndCity (DMSC.PaymentService $ getCfg orgPaymentsConfig) merchantOpCityId
      >>= fromMaybeM (MerchantServiceConfigNotFound merchantOpCityId.getId "Payments" (show $ getCfg orgPaymentsConfig))
  case orgPaymentServiceConfig.serviceConfig of
    DMSC.PaymentServiceConfig msc -> func msc req
    _ -> throwError $ InternalError "Unknown Service Config"
