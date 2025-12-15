{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Tools.Payment where

import qualified Domain.Types.Extra.MerchantPaymentMethod as DMPM
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MerchantServiceConfig as DMSC
import qualified Domain.Types.MerchantServiceUsageConfig as DMSUC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Plan as DPlan
import qualified Kernel.External.Payment.Interface as Payment
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Types.Version
import Kernel.Utils.Common
import Kernel.Utils.Version
import qualified Storage.Cac.MerchantServiceUsageConfig as QOMC
import qualified Storage.Cac.TransporterConfig as SCTC
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as CQMSC
import qualified Storage.Queries.DriverPlan as QDPlan

createOrder :: ServiceFlow m r => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> DMSC.ServiceName -> Maybe Text -> m (Payment.CreateOrderReq -> m Payment.CreateOrderResp, Maybe Text)
createOrder = runWithServiceConfigAndName Payment.createOrder

orderStatus :: ServiceFlow m r => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> DMSC.ServiceName -> Maybe Text -> Payment.OrderStatusReq -> m Payment.OrderStatusResp
orderStatus = runWithUnWrap Payment.orderStatus

offerList :: ServiceFlow m r => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> DMSC.ServiceName -> Maybe Text -> Payment.OfferListReq -> m Payment.OfferListResp
offerList = runWithUnWrap Payment.offerList

offerApply :: ServiceFlow m r => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> DMSC.ServiceName -> Maybe Text -> Payment.OfferApplyReq -> m Payment.OfferApplyResp
offerApply = runWithUnWrap Payment.offerApply

mandateRevoke :: ServiceFlow m r => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> DMSC.ServiceName -> Maybe Text -> Payment.MandateRevokeReq -> m Payment.MandateRevokeRes
mandateRevoke = runWithUnWrap Payment.mandateRevoke

mandateNotification :: (ServiceFlow m r) => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> DMSC.ServiceName -> Maybe Text -> Payment.MandateNotificationReq -> m Payment.MandateNotificationRes
mandateNotification = runWithUnWrap Payment.mandateNotification

mandateNotificationStatus :: (ServiceFlow m r) => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> DMSC.ServiceName -> Maybe Text -> Payment.NotificationStatusReq -> m Payment.NotificationStatusResp
mandateNotificationStatus = runWithUnWrap Payment.mandateNotificationStatus

mandateExecution :: ServiceFlow m r => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> DMSC.ServiceName -> Maybe Text -> Payment.MandateExecutionReq -> m Payment.MandateExecutionRes
mandateExecution = runWithUnWrap Payment.mandateExecution

verifyVpa :: ServiceFlow m r => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> DMSC.ServiceName -> Maybe Text -> Payment.VerifyVPAReq -> m Payment.VerifyVPAResp
verifyVpa = runWithUnWrap Payment.verifyVPA

runWithServiceConfigAndName ::
  ServiceFlow m r =>
  (Payment.PaymentServiceConfig -> Maybe Text -> req -> m resp) ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  DMSC.ServiceName ->
  Maybe Text ->
  m (req -> m resp, Maybe Text)
runWithServiceConfigAndName func merchantId merchantOperatingCity serviceName mRoutingId = do
  merchantServiceConfig <-
    CQMSC.findByServiceAndCity serviceName merchantOperatingCity
      >>= fromMaybeM (MerchantServiceConfigNotFound merchantId.getId "Payment" (show serviceName))
  logDebug $ "runWithServiceConfigAndName: getRoutingId " <> show getRoutingId
  logDebug $ "runWithServiceConfigAndName: serviceConfig " <> show merchantServiceConfig.serviceConfig
  case merchantServiceConfig.serviceConfig of
    DMSC.PaymentServiceConfig vsc -> return (func vsc getRoutingId, getPclient vsc)
    DMSC.RentalPaymentServiceConfig vsc -> return (func vsc getRoutingId, getPclient vsc)
    DMSC.CautioPaymentServiceConfig vsc -> return (func vsc getRoutingId, getPclient vsc)
    _ -> throwError $ InternalError "Unknown Service Config"
  where
    getPclient vsc = do
      case vsc of
        Payment.JuspayConfig config -> config.pseudoClientId
        _ -> Nothing

    getRoutingId = do
      case serviceName of
        DMSC.PaymentService Payment.AAJuspay -> mRoutingId
        _ -> Nothing

createConnectAccount ::
  ServiceFlow m r =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Maybe DMPM.PaymentMode ->
  Payment.CreateConnectAccountReq ->
  m Payment.CreateConnectAccountResp
createConnectAccount = runWithServiceConfig Payment.createConnectAccount (.createBankAccount)

retryAccountLink ::
  ServiceFlow m r =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Maybe DMPM.PaymentMode ->
  Payment.AccountId ->
  m Payment.RetryAccountLink
retryAccountLink = runWithServiceConfig Payment.retryAccountLink (.retryBankAccountLink)

getAccount ::
  ServiceFlow m r =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Maybe DMPM.PaymentMode ->
  Payment.AccountId ->
  m Payment.ConnectAccountResp
getAccount = runWithServiceConfig Payment.getAccount (.getBankAccount)

modifyPaymentServiceByMode :: Payment.PaymentService -> DMPM.PaymentMode -> Payment.PaymentService
modifyPaymentServiceByMode Payment.Stripe DMPM.LIVE = Payment.Stripe
modifyPaymentServiceByMode Payment.Stripe DMPM.TEST = Payment.StripeTest
modifyPaymentServiceByMode Payment.StripeTest _ = Payment.StripeTest
modifyPaymentServiceByMode Payment.Juspay _ = Payment.Juspay
modifyPaymentServiceByMode Payment.AAJuspay _ = Payment.AAJuspay

runWithServiceConfig ::
  ServiceFlow m r =>
  (Payment.PaymentServiceConfig -> req -> m resp) ->
  (DMSUC.MerchantServiceUsageConfig -> Payment.PaymentService) ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Maybe DMPM.PaymentMode ->
  req ->
  m resp
runWithServiceConfig func getCfg _merchantId merchantOpCityId paymentMode req = do
  orgPaymentsConfig <- QOMC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantOpCityId.getId)
  let paymentService = modifyPaymentServiceByMode (getCfg orgPaymentsConfig) (fromMaybe DMPM.LIVE paymentMode)
  orgPaymentServiceConfig <-
    CQMSC.findByServiceAndCity (DMSC.PaymentService paymentService) merchantOpCityId
      >>= fromMaybeM (MerchantServiceConfigNotFound merchantOpCityId.getId "Payments" (show paymentService))
  case orgPaymentServiceConfig.serviceConfig of
    DMSC.PaymentServiceConfig msc -> func msc req
    _ -> throwError $ InternalError "Unknown Service Config"

runWithUnWrap ::
  ServiceFlow m r =>
  (Payment.PaymentServiceConfig -> Maybe Text -> req -> m resp) ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  DMSC.ServiceName ->
  Maybe Text ->
  req ->
  m resp
runWithUnWrap func merchantId merchantOperatingCity serviceName mRoutingId req = do
  (call, _) <- runWithServiceConfigAndName func merchantId merchantOperatingCity serviceName mRoutingId
  call req

decidePaymentService :: (ServiceFlow m r) => DMSC.ServiceName -> Maybe Version -> Id DMOC.MerchantOperatingCity -> m DMSC.ServiceName
decidePaymentService paymentServiceName clientSdkVersion merchantOpCityId = do
  transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  let paymentService = case clientSdkVersion of
        Just v
          | v >= textToVersionDefault transporterConfig.aaEnabledClientSdkVersion -> DMSC.PaymentService Payment.AAJuspay
        _ -> paymentServiceName
  logDebug $ "decidePaymentService: clientSdkVersion " <> show clientSdkVersion
  logDebug $ "decidePaymentService: transporterConfig.aaEnabledClientSdkVersion " <> show (textToVersionDefault transporterConfig.aaEnabledClientSdkVersion)
  logDebug $ "decidePaymentService: PaymentServiceName" <> show paymentService
  return paymentService

decidePaymentServiceForRecurring :: (ServiceFlow m r) => DMSC.ServiceName -> Id DP.Person -> Id DMOC.MerchantOperatingCity -> DPlan.ServiceNames -> m DMSC.ServiceName
decidePaymentServiceForRecurring paymentServiceName driverId merchantOpCityId serviceName = do
  transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  case transporterConfig.isAAEnabledForRecurring of
    Just True -> do
      mDriverPlan <- QDPlan.findByDriverIdWithServiceName driverId serviceName
      case mDriverPlan of
        Just driverPlan -> do
          now <- getCurrentTime
          let mandateSetupTime = fromMaybe now driverPlan.mandateSetupDate
              elapsed = diffUTCTime now mandateSetupTime
              sixHours = 6 * 60 * 60
          pure $
            if elapsed > sixHours
              then DMSC.PaymentService Payment.AAJuspay
              else paymentServiceName
        Nothing -> pure paymentServiceName
    _ -> pure paymentServiceName
