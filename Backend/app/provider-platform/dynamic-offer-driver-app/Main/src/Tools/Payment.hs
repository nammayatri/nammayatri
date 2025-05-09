{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Tools.Payment where

import qualified Data.Text as T
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MerchantServiceConfig as DMSC
import qualified Domain.Types.MerchantServiceUsageConfig as DMSUC
import qualified EulerHS.Language as L
import qualified Kernel.External.Payment.Interface as Payment
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Types.Version
import Kernel.Utils.Common
import Kernel.Utils.Version
import qualified Storage.Cac.MerchantServiceUsageConfig as QOMC
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as CQMSC
import System.Environment as SE

createOrder :: ServiceFlow m r => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> DMSC.ServiceName -> m (Payment.CreateOrderReq -> m Payment.CreateOrderResp, Maybe Text)
createOrder = runWithServiceConfigAndName Payment.createOrder

orderStatus :: ServiceFlow m r => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> DMSC.ServiceName -> Payment.OrderStatusReq -> m Payment.OrderStatusResp
orderStatus = runWithUnWrap Payment.orderStatus

offerList :: ServiceFlow m r => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> DMSC.ServiceName -> Payment.OfferListReq -> m Payment.OfferListResp
offerList = runWithUnWrap Payment.offerList

offerApply :: ServiceFlow m r => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> DMSC.ServiceName -> Payment.OfferApplyReq -> m Payment.OfferApplyResp
offerApply = runWithUnWrap Payment.offerApply

mandateRevoke :: ServiceFlow m r => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> DMSC.ServiceName -> Payment.MandateRevokeReq -> m Payment.MandateRevokeRes
mandateRevoke = runWithUnWrap Payment.mandateRevoke

mandateNotification :: (ServiceFlow m r) => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> DMSC.ServiceName -> Payment.MandateNotificationReq -> m Payment.MandateNotificationRes
mandateNotification = runWithUnWrap Payment.mandateNotification

mandateNotificationStatus :: (ServiceFlow m r) => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> DMSC.ServiceName -> Payment.NotificationStatusReq -> m Payment.NotificationStatusResp
mandateNotificationStatus = runWithUnWrap Payment.mandateNotificationStatus

mandateExecution :: ServiceFlow m r => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> DMSC.ServiceName -> Payment.MandateExecutionReq -> m Payment.MandateExecutionRes
mandateExecution = runWithUnWrap Payment.mandateExecution

verifyVpa :: ServiceFlow m r => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> DMSC.ServiceName -> Payment.VerifyVPAReq -> m Payment.VerifyVPAResp
verifyVpa = runWithUnWrap Payment.verifyVPA

runWithServiceConfigAndName ::
  ServiceFlow m r =>
  (Payment.PaymentServiceConfig -> req -> m resp) ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  DMSC.ServiceName ->
  m (req -> m resp, Maybe Text)
runWithServiceConfigAndName func merchantId merchantOperatingCity serviceName = do
  merchantServiceConfig <-
    CQMSC.findByServiceAndCity serviceName merchantOperatingCity
      >>= fromMaybeM (MerchantServiceConfigNotFound merchantId.getId "Payment" (show serviceName))
  case merchantServiceConfig.serviceConfig of
    DMSC.PaymentServiceConfig vsc -> return (func vsc, getPclient vsc)
    DMSC.RentalPaymentServiceConfig vsc -> return (func vsc, getPclient vsc)
    DMSC.CautioPaymentServiceConfig vsc -> return (func vsc, getPclient vsc)
    _ -> throwError $ InternalError "Unknown Service Config"
  where
    getPclient vsc = do
      case vsc of
        Payment.JuspayConfig config -> config.pseudoClientId
        _ -> Nothing

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

runWithUnWrap ::
  ServiceFlow m r =>
  (Payment.PaymentServiceConfig -> req -> m resp) ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  DMSC.ServiceName ->
  req ->
  m resp
runWithUnWrap func merchantId merchantOperatingCity serviceName req = do
  (call, _) <- runWithServiceConfigAndName func merchantId merchantOperatingCity serviceName
  call req

decidePaymentService :: (ServiceFlow m r) => DMSC.ServiceName -> Maybe Version -> m DMSC.ServiceName
decidePaymentService paymentServiceName clientSdkVersion = do
  aaClientSdkVersion <- L.runIO $ (T.pack . (fromMaybe "") <$> SE.lookupEnv "AA_ENABLED_CLIENT_SDK_VERSION")
  return $ case clientSdkVersion of
    Just v
      | v >= textToVersionDefault aaClientSdkVersion -> DMSC.PaymentService Payment.AAJuspay
    _ -> paymentServiceName
