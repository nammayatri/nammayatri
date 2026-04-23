{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Tools.Payout
  ( createPayoutOrder,
    payoutOrderStatus,
    PayoutServiceNameOption (..),
  )
where

import qualified Data.Text as T
import qualified Domain.Types.Extra.MerchantPaymentMethod as DMPM
import qualified Domain.Types.Extra.Plan as DPlan
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MerchantServiceConfig as DMSC
import qualified Domain.Types.MerchantServiceUsageConfig as DMSUC
import qualified Domain.Types.Person as DP
import qualified Kernel.External.Payout.Interface as Payout
import qualified Kernel.External.Payout.Types as PT
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Types.Version
import Kernel.Utils.Common
import Kernel.Utils.Version
import qualified Lib.Payment.Domain.Action as DPayment
import qualified Storage.Cac.MerchantServiceUsageConfig as CMSUC
import qualified Storage.Cac.TransporterConfig as SCTC
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as CQMSC
import qualified Storage.CachedQueries.SubscriptionConfig as CQSC
import qualified Storage.Queries.DriverBankAccount as QDBA
import Tools.Error

data PayoutServiceNameOption = MerchantServiceUsageConfigOption | SubscriptionConfigOption DPlan.ServiceNames

createPayoutOrder ::
  (ServiceFlow m r, HasFlowEnv m r '["selfBaseUrl" ::: BaseUrl]) =>
  PayoutServiceNameOption ->
  (PT.PayoutService -> DMSC.ServiceName) ->
  Maybe Version ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Id DP.Person ->
  Maybe DMPM.PaymentMode ->
  DPayment.CreatePayoutServiceReq ->
  m Payout.CreatePayoutOrderResp
createPayoutOrder = runWithServiceConfigAndName Payout.createPayoutOrder (.createPayoutOrder) DPayment.mkCreatePayoutOrderReq

payoutOrderStatus ::
  ServiceFlow m r =>
  PayoutServiceNameOption ->
  (PT.PayoutService -> DMSC.ServiceName) ->
  Maybe Version ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Id DP.Person ->
  Maybe DMPM.PaymentMode ->
  Maybe Text ->
  DPayment.PayoutStatusServiceReq ->
  m Payout.PayoutOrderStatusResp
payoutOrderStatus payoutServiceNameOption serviceType clientSdkVersion merchantId merchantOperatingCityId personId paymentMode idAssignedByServiceProvider serviceReq =
  runWithServiceConfigAndName Payout.payoutOrderStatus (.payoutOrderStatus) (DPayment.mkPayoutOrderStatusReq idAssignedByServiceProvider) payoutServiceNameOption serviceType clientSdkVersion merchantId merchantOperatingCityId personId paymentMode serviceReq

runWithServiceConfigAndName ::
  ServiceFlow m r =>
  (Payout.PayoutServiceConfig -> req -> m resp) ->
  (DMSUC.MerchantServiceUsageConfig -> Payout.PayoutService) ->
  (Maybe Text -> Maybe Text -> serviceReq -> req) ->
  PayoutServiceNameOption ->
  (PT.PayoutService -> DMSC.ServiceName) ->
  Maybe Version ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Id DP.Person ->
  Maybe DMPM.PaymentMode ->
  serviceReq ->
  m resp
runWithServiceConfigAndName func getCfg mkReq payoutServiceNameOption serviceType clientSdkVersion merchantId merchantOperatingCityId personId paymentMode serviceReq = do
  payoutServiceNameRaw <- case payoutServiceNameOption of
    MerchantServiceUsageConfigOption -> do
      orgPaymentsConfig <- CMSUC.findByMerchantOpCityId merchantOperatingCityId Nothing >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantOperatingCityId.getId)
      pure $ serviceType (getCfg orgPaymentsConfig)
    SubscriptionConfigOption serviceName -> do
      subscriptionConfig <- do
        CQSC.findSubscriptionConfigsByMerchantOpCityIdAndServiceName merchantOperatingCityId Nothing serviceName
          >>= fromMaybeM (NoSubscriptionConfigForService merchantOperatingCityId.getId $ show serviceName)
      pure $ fromMaybe (serviceType PT.Juspay) subscriptionConfig.payoutServiceName
  payoutServiceName <- modifyServiceName payoutServiceNameRaw (fromMaybe DMPM.LIVE paymentMode) clientSdkVersion merchantOperatingCityId
  merchantServiceConfig <-
    CQMSC.findByServiceAndCity payoutServiceName merchantOperatingCityId
      >>= fromMaybeM (uncurry (MerchantServiceConfigNotFound merchantId.getId) (showPayoutServiceType payoutServiceName))
  case merchantServiceConfig.serviceConfig of
    DMSC.PayoutServiceConfig vsc -> callFunc vsc payoutServiceName
    DMSC.RentalPayoutServiceConfig vsc -> callFunc vsc payoutServiceName
    DMSC.RidePayoutServiceConfig vsc -> callFunc vsc payoutServiceName
    _ -> throwError $ InternalError "Unknown Service Config"
  where
    mRoutingId = Just personId.getId

    getRoutingId = \case
      DMSC.PayoutService PT.AAJuspay -> mRoutingId -- FIXME should we use different serviceType here?
      _ -> Nothing

    callFunc vsc payoutServiceName = case vsc of
      Payout.JuspayConfig _ -> do
        let mConnectedAccountId = Nothing
        func vsc (mkReq (getRoutingId payoutServiceName) mConnectedAccountId serviceReq)
      Payout.StripeConfig _ -> do
        personBankAccount <- QDBA.findByPrimaryKey personId >>= fromMaybeM (InvalidRequest "Driver bank acount not found") -- TODO add error code
        let mConnectedAccountId = Just personBankAccount.accountId
        -- TODO check payment mode is the same
        -- let paymentMode = fromMaybe DMPM.LIVE personBankAccount.paymentMode
        func vsc (mkReq Nothing mConnectedAccountId serviceReq)

showPayoutServiceType :: DMSC.ServiceName -> (Text, Text)
showPayoutServiceType serviceName = do
  case T.splitOn "_" $ show serviceName of
    a : b : _ -> (a, b)
    [a] -> (a, "Unknown")
    [] -> ("Unknown", "Unknown")

data Service = Stripe | Juspay

getService :: Payout.PayoutService -> Service
getService Payout.Stripe = Stripe
getService Payout.StripeTest = Stripe
getService Payout.Juspay = Juspay
getService Payout.AAJuspay = Juspay

modifyServiceName ::
  (ServiceFlow m r) =>
  DMSC.ServiceName ->
  DMPM.PaymentMode ->
  Maybe Version ->
  Id DMOC.MerchantOperatingCity ->
  m DMSC.ServiceName
modifyServiceName serviceName paymentMode clientSdkVersion merchantOpCityId =
  case serviceName of
    DMSC.PayoutService payoutService -> modifyPayoutService DMSC.PayoutService payoutService
    DMSC.RentalPayoutService payoutService -> modifyPayoutService DMSC.RentalPayoutService payoutService
    DMSC.RidePayoutService payoutService -> modifyPayoutService DMSC.RidePayoutService payoutService
    _ -> throwError $ InternalError "Unknown Service Name"
  where
    modifyPayoutService serviceType payoutService =
      case getService payoutService of
        Juspay -> decidePayoutService serviceName clientSdkVersion merchantOpCityId
        Stripe -> pure . serviceType $ modifyPayoutServiceByMode payoutService paymentMode

-- relevant only for Stripe
modifyPayoutServiceByMode :: PT.PayoutService -> DMPM.PaymentMode -> PT.PayoutService
modifyPayoutServiceByMode Payout.Stripe DMPM.LIVE = Payout.Stripe
modifyPayoutServiceByMode Payout.Stripe DMPM.TEST = Payout.StripeTest
modifyPayoutServiceByMode Payout.StripeTest _ = Payout.StripeTest
modifyPayoutServiceByMode Payout.Juspay _ = Payout.Juspay
modifyPayoutServiceByMode Payout.AAJuspay _ = Payout.AAJuspay

-- relevant only for Juspay
decidePayoutService :: ServiceFlow m r => DMSC.ServiceName -> Maybe Version -> Id DMOC.MerchantOperatingCity -> m DMSC.ServiceName
decidePayoutService payoutServiceName clientSdkVersion merchantOpCityId = do
  transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  return $ case clientSdkVersion of
    Just v
      | v >= textToVersionDefault transporterConfig.aaEnabledClientSdkVersion -> DMSC.PayoutService PT.AAJuspay -- FIXME should we use different serviceType here?
    _ -> payoutServiceName
