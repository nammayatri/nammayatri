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
  )
where

import qualified Data.Text as T
import qualified Domain.Types.Extra.MerchantPaymentMethod as DMPM
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MerchantServiceConfig as DMSC
import qualified Domain.Types.MerchantServiceUsageConfig as DMSUC
import qualified EulerHS.Language as L
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
import Storage.ConfigPilot.Config.MerchantServiceConfig (MerchantServiceConfigDimensions (..))
import Storage.ConfigPilot.Config.MerchantServiceUsageConfig (MerchantServiceUsageConfigDimensions (..))
import Storage.ConfigPilot.Interface.Types (getConfig, getOneConfig)
import qualified System.Environment as SE

createPayoutOrder ::
  (ServiceFlow m r, HasFlowEnv m r '["selfBaseUrl" ::: BaseUrl]) =>
  Maybe Version ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Maybe Text ->
  Maybe DMPM.PaymentMode ->
  DPayment.CreatePayoutServiceReq ->
  m Payout.CreatePayoutOrderResp
createPayoutOrder = runWithServiceConfigAndName Payout.createPayoutOrder (.createPayoutOrder) DPayment.mkCreatePayoutOrderReq

payoutOrderStatus ::
  ServiceFlow m r =>
  Maybe Version ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Maybe Text ->
  Maybe DMPM.PaymentMode ->
  Maybe Text ->
  DPayment.PayoutStatusServiceReq ->
  m Payout.PayoutOrderStatusResp
payoutOrderStatus clientSdkVersion merchantId merchantOperatingCityId mRoutingId paymentMode idAssignedByServiceProvider serviceReq =
  runWithServiceConfigAndName Payout.payoutOrderStatus (.payoutOrderStatus) (DPayment.mkPayoutOrderStatusReq idAssignedByServiceProvider) clientSdkVersion merchantId merchantOperatingCityId mRoutingId paymentMode serviceReq

runWithServiceConfigAndName ::
  ServiceFlow m r =>
  (Payout.PayoutServiceConfig -> req -> m resp) ->
  (DMSUC.MerchantServiceUsageConfig -> Payout.PayoutService) ->
  (Maybe Text -> Maybe Text -> serviceReq -> req) ->
  Maybe Version ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Maybe Text ->
  Maybe DMPM.PaymentMode ->
  serviceReq ->
  m resp
runWithServiceConfigAndName func getCfg mkReq clientSdkVersion merchantId merchantOperatingCityId mRoutingId paymentMode serviceReq = do
  orgPaymentsConfig <- getConfig (MerchantServiceUsageConfigDimensions {merchantOperatingCityId = merchantOperatingCityId.getId}) >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantOperatingCityId.getId)
  let payoutServiceNameRaw = DMSC.PayoutService (getCfg orgPaymentsConfig)
  payoutServiceName <- modifyServiceName payoutServiceNameRaw (fromMaybe DMPM.LIVE paymentMode) clientSdkVersion
  merchantServiceConfig <-
    getOneConfig (MerchantServiceConfigDimensions {merchantOperatingCityId = merchantOperatingCityId.getId, merchantId = merchantId.getId, serviceName = Just payoutServiceName})
      >>= fromMaybeM (MerchantServiceConfigNotFound merchantId.getId "Payout" (show Payout.Juspay))
  case merchantServiceConfig.serviceConfig of
    DMSC.PayoutServiceConfig vsc -> case vsc of
      Payout.JuspayConfig _ -> do
        let mConnectedAccountId = Nothing
        func vsc (mkReq (getRoutingId payoutServiceName) mConnectedAccountId serviceReq)
      Payout.StripeConfig _ -> throwError (InvalidRequest "Stripe payouts do not supported")
    _ -> throwError $ InternalError "Unknown Service Config"
  where
    getRoutingId = \case
      DMSC.PayoutService PT.AAJuspay -> mRoutingId
      _ -> Nothing

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
  m DMSC.ServiceName
modifyServiceName serviceName paymentMode clientSdkVersion =
  case serviceName of
    DMSC.PayoutService payoutService -> modifyPayoutService DMSC.PayoutService payoutService
    _ -> throwError $ InternalError "Unknown Service Name"
  where
    modifyPayoutService serviceType payoutService =
      case getService payoutService of
        Juspay -> decidePayoutService serviceName clientSdkVersion
        Stripe -> pure . serviceType $ modifyPayoutServiceByMode payoutService paymentMode

-- relevant only for Stripe
modifyPayoutServiceByMode :: PT.PayoutService -> DMPM.PaymentMode -> PT.PayoutService
modifyPayoutServiceByMode Payout.Stripe DMPM.LIVE = Payout.Stripe
modifyPayoutServiceByMode Payout.Stripe DMPM.TEST = Payout.StripeTest
modifyPayoutServiceByMode Payout.StripeTest _ = Payout.StripeTest
modifyPayoutServiceByMode Payout.Juspay _ = Payout.Juspay
modifyPayoutServiceByMode Payout.AAJuspay _ = Payout.AAJuspay

-- relevant only for Juspay
decidePayoutService :: ServiceFlow m r => DMSC.ServiceName -> Maybe Version -> m DMSC.ServiceName
decidePayoutService payoutServiceName clientSdkVersion = do
  aaClientSdkVersion <- L.runIO $ (T.pack . (fromMaybe "999.999.999") <$> SE.lookupEnv "AA_ENABLED_CLIENT_SDK_VERSION")
  return $ case clientSdkVersion of
    Just v
      | v >= textToVersionDefault aaClientSdkVersion -> DMSC.PayoutService PT.AAJuspay
    _ -> payoutServiceName
