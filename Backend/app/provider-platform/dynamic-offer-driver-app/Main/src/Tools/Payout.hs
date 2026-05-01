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
    getCreatePayoutServiceFlow,
    getPayoutStatusServiceFlow,
    PayoutServiceNameOption (..),
  )
where

import qualified Data.Text as T
import qualified Domain.Types.DriverBankAccount as DDBA
import qualified Domain.Types.Extra.MerchantPaymentMethod as DMPM
import qualified Domain.Types.Extra.Plan as DPlan
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
  DMSC.ServiceName ->
  Id DMOC.MerchantOperatingCity ->
  Id DP.Person ->
  Maybe DDBA.DriverBankAccount ->
  DPayment.CreatePayoutServiceReq ->
  m Payout.CreatePayoutOrderResp
createPayoutOrder = runWithServiceConfigAndName Payout.createPayoutOrder DPayment.mkCreatePayoutOrderReq

payoutOrderStatus ::
  ServiceFlow m r =>
  DMSC.ServiceName ->
  Id DMOC.MerchantOperatingCity ->
  Id DP.Person ->
  Maybe DDBA.DriverBankAccount ->
  Maybe Text ->
  DPayment.PayoutStatusServiceReq ->
  m Payout.PayoutOrderStatusResp
payoutOrderStatus payoutServiceName merchantOperatingCityId personId mbPersonBankAccount idAssignedByServiceProvider serviceReq =
  runWithServiceConfigAndName Payout.payoutOrderStatus (DPayment.mkPayoutOrderStatusReq idAssignedByServiceProvider) payoutServiceName merchantOperatingCityId personId mbPersonBankAccount serviceReq

runWithServiceConfigAndName ::
  ServiceFlow m r =>
  (Payout.PayoutServiceConfig -> req -> m resp) ->
  (Maybe Text -> Maybe Text -> serviceReq -> req) ->
  DMSC.ServiceName ->
  Id DMOC.MerchantOperatingCity ->
  Id DP.Person ->
  Maybe DDBA.DriverBankAccount ->
  serviceReq ->
  m resp
runWithServiceConfigAndName func mkReq payoutServiceName merchantOperatingCityId personId mbPersonBankAccount serviceReq = do
  merchantServiceConfig <-
    CQMSC.findByServiceAndCity payoutServiceName merchantOperatingCityId
      >>= fromMaybeM (uncurry (MerchantServiceConfigNotFound merchantOperatingCityId.getId) (showPayoutServiceType payoutServiceName))
  case merchantServiceConfig.serviceConfig of
    DMSC.PayoutServiceConfig vsc -> callFunc vsc
    DMSC.RentalPayoutServiceConfig vsc -> callFunc vsc
    DMSC.RidePayoutServiceConfig vsc -> callFunc vsc
    _ -> throwError $ InternalError "Unknown Service Config"
  where
    mRoutingId = Just personId.getId

    getRoutingId = \case
      DMSC.PayoutService PT.AAJuspay -> mRoutingId -- FIXME should we use different serviceType here?
      _ -> Nothing

    callFunc vsc = case vsc of
      Payout.JuspayConfig _ -> do
        let mConnectedAccountId = Nothing
        func vsc (mkReq (getRoutingId payoutServiceName) mConnectedAccountId serviceReq)
      Payout.StripeConfig _ -> do
        let mConnectedAccountId = mbPersonBankAccount <&> (.accountId)
        func vsc (mkReq Nothing mConnectedAccountId serviceReq)

showPayoutServiceType :: DMSC.ServiceName -> (Text, Text)
showPayoutServiceType serviceName = do
  case T.splitOn "_" $ show serviceName of
    a : b : _ -> (a, b)
    [a] -> (a, "Unknown")
    [] -> ("Unknown", "Unknown")

getCreatePayoutServiceFlow ::
  ServiceFlow m r =>
  PayoutServiceNameOption ->
  (PT.PayoutService -> DMSC.ServiceName) ->
  Maybe Version ->
  Id DMOC.MerchantOperatingCity ->
  Id DP.Person ->
  m (Payout.PayoutServiceFlow, DMSC.ServiceName, Maybe DDBA.DriverBankAccount)
getCreatePayoutServiceFlow = getPayoutServiceFlow (.createPayoutOrder)

getPayoutStatusServiceFlow ::
  ServiceFlow m r =>
  PayoutServiceNameOption ->
  (PT.PayoutService -> DMSC.ServiceName) ->
  Maybe Version ->
  Id DMOC.MerchantOperatingCity ->
  Id DP.Person ->
  m (Payout.PayoutServiceFlow, DMSC.ServiceName, Maybe DDBA.DriverBankAccount)
getPayoutStatusServiceFlow = getPayoutServiceFlow (.payoutOrderStatus)

-- flow differentiate between Stripe and Juspay
getPayoutServiceFlow ::
  ServiceFlow m r =>
  (DMSUC.MerchantServiceUsageConfig -> Payout.PayoutService) ->
  PayoutServiceNameOption ->
  (PT.PayoutService -> DMSC.ServiceName) ->
  Maybe Version ->
  Id DMOC.MerchantOperatingCity ->
  Id DP.Person ->
  m (Payout.PayoutServiceFlow, DMSC.ServiceName, Maybe DDBA.DriverBankAccount)
getPayoutServiceFlow getCfg payoutServiceNameOption serviceType clientSdkVersion merchantOperatingCityId personId = do
  payoutServiceNameRaw <- case payoutServiceNameOption of
    MerchantServiceUsageConfigOption -> do
      orgPaymentsConfig <- CMSUC.findByMerchantOpCityId merchantOperatingCityId Nothing >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantOperatingCityId.getId)
      pure $ serviceType (getCfg orgPaymentsConfig)
    SubscriptionConfigOption serviceName -> do
      subscriptionConfig <- do
        CQSC.findSubscriptionConfigsByMerchantOpCityIdAndServiceName merchantOperatingCityId Nothing serviceName
          >>= fromMaybeM (NoSubscriptionConfigForService merchantOperatingCityId.getId $ show serviceName)
      pure $ fromMaybe (serviceType PT.Juspay) subscriptionConfig.payoutServiceName
  -- PersonBankAccount required only for Stripe service
  (payoutServiceFlow, mbPersonBankAccount) <- case payoutServiceNameRaw of
    DMSC.PayoutService payoutService -> fetchPersonBankAccount payoutService
    DMSC.RentalPayoutService payoutService -> fetchPersonBankAccount payoutService
    DMSC.RidePayoutService payoutService -> fetchPersonBankAccount payoutService
    _ -> throwError $ InternalError "Unknown Service Name"
  let mbPaymentMode = mbPersonBankAccount >>= (.paymentMode)
  payoutServiceName <- modifyServiceName payoutServiceNameRaw (fromMaybe DMPM.LIVE mbPaymentMode) clientSdkVersion merchantOperatingCityId
  pure (payoutServiceFlow, payoutServiceName, mbPersonBankAccount)
  where
    fetchPersonBankAccount payoutService = do
      let payoutServiceFlow = Payout.castPayoutServiceFlow payoutService
      mbPersonBankAccount <- case payoutServiceFlow of
        Payout.StripeFlow -> Just <$> QDBA.findByPrimaryKey personId >>= fromMaybeM (InvalidRequest "Driver bank aсcount not found") -- TODO add error code
        Payout.JuspayFlow -> pure Nothing
      pure (payoutServiceFlow, mbPersonBankAccount)

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
      case Payout.castPayoutServiceFlow payoutService of
        Payout.JuspayFlow -> decidePayoutService serviceName clientSdkVersion merchantOpCityId
        Payout.StripeFlow -> pure . serviceType $ modifyPayoutServiceByMode payoutService paymentMode

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
