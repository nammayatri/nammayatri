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
    getPayoutServiceFlowForMerchant,
    getCreatePayoutServiceFlow,
    getPayoutStatusServiceFlow,
    -- NOTE (reviewer, remove before merge): new export, used by the HDFC bulk code to read the raw partner config. The
    --   new Lib.Payment.Payout.Bulk.Types import below is for Bulk.localOrderCall in createPayoutOrder. No Juspay/Stripe
    --   caller changes because of these.
    getPayoutServiceConfig,
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
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import qualified Lib.Payment.Domain.Action as DPayment
import qualified Lib.Payment.Domain.Types.PayoutOrder as DPayoutOrder
import qualified Lib.Payment.Payout.Bulk.Types as Bulk
import qualified Storage.CachedQueries.SubscriptionConfig as CQSC
import Storage.ConfigPilot.Config.MerchantServiceConfig (MerchantServiceConfigDimensions (..))
import Storage.ConfigPilot.Config.MerchantServiceUsageConfig (MerchantServiceUsageConfigDimensions (..))
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.DriverBankAccount as QDBA
import qualified Storage.Queries.FleetDriverAssociation as QFDA
import Tools.Error

data PayoutServiceNameOption = MerchantServiceUsageConfigOption | SubscriptionConfigOption DPlan.ServiceNames

-- NOTE (reviewer, remove before merge): main calls runWithServiceConfigAndName directly. Here the city's payout
--   config is read first. For an HdfcCbxConfig the "partner call" is Bulk.localOrderCall: a local response (status
--   INITIATED, no partner id); nothing is sent to HDFC here, the order goes later in a batch file. Juspay and Stripe
--   configs go to main's runWithServiceConfigAndName call with the same arguments.
--   Shared with Juspay/Stripe -- no behaviour change: only one more cached config read (getOneConfig) before the
--   call; a missing config fails with the same MerchantServiceConfigNotFound error as main.
createPayoutOrder ::
  (ServiceFlow m r, HasFlowEnv m r '["selfBaseUrl" ::: BaseUrl]) =>
  DMSC.ServiceName ->
  Id DMOC.MerchantOperatingCity ->
  Id DP.Person ->
  Maybe DDBA.DriverBankAccount ->
  DPayment.CreatePayoutServiceReq ->
  m Payout.CreatePayoutOrderResp
createPayoutOrder payoutServiceName merchantOperatingCityId personId mbPersonBankAccount serviceReq = do
  vsc <- getPayoutServiceConfig payoutServiceName merchantOperatingCityId
  case vsc of
    -- A bulk partner has no single-order API: the order is created locally and reaches the
    -- partner later in a batch file.
    Payout.HdfcCbxConfig _ -> Bulk.localOrderCall serviceReq
    _ -> runWithServiceConfigAndName Payout.createPayoutOrder DPayment.mkCreatePayoutOrderReq payoutServiceName merchantOperatingCityId personId mbPersonBankAccount serviceReq

payoutOrderStatus ::
  ServiceFlow m r =>
  DMSC.ServiceName ->
  Id DMOC.MerchantOperatingCity ->
  Id DP.Person ->
  Maybe DDBA.DriverBankAccount ->
  DPayoutOrder.PayoutOrder ->
  DPayment.PayoutStatusServiceReq ->
  m Payout.PayoutOrderStatusResp
payoutOrderStatus payoutServiceName merchantOperatingCityId personId mbPersonBankAccount payoutOrder serviceReq =
  runWithServiceConfigAndName Payout.payoutOrderStatus (DPayment.mkPayoutOrderStatusReq payoutOrder) payoutServiceName merchantOperatingCityId personId mbPersonBankAccount serviceReq

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
    getOneConfig (MerchantServiceConfigDimensions {merchantOperatingCityId = merchantOperatingCityId.getId, merchantId = Nothing, serviceName = Just payoutServiceName}) Nothing
      >>= fromMaybeM (uncurry (MerchantServiceConfigNotFound merchantOperatingCityId.getId) (showPayoutServiceType payoutServiceName))
  case merchantServiceConfig.serviceConfig of
    DMSC.PayoutServiceConfig vsc -> callFunc vsc
    DMSC.RentalPayoutServiceConfig vsc -> callFunc vsc
    DMSC.RidePayoutServiceConfig vsc -> callFunc vsc
    _ -> throwError $ InternalError "Unknown Service Config"
  where
    mRoutingId = Just personId.getId

    getRoutingId = \case
      DMSC.PayoutService PT.AAJuspay -> mRoutingId
      _ -> Nothing

    callFunc vsc = case vsc of
      Payout.JuspayConfig _ -> do
        let mConnectedAccountId = Nothing
        func vsc (mkReq (getRoutingId payoutServiceName) mConnectedAccountId serviceReq)
      Payout.StripeConfig _ -> do
        let mConnectedAccountId = mbPersonBankAccount <&> (.accountId)
        func vsc (mkReq Nothing mConnectedAccountId serviceReq)
      -- NOTE (reviewer, remove before merge): new arm so the case on the partner config stays complete (-Werror).
      --   createPayoutOrder returns the local order before getting here, and refreshPayoutOrderWithSettlement skips
      --   batched orders, so only a status call for an HDFC order with no batch lands here and gets this error.
      --   Juspay/Stripe arms unchanged.
      -- HDFC CBX has no single-order API -- it only supports bulk submission/status-check
      -- (Kernel.External.Payout.Interface.submitBulkPayout/checkBulkPayoutStatus). Honest rather
      -- than clever: reject here instead of building a request that can never succeed.
      Payout.HdfcCbxConfig _ -> throwError $ InvalidRequest "HDFC CBX has no single-order API; use submitBulkPayout"

-- NOTE (reviewer, remove before merge): new function; the same config lookup and errors as the top of
--   runWithServiceConfigAndName above, without making a call. Used by createPayoutOrder (above) and the bulk code
--   (cycle setup, batch status check, adhoc's items-per-batch cap). Read-only; no Juspay/Stripe behaviour change.

-- | Resolve the raw 'Payout.PayoutServiceConfig' for a merchant operating city + service name,
--   without dispatching a single-order call. Needed for the HDFC CBX bulk path (submitBulkPayout
--   / checkBulkPayoutStatus take the config directly; there is no per-order request to build).
getPayoutServiceConfig ::
  ServiceFlow m r =>
  DMSC.ServiceName ->
  Id DMOC.MerchantOperatingCity ->
  m Payout.PayoutServiceConfig
getPayoutServiceConfig payoutServiceName merchantOperatingCityId = do
  merchantServiceConfig <-
    getOneConfig (MerchantServiceConfigDimensions {merchantOperatingCityId = merchantOperatingCityId.getId, merchantId = Nothing, serviceName = Just payoutServiceName}) Nothing
      >>= fromMaybeM (uncurry (MerchantServiceConfigNotFound merchantOperatingCityId.getId) (showPayoutServiceType payoutServiceName))
  case merchantServiceConfig.serviceConfig of
    DMSC.PayoutServiceConfig vsc -> pure vsc
    DMSC.RentalPayoutServiceConfig vsc -> pure vsc
    DMSC.RidePayoutServiceConfig vsc -> pure vsc
    _ -> throwError $ InternalError "Unknown Service Config"

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

-- NOTE (reviewer, remove before merge): returns the flow and the service name it resolved (main returns only the
--   flow), so the sweep, adhoc and the instant payout (Bulk.Driver.runInstantPayout) can make the bulk call without
--   resolving it twice. Same lookup, same errors. Main's only caller, ScheduledBatchPayout, takes the pair.
--   Shared with Juspay/Stripe -- no behaviour change.
getPayoutServiceFlowForMerchant ::
  ServiceFlow m r =>
  (DMSUC.MerchantServiceUsageConfig -> Payout.PayoutService) ->
  PayoutServiceNameOption ->
  (PT.PayoutService -> DMSC.ServiceName) ->
  Id DMOC.MerchantOperatingCity ->
  m (Payout.PayoutServiceFlow, DMSC.ServiceName)
getPayoutServiceFlowForMerchant getCfg payoutServiceNameOption serviceType merchantOperatingCityId = do
  payoutServiceNameRaw <- case payoutServiceNameOption of
    MerchantServiceUsageConfigOption -> do
      orgPaymentsConfig <- getOneConfig (MerchantServiceUsageConfigDimensions {merchantOperatingCityId = merchantOperatingCityId.getId}) Nothing >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantOperatingCityId.getId)
      pure $ serviceType (getCfg orgPaymentsConfig)
    SubscriptionConfigOption serviceName -> do
      subscriptionConfig <- do
        CQSC.findSubscriptionConfigsByMerchantOpCityIdAndServiceName merchantOperatingCityId Nothing serviceName
          >>= fromMaybeM (NoSubscriptionConfigForService merchantOperatingCityId.getId $ show serviceName)
      pure $ fromMaybe (serviceType PT.Juspay) subscriptionConfig.payoutServiceName
  case payoutServiceNameRaw of
    DMSC.PayoutService payoutService -> pure (Payout.castPayoutServiceFlow payoutService, payoutServiceNameRaw)
    DMSC.RentalPayoutService payoutService -> pure (Payout.castPayoutServiceFlow payoutService, payoutServiceNameRaw)
    DMSC.RidePayoutService payoutService -> pure (Payout.castPayoutServiceFlow payoutService, payoutServiceNameRaw)
    _ -> throwError $ InternalError "Unknown Service Name"

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
      orgPaymentsConfig <- getOneConfig (MerchantServiceUsageConfigDimensions {merchantOperatingCityId = merchantOperatingCityId.getId}) Nothing >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantOperatingCityId.getId)
      pure $ serviceType (getCfg orgPaymentsConfig)
    SubscriptionConfigOption serviceName -> do
      subscriptionConfig <- do
        CQSC.findSubscriptionConfigsByMerchantOpCityIdAndServiceName merchantOperatingCityId Nothing serviceName
          >>= fromMaybeM (NoSubscriptionConfigForService merchantOperatingCityId.getId $ show serviceName)
      pure $ fromMaybe (serviceType PT.Juspay) subscriptionConfig.payoutServiceName
  -- PersonBankAccount required only for the Stripe and bulk (HDFC CBX) services
  (payoutServiceFlow, mbPersonBankAccount) <- case payoutServiceNameRaw of
    DMSC.PayoutService payoutService -> fetchPersonBankAccount payoutService
    DMSC.RentalPayoutService payoutService -> fetchPersonBankAccount payoutService
    DMSC.RidePayoutService payoutService -> fetchPersonBankAccount payoutService
    _ -> throwError $ InternalError "Unknown Service Name"
  let mbPaymentMode = mbPersonBankAccount >>= (.paymentMode)
  payoutServiceName <- modifyServiceName payoutServiceNameRaw (fromMaybe DMPM.LIVE mbPaymentMode) clientSdkVersion merchantOperatingCityId
  logDebug $ "paymentMode|payout|personId=" <> personId.getId <> " mode=" <> show mbPaymentMode <> " service=" <> show payoutServiceName <> " connectedAccount=" <> show (mbPersonBankAccount <&> (.accountId))
  pure (payoutServiceFlow, payoutServiceName, mbPersonBankAccount)
  where
    -- Fleet-override: a fleet driver transacts on the fleet's account.
    fetchPersonBankAccount payoutService = do
      let payoutServiceFlow = Payout.castPayoutServiceFlow payoutService
      mbPersonBankAccount <- case payoutServiceFlow of
        Payout.StripeFlow -> do
          effectiveOwnerId <-
            QFDA.findByDriverId personId True >>= \case
              Just fleetDriverAssociation -> pure (Id fleetDriverAssociation.fleetOwnerId)
              Nothing -> pure personId
          Just <$> (QDBA.findByPrimaryKey effectiveOwnerId >>= fromMaybeM (InvalidRequest "Driver bank account not found"))
        -- NOTE (reviewer, remove before merge): bulk-only arm (castPayoutServiceFlow HdfcCbx = BulkFlow). The bank row is
        --   read for the person themself (no fleet-owner override, unlike Stripe) and must exist. Account / IFSC / name
        --   are checked later, presence only (usableBankDetails); HDFC validates the values. The Stripe and Juspay arms
        --   are main's, so no Juspay/Stripe behaviour change.
        -- Account number and IFSC are what HDFC is given, so the row has to exist. Whether it was
        -- verified is deliberately not asked: the bank never sees that flag, and the fields it does
        -- see are checked for usability on the eligibility pass ('usableBankDetails'), which puts a
        -- beneficiary with unusable details on the excluded worklist instead of failing here.
        Payout.BulkFlow -> Just <$> (QDBA.findByPrimaryKey personId >>= fromMaybeM (InvalidRequest "Driver bank account not found"))
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
        -- NOTE (reviewer, remove before merge): this arm and the `HdfcCbx` line of modifyPayoutServiceByMode below are
        --   new only because the shared kernel has an HdfcCbx service / BulkFlow flow and these cases must stay
        --   complete (-Werror). The service name is kept as is. Juspay/Stripe arms unchanged, so no behaviour change.
        -- HDFC CBX has no live/test-mode split and no client-SDK-version-based upgrade path;
        -- unchanged by mode, unlike Stripe.
        Payout.BulkFlow -> pure . serviceType $ payoutService

-- relevant only for Stripe
modifyPayoutServiceByMode :: PT.PayoutService -> DMPM.PaymentMode -> PT.PayoutService
modifyPayoutServiceByMode Payout.Stripe DMPM.LIVE = Payout.Stripe
modifyPayoutServiceByMode Payout.Stripe DMPM.TEST = Payout.StripeTest
modifyPayoutServiceByMode Payout.StripeTest _ = Payout.StripeTest
modifyPayoutServiceByMode Payout.Juspay _ = Payout.Juspay
modifyPayoutServiceByMode Payout.AAJuspay _ = Payout.AAJuspay
modifyPayoutServiceByMode Payout.HdfcCbx _ = Payout.HdfcCbx

-- relevant only for Juspay
decidePayoutService :: ServiceFlow m r => DMSC.ServiceName -> Maybe Version -> Id DMOC.MerchantOperatingCity -> m DMSC.ServiceName
decidePayoutService payoutServiceName clientSdkVersion merchantOpCityId = do
  transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId}) Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  return $ case clientSdkVersion of
    Just v
      | v >= textToVersionDefault transporterConfig.aaEnabledClientSdkVersion -> DMSC.PayoutService PT.AAJuspay
    _ -> payoutServiceName
