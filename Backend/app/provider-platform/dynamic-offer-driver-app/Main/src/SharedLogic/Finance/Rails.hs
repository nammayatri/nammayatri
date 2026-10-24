{-
  SharedLogic.Finance.Rails

  The driver app's TransferRail records: how the finance-kernel transfer orchestration
  talks to the actual payout rails. The PayoutRequest / PayoutOrder rows stay — they
  are the rail layer — but the hold/submit ordering, compensation and canonical status
  now live in the kernel, and 'classifyPayoutOrderStatus' is THE one place Juspay's
  payout status vocabulary is interpreted for wallet money.
-}
module SharedLogic.Finance.Rails
  ( juspayPayoutRailKey,
    stripePayoutRailKey,
    hdfcBulkRailKey,
    juspayPgRailKey,
    railKeyForFlow,
    classifyPayoutOrderStatus,
    classifyPaymentTransactionStatus,
    mkWalletPayoutRail,
    settlementRailByKey,
    mkJuspayPgInboundRail,
  )
where

import qualified Data.Aeson as A
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MerchantServiceConfig as DMSC
import qualified Domain.Types.Person as DP
import qualified Kernel.Beam.Functions as B
import Kernel.External.Encryption
import qualified Kernel.External.Payment.Interface.Types as Payment
import qualified Kernel.External.Payout.Interface as IPayout
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import Kernel.Storage.Esqueleto (EsqDBReplicaFlow)
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import qualified Lib.Finance.Core.Types as Finance
import qualified Lib.Finance.Storage.Beam.BeamFlow as FinanceBeamFlow
import qualified Lib.Finance.Transfer.Rail as Rail
import qualified Lib.Finance.Transfer.RailStatus as RailStatus
import qualified Lib.Payment.Domain.Action as DPayment
import qualified Lib.Payment.Domain.Types.Common as DPayment
import qualified Lib.Payment.Domain.Types.PaymentOrder as DOrder
import qualified Lib.Payment.Domain.Types.PayoutOrder as DPayoutOrder
import qualified Lib.Payment.Payout.Request as PayoutRequest
import qualified Lib.Payment.Storage.Beam.BeamFlow as PaymentBeamFlow
import qualified Lib.Payment.Storage.Queries.PaymentOrder as QOrder
import SharedLogic.Payment (applyPseudoClientId)
import Storage.ConfigPilot.Config.MerchantServiceUsageConfig (MerchantServiceUsageConfigDimensions (..))
import qualified Storage.Queries.Person as QP
import Tools.Error
import qualified Tools.Payment as TPayment

juspayPayoutRailKey, stripePayoutRailKey, hdfcBulkRailKey, juspayPgRailKey :: Text
juspayPayoutRailKey = "JUSPAY_PAYOUT"
stripePayoutRailKey = "STRIPE_PAYOUT"
hdfcBulkRailKey = "HDFC_BULK"
juspayPgRailKey = "JUSPAY_PG"

railKeyForFlow :: IPayout.PayoutServiceFlow -> Text
railKeyForFlow = \case
  IPayout.JuspayFlow -> juspayPayoutRailKey
  IPayout.StripeFlow -> stripePayoutRailKey
  IPayout.BulkFlow -> hdfcBulkRailKey

-- | THE Juspay payout status mapping. Total: an unparseable status is nobody's guess,
--   so it goes to review instead of being treated as pending forever.
-- | Delegates to the kernel's canonical mapping (Lib.Finance.Transfer.RailStatus) —
--   one interpreter for Juspay's payout vocabulary across both apps.
classifyPayoutOrderStatus :: Text -> Rail.TransferOutcome
classifyPayoutOrderStatus = RailStatus.classifyJuspayPayoutStatus

-- | Payment (collection) status mapping for inbound transfers (wallet topup);
--   kernel-canonical.
classifyPaymentTransactionStatus :: Payment.TransactionStatus -> Rail.TransferOutcome
classifyPaymentTransactionStatus = RailStatus.classifyJuspayTransactionStatus

-- | The wallet payout rail for one specific PayoutRequest: the submit closure executes
--   that request through lib/payment exactly as before (PayoutOrder row, PR status
--   history), so dashboards keep working — those statuses are now derived, not
--   authoritative. The kernel decides ordering from 'holdOrdering'.
mkWalletPayoutRail ::
  ( EncFlow m r,
    PaymentBeamFlow.BeamFlow m r,
    FinanceBeamFlow.BeamFlow m r,
    Finance.HasActorInfo m r
  ) =>
  IPayout.PayoutServiceFlow ->
  PayoutRequest.PayoutRequest ->
  Maybe HighPrecMoney -> -- explicit transferAmount override
  Currency ->
  (DPayment.CreatePayoutServiceReq -> m IPayout.CreatePayoutOrderResp) ->
  (DPayoutOrder.PayoutOrder -> m ()) -> -- afterPayoutOrderCreated
  (Text -> m ()) -> -- on confirmed rejection (fail the bulk order, etc.)
  Rail.TransferRail m
mkWalletPayoutRail payoutServiceFlow payoutRequest mbTransferAmount currency payoutCall afterOrderCreated onConfirmedRejection =
  Rail.TransferRail
    { railKey = railKeyForFlow payoutServiceFlow,
      holdOrdering = case payoutServiceFlow of
        IPayout.BulkFlow -> Rail.HoldThenSubmit
        _ -> Rail.SubmitThenHold,
      submit = \_submission -> do
        outcome <- PayoutRequest.executePayoutRequestWithOutcome mbTransferAmount currency payoutServiceFlow payoutRequest payoutCall afterOrderCreated
        case outcome of
          PayoutRequest.Executed order ->
            pure Rail.RailAccepted {railOrderId = order.orderId, railStatus = show order.status}
          PayoutRequest.ConfirmedFailure msg -> do
            onConfirmedRejection msg
            pure (Rail.RailRejected msg)
          PayoutRequest.AmbiguousFailure msg ->
            pure (Rail.RailAmbiguous msg)
          -- Request already in flight (or terminal): nothing new was sent; the money must
          -- stay parked and the batch/status check resolves it.
          PayoutRequest.NotExecutable status ->
            pure (Rail.RailDeferred ("REQUEST_" <> show status)),
      -- Webhooks and the existing status-check jobs push fresh raw statuses through
      -- applyRailStatus; nothing in the driver app pulls via the kernel yet.
      pollStatus = \_ -> pure Nothing,
      classifyStatus = classifyPayoutOrderStatus,
      collectPayload = Nothing
    }

-- | Settlement-time rail: webhooks only need classifyStatus (and the key must round-trip
--   from the transfer row). Its submit must never be reached; if it is, the transfer
--   parks in NEEDS_REVIEW rather than guessing.
settlementRailByKey :: (Applicative m) => Text -> Rail.TransferRail m
settlementRailByKey key =
  Rail.TransferRail
    { railKey = key,
      holdOrdering = Rail.SubmitThenHold,
      submit = \_ -> pure (Rail.RailAmbiguous "settlement-time rail cannot submit"),
      pollStatus = \_ -> pure Nothing,
      classifyStatus = classifyPayoutOrderStatus,
      collectPayload = Nothing
    }

-- | Inbound (collection) rail. The whole recharge process lives in the kernel
--   (initiateTransferIn: gate, reuse/supersede, order creation, transfer row, payload);
--   this module contributes only the PSP-speak below. The domain passes ids, nothing
--   else. Statuses arrive via the webhook and are classified by the kernel-canonical
--   payment mapping, so classifyStatus here never runs in practice.
mkJuspayPgInboundRail ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    EncFlow m r,
    Finance.HasActorInfo m r,
    ServiceFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl]
  ) =>
  Id DP.Person ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Rail.TransferRail m
mkJuspayPgInboundRail driverId merchantId mocId =
  Rail.TransferRail
    { railKey = juspayPgRailKey,
      holdOrdering = Rail.SubmitThenHold,
      submit = \_ -> pure (Rail.RailDeferred "COLLECTION_RAIL"),
      pollStatus = \_ -> pure Nothing,
      classifyStatus = \_ -> Rail.OutcomePending,
      collectPayload = Just $ \req -> do
        (resp, orderId) <- createTopupOrder (driverId, merchantId, mocId) req.submission.netAmount (Id <$> req.reuseRailOrderId)
        pure Rail.RailCollectResult {railOrderId = orderId.getId, payload = A.toJSON resp}
    }

-- | PSP-speak only, deliberately NOT exported: the one place that knows how to create
--   (or re-fetch) a Juspay collection order for a wallet topup. Reachable solely through
--   'mkJuspayPgInboundRail', i.e. through the kernel's initiateTransferIn.
createTopupOrder ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    EncFlow m r,
    Finance.HasActorInfo m r,
    ServiceFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl]
  ) =>
  (Id DP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  HighPrecMoney ->
  Maybe (Id DOrder.PaymentOrder) ->
  m (Payment.CreateOrderResp, Id DOrder.PaymentOrder)
createTopupOrder (driverId, merchantId, mocId) amount mbExistingOrderId = do
  driver <- B.runInReplica $ QP.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  driverPhone <- driver.mobileNumber & fromMaybeM (PersonFieldNotPresent "mobileNumber") >>= decrypt
  merchantServiceUsageConfig <-
    getOneConfig (MerchantServiceUsageConfigDimensions {merchantOperatingCityId = mocId.getId}) Nothing
      >>= fromMaybeM (MerchantServiceUsageConfigNotFound mocId.getId)
  let paymentServiceName = DMSC.AirportReachargeService merchantServiceUsageConfig.createBankAccount
  (orderId, orderShortId) <- handleExistingOrder mbExistingOrderId
  offerBasket <- TPayment.mkOfferBasket mocId (Just paymentServiceName) amount
  nwAddress <- asks (.nwAddress)
  let createOrderReq =
        Payment.CreateOrderReq
          { orderId = orderId,
            orderShortId = orderShortId,
            amount = amount,
            customerId = driver.id.getId,
            customerEmail = fromMaybe "test@juspay.in" driver.email,
            customerPhone = driverPhone,
            customerFirstName = Just driver.firstName,
            customerLastName = driver.lastName,
            createMandate = Nothing,
            mandateMaxAmount = Nothing,
            mandateFrequency = Nothing,
            mandateEndDate = Nothing,
            mandateStartDate = Nothing,
            optionsGetUpiDeepLinks = Nothing,
            metadataExpiryInMins = Nothing,
            splitSettlementDetails = Nothing,
            metadataGatewayReferenceId = Nothing,
            basket = offerBasket,
            paymentRules = Nothing,
            autoRefundPostSuccess = Nothing,
            webhookUrl = Just nwAddress,
            paymentFilter = Nothing,
            udf1 = Nothing,
            udf2 = Nothing,
            udf3 = Nothing
          }
  (createOrderCall, pseudoClientId) <- TPayment.createOrder merchantId mocId paymentServiceName (Just driver.id.getId)
  let commonMerchantId = cast @DM.Merchant @DPayment.Merchant merchantId
      commonPersonId = cast @DP.Person @DPayment.Person driverId
  mbResp <-
    DPayment.createOrderService
      commonMerchantId
      (Just $ cast mocId)
      commonPersonId
      Nothing
      (Just DPayment.DRIVER_WALLET_TOPUP)
      DOrder.Normal
      False
      createOrderReq
      createOrderCall
      Nothing
      False
      Nothing
      False -- skipCreateOrderCall
  case mbResp of
    Just createOrderRes ->
      pure (applyPseudoClientId pseudoClientId createOrderRes, Id orderId)
    Nothing -> do
      -- The existing PaymentOrder was past auth-token expiry; createOrderService just marked it
      -- CLIENT_AUTH_TOKEN_EXPIRED. Retry once with a fresh orderId.
      newOrderId <- generateGUID
      newOrderShortId <- generateShortId
      let retryReq = (createOrderReq :: Payment.CreateOrderReq) {Payment.orderId = newOrderId, Payment.orderShortId = newOrderShortId.getShortId}
      mbRetryResp <-
        DPayment.createOrderService
          commonMerchantId
          (Just $ cast mocId)
          commonPersonId
          Nothing
          (Just DPayment.DRIVER_WALLET_TOPUP)
          DOrder.Normal
          False
          retryReq
          createOrderCall
          Nothing
          False
          Nothing
          False -- skipCreateOrderCall
      case mbRetryResp of
        Just retryRes ->
          pure (applyPseudoClientId pseudoClientId retryRes, Id newOrderId)
        Nothing ->
          throwError $ InternalError "Failed to create wallet topup payment order"
  where
    handleExistingOrder mbOrderId = do
      mbOrder <- maybe (pure Nothing) QOrder.findById mbOrderId
      case mbOrder of
        Just order -> pure (order.id.getId, order.shortId.getShortId)
        Nothing -> do
          newOrderId <- generateGUID
          newOrderShortId <- generateShortId
          pure (newOrderId, newOrderShortId.getShortId)
