{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.Payout
  ( juspayPayoutWebhookHandler,
    castPayoutOrderStatus,
    payoutProcessingLockKey,
    processPreviousPayoutAmount,
    refreshPayoutOrderWithSettlement,
    stripePayoutWebhookHandler,
    stripeTestPayoutWebhookHandler,
  )
where

import qualified Data.Aeson as A
import Data.Time (utctDay)
import qualified Domain.Action.UI.DriverCoin as DriverCoin
import Domain.Action.UI.DriverWallet (counterpartyFromRole, makePayoutEntryIdsKey)
import Domain.Action.UI.Ride.EndRide.Internal (makeWalletRunningBalanceLockKey)
import qualified Domain.Types.DailyStats as DS
import qualified Domain.Types.DriverFee as DDF
import qualified Domain.Types.Extra.MerchantPaymentMethod as DMPM
import qualified Domain.Types.Extra.MerchantServiceConfig as DEMSC
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MerchantServiceConfig as DMSC
import qualified Domain.Types.PayoutConfig as DPC
import qualified Domain.Types.Person as Person
import qualified Domain.Types.Plan as DP
import qualified Domain.Types.ScheduledPayout as DSP
import qualified Domain.Types.VehicleCategory as DVC
import Environment
import Kernel.Beam.Functions as B (runInReplica)
import Kernel.External.Encryption (decrypt)
import qualified Kernel.External.Notification.FCM.Types as FCM
import qualified Kernel.External.Payment.Stripe.Types.Common as PaymentStripe
import qualified Kernel.External.Payout.Interface as Juspay
import qualified Kernel.External.Payout.Interface.Events.Types as PayoutEvents
import qualified Kernel.External.Payout.Interface.Juspay as Juspay
import qualified Kernel.External.Payout.Interface.Stripe as IStripe
import qualified Kernel.External.Payout.Interface.Types as IPayout
import qualified Kernel.External.Payout.Juspay.Types.Payout as Payout
import qualified Kernel.External.Payout.Types as TPayout
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer)
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id
import Kernel.Types.Servant (RawByteString (..))
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import qualified Lib.Finance.Core.Types as Finance
import qualified Lib.Payment.Domain.Action as DPayment
import qualified Lib.Payment.Domain.Types.Common as DPayment
import qualified Lib.Payment.Domain.Types.PayoutOrder as DPayoutOrder
import qualified Lib.Payment.Domain.Types.PayoutRequest as DPR
import qualified Lib.Payment.Payout.RequestStatus as RequestStatus
import qualified Lib.Payment.Storage.Queries.PayoutOrder as QPayoutOrder
import qualified Lib.Payment.Storage.Queries.PayoutRequest as QPR
import Servant (BasicAuthData)
import qualified SharedLogic.DriverFee as SLDriverFee
import SharedLogic.Finance.Wallet
import SharedLogic.Merchant
import qualified SharedLogic.MessageBuilder as MessageBuilder
import qualified SharedLogic.Ride as SharedRide
import Storage.Beam.Finance ()
import Storage.Beam.Payment ()
import qualified Storage.Cac.TransporterConfig as SCTC
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as CQMSC
import qualified Storage.CachedQueries.Merchant.PayoutConfig as CQPC
import qualified Storage.CachedQueries.SubscriptionConfig as CQSC
import Storage.ConfigPilot.Config.MerchantServiceConfig (MerchantServiceConfigDimensions (..))
import Storage.ConfigPilot.Config.PayoutConfig (PayoutConfigDimensions (..))
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.DailyStats as QDailyStats
import qualified Storage.Queries.DriverFee as QDF
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.DriverStats as QDriverStats
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.ScheduledPayout as QSP
import qualified Storage.Queries.ScheduledPayoutExtra as QSPE
import qualified Storage.Queries.Vehicle as QV
import Tools.Error
import qualified Tools.Notifications as Notify
import qualified Tools.Payout as Payout
import qualified Tools.SMS as Sms

-- webhook ----------------------------------------------------------

type CallPayoutServiceAction =
  Text ->
  Id Person.Person ->
  DPC.PayoutConfig ->
  Flow (Payout.PayoutOrderStatus, Text)

juspayPayoutWebhookHandler ::
  ShortId DM.Merchant ->
  Maybe Context.City ->
  Maybe DP.ServiceNames ->
  BasicAuthData ->
  Value ->
  Flow AckResponse
juspayPayoutWebhookHandler merchantShortId mbOpCity mbServiceName authData value = do
  (psc, merchantOperatingCityId, merchantId) <- fetchPaymentServiceConfig merchantShortId mbOpCity mbServiceName TPayout.Juspay
  orderStatusResp <- Juspay.payoutOrderStatusWebhook psc authData value
  osr <- case orderStatusResp of
    Nothing -> throwError $ InternalError "Order Contents not found."
    Just osr' -> pure osr'
  logDebug $ "Webhook Payout Resp: " <> show osr
  case osr of
    IPayout.OrderStatusPayoutResp {..} -> do
      payoutSettlementAction merchantId merchantOperatingCityId payoutStatus amount payoutOrderId callJuspayPayoutServiceAction
    IPayout.BadStatusResp -> pure ()
  pure Ack
  where
    callJuspayPayoutServiceAction :: CallPayoutServiceAction
    callJuspayPayoutServiceAction payoutOrderId driverId payoutConfig = do
      driver <- B.runInReplica $ QP.findById driverId >>= fromMaybeM (PersonDoesNotExist driverId.getId)
      payoutOrder <- QPayoutOrder.findByOrderId payoutOrderId >>= fromMaybeM (PayoutOrderNotFound payoutOrderId)
      let payoutServiceNameCons = case payoutOrder.entityName of
            Just DPayment.SPECIAL_ZONE_PAYOUT -> DEMSC.RidePayoutService
            _ -> DEMSC.PayoutService
      (_payoutServiceFlow, payoutServiceName, mbPersonBankAccount) <- Payout.getPayoutStatusServiceFlow Payout.MerchantServiceUsageConfigOption payoutServiceNameCons driver.clientSdkVersion driver.merchantOperatingCityId driverId
      let createPayoutOrderStatusReq = DPayment.PayoutStatusServiceReq {orderId = payoutOrderId, mbExpand = payoutConfig.expand}
          createPayoutOrderStatusCall = Payout.payoutOrderStatus payoutServiceName driver.merchantOperatingCityId driverId mbPersonBankAccount
      payoutStatusResp <- DPayment.payoutStatusService (cast driver.merchantId) (cast driver.id) createPayoutOrderStatusReq createPayoutOrderStatusCall
      pure (payoutStatusResp.status, payoutStatusResp.orderId)

castPayoutOrderStatus :: Payout.PayoutOrderStatus -> DS.PayoutStatus
castPayoutOrderStatus payoutOrderStatus =
  case payoutOrderStatus of
    Payout.SUCCESS -> DS.Success
    Payout.FULFILLMENTS_SUCCESSFUL -> DS.Success
    Payout.ERROR -> DS.Failed
    Payout.FAILURE -> DS.Failed
    Payout.FULFILLMENTS_FAILURE -> DS.Failed
    Payout.CANCELLED -> DS.ManualReview
    Payout.FULFILLMENTS_CANCELLED -> DS.ManualReview
    Payout.FULFILLMENTS_MANUAL_REVIEW -> DS.ManualReview
    _ -> DS.Processing

casPayoutOrderStatusToDFeeStatus :: Payout.PayoutOrderStatus -> DDF.DriverFeeStatus
casPayoutOrderStatusToDFeeStatus payoutOrderStatus =
  case payoutOrderStatus of
    Payout.SUCCESS -> DDF.REFUNDED
    Payout.FULFILLMENTS_SUCCESSFUL -> DDF.REFUNDED
    Payout.ERROR -> DDF.REFUND_FAILED
    Payout.FAILURE -> DDF.REFUND_FAILED
    Payout.FULFILLMENTS_FAILURE -> DDF.REFUND_FAILED
    Payout.CANCELLED -> DDF.REFUND_MANUAL_REVIEW_REQUIRED
    Payout.FULFILLMENTS_CANCELLED -> DDF.REFUND_MANUAL_REVIEW_REQUIRED
    Payout.FULFILLMENTS_MANUAL_REVIEW -> DDF.REFUND_MANUAL_REVIEW_REQUIRED
    _ -> DDF.REFUND_PENDING

castPayoutOrderStatusToScheduledPayoutStatus :: Payout.PayoutOrderStatus -> DSP.ScheduledPayoutStatus
castPayoutOrderStatusToScheduledPayoutStatus payoutOrderStatus =
  case payoutOrderStatus of
    Payout.SUCCESS -> DSP.CREDITED
    Payout.FULFILLMENTS_SUCCESSFUL -> DSP.CREDITED
    Payout.ERROR -> DSP.AUTO_PAY_FAILED
    Payout.FAILURE -> DSP.AUTO_PAY_FAILED
    Payout.FULFILLMENTS_FAILURE -> DSP.AUTO_PAY_FAILED
    Payout.CANCELLED -> DSP.CANCELLED
    Payout.FULFILLMENTS_CANCELLED -> DSP.CANCELLED
    Payout.FULFILLMENTS_MANUAL_REVIEW -> DSP.PROCESSING
    _ -> DSP.PROCESSING

payoutProcessingLockKey :: Text -> Text
payoutProcessingLockKey driverId = "Payout:Processing:DriverId" <> driverId

-- stripe webhook ---------------------------------------------------

stripePayoutWebhookHandler,
  stripeTestPayoutWebhookHandler ::
    ShortId DM.Merchant ->
    Maybe Context.City ->
    Maybe DP.ServiceNames ->
    Maybe Text ->
    RawByteString ->
    Flow AckResponse
stripePayoutWebhookHandler = stripePayoutWebhookHandler' DMPM.LIVE
stripeTestPayoutWebhookHandler = stripePayoutWebhookHandler' DMPM.TEST

stripePayoutWebhookHandler' ::
  DMPM.PaymentMode ->
  ShortId DM.Merchant ->
  Maybe Context.City ->
  Maybe DP.ServiceNames ->
  Maybe Text ->
  RawByteString ->
  Flow AckResponse
stripePayoutWebhookHandler' paymentMode merchantShortId mbOpCity mbServiceName mbSigHeader rawBytes = do
  let serviceName = case paymentMode of
        DMPM.LIVE -> TPayout.Stripe
        DMPM.TEST -> TPayout.StripeTest
  (payoutServiceConfig, merchantOperatingCityId, merchantId) <- fetchPaymentServiceConfig merchantShortId mbOpCity mbServiceName serviceName
  let checkDuplicatedEvent eventId =
        isDuplicateStripeWebhookEvent
          (stripePayoutWebhookEventDedupKey paymentMode eventId)
          stripeWebhookEventDedupTtl7Days
  IStripe.payoutStripeServiceEventWebhook payoutServiceConfig checkDuplicatedEvent (stripePayoutWebhookAction merchantId merchantOperatingCityId) mbSigHeader rawBytes

-- | TTL for Stripe webhook event deduplication (covers Stripe retry window with margin).
stripeWebhookEventDedupTtl7Days :: Redis.ExpirationTime
stripeWebhookEventDedupTtl7Days = 7 * 24 * 60 * 60

stripePayoutWebhookEventDedupKey :: DMPM.PaymentMode -> Id PaymentStripe.Event -> Text
stripePayoutWebhookEventDedupKey paymentMode eventId = "Stripe:Payout:" <> show paymentMode <> ":Webhook:Event-" <> eventId.getId

-- | Marks a Stripe webhook event as seen (SET NX + EX). Returns 'True' if this delivery is a duplicate.
isDuplicateStripeWebhookEvent :: (Redis.HedisFlow m env, TryException m) => Text -> Redis.ExpirationTime -> m Bool
isDuplicateStripeWebhookEvent key ttl =
  not <$> Redis.setNxExpire key ttl True

stripePayoutWebhookAction ::
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  PayoutEvents.PayoutServiceEventResp ->
  Text ->
  Flow AckResponse
stripePayoutWebhookAction merchantId merchantOperatingCityId resp respDump = do
  let stripeWebhookData = DPayment.mkPayoutStripeWebhookData resp.eventData
      commonMerchantOperatingCityId = Kernel.Types.Id.cast @DMOC.MerchantOperatingCity @DPayment.MerchantOperatingCity merchantOperatingCityId
  case stripeWebhookData of
    DPayment.PayoutStripePayoutWebhookData pObj -> do
      payoutOrderId <- pObj.orderId & fromMaybeM (InvalidRequest $ "order_id not found in Stripe payout metadata for eventId: " <> resp.id.getId)
      payoutSettlementAction merchantId merchantOperatingCityId (IStripe.castPayoutStatus pObj.status) pObj.amount payoutOrderId (callStripePayoutServiceAction stripeWebhookData pObj)
    DPayment.PayoutStripeTransferWebhookData _tObj -> do
      void $ DPayment.stripePayoutWebhookService commonMerchantOperatingCityId resp respDump stripeWebhookData
    DPayment.SkipPayoutStripeWebhookData -> do
      void $ DPayment.stripePayoutWebhookService commonMerchantOperatingCityId resp respDump stripeWebhookData
  pure Ack
  where
    callStripePayoutServiceAction :: DPayment.PayoutStripeWebhookData -> PayoutEvents.Payout -> CallPayoutServiceAction
    callStripePayoutServiceAction stripeWebhookData pObj payoutOrderId _driverId _payoutConfig = do
      let commonMerchantOperatingCityId = Kernel.Types.Id.cast @DMOC.MerchantOperatingCity @DPayment.MerchantOperatingCity merchantOperatingCityId
      void $ DPayment.stripePayoutWebhookService commonMerchantOperatingCityId resp respDump stripeWebhookData
      pure (IStripe.castPayoutStatus pObj.status, payoutOrderId)

fetchPaymentServiceConfig ::
  ShortId DM.Merchant ->
  Maybe Context.City ->
  Maybe DP.ServiceNames ->
  TPayout.PayoutService ->
  Flow (IPayout.PayoutServiceConfig, Id DMOC.MerchantOperatingCity, Id DM.Merchant)
fetchPaymentServiceConfig merchantShortId mbOpCity mbServiceName service = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOperatingCityId <- CQMOC.getMerchantOpCityId Nothing merchant mbOpCity
  let merchantId = merchant.id
  payoutServiceName' <- case mbServiceName of
    Just serviceName | serviceName == DP.YATRI_RENTAL -> return $ DEMSC.RidePayoutService service
    Just serviceName -> do
      subscriptionConfig <- do
        CQSC.findSubscriptionConfigsByMerchantOpCityIdAndServiceName merchantOperatingCityId Nothing serviceName
          >>= fromMaybeM (NoSubscriptionConfigForService merchantOperatingCityId.getId $ show serviceName)
      case fromMaybe (DEMSC.PayoutService service) subscriptionConfig.payoutServiceName of
        DEMSC.PayoutService subscriptionService -> DEMSC.PayoutService <$> guardSubscriptionPayoutService subscriptionService
        DEMSC.RentalPayoutService subscriptionService -> DEMSC.RentalPayoutService <$> guardSubscriptionPayoutService subscriptionService
        DEMSC.RidePayoutService subscriptionService -> DEMSC.RidePayoutService <$> guardSubscriptionPayoutService subscriptionService
        _ -> throwError $ InternalError "Unknown Payout Service"
    Nothing -> return $ DEMSC.PayoutService service
  merchantServiceConfig <-
    getOneConfig (MerchantServiceConfigDimensions {merchantOperatingCityId = merchantOperatingCityId.getId, merchantId = Nothing, serviceName = Just payoutServiceName'}) (Just (maybeToList <$> CQMSC.findByServiceAndCity payoutServiceName' merchantOperatingCityId))
      >>= fromMaybeM (MerchantServiceConfigNotFound merchantId.getId "Payout" (show service))
  psc <- case merchantServiceConfig.serviceConfig of
    DMSC.PayoutServiceConfig psc' -> pure psc'
    DMSC.RentalPayoutServiceConfig psc' -> pure psc'
    DMSC.RidePayoutServiceConfig psc' -> pure psc'
    _ -> throwError $ InternalError "Unknown Service Config"
  pure (psc, merchantOperatingCityId, merchantId)
  where
    guardSubscriptionPayoutService subscriptionService = do
      let webhookFlow = TPayout.castPayoutServiceFlow service
      unless (TPayout.castPayoutServiceFlow subscriptionService == webhookFlow) $
        throwError $ InternalError "Unknown Payout Service"
      pure case webhookFlow of
        TPayout.StripeFlow -> service -- we should keep differentiation between Stripe and StripeTest, depending to which webhook triggered
        TPayout.JuspayFlow -> subscriptionService

isPayoutOrderSuccess :: IPayout.PayoutOrderStatus -> Bool
isPayoutOrderSuccess status = status `elem` [Payout.SUCCESS, Payout.FULFILLMENTS_SUCCESSFUL]

payoutSettlementAction ::
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  IPayout.PayoutOrderStatus ->
  HighPrecMoney ->
  Text ->
  CallPayoutServiceAction ->
  Flow ()
payoutSettlementAction merchantId merchantOperatingCityId payoutStatus amount payoutOrderId callPayoutServiceAction = do
  payoutOrder <- QPayoutOrder.findByOrderId payoutOrderId >>= fromMaybeM (PayoutOrderNotFound payoutOrderId)
  runPayoutSettlement merchantId merchantOperatingCityId payoutStatus amount payoutOrder callPayoutServiceAction

runPayoutSettlement ::
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  IPayout.PayoutOrderStatus ->
  HighPrecMoney ->
  DPayoutOrder.PayoutOrder ->
  CallPayoutServiceAction ->
  Flow ()
runPayoutSettlement merchantId merchantOperatingCityId payoutStatus amount payoutOrder callPayoutServiceAction =
  case payoutOrder.entityName of
    Just DPayment.SPECIAL_ZONE_PAYOUT ->
      settleSpecialZonePayout merchantOperatingCityId payoutOrder callPayoutServiceAction
    _ ->
      unless (isPayoutOrderSuccess payoutOrder.status) $
        settlePayoutEntities merchantId merchantOperatingCityId payoutStatus amount payoutOrder callPayoutServiceAction

settleSpecialZonePayout ::
  Id DMOC.MerchantOperatingCity ->
  DPayoutOrder.PayoutOrder ->
  CallPayoutServiceAction ->
  Flow ()
settleSpecialZonePayout merchantOperatingCityId payoutOrder callPayoutServiceAction = do
  let mbPayoutRequestId = listToMaybe (fromMaybe [] payoutOrder.entityIds)
  case mbPayoutRequestId of
    Nothing -> throwError $ InternalError "PayoutRequest ID not found"
    Just payoutRequestId -> do
      mbPayoutRequest <- QPR.findById (Id payoutRequestId)
      payoutConfig <- getPayoutConfigForCustomer merchantOperatingCityId payoutOrder.customerId
      (updPayoutStatus, _) <- callPayoutServiceAction payoutOrder.orderId (Id payoutOrder.customerId) payoutConfig
      case mbPayoutRequest of
        Just payoutRequest -> do
          let newStatus = RequestStatus.castPayoutOrderStatusToPayoutRequestStatus updPayoutStatus
          when (payoutRequest.status /= newStatus && payoutRequest.status `notElem` [DPR.CREDITED, DPR.CASH_PAID, DPR.CASH_PENDING]) do
            when (newStatus `elem` [DPR.CANCELLED, DPR.AUTO_PAY_FAILED, DPR.FAILED]) $ do
              SharedRide.safeRevertVehicleBalanceForPayout payoutRequest
            when (newStatus `elem` [DPR.CREDITED, DPR.CASH_PAID]) $ do
              let redisKey = mkSpecialZonePayoutSmsKey payoutRequest.id newStatus
              alreadySent <- Redis.get redisKey
              case (alreadySent :: Maybe Bool) of
                Just True -> pure ()
                _ -> do
                  Redis.setExp redisKey True (60 * 60 * 24)
                  fork "Send Special Zone Payout SMS to Driver" $
                    sendSpecialZonePayoutSms merchantOperatingCityId payoutRequest
        Nothing -> do
          scheduledPayout <- QSP.findById (Id payoutRequestId) >>= fromMaybeM (InternalError $ "PayoutRequest/ScheduledPayout Not Found: " <> show payoutRequestId)
          let newStatus = castPayoutOrderStatusToScheduledPayoutStatus updPayoutStatus
          when (scheduledPayout.status /= newStatus && scheduledPayout.status `notElem` [DSP.CREDITED, DSP.CASH_PAID, DSP.CASH_PENDING]) do
            let statusMsg = "Bank Webhook: " <> show updPayoutStatus
            QSPE.updateStatusWithHistoryById newStatus (Just statusMsg) scheduledPayout

settlePayoutEntities ::
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  IPayout.PayoutOrderStatus ->
  HighPrecMoney ->
  DPayoutOrder.PayoutOrder ->
  CallPayoutServiceAction ->
  Flow ()
settlePayoutEntities merchantId merchantOperatingCityId payoutStatus amount payoutOrder callPayoutServiceAction = do
  payoutConfig <- getPayoutConfigForCustomer merchantOperatingCityId payoutOrder.customerId
  when (isPayoutOrderSuccess payoutStatus) do
    person' <- QP.findById (Id payoutOrder.customerId) >>= fromMaybeM (PersonNotFound payoutOrder.customerId)
    let isFleetOwnerRole = person'.role `elem` [Person.FLEET_OWNER, Person.FLEET_BUSINESS]
    unless isFleetOwnerRole do
      driverStats <- QDriverStats.findById (Id payoutOrder.customerId) >>= fromMaybeM (PersonNotFound payoutOrder.customerId)
      QDriverStats.updateTotalPayoutAmountPaid (Just (fromMaybe 0 driverStats.totalPayoutAmountPaid + amount)) (Id payoutOrder.customerId)
      updateDFeeStatusForPayoutRegistrationRefund payoutOrder.customerId
  case payoutOrder.entityName of
    Just DPayment.DRIVER_DAILY_STATS -> do
      forM_ (listToMaybe =<< payoutOrder.entityIds) $ \dailyStatsId -> do
        dailyStats <- QDailyStats.findByPrimaryKey dailyStatsId >>= fromMaybeM (InternalError "DailyStats Not Found")
        Redis.withWaitOnLockRedisWithExpiry (payoutProcessingLockKey dailyStats.driverId.getId) 3 3 $ do
          let dPayoutStatus = castPayoutOrderStatus payoutStatus
          when (dailyStats.payoutStatus /= DS.Success) $ do
            QDailyStats.updatePayoutStatusById dPayoutStatus dailyStatsId
        fork "Update Payout Status and Transactions for DailyStats" $ do
          callPayoutService dailyStats.driverId payoutConfig
    Just DPayment.MANUAL -> do
      forM_ (listToMaybe =<< payoutOrder.entityIds) $ \driverId -> do
        fork "Update Payout Status and Transactions for Manual Payout" $ do
          callPayoutService (Id driverId) payoutConfig
    Just DPayment.REGISTRATION_REFUND -> do
      let driverId = Id payoutOrder.customerId
      fork "Update Payout Status and Transactions for Payout Registration Refund Payout" $ do
        callPayoutService driverId payoutConfig
    Just DPayment.COINS_REDEMPTION -> do
      let driverId = Id payoutOrder.customerId
      fork "Update Payout Status and Transactions for Coins Redemption" $ do
        callPayoutServiceForCoinsRedemption driverId payoutConfig
    Just DPayment.BACKLOG -> do
      whenJust payoutOrder.entityIds $ \entityIds -> do
        fork "Update Payout Status for Backlog" $ do
          mapM_ (updateStatsWithLock payoutConfig) entityIds
    Just DPayment.RETRY_VIA_DASHBOARD -> do
      whenJust payoutOrder.entityIds $ \entityIds -> do
        fork "Update Payout Status for Retried Orders" $ do
          mapM_ (updateStatsWithLock payoutConfig) entityIds
    Just DPayment.DAILY_STATS_VIA_DASHBOARD -> do
      forM_ (listToMaybe =<< payoutOrder.entityIds) $ \dailyStatsId -> do
        dailyStats <- QDailyStats.findByPrimaryKey dailyStatsId >>= fromMaybeM (InternalError "DailyStats Not Found")
        Redis.withWaitOnLockRedisWithExpiry (payoutProcessingLockKey dailyStats.driverId.getId) 3 3 $ do
          let dPayoutStatus = castPayoutOrderStatus payoutStatus
          when (dailyStats.payoutStatus /= DS.Success) $ QDailyStats.updatePayoutStatusById dPayoutStatus dailyStatsId
        fork "Update Payout Status For DailyStats Via Dashboard" $ do
          callPayoutService dailyStats.driverId payoutConfig
    Just DPayment.DRIVER_FEE -> do
      let dPayoutStatus = casPayoutOrderStatusToDFeeStatus payoutStatus
      driverIdsWithServiceName <- do
        forM (fromMaybe [] payoutOrder.entityIds) $ \driverFeeId -> do
          driverFee <- QDF.findById (Id driverFeeId) >>= fromMaybeM (InternalError "DriverFee Not Found")
          Redis.withWaitOnLockRedisWithExpiry (payoutProcessingLockKey driverFee.driverId.getId) 3 3 $ do
            let refundData =
                  DDF.RefundInfo
                    { status = Just dPayoutStatus,
                      refundEntityId = Nothing,
                      refundedAmount = Nothing,
                      refundedAt = Nothing,
                      refundedBy = Nothing
                    }
            when (driverFee.status == DDF.REFUND_PENDING) $ QDF.updateRefundData (Id driverFeeId) refundData
          fork "Update Payout Status and Transactions for DriverFee" $ do
            callPayoutService driverFee.driverId payoutConfig
          return (driverFee.driverId, driverFee.serviceName)
      let mbDriverIdAndServiceName = listToMaybe driverIdsWithServiceName
      whenJust mbDriverIdAndServiceName $ \(driverId, serviceName) -> do
        when (dPayoutStatus == DDF.REFUNDED) $ do
          dueDriverFees <- QDF.findAllByStatusAndDriverIdWithServiceName driverId [DDF.PAYMENT_OVERDUE] Nothing serviceName
          SLDriverFee.adjustDues dueDriverFees
    Just DPayment.DRIVER_WALLET_TRANSACTION -> do
      let driverId = Id payoutOrder.customerId
      let mbPayoutRequestId = listToMaybe (fromMaybe [] payoutOrder.entityIds)
      mbPayoutReq <- case mbPayoutRequestId of
        Nothing -> pure Nothing
        Just prId -> QPR.findById (Id prId)

      Redis.withWaitOnLockRedisWithExpiry (makeWalletRunningBalanceLockKey driverId.getId) 10 10 $ do
        (updPayoutStatus, _) <- callPayoutServiceAction payoutOrder.orderId driverId payoutConfig
        person <- QP.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
        let counterparty = counterpartyFromRole person.role
        when (isPayoutOrderSuccess updPayoutStatus) $ do
          transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOperatingCityId.getId}) (Just (SCTC.findByMerchantOpCityId merchantOperatingCityId Nothing)) >>= fromMaybeM (TransporterConfigNotFound merchantOperatingCityId.getId)
          let metadata =
                A.object
                  [ "driverPayable" A..= (-1 * amount),
                    "payoutOrderId" A..= payoutOrder.id.getId
                  ]
          void $
            createWalletEntryDelta
              counterparty
              driverId.getId
              (negate amount)
              transporterConfig.currency
              payoutOrder.merchantId
              merchantOperatingCityId.getId
              walletReferencePayout
              payoutOrder.id.getId
              (Just metadata)
              >>= fromEitherM (\err -> InternalError ("Failed to create wallet payout entry: " <> show err))

          whenJust mbPayoutReq $ \payoutReq -> do
            mbEntryIds <- Redis.get (makePayoutEntryIdsKey payoutReq.id.getId)
            case mbEntryIds of
              Just entryIds -> do
                settleWalletEntries (map Id entryIds) payoutReq.id.getId
                Redis.del (makePayoutEntryIdsKey payoutReq.id.getId)
              Nothing -> logInfo $ "No stashed entry IDs found for payoutRequest " <> payoutReq.id.getId

        let (notificationTitle, notificationMessage, notificationType) =
              if isPayoutOrderSuccess updPayoutStatus
                then ("Payout Complete", "Your payout of Rs." <> show amount <> " has been successfully settled to your bank account.", FCM.PAYOUT_COMPLETED)
                else ("Payout Failed", "Your payout of Rs." <> show amount <> " has failed. Please retry or contact support.", FCM.PAYOUT_FAILED)
        Notify.sendNotificationToDriver person.merchantOperatingCityId FCM.SHOW Nothing notificationType notificationTitle notificationMessage person person.deviceToken
    _ -> pure ()
  where
    updateDFeeStatusForPayoutRegistrationRefund driverId = do
      mbDriverFee <- QDF.findLatestByFeeTypeAndStatusWithServiceName DDF.PAYOUT_REGISTRATION [DDF.REFUND_PENDING] (Id driverId) DP.YATRI_SUBSCRIPTION
      whenJust mbDriverFee $ \driverFee -> do
        now <- getCurrentTime
        QDF.updateStatus DDF.REFUNDED driverFee.id now

    callPayoutService driverId payoutConfig = do
      void $ callPayoutServiceAction payoutOrder.orderId driverId payoutConfig

    callPayoutServiceForCoinsRedemption driverId payoutConfig = do
      (updPayoutOrderStatus, orderId) <- callPayoutServiceAction payoutOrder.orderId driverId payoutConfig
      case updPayoutOrderStatus of
        Payout.FULFILLMENTS_FAILURE ->
          DriverCoin.refundCoins driverId merchantId merchantOperatingCityId orderId
        _ -> pure ()

    updateStatsWithLock payoutConfig dStatsId = do
      let dPayoutStatus = castPayoutOrderStatus payoutStatus
      dailyStats <- QDailyStats.findByPrimaryKey dStatsId >>= fromMaybeM (InternalError "DailyStats Not Found")
      Redis.withWaitOnLockRedisWithExpiry (payoutProcessingLockKey dailyStats.driverId.getId) 3 3 $ do
        when (dailyStats.payoutStatus /= DS.Success) $ QDailyStats.updatePayoutStatusById dPayoutStatus dStatsId
      callPayoutServiceAction payoutOrder.orderId dailyStats.driverId payoutConfig

-- | Poll Juspay and run the same settlement as the webhook when payout_order is not
-- already SUCCESS; otherwise return the order as-is.
refreshPayoutOrderWithSettlement ::
  DPayoutOrder.PayoutOrder ->
  Flow DPayoutOrder.PayoutOrder
refreshPayoutOrderWithSettlement payoutOrder =
  if isPayoutOrderSuccess payoutOrder.status
    then pure payoutOrder
    else case payoutOrder.entityName of
      Nothing -> pure payoutOrder
      Just _ -> do
        person <- QP.findById (Id payoutOrder.customerId) >>= fromMaybeM (PersonNotFound payoutOrder.customerId)
        let merchantOpCityId = maybe person.merchantOperatingCityId Id payoutOrder.merchantOperatingCityId
            merchantId = Id payoutOrder.merchantId
        payoutConfig <- getPayoutConfigForCustomer merchantOpCityId payoutOrder.customerId
        let callAction = callPayoutServiceActionForRefresh merchantId merchantOpCityId payoutOrder.entityName
        (updPayoutStatus, _) <- callAction payoutOrder.orderId (Id payoutOrder.customerId) payoutConfig
        runPayoutSettlement merchantId merchantOpCityId updPayoutStatus payoutOrder.amount.amount payoutOrder callAction
        QPayoutOrder.findByOrderId payoutOrder.orderId >>= \case
          Nothing -> throwError $ PayoutOrderNotFound payoutOrder.orderId
          Just finalOrder -> pure finalOrder

getPayoutConfigForCustomer ::
  Id DMOC.MerchantOperatingCity ->
  Text ->
  Flow DPC.PayoutConfig
getPayoutConfigForCustomer merchantOperatingCityId customerId = do
  mbVehicle <- QV.findById (Id customerId)
  let vehicleCategory = fromMaybe DVC.AUTO_CATEGORY ((.category) =<< mbVehicle)
  getOneConfig
    (PayoutConfigDimensions {merchantOperatingCityId = merchantOperatingCityId.getId, vehicleCategory = Just vehicleCategory, isPayoutEnabled = Nothing})
    (Just (maybeToList <$> CQPC.findByPrimaryKey merchantOperatingCityId vehicleCategory Nothing))
    >>= fromMaybeM (PayoutConfigNotFound (show vehicleCategory) merchantOperatingCityId.getId)

callPayoutServiceActionForRefresh ::
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Maybe DPayment.EntityName ->
  CallPayoutServiceAction
callPayoutServiceActionForRefresh _merchantId merchantOpCityId mbEntityName payoutOrderId driverId payoutConfig = do
  driver <- B.runInReplica $ QP.findById driverId >>= fromMaybeM (PersonDoesNotExist driverId.getId)
  let payoutServiceNameCons = case mbEntityName of
        Just DPayment.SPECIAL_ZONE_PAYOUT -> DEMSC.RidePayoutService
        _ -> DEMSC.PayoutService
  (_payoutServiceFlow, payoutServiceName, mbPersonBankAccount) <-
    Payout.getPayoutStatusServiceFlow Payout.MerchantServiceUsageConfigOption payoutServiceNameCons driver.clientSdkVersion merchantOpCityId driverId
  let createPayoutOrderStatusReq = DPayment.PayoutStatusServiceReq {orderId = payoutOrderId, mbExpand = payoutConfig.expand}
      createPayoutOrderStatusCall = Payout.payoutOrderStatus payoutServiceName merchantOpCityId driverId mbPersonBankAccount
  payoutStatusResp <- DPayment.payoutStatusService (cast driver.merchantId) (cast driver.id) createPayoutOrderStatusReq createPayoutOrderStatusCall
  pure (payoutStatusResp.status, payoutStatusResp.orderId)

processPreviousPayoutAmount :: (CacheFlow m r, EsqDBFlow m r, EncFlow m r, HasFlowEnv m r '["selfBaseUrl" ::: BaseUrl], HasKafkaProducer r, Finance.HasActorInfo m r) => Id Person.Person -> Maybe Text -> Id DMOC.MerchantOperatingCity -> m ()
processPreviousPayoutAmount personId mbVpa merchOpCity = do
  mbVehicle <- QV.findById personId
  let vehicleCategory = fromMaybe DVC.AUTO_CATEGORY ((.category) =<< mbVehicle)
  payoutConfig <- getOneConfig (PayoutConfigDimensions {merchantOperatingCityId = merchOpCity.getId, vehicleCategory = Just vehicleCategory, isPayoutEnabled = Nothing}) (Just (maybeToList <$> CQPC.findByPrimaryKey merchOpCity vehicleCategory Nothing)) >>= fromMaybeM (PayoutConfigNotFound (show vehicleCategory) merchOpCity.getId)
  redisLockDriverId <- Redis.tryLockRedis lockKey 10800
  dInfo <- QDI.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  when (payoutConfig.isPayoutEnabled && redisLockDriverId && dInfo.isBlockedForReferralPayout /= Just True) do
    dailyStats_ <- QDailyStats.findAllByPayoutStatusAndReferralEarningsAndDriver DS.PendingForVpa personId
    transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchOpCity.getId}) (Just (SCTC.findByMerchantOpCityId merchOpCity Nothing)) >>= fromMaybeM (TransporterConfigNotFound merchOpCity.getId)
    localTime <- getLocalCurrentTime transporterConfig.timeDiffFromUtc
    let dailyStats = filter (\ds -> (ds.activatedValidRides <= transporterConfig.maxPayoutReferralForADay) && ds.merchantLocalDate /= (utctDay localTime)) dailyStats_ -- filter out the flagged payouts and current day payout earning
    when (length dailyStats > 0) $ do
      person <- QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
      let statsIds = map (.id) dailyStats
          pendingAmount = sum (map (.referralEarnings) dailyStats) + sum (map (.d2dReferralEarnings) dailyStats)
      (payoutServiceFlow, payoutServiceName, mbPersonBankAccount) <- Payout.getCreatePayoutServiceFlow Payout.MerchantServiceUsageConfigOption DEMSC.PayoutService person.clientSdkVersion merchOpCity person.id
      let payoutVpaValid = case payoutServiceFlow of
            TPayout.JuspayFlow -> isJust mbVpa
            TPayout.StripeFlow -> True
      case (payoutVpaValid, pendingAmount <= payoutConfig.thresholdPayoutAmountPerPerson) of
        (True, True) -> do
          uid <- generateGUID
          Redis.withWaitOnLockRedisWithExpiry (payoutProcessingLockKey personId.getId) 3 3 $ do
            mapM_ (QDailyStats.updatePayoutStatusById DS.Processing) statsIds
            mapM_ (QDailyStats.updatePayoutOrderId (Just uid)) statsIds
          phoneNo <- mapM decrypt person.mobileNumber
          let createPayoutOrderReq = DPayment.mkCreatePayoutServiceReq uid pendingAmount transporterConfig.currency phoneNo person.email personId.getId payoutConfig.remark (Just person.firstName) mbVpa payoutConfig.orderType payoutServiceFlow Nothing
          let entityName = DPayment.BACKLOG
              createPayoutOrderCall = Payout.createPayoutOrder payoutServiceName merchOpCity person.id mbPersonBankAccount
          merchantOperatingCity <- CQMOC.findById (cast merchOpCity) >>= fromMaybeM (MerchantOperatingCityNotFound merchOpCity.getId)
          logDebug $ "calling create payoutOrder with driverId: " <> personId.getId <> " | amount: " <> show pendingAmount <> " | orderId: " <> show uid
          void $ DPayment.createPayoutService (cast person.merchantId) (Just $ cast merchOpCity) (cast personId) (Just statsIds) (Just entityName) (show merchantOperatingCity.city) createPayoutOrderReq createPayoutOrderCall Nothing
        (_, False) -> do
          Redis.withWaitOnLockRedisWithExpiry (payoutProcessingLockKey personId.getId) 3 3 $ do
            mapM_ (QDailyStats.updatePayoutStatusById DS.ManualReview) statsIds -- don't pay if amount is greater than threshold amount
        _ -> pure ()
  where
    lockKey = "ProcessBacklogPayout:DriverId-" <> personId.getId

mkSpecialZonePayoutSmsKey :: Id DPR.PayoutRequest -> DPR.PayoutRequestStatus -> Text
mkSpecialZonePayoutSmsKey payoutRequestId status =
  "SpecialZonePayoutSms:" <> payoutRequestId.getId <> ":" <> show status

sendSpecialZonePayoutSms ::
  Id DMOC.MerchantOperatingCity ->
  DPR.PayoutRequest ->
  Flow ()
sendSpecialZonePayoutSms merchantOpCityId payoutRequest = do
  case payoutRequest.amount of
    Nothing -> logInfo $ "Skipping special zone payout SMS, amount missing for payoutRequest " <> payoutRequest.id.getId
    Just amountVal ->
      when (amountVal > 0.0) $ do
        let driverId = Id payoutRequest.beneficiaryId :: Id Person.Person
        driver <- QP.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
        mbDisplayBookingId <- case payoutRequest.entityRefId of
          Nothing -> pure Nothing
          Just bookingIdTxt -> do
            mbBooking <- QRB.findById (Id bookingIdTxt)
            pure (mbBooking >>= (.displayBookingId))
        smsCfg <- asks (.smsCfg)
        mobile <- mapM decrypt driver.mobileNumber >>= fromMaybeM (PersonFieldNotPresent "mobileNumber")
        let countryCode = fromMaybe "+91" driver.mobileCountryCode
            phoneNumber = countryCode <> mobile
            sender = smsCfg.sender
        (mbSender, message, templateId, messageType) <-
          MessageBuilder.buildDriverPayoutMessage
            merchantOpCityId
            MessageBuilder.BuildDriverPayoutMessageReq
              { payoutAmount = show amountVal,
                bookingId = mbDisplayBookingId
              }
        Sms.sendSMS driver.merchantId merchantOpCityId (Sms.SendSMSReq message phoneNumber (fromMaybe sender mbSender) templateId messageType)
          >>= Sms.checkSmsResult
