{-# OPTIONS_GHC -Wno-deprecations #-}

module Domain.Action.UI.Payout
  ( payoutProcessingLockKey,
    castPayoutOrderStatus,
    castOrderStatus,
    isPayoutOrderSuccess,
    isPayoutStatusFailed,
    PayoutSettlementFlow,
    runRiderPayoutSettlement,
    refreshPayoutOrderWithSettlement,
  )
where

import qualified Domain.Types.FRFSTicketBooking as DFTB
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.PayoutConfig as DPayoutConfig
import qualified Domain.Types.Person as DP
import qualified Domain.Types.PersonStats as DPS
import qualified Domain.Types.VehicleCategory as DV
import Kernel.Beam.Functions as B (runInReplica)
import qualified Kernel.External.Payout.Interface.Types as IPayout
import qualified Kernel.External.Payout.Juspay.Types.Payout as Payout
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import qualified Lib.Finance.Core.Types as Finance
import Lib.Finance.Ledger.PayoutSettlement (PayoutOutcome (..))
import qualified Lib.Finance.Storage.Beam.BeamFlow as FinanceBeamFlow
import qualified Lib.Payment.Domain.Action as DPayment
import qualified Lib.Payment.Domain.Types.Common as DPayment
import qualified Lib.Payment.Domain.Types.PayoutOrder as DPayoutOrder
import qualified Lib.Payment.Payout.Request as PayoutRequest
import Lib.Payment.Payout.StatusCheck (isPayoutOrderFailed, isPayoutOrderSuccess, isPayoutStatusFailed)
import qualified Lib.Payment.Storage.Beam.BeamFlow as PaymentBeamFlow
import qualified Lib.Payment.Storage.Queries.PayoutOrder as QPayoutOrder
import qualified Lib.Payment.Storage.Queries.PayoutRequest as QPR
import qualified SharedLogic.Finance.RidePayment as RidePaymentFinance
import Storage.Beam.Payment ()
import qualified Storage.CachedQueries.Merchant.MerchantPushNotification as CPN
import Storage.ConfigPilot.Config.PayoutConfig (PayoutConfigDimensions (..))
import qualified Storage.Queries.FRFSTicketBooking as QFTB
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.PersonStats as QPersonStats
import Tools.Error
import qualified Tools.EventTracking as ET
import qualified Tools.Notifications as Notify
import qualified Tools.Payout as PayoutTools

payoutProcessingLockKey :: Text -> Text
payoutProcessingLockKey bookingId = "Payout:Processing:bookingId" <> bookingId

payoutSettlementLockKey :: Text -> Text
payoutSettlementLockKey payoutRequestId = "Payout:Settlement:payoutRequestId:" <> payoutRequestId

type PayoutSettlementFlow m r =
  ( ServiceFlow m r,
    EsqDBFlow m r,
    CacheFlow m r,
    EsqDBReplicaFlow m r,
    Finance.HasActorInfo m r,
    PaymentBeamFlow.BeamFlow m r,
    FinanceBeamFlow.BeamFlow m r
  )

castPayoutOrderStatus :: Payout.PayoutOrderStatus -> DFTB.CashbackStatus
castPayoutOrderStatus payoutOrderStatus =
  case payoutOrderStatus of
    Payout.SUCCESS -> DFTB.SUCCESSFUL
    Payout.FULFILLMENTS_SUCCESSFUL -> DFTB.SUCCESSFUL
    Payout.ERROR -> DFTB.CASHBACK_FAILED
    Payout.FAILURE -> DFTB.CASHBACK_FAILED
    Payout.FULFILLMENTS_FAILURE -> DFTB.CASHBACK_FAILED
    Payout.CANCELLED -> DFTB.MANUAL_VERIFICATION
    Payout.FULFILLMENTS_CANCELLED -> DFTB.MANUAL_VERIFICATION
    Payout.FULFILLMENTS_MANUAL_REVIEW -> DFTB.MANUAL_VERIFICATION
    _ -> DFTB.PROCESSING

castOrderStatus :: Payout.PayoutOrderStatus -> DPS.PayoutStatus
castOrderStatus payoutOrderStatus =
  case payoutOrderStatus of
    Payout.SUCCESS -> DPS.Success
    Payout.FULFILLMENTS_SUCCESSFUL -> DPS.Success
    Payout.ERROR -> DPS.Failed
    Payout.FAILURE -> DPS.Failed
    Payout.FULFILLMENTS_FAILURE -> DPS.Failed
    Payout.CANCELLED -> DPS.Failed
    Payout.FULFILLMENTS_CANCELLED -> DPS.Failed
    Payout.FULFILLMENTS_MANUAL_REVIEW -> DPS.ManualReview
    _ -> DPS.Processing

runRiderPayoutSettlement ::
  (PayoutSettlementFlow m r) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  IPayout.PayoutOrderStatus ->
  DPayoutOrder.PayoutOrder ->
  m ()
runRiderPayoutSettlement merchantId merchantOperatingCityId payoutStatus payoutOrder =
  unless (isPayoutOrderSuccess payoutOrder.status && payoutOrder.entityName /= Just DPayment.RIDE_OFFER_CASHBACK) $ do
    let personId = Id payoutOrder.customerId
    payoutConfig <-
      getOneConfig
        (PayoutConfigDimensions {merchantOperatingCityId = merchantOperatingCityId.getId, vehicleCategory = Just DV.AUTO_CATEGORY, isPayoutEnabled = Nothing, payoutEntity = Nothing})
        Nothing
        >>= fromMaybeM (PayoutConfigNotFound "AUTO_CATEGORY" merchantOperatingCityId.getId)
    personStats <- QPersonStats.findByPersonId personId >>= fromMaybeM (PersonStatsNotFound personId.getId)
    person <- B.runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
    case payoutOrder.entityName of
      Just DPayment.METRO_BOOKING_CASHBACK -> do
        forM_ (listToMaybe =<< payoutOrder.entityIds) $ \bookingId -> do
          when (isPayoutOrderSuccess payoutStatus) $
            QFTB.updatePayoutStatusById (Just $ castPayoutOrderStatus payoutStatus) (Id bookingId)
          fork "Update Payout Status and Transactions for MetroBooking" $
            callPayoutService payoutOrder payoutConfig person
      Just DPayment.REFERRAL_AWARD_RIDE -> do
        when (isPayoutOrderSuccess payoutStatus) $ do
          notifyPersonOnAmountCredit person
          QPersonStats.updateReferralAmountPaid (personStats.referralAmountPaid + payoutOrder.amount.amount) personId
        fork "Update Payout Status and Transactions for Referral Award" $
          callPayoutService payoutOrder payoutConfig person
      Just DPayment.REFERRED_BY_AWARD -> do
        when (isPayoutOrderSuccess payoutStatus) $ do
          notifyPersonOnAmountCredit person
          QPersonStats.updateReferredByEarningsPayoutStatusAndAmountPaid (Just $ castOrderStatus payoutStatus) (personStats.referralAmountPaid + payoutOrder.amount.amount) personId
        fork "Update Payout Status and Transactions for ReferredBy Award" $
          callPayoutService payoutOrder payoutConfig person
      Just DPayment.BACKLOG -> do
        when (isPayoutOrderSuccess payoutStatus) $ do
          notifyPersonOnAmountCredit person
          QPersonStats.updateBacklogStatusAndAmountPaid (Just $ castOrderStatus payoutStatus) (personStats.referralAmountPaid + payoutOrder.amount.amount) personId
        fork "Update Payout Status and Transactions for Backlog Referral Award" $
          callPayoutService payoutOrder payoutConfig person
      Just DPayment.REFERRED_BY_AND_BACKLOG_AWARD -> do
        when (isPayoutOrderSuccess payoutStatus) $ do
          let mbStatus = Just $ castOrderStatus payoutStatus
          notifyPersonOnAmountCredit person
          QPersonStats.updateBacklogAndReferredByPayoutStatusAndAmountPaid mbStatus mbStatus (personStats.referralAmountPaid + payoutOrder.amount.amount) personId
        fork "Update Payout Status and Transactions for Referred By And Backlog Award" $
          callPayoutService payoutOrder payoutConfig person
      Just DPayment.RIDE_OFFER_CASHBACK -> do
        let mbPayoutRequestId = listToMaybe (fromMaybe [] payoutOrder.entityIds)
        whenJust mbPayoutRequestId $ \prId -> Redis.withWaitAndLockMasterCloudCrossAppRedis "payout" "waitForSettlementLock" (payoutSettlementLockKey prId) 60 100 $ do
          mbPayoutReq <- QPR.findById (Id prId)
          whenJust mbPayoutReq $ \payoutReq -> do
            let ctx =
                  RidePaymentFinance.buildRiderFinanceCtx
                    merchantId.getId
                    merchantOperatingCityId.getId
                    payoutOrder.amount.currency
                    True
                    payoutOrder.customerId
                    payoutReq.id.getId
                    Nothing
                    Nothing
                    Nothing
            entryIds <- map Id <$> PayoutRequest.getPayoutLedgerEntryIds payoutReq
            if isPayoutOrderSuccess payoutStatus
              then do
                when (null entryIds) $
                  logError $ "No stashed entry IDs found for payoutRequest " <> payoutReq.id.getId
                RidePaymentFinance.settleCashbackPayoutLedger ctx payoutOrder.amount.amount entryIds PayoutSucceeded
                  >>= either (\err -> logError $ "Failed to settle cashback payout: " <> show err) (const (pure ()))
                PayoutRequest.clearPayoutLedgerEntryIds payoutReq.id.getId
                Notify.notifyRiderPayoutStatus person "OFFER_CASHBACK_COMPLETED" payoutOrder.amount.amount
                fork "event_tracking: offer_cashback_credited" $
                  ET.trackEvent merchantId merchantOperatingCityId $
                    ET.OfferCashbackCredited payoutOrder.customerId payoutReq.id.getId payoutOrder.amount.amount
              else when (isPayoutOrderFailed payoutStatus) $ do
                RidePaymentFinance.settleCashbackPayoutLedger ctx payoutOrder.amount.amount entryIds (PayoutFailed ("Payout failed: " <> show payoutStatus))
                  >>= either (\err -> logError $ "Failed to reverse cashback payout: " <> show err) (const (pure ()))
                -- TODO: remove post release, kept only for backward compatibility with payouts initiated before OwnerPayoutLiability:
                -- those reserved their accrual entries as PROCESSING, so a failed one must flip them back to UNSETTLED.
                RidePaymentFinance.releaseCashbackEntriesReservation entryIds
                PayoutRequest.clearPayoutLedgerEntryIds payoutReq.id.getId
                Notify.notifyRiderPayoutStatus person "OFFER_CASHBACK_FAILED" payoutOrder.amount.amount
        fork "Update Payout Status and Transactions for RideOfferCashback" $
          callPayoutService payoutOrder payoutConfig person
      _ -> logTagError "Webhook Handler Error" $ "Unsupported Payout Entity:" <> show payoutOrder.entityName

refreshPayoutOrderWithSettlement ::
  (PayoutSettlementFlow m r) =>
  DPayoutOrder.PayoutOrder ->
  m DPayoutOrder.PayoutOrder
refreshPayoutOrderWithSettlement payoutOrder =
  if isPayoutOrderSuccess payoutOrder.status
    then pure payoutOrder
    else case payoutOrder.entityName of
      Nothing -> pure payoutOrder
      Just _ -> do
        person <- QPerson.findById (Id payoutOrder.customerId) >>= fromMaybeM (PersonNotFound payoutOrder.customerId)
        let merchantOperatingCityId = maybe person.merchantOperatingCityId Id payoutOrder.merchantOperatingCityId
            merchantId = person.merchantId
        payoutConfig <-
          getOneConfig
            (PayoutConfigDimensions {merchantOperatingCityId = merchantOperatingCityId.getId, vehicleCategory = Just DV.AUTO_CATEGORY, isPayoutEnabled = Nothing, payoutEntity = Nothing})
            Nothing
            >>= fromMaybeM (PayoutConfigNotFound "AUTO_CATEGORY" merchantOperatingCityId.getId)
        let payoutStatusServiceReq = DPayment.PayoutStatusServiceReq {orderId = payoutOrder.orderId, mbExpand = payoutConfig.expand}
            createPayoutOrderStatusCall = PayoutTools.payoutOrderStatus person.clientSdkVersion person.merchantId person.merchantOperatingCityId (Just person.id.getId)
        void $ DPayment.payoutStatusService (cast merchantId) (cast person.id) payoutStatusServiceReq createPayoutOrderStatusCall
        refreshed <- QPayoutOrder.findByOrderId payoutOrder.orderId >>= fromMaybeM (PayoutOrderNotFound payoutOrder.orderId)
        runRiderPayoutSettlement merchantId merchantOperatingCityId refreshed.status refreshed
        QPayoutOrder.findByOrderId payoutOrder.orderId >>= \case
          Nothing -> throwError $ PayoutOrderNotFound payoutOrder.orderId
          Just finalOrder -> pure finalOrder

callPayoutService :: (PayoutSettlementFlow m r) => DPayoutOrder.PayoutOrder -> DPayoutConfig.PayoutConfig -> DP.Person -> m ()
callPayoutService payoutOrder payoutConfig person = do
  let personId = person.id
      payoutStatusServiceReq = DPayment.PayoutStatusServiceReq {orderId = payoutOrder.orderId, mbExpand = payoutConfig.expand}
      createPayoutOrderStatusCall = PayoutTools.payoutOrderStatus person.clientSdkVersion person.merchantId person.merchantOperatingCityId (Just personId.getId)
  void $ DPayment.payoutStatusService (cast person.merchantId) (cast personId) payoutStatusServiceReq createPayoutOrderStatusCall

notifyPersonOnAmountCredit :: (PayoutSettlementFlow m r) => DP.Person -> m ()
notifyPersonOnAmountCredit person = do
  let pnKey = "REFERRAL_BONUS_EARNED"
  mbMerchantPN <- CPN.findMatchingMerchantPNInRideFlow person.merchantOperatingCityId pnKey Nothing Nothing person.language []
  whenJust mbMerchantPN $ \merchantPN -> do
    let entityData = Notify.NotifReq {title = merchantPN.title, message = merchantPN.body}
    Notify.notifyPersonOnEvents person entityData merchantPN.fcmNotificationType (Just merchantPN.notificationCategory)
