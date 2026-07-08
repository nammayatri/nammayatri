{-# OPTIONS_GHC -Wno-deprecations #-}

module Domain.Action.UI.Payout
  ( payoutProcessingLockKey,
    castPayoutOrderStatus,
    castOrderStatus,
    isPayoutOrderSuccess,
    isPayoutStatusFailed,
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
import Environment
import Kernel.Beam.Functions as B (runInReplica)
import qualified Kernel.External.Payout.Interface.Types as IPayout
import qualified Kernel.External.Payout.Juspay.Types.Payout as Payout
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Payment.Domain.Action as DPayment
import qualified Lib.Payment.Domain.Types.Common as DPayment
import qualified Lib.Payment.Domain.Types.PayoutOrder as DPayoutOrder
import qualified Lib.Payment.Storage.Queries.PayoutOrder as QPayoutOrder
import qualified Lib.Payment.Storage.Queries.PayoutRequest as QPR
import qualified SharedLogic.Finance.RidePayment as RidePaymentFinance
import Storage.Beam.Payment ()
import qualified Storage.CachedQueries.Merchant.MerchantPushNotification as CPN
import Storage.ConfigPilot.Config.PayoutConfig (PayoutDimensions (..))
import Storage.ConfigPilot.Interface.Types (getOneConfig)
import qualified Storage.Queries.FRFSTicketBooking as QFTB
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.PersonStats as QPersonStats
import Tools.Error
import qualified Tools.Notifications as Notify
import qualified Tools.Payout as PayoutTools

payoutProcessingLockKey :: Text -> Text
payoutProcessingLockKey bookingId = "Payout:Processing:bookingId" <> bookingId

isPayoutOrderSuccess :: IPayout.PayoutOrderStatus -> Bool
isPayoutOrderSuccess status = status `elem` [Payout.SUCCESS, Payout.FULFILLMENTS_SUCCESSFUL]

isPayoutStatusFailed :: IPayout.PayoutOrderStatus -> Bool
isPayoutStatusFailed status = status `elem` [Payout.FAILURE, Payout.FULFILLMENTS_FAILURE, Payout.FULFILLMENTS_CANCELLED]

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
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  IPayout.PayoutOrderStatus ->
  DPayoutOrder.PayoutOrder ->
  Flow ()
runRiderPayoutSettlement merchantId merchantOperatingCityId payoutStatus payoutOrder =
  unless (isPayoutOrderSuccess payoutOrder.status) $ do
    let personId = Id payoutOrder.customerId
    payoutConfig <-
      getOneConfig
        (PayoutDimensions {merchantOperatingCityId = merchantOperatingCityId.getId, vehicleCategory = Just DV.AUTO_CATEGORY, isPayoutEnabled = Nothing, payoutEntity = Nothing})
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
        if isPayoutOrderSuccess payoutStatus
          then whenJust mbPayoutRequestId $ \prId -> do
            mbPayoutReq <- QPR.findById (Id prId)
            whenJust mbPayoutReq $ \payoutReq -> do
              let entryIds = map Id (fromMaybe [] payoutReq.ledgerEntryIds)
              when (null entryIds) $
                logError $ "No stashed entry IDs found for payoutRequest " <> payoutReq.id.getId
              let ctx =
                    RidePaymentFinance.buildRiderFinanceCtx
                      merchantId.getId
                      merchantOperatingCityId.getId
                      payoutOrder.amount.currency
                      True
                      payoutOrder.customerId
                      ""
                      Nothing
                      Nothing
                      Nothing
              void $ RidePaymentFinance.markCashbackEntriesAsPaidOut ctx entryIds payoutOrder.amount.amount payoutReq.id.getId
          else when (isPayoutStatusFailed payoutStatus) $
            whenJust mbPayoutRequestId $ \prId -> do
              mbPayoutReq <- QPR.findById (Id prId)
              whenJust mbPayoutReq $ \payoutReq -> do
                let entryIds = map Id (fromMaybe [] payoutReq.ledgerEntryIds)
                RidePaymentFinance.releaseCashbackEntriesReservation entryIds
                logInfo $ "Released cashback reservation after webhook failure for payoutRequest " <> payoutReq.id.getId
        fork "Update Payout Status and Transactions for RideOfferCashback" $
          callPayoutService payoutOrder payoutConfig person
      _ -> logTagError "Webhook Handler Error" $ "Unsupported Payout Entity:" <> show payoutOrder.entityName

refreshPayoutOrderWithSettlement ::
  DPayoutOrder.PayoutOrder ->
  Flow DPayoutOrder.PayoutOrder
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
            (PayoutDimensions {merchantOperatingCityId = merchantOperatingCityId.getId, vehicleCategory = Just DV.AUTO_CATEGORY, isPayoutEnabled = Nothing, payoutEntity = Nothing})
            >>= fromMaybeM (PayoutConfigNotFound "AUTO_CATEGORY" merchantOperatingCityId.getId)
        let payoutStatusServiceReq = DPayment.PayoutStatusServiceReq {orderId = payoutOrder.orderId, mbExpand = payoutConfig.expand}
            createPayoutOrderStatusCall = PayoutTools.payoutOrderStatus person.clientSdkVersion person.merchantId person.merchantOperatingCityId (Just person.id.getId)
        void $ DPayment.payoutStatusService (cast merchantId) (cast person.id) payoutStatusServiceReq createPayoutOrderStatusCall
        refreshed <- QPayoutOrder.findByOrderId payoutOrder.orderId >>= fromMaybeM (PayoutOrderNotFound payoutOrder.orderId)
        runRiderPayoutSettlement merchantId merchantOperatingCityId refreshed.status refreshed
        QPayoutOrder.findByOrderId payoutOrder.orderId >>= fromMaybeM (PayoutOrderNotFound payoutOrder.orderId)

callPayoutService :: DPayoutOrder.PayoutOrder -> DPayoutConfig.PayoutConfig -> DP.Person -> Flow ()
callPayoutService payoutOrder payoutConfig person = do
  let personId = person.id
      payoutStatusServiceReq = DPayment.PayoutStatusServiceReq {orderId = payoutOrder.orderId, mbExpand = payoutConfig.expand}
      createPayoutOrderStatusCall = PayoutTools.payoutOrderStatus person.clientSdkVersion person.merchantId person.merchantOperatingCityId (Just personId.getId)
  void $ DPayment.payoutStatusService (cast person.merchantId) (cast personId) payoutStatusServiceReq createPayoutOrderStatusCall

notifyPersonOnAmountCredit :: DP.Person -> Flow ()
notifyPersonOnAmountCredit person = do
  let pnKey = "REFERRAL_BONUS_EARNED"
  mbMerchantPN <- CPN.findMatchingMerchantPNInRideFlow person.merchantOperatingCityId pnKey Nothing Nothing person.language []
  whenJust mbMerchantPN $ \merchantPN -> do
    let entityData = Notify.NotifReq {title = merchantPN.title, message = merchantPN.body}
    Notify.notifyPersonOnEvents person entityData merchantPN.fcmNotificationType
