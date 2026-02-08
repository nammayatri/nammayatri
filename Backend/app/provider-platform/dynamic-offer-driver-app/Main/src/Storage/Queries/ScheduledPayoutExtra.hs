module Storage.Queries.ScheduledPayoutExtra where

import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.PayoutStatusHistory as DPSH
import qualified Domain.Types.ScheduledPayout as DSP
import Kernel.Beam.Functions (updateOneWithKV)
import Kernel.External.Encryption (decrypt)
import qualified Kernel.External.Payout.Juspay.Types.Payout as Payout
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import Kernel.Sms.Config (SmsConfig)
import qualified Kernel.Storage.Hedis.Queries as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified SharedLogic.MessageBuilder as MessageBuilder
import qualified Storage.Beam.ScheduledPayout as Beam
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.PayoutStatusHistory as QPSH
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.ScheduledPayout as QSP
import Tools.Error
import qualified Tools.SMS as SMS

-- | Update ScheduledPayout status and record history entry in one call
-- This centralizes all status updates to also track history
updateStatusWithHistoryById ::
  (ServiceFlow m r, HasFlowEnv m r '["smsCfg" ::: SmsConfig]) =>
  DSP.ScheduledPayoutStatus ->
  Maybe Text ->
  DSP.ScheduledPayout ->
  m ()
updateStatusWithHistoryById newStatus message scheduledPayout = do
  now <- getCurrentTime

  -- 1. Update the ScheduledPayout status
  QSP.updateStatusById newStatus scheduledPayout.id

  -- 2. Create history entry
  historyId <- Id <$> generateGUID
  let historyEntry =
        DPSH.PayoutStatusHistory
          { id = historyId,
            scheduledPayoutId = scheduledPayout.id,
            status = newStatus,
            message = message,
            createdAt = now,
            updatedAt = now,
            merchantId = scheduledPayout.merchantId,
            merchantOperatingCityId = scheduledPayout.merchantOperatingCityId
          }
  QPSH.create historyEntry
  when (newStatus `elem` [DSP.CREDITED, DSP.CASH_PAID]) $ do
    let redisKey = mkPayoutNotificationKey scheduledPayout.id newStatus
    alreadySent <- Hedis.get redisKey
    case (alreadySent :: Maybe Bool) of
      Just True -> pure ()
      _ -> do
        Hedis.setExp redisKey True (60 * 60 * 24) -- 24 hours TTL
        fork "Send Payout Notification to Driver" $ do
          case (scheduledPayout.merchantId, scheduledPayout.merchantOperatingCityId) of
            (Just mId, Just mocId) -> do
              whenJust scheduledPayout.amount $ \amountVal -> do
                when (amountVal > 0.0) $ sendDriverPayoutSms mId mocId scheduledPayout amountVal
            _ -> pure ()

mkPayoutNotificationKey :: Id DSP.ScheduledPayout -> DSP.ScheduledPayoutStatus -> Text
mkPayoutNotificationKey payoutId status =
  "PayoutNotification:" <> getId payoutId <> ":" <> show status

sendDriverPayoutSms ::
  (ServiceFlow m r, HasFlowEnv m r '["smsCfg" ::: SmsConfig]) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  DSP.ScheduledPayout ->
  HighPrecMoney ->
  m ()
sendDriverPayoutSms mId mocId scheduledPayout amount = do
  let driverId = Id scheduledPayout.driverId
  driver <- QP.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  booking <- QBooking.findById (Id scheduledPayout.bookingId) >>= fromMaybeM (BookingNotFound scheduledPayout.bookingId)
  smsCfg <- asks (.smsCfg)
  mobile <- mapM decrypt driver.mobileNumber >>= fromMaybeM (PersonFieldNotPresent "mobileNumber")
  let countryCode = fromMaybe "+91" driver.mobileCountryCode
      phoneNumber = countryCode <> mobile
      sender = smsCfg.sender
      amountText = show amount
  (mbSender, message, templateId) <-
    MessageBuilder.buildDriverPayoutMessage
      mocId
      (MessageBuilder.BuildDriverPayoutMessageReq {payoutAmount = amountText, bookingId = booking.displayBookingId})
  SMS.sendSMS mId mocId (SMS.SendSMSReq message phoneNumber (fromMaybe sender mbSender) templateId) >>= SMS.checkSmsResult

-- | Create initial history entry when ScheduledPayout is first created
createInitialHistory ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  DSP.ScheduledPayout ->
  m ()
createInitialHistory scheduledPayout = do
  historyId <- Id <$> generateGUID
  let historyEntry =
        DPSH.PayoutStatusHistory
          { id = historyId,
            scheduledPayoutId = scheduledPayout.id,
            status = scheduledPayout.status,
            message = Just (getStatusMessage scheduledPayout.status),
            createdAt = scheduledPayout.createdAt,
            updatedAt = scheduledPayout.createdAt,
            merchantId = scheduledPayout.merchantId,
            merchantOperatingCityId = scheduledPayout.merchantOperatingCityId
          }
  QPSH.create historyEntry

-- | Status message helpers for consistent history messages
getStatusMessage :: DSP.ScheduledPayoutStatus -> Text
getStatusMessage DSP.INITIATED = "Payout scheduled"
getStatusMessage DSP.PROCESSING = "Payment in progress"
getStatusMessage DSP.CREDITED = "Payment credited to bank"
getStatusMessage DSP.AUTO_PAY_FAILED = "Auto-pay failed"
getStatusMessage DSP.CANCELLED = "Payment cancelled"
getStatusMessage DSP.CASH_PAID = "Payment marked as cash paid"
getStatusMessage DSP.CASH_PENDING = "Payment marked as cash pending"
getStatusMessage DSP.RETRYING = "Retrying payment..."
getStatusMessage DSP.FAILED = "Payment failed/cancelled"

-- | Cast Juspay payout order status to ScheduledPayoutStatus for Special Zone Payouts
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
    Payout.FULFILLMENTS_MANUAL_REVIEW -> DSP.PROCESSING -- Keep processing for manual review
    _ -> DSP.PROCESSING

-- | Update the payout transaction ID after receiving it from the payout service
updatePayoutTransactionId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Maybe Text ->
  Id DSP.ScheduledPayout ->
  m ()
updatePayoutTransactionId txnId scheduledPayoutId = do
  now <- getCurrentTime
  updateOneWithKV
    [Se.Set Beam.payoutTransactionId txnId, Se.Set Beam.updatedAt now]
    [Se.Is Beam.id $ Se.Eq (getId scheduledPayoutId)]
