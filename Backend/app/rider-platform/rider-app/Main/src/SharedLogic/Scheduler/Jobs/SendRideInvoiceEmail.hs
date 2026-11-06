module SharedLogic.Scheduler.Jobs.SendRideInvoiceEmail (sendRideInvoiceEmailJob) where

import qualified Domain.Types.Booking.API as DBAPI
import Kernel.External.Types (Language (ENGLISH))
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getConfig)
import qualified Lib.Finance.Domain.Types.Invoice as FInvoice
import Lib.Scheduler
import SharedLogic.InvoiceEmail
import SharedLogic.InvoiceEmailSchedule (isInvoiceEmailEnabled)
import SharedLogic.JobScheduler
import Storage.Beam.SchedulerJob ()
import Storage.ConfigPilot.Config.RiderConfig (RiderConfigDimensions (..))
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QRide
import Tools.Error

sendRideInvoiceEmailJob ::
  InvoiceEmailFlow m r =>
  Job 'SendRideInvoiceEmail ->
  m ExecutionResult
sendRideInvoiceEmailJob Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) $ do
  let SendRideInvoiceEmailJobData {rideId, templateKey, refundRequestId} = jobInfo.jobData
      sentKey = invoiceEmailSentKey templateKey rideId refundRequestId
      lockKey = invoiceEmailLockKey templateKey rideId refundRequestId
  ride <- QRide.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  booking <- QRB.findById ride.bookingId >>= fromMaybeM (BookingDoesNotExist ride.bookingId.getId)
  riderConfig <- getConfig (RiderConfigDimensions {merchantOperatingCityId = booking.merchantOperatingCityId.getId}) Nothing >>= fromMaybeM (RiderConfigDoesNotExist booking.merchantOperatingCityId.getId)
  alreadySent <- isJust <$> Hedis.get @Bool sentKey
  if not (isInvoiceEmailEnabled riderConfig) || alreadySent
    then pure Complete
    else do
      mbInvoice <- findInvoiceForEmail templateKey rideId refundRequestId [FInvoice.Paid]
      person <- QPerson.findById booking.riderId >>= fromMaybeM (PersonDoesNotExist booking.riderId.getId)
      mbRecipient <- resolveInvoiceRecipient person booking.billingCategory
      mbTemplate <- findEmailTemplate booking.merchantOperatingCityId templateKey (fromMaybe ENGLISH person.language)
      case (mbInvoice, mbRecipient, mbTemplate) of
        (Nothing, _, _) -> do
          logInfo $ "No Paid invoice for " <> show templateKey <> " email on ride " <> rideId.getId
          pure Complete
        (_, Nothing, _) -> do
          logInfo $ "No email on file for rider of ride " <> rideId.getId <> ", skipping invoice email"
          pure Complete
        (_, _, Nothing) -> do
          logError $ "No " <> show templateKey <> " email template for city " <> booking.merchantOperatingCityId.getId
          pure $ Terminate "email template not found"
        (Just invoice, Just recipient, Just template) -> do
          gotLock <- Hedis.tryLockRedis lockKey 300
          if not gotLock
            then pure Complete
            else do
              result <- withTryCatch "sendInvoiceEmail" $ do
                bookingApi <- DBAPI.buildBookingAPIEntity booking person.id True
                sendInvoiceEmail template riderConfig person bookingApi invoice recipient
              case result of
                Right () -> do
                  Hedis.setExp sentKey True (7 * 24 * 60 * 60)
                  pure Complete
                Left err -> do
                  Hedis.unlockRedis lockKey
                  logError $ "Invoice email failed for ride " <> rideId.getId <> ": " <> show err
                  pure Retry
