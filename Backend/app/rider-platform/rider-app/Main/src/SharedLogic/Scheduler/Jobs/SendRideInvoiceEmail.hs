module SharedLogic.Scheduler.Jobs.SendRideInvoiceEmail (sendRideInvoiceEmailJob) where

import qualified Domain.Types.Booking.API as DBAPI
import Kernel.External.Types (SchedulerFlow)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getConfig)
import qualified Lib.Finance.Domain.Types.Invoice as FInvoice
import Lib.Scheduler
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import SharedLogic.InvoiceEmail
import SharedLogic.JobScheduler
import Storage.Beam.SchedulerJob ()
import Storage.ConfigPilot.Config.RiderConfig (RiderConfigDimensions (..))
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.Person as QPerson
import Tools.Error

sendRideInvoiceEmailJob ::
  ( InvoiceEmailFlow m r,
    SchedulerFlow r,
    HasField "blackListedJobs" r [Text]
  ) =>
  Job 'SendRideInvoiceEmail ->
  m ExecutionResult
sendRideInvoiceEmailJob Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) $ do
  let jobData = jobInfo.jobData
      rideId = jobData.rideId
  booking <- QRB.findById jobData.bookingId >>= fromMaybeM (BookingDoesNotExist jobData.bookingId.getId)
  riderConfig <- getConfig (RiderConfigDimensions {merchantOperatingCityId = booking.merchantOperatingCityId.getId}) Nothing >>= fromMaybeM (RiderConfigDoesNotExist booking.merchantOperatingCityId.getId)
  case getInvoiceEmailConfig riderConfig of
    Just cfg | cfg.sendOnRideEnd -> do
      alreadySent <- isJust <$> Hedis.get @Bool (rideEndInvoiceEmailSentKey rideId)
      if alreadySent
        then pure Complete
        else do
          let retryOrStop reason =
                if jobData.attempt < cfg.maxRetries
                  then do
                    createJobIn @_ @'SendRideInvoiceEmail (Just booking.merchantId) (Just booking.merchantOperatingCityId) (fromIntegral cfg.retryIntervalSeconds) jobData {attempt = jobData.attempt + 1}
                    logInfo $ "Ride invoice email retry scheduled for ride " <> rideId.getId <> ": " <> reason
                    pure Complete
                  else pure (Terminate reason)
          mbInvoice <- findRideInvoice rideId [FInvoice.Paid]
          case mbInvoice of
            Nothing -> retryOrStop "ride invoice not paid yet"
            Just invoice -> do
              person <- QPerson.findById booking.riderId >>= fromMaybeM (PersonDoesNotExist booking.riderId.getId)
              mbRecipient <- resolveInvoiceRecipient person booking.billingCategory
              case mbRecipient of
                Nothing -> do
                  logInfo $ "No email on file for rider of ride " <> rideId.getId <> ", skipping invoice email"
                  pure Complete
                Just recipient -> do
                  gotLock <- Hedis.tryLockRedis (rideEndInvoiceEmailLockKey rideId) 300
                  if not gotLock
                    then pure Complete
                    else do
                      result <- withTryCatch "sendRideInvoiceEmail" $ do
                        bookingApi <- DBAPI.buildBookingAPIEntity booking person.id True
                        sendRideInvoiceEmail cfg riderConfig person bookingApi invoice recipient
                      case result of
                        Right () -> do
                          Hedis.setExp (rideEndInvoiceEmailSentKey rideId) True (7 * 24 * 60 * 60)
                          pure Complete
                        Left err -> do
                          Hedis.unlockRedis (rideEndInvoiceEmailLockKey rideId)
                          logError $ "Ride invoice email failed for ride " <> rideId.getId <> ": " <> show err
                          retryOrStop "send failed"
    _ -> pure Complete
