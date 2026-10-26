module SharedLogic.InvoiceEmailSchedule (scheduleRideInvoiceEmail) where

import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.Ride as DRide
import Kernel.External.Types (SchedulerFlow)
import Kernel.Prelude
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getConfig)
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import SharedLogic.JobScheduler
import Storage.Beam.SchedulerJob ()
import Storage.ConfigPilot.Config.RiderConfig (RiderConfigDimensions (..))

scheduleRideInvoiceEmail ::
  ( MonadFlow m,
    CacheFlow m r,
    EsqDBFlow m r,
    SchedulerFlow r,
    HasField "blackListedJobs" r [Text]
  ) =>
  DRB.Booking ->
  DRide.Ride ->
  m ()
scheduleRideInvoiceEmail booking ride = do
  mbRiderConfig <- getConfig (RiderConfigDimensions {merchantOperatingCityId = booking.merchantOperatingCityId.getId}) Nothing
  whenJust (mbRiderConfig >>= (.invoiceEmailConfig)) $ \cfg ->
    when (cfg.enabled && cfg.sendOnRideEnd) $ do
      let jobData = SendRideInvoiceEmailJobData {bookingId = booking.id, rideId = ride.id, attempt = 0}
      createJobIn @_ @'SendRideInvoiceEmail (Just booking.merchantId) (Just booking.merchantOperatingCityId) (fromIntegral cfg.sendDelaySeconds) jobData
      logInfo $ "Scheduled ride invoice email for ride " <> ride.id.getId
