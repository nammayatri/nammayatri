module SharedLogic.InvoiceEmailSchedule
  ( isInvoiceEmailEnabled,
    scheduleInvoiceEmail,
    schedulePaidInvoiceEmails,
  )
where

import qualified Domain.Types.EmailTemplate as DET
import Domain.Types.Invoice (InvoiceType (..))
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.RiderConfig as DRC
import Kernel.External.Types (SchedulerFlow)
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getConfig)
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import SharedLogic.JobScheduler
import Storage.Beam.SchedulerJob ()
import Storage.ConfigPilot.Config.RiderConfig (RiderConfigDimensions (..))

isInvoiceEmailEnabled :: DRC.RiderConfig -> Bool
isInvoiceEmailEnabled riderConfig = riderConfig.invoiceEmailEnabled == Just True

scheduleInvoiceEmail ::
  ( MonadFlow m,
    CacheFlow m r,
    EsqDBFlow m r,
    SchedulerFlow r,
    HasField "blackListedJobs" r [Text]
  ) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Id DRide.Ride ->
  DET.EmailTemplateKey ->
  Maybe Text ->
  m ()
scheduleInvoiceEmail merchantId merchantOperatingCityId rideId templateKey refundRequestId = do
  result <- withTryCatch "scheduleInvoiceEmail" $ do
    mbRiderConfig <- getConfig (RiderConfigDimensions {merchantOperatingCityId = merchantOperatingCityId.getId}) Nothing
    when (maybe False isInvoiceEmailEnabled mbRiderConfig) $ do
      let jobData = SendRideInvoiceEmailJobData {rideId, templateKey, refundRequestId}
      createJobIn @_ @'SendRideInvoiceEmail (Just merchantId) (Just merchantOperatingCityId) invoiceEmailDelay jobData
      logInfo $ "Scheduled " <> show templateKey <> " email for ride " <> rideId.getId
  whenLeft' result $ \err ->
    logError $ "Failed to schedule " <> show templateKey <> " email for ride " <> rideId.getId <> ": " <> show err
  where
    invoiceEmailDelay = 30
    whenLeft' either' action = either action (const (pure ())) either'

schedulePaidInvoiceEmails ::
  ( MonadFlow m,
    CacheFlow m r,
    EsqDBFlow m r,
    SchedulerFlow r,
    HasField "blackListedJobs" r [Text]
  ) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Id DRide.Ride ->
  [InvoiceType] ->
  m ()
schedulePaidInvoiceEmails merchantId merchantOperatingCityId rideId invoiceTypes =
  forM_ (mapMaybe toTemplateKey invoiceTypes) $ \templateKey ->
    scheduleInvoiceEmail merchantId merchantOperatingCityId rideId templateKey Nothing
  where
    toTemplateKey = \case
      Ride -> Just DET.RIDE_INVOICE
      RideCancellation -> Just DET.RIDE_CANCELLATION_INVOICE
      _ -> Nothing
