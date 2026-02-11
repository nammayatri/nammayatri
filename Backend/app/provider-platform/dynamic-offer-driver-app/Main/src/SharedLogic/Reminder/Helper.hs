{-
Copyright 2022-23, Juspay India Pvt Ltd

This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Reminder.Helper
  ( createOrUpdateReminderForDocumentExpiry,
    cancelRemindersForEntity,
    cancelRemindersForDriverByDocumentType,
    cancelRemindersForRCByDocumentType,
    createReminderForDocumentType,
    recordDocumentCompletion,
    checkAndCreateReminderIfNeeded,
  )
where

import qualified Data.List as List
import Data.List.NonEmpty (nonEmpty)
import Data.Ord (comparing)
import qualified Data.Time as Time
import qualified Domain.Types.DocumentReminderHistory as DRH
import qualified Domain.Types.DocumentVerificationConfig as DVC
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Reminder as DR
import qualified Domain.Types.ReminderConfig as DRC
import qualified Domain.Types.VehicleRegistrationCertificate as DVRC
import Kernel.Beam.Lib.UtilsTH (HasSchemaName)
import Kernel.External.Encryption (EncFlow)
import Kernel.External.Types (SchedulerFlow, ServiceFlow)
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, generateGUID, getCurrentTime)
import Kernel.Utils.Logging (logInfo)
import Lib.Scheduler.JobStorageType.DB.Table (SchedulerJobT)
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import SharedLogic.Allocator (AllocatorJobType (..))
import qualified SharedLogic.Allocator as Allocator
import Storage.Beam.SchedulerJob ()
import qualified Storage.Cac.TransporterConfig as CCT
import qualified Storage.Queries.DocumentReminderHistory as QDRH
import qualified Storage.Queries.DriverRCAssociation as QDRCA
import qualified Storage.Queries.DriverRCAssociationExtra as QDRCAExtra
import qualified Storage.Queries.DriverStats as QDriverStats
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.RCStatsExtra as QRCStats
import qualified Storage.Queries.Reminder as QReminder
import qualified Storage.Queries.ReminderConfig as QReminderConfig

-- ============================================================================
-- Helper functions for common patterns
-- ============================================================================

-- | Check if reminder system is enabled and get config for a document type
-- Returns Nothing if system is disabled, person is not a driver, or config not found/disabled
getReminderConfigIfEnabled ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  Id DP.Person ->
  Id DMOC.MerchantOperatingCity ->
  DVC.DocumentType ->
  m (Maybe DRC.ReminderConfig)
getReminderConfigIfEnabled driverId merchantOpCityId documentType = do
  mbPerson <- QPerson.findById driverId
  case mbPerson of
    Just person | person.role == DP.DRIVER -> do
      mbTransporterConfig <- CCT.findByMerchantOpCityId merchantOpCityId Nothing
      case mbTransporterConfig >>= (.reminderSystemEnabled) of
        Just True -> do
          mbReminderConfig <- QReminderConfig.findByMerchantOpCityIdAndDocumentType merchantOpCityId documentType
          case mbReminderConfig of
            Just config | config.enabled -> pure $ Just config
            _ -> pure Nothing
        _ -> pure Nothing
    _ -> pure Nothing

-- | Get current ride count based on document type and entity type
getCurrentRideCount ::
  ( MonadFlow m,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    CacheFlow m r
  ) =>
  DVC.DocumentType ->
  DRH.EntityType ->
  Text ->
  Maybe (Id DP.Person) ->
  m Int
getCurrentRideCount documentType entityType entityIdText mbDriverId = case (documentType, entityType, mbDriverId) of
  (DVC.DriverInspectionForm, DRH.DRIVER, Just driverId) -> do
    (rideCount, _) <- QDriverStats.findTotalRides (cast driverId)
    pure rideCount
  (DVC.TrainingForm, DRH.DRIVER, Just driverId) -> do
    (rideCount, _) <- QDriverStats.findTotalRides (cast driverId)
    pure rideCount
  (DVC.VehicleInspectionForm, DRH.RC, _) -> do
    rcId <- Id entityIdText & pure
    QRCStats.findTotalRides rcId
  _ -> pure 0

-- | Cancel pending reminders matching a filter
cancelPendingReminders ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  [DR.Reminder] ->
  Text ->
  m ()
cancelPendingReminders reminders context = do
  forM_ reminders $ \reminder -> do
    QReminder.updateByPrimaryKey reminder {DR.status = DR.CANCELLED}
    logInfo $ "Cancelled reminder " <> reminder.id.getId <> " (" <> context <> ")"

-- | Create a single reminder record
createReminderRecord ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    CoreMetrics m
  ) =>
  DVC.DocumentType ->
  Text ->
  Id DP.Person ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  UTCTime ->
  UTCTime ->
  Int ->
  m (Id DR.Reminder)
createReminderRecord documentType entityId driverId merchantId merchantOpCityId dueDate reminderDate intervalIndex = do
  now <- getCurrentTime
  reminderId <- Id <$> generateGUID
  QReminder.create
    DR.Reminder
      { id = reminderId,
        documentType = documentType,
        entityId = entityId,
        driverId = driverId,
        merchantId = merchantId,
        merchantOperatingCityId = merchantOpCityId,
        dueDate = dueDate,
        reminderDate = reminderDate,
        currentIntervalIndex = intervalIndex,
        status = DR.PENDING,
        metadata = Nothing,
        createdAt = now,
        updatedAt = now
      }
  pure reminderId

-- | Schedule ProcessReminder job for a reminder
scheduleProcessReminderJob ::
  ( MonadFlow m,
    CacheFlow m r,
    SchedulerFlow r,
    ServiceFlow m r,
    HasField "blackListedJobs" r [Text],
    HasSchemaName SchedulerJobT
  ) =>
  Id DR.Reminder ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  NominalDiffTime ->
  m ()
scheduleProcessReminderJob reminderId merchantId merchantOpCityId scheduleAfter = do
  let jobData = Allocator.ProcessReminderJobData {reminderId = reminderId, merchantId = merchantId, merchantOperatingCityId = merchantOpCityId}
  void $ createJobIn @_ @'ProcessReminder (Just merchantId) (Just merchantOpCityId) scheduleAfter jobData

-- ============================================================================
-- Public API functions
-- ============================================================================

-- | Create all reminder entries at once when a document expiry date is captured
-- Creates separate Reminder entries for each interval (e.g., T-30, T-15, T-1)
-- This should be called whenever a document with an expiry date is created or updated
-- Only creates reminders for DRIVER role - returns early if person is not a driver
-- Schedules ProcessReminder jobs directly instead of relying on ReminderMaster
createOrUpdateReminderForDocumentExpiry ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    SchedulerFlow r,
    ServiceFlow m r,
    HasField "blackListedJobs" r [Text],
    HasSchemaName SchedulerJobT
  ) =>
  DVC.DocumentType ->
  Id DP.Person ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Text ->
  UTCTime ->
  m ()
createOrUpdateReminderForDocumentExpiry documentType driverId merchantId merchantOpCityId entityId expiryDate = do
  mbConfig <- getReminderConfigIfEnabled driverId merchantOpCityId documentType
  case mbConfig of
    Just config -> do
      -- Cancel any existing reminders for this entity/document type
      cancelRemindersForEntity entityId documentType

      -- Create reminders for ALL intervals at once
      let intervals = config.reminderIntervals
      if null intervals
        then logInfo $ "No reminder intervals configured for documentType: " <> show documentType
        else do
          now <- getCurrentTime
          -- Create a Reminder entry for each interval and schedule ProcessReminder job
          forM_ (zip intervals [0 ..]) $ \(intervalMinutes, intervalIndex) -> do
            let reminderDate = Time.addUTCTime (fromIntegral (- intervalMinutes) * 60) expiryDate
            reminderId <- createReminderRecord documentType entityId driverId merchantId merchantOpCityId expiryDate reminderDate intervalIndex
            let scheduleAfter = max 0 (Time.diffUTCTime reminderDate now)
            scheduleProcessReminderJob reminderId merchantId merchantOpCityId scheduleAfter
          logInfo $
            "Created "
              <> show (length intervals)
              <> " reminders for documentType: "
              <> show documentType
              <> ", entityId: "
              <> entityId
              <> ", expiryDate: "
              <> show expiryDate
              <> " and scheduled ProcessReminder jobs"
    Nothing -> logInfo $ "Reminder system disabled or config not found for documentType: " <> show documentType <> " in city: " <> merchantOpCityId.getId

-- | Cancel all pending reminders for a specific entity and document type
-- This is called when a document is updated to cancel old reminders
cancelRemindersForEntity ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  Text ->
  DVC.DocumentType ->
  m ()
cancelRemindersForEntity entityId documentType = do
  existingReminders <- QReminder.findAllByEntityIdAndDocumentType entityId documentType
  let pendingReminders = filter ((== DR.PENDING) . (.status)) existingReminders
  cancelPendingReminders pendingReminders $ "entity " <> entityId

-- | Cancel all pending reminders for a driver by document type
-- This is called when inspection/training is completed to cancel pending reminders
cancelRemindersForDriverByDocumentType ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  Id DP.Person ->
  DVC.DocumentType ->
  m ()
cancelRemindersForDriverByDocumentType driverId documentType = do
  allPendingReminders <- QReminder.findAllPendingByDriverId driverId DR.PENDING
  let matchingReminders = filter ((== documentType) . (.documentType)) allPendingReminders
  cancelPendingReminders matchingReminders $ "driver " <> driverId.getId <> " (documentType: " <> show documentType <> ")"

-- | Cancel all pending reminders for all drivers using a specific RC by document type
-- This is called when a vehicle inspection is completed to cancel reminders for all drivers using that RC
cancelRemindersForRCByDocumentType ::
  ( MonadFlow m,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    CacheFlow m r
  ) =>
  Id DVRC.VehicleRegistrationCertificate ->
  DVC.DocumentType ->
  m ()
cancelRemindersForRCByDocumentType rcId documentType = do
  -- Find all active driver associations for this RC
  rcAssociations <- QDRCAExtra.findAllActiveAssociationByRCId rcId
  -- Cancel reminders for each driver using this RC
  forM_ rcAssociations $ \assoc -> do
    allPendingReminders <- QReminder.findAllPendingByDriverId assoc.driverId DR.PENDING
    let matchingReminders = filter ((== documentType) . (.documentType)) allPendingReminders
    cancelPendingReminders matchingReminders $ "driver " <> assoc.driverId.getId <> " (RC: " <> rcId.getId <> ", documentType: " <> show documentType <> ")"

-- | Determine if a document type is RC-based (vehicle-level) or driver-based
isRCDocumentType :: DVC.DocumentType -> Bool
isRCDocumentType DVC.VehicleInspectionForm = True
isRCDocumentType _ = False

-- | Create a reminder for any document type (for manual trigger or auto-trigger)
-- This creates a reminder that will be sent immediately or based on reminderIntervals
-- If intervals are provided, use them; otherwise fallback to config
-- Schedules ProcessReminder jobs directly instead of relying on ReminderMaster
createReminderForDocumentType ::
  ( MonadFlow m,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    CacheFlow m r,
    SchedulerFlow r,
    ServiceFlow m r,
    EncFlow m r,
    CoreMetrics m,
    HasField "blackListedJobs" r [Text]
  ) =>
  DVC.DocumentType ->
  Id DP.Person ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Maybe UTCTime ->
  Maybe [Int] ->
  m ()
createReminderForDocumentType documentType driverId merchantId merchantOpCityId mbDueDate mbIntervals = do
  mbConfig <- getReminderConfigIfEnabled driverId merchantOpCityId documentType
  case mbConfig of
    Just config -> do
      cancelRemindersForDriverByDocumentType driverId documentType

      now <- getCurrentTime
      let dueDate = fromMaybe now mbDueDate
          -- Use provided intervals or fallback to config
          intervals = fromMaybe config.reminderIntervals mbIntervals
          entityId = driverId.getId

      if null intervals
        then do
          reminderId <- createReminderRecord documentType entityId driverId merchantId merchantOpCityId dueDate now 0
          scheduleProcessReminderJob reminderId merchantId merchantOpCityId 0
          logInfo $ "Created immediate reminder for " <> show documentType <> " for driver " <> driverId.getId <> " and scheduled ProcessReminder job"
        else do
          forM_ (zip intervals [0 ..]) $ \(intervalMinutes, intervalIndex) -> do
            let reminderDate = Time.addUTCTime (fromIntegral (- intervalMinutes) * 60) dueDate
            reminderId <- createReminderRecord documentType entityId driverId merchantId merchantOpCityId dueDate reminderDate intervalIndex
            let scheduleAfter = max 0 (Time.diffUTCTime reminderDate now)
            scheduleProcessReminderJob reminderId merchantId merchantOpCityId scheduleAfter
          logInfo $ "Created " <> show (length intervals) <> " reminders for " <> show documentType <> " for driver " <> driverId.getId <> " and scheduled ProcessReminder jobs"
    Nothing -> logInfo $ "Reminder system disabled or config not found for " <> show documentType <> " in city: " <> merchantOpCityId.getId

-- | Record document completion - stores completion date and ride count
-- This should be called when a document/inspection/training is approved/completed
-- Always creates a new history record (stores all completion histories, not just latest)
-- Works for both driver-level (DRIVER_INSPECTION, TRAINING_VIDEO) and RC-level (VEHICLE_INSPECTION) documents
-- If daysThreshold is configured, proactively creates a reminder scheduled for completionDate + daysThreshold
--
-- This function accepts DocumentType for document expiry reminders, but for reminder-specific types
-- (VehicleInspection, DriverInspection, TrainingVideo), use recordReminderCompletion instead
recordDocumentCompletion ::
  ( MonadFlow m,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    CacheFlow m r,
    SchedulerFlow r,
    ServiceFlow m r,
    EncFlow m r,
    CoreMetrics m,
    HasField "blackListedJobs" r [Text]
  ) =>
  DVC.DocumentType ->
  Text -> -- entityId (driverId or rcId as Text)
  DRH.EntityType -> -- entityType (DRIVER or RC)
  Maybe (Id DP.Person) -> -- driverId for ride count calculation (if needed)
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  m ()
recordDocumentCompletion documentType entityIdText entityType mbDriverId merchantId merchantOpCityId = do
  now <- getCurrentTime
  -- Get current ride count based on document type and entity type
  currentRideCount <- getCurrentRideCount documentType entityType entityIdText mbDriverId

  -- Always create a new history record (store all completions)
  historyId <- Id <$> generateGUID
  QDRH.create
    DRH.DocumentReminderHistory
      { id = historyId,
        documentType = documentType,
        entityId = entityIdText,
        entityType = entityType,
        merchantId = merchantId,
        merchantOperatingCityId = merchantOpCityId,
        completionDate = now,
        rideCountAtCompletion = currentRideCount,
        createdAt = now,
        updatedAt = now
      }
  logInfo $ "Recorded completion history for " <> show documentType <> " (entity: " <> entityIdText <> ", completionDate: " <> show now <> ")"

  -- Proactively create reminder if daysThreshold is configured
  -- This ensures reminders are created on time even if driver doesn't take rides
  mbDriverIdForReminder <- case entityType of
    DRH.DRIVER -> pure $ Just (Id entityIdText)
    DRH.RC -> do
      -- For RC-level documents, find the driver associated with this RC
      rcId <- Id entityIdText & pure
      rcAssocs <- QDRCAExtra.findAllActiveAssociationByRCId rcId
      case rcAssocs of
        [] -> do
          logInfo $ "No active driver association found for RC " <> entityIdText <> ", skipping proactive reminder creation"
          pure Nothing
        (rcAssoc : _) -> pure $ Just rcAssoc.driverId

  case mbDriverIdForReminder of
    Just driverId -> do
      mbConfig <- getReminderConfigIfEnabled driverId merchantOpCityId documentType
      case mbConfig of
        Just config -> case config.daysThreshold of
          Just daysThreshold -> do
            -- Calculate the reminder date: completionDate + daysThreshold
            let thresholdDate = Time.addUTCTime (fromIntegral daysThreshold * 24 * 60 * 60) now
            logInfo $
              "Creating proactive reminder for "
                <> show documentType
                <> " (driver: "
                <> driverId.getId
                <> ", entity: "
                <> entityIdText
                <> ", thresholdDate: "
                <> show thresholdDate
                <> ", daysThreshold: "
                <> show daysThreshold
                <> ")"
            -- Create reminder scheduled for the threshold date
            -- This will automatically schedule ProcessReminder job for that date
            createReminderForDocumentType documentType driverId merchantId merchantOpCityId (Just thresholdDate) Nothing
          Nothing -> pure () -- No daysThreshold configured, skip proactive reminder
        Nothing -> pure () -- Reminder system disabled or config not found
    Nothing -> pure () -- Could not determine driverId, skip proactive reminder

-- | Check if reminder should be created based on thresholds (days or rides) for any document type
-- This should be called after ride completion to check if thresholds are met
-- Works for both driver-level and RC-level documents
checkAndCreateReminderIfNeeded ::
  ( MonadFlow m,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    CacheFlow m r,
    SchedulerFlow r,
    ServiceFlow m r,
    EncFlow m r,
    CoreMetrics m,
    HasField "blackListedJobs" r [Text]
  ) =>
  DVC.DocumentType ->
  Id DP.Person ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  m ()
checkAndCreateReminderIfNeeded documentType driverId merchantId merchantOpCityId = do
  mbConfig <- getReminderConfigIfEnabled driverId merchantOpCityId documentType
  case mbConfig of
    Just config -> do
      -- Check if thresholds are configured
      when (isJust config.daysThreshold || isJust config.ridesThreshold) $ do
        -- Determine entity based on document type
        mbEntityId <-
          if isRCDocumentType documentType
            then do
              -- For RC-level documents, find driver's active RC
              mbRCAssoc <- QDRCA.findActiveAssociationByDriver driverId True
              pure $ (\rcAssoc -> (rcAssoc.rcId.getId, DRH.RC)) <$> mbRCAssoc
            else do
              -- For driver-level documents, use driver ID
              pure $ Just (driverId.getId, DRH.DRIVER)

        case mbEntityId of
          Just (entityIdText, entityType) -> do
            -- Get all completion histories and find the latest one
            allHistories <- QDRH.findAllByDocumentTypeAndEntity documentType entityIdText entityType
            let mbHistory = List.maximumBy (comparing (.completionDate)) <$> nonEmpty allHistories
            case mbHistory of
              Just history -> do
                now <- getCurrentTime
                -- Get current ride count for rides threshold check
                currentRideCount <- getCurrentRideCount documentType entityType entityIdText (Just driverId)

                -- Check days threshold
                let daysSinceCompletion = Time.diffUTCTime now history.completionDate / (24 * 60 * 60)
                    daysThresholdMet = case config.daysThreshold of
                      Just threshold -> daysSinceCompletion >= fromIntegral threshold
                      Nothing -> False

                -- Check rides threshold
                let ridesSinceCompletion = currentRideCount - history.rideCountAtCompletion
                    ridesThresholdMet = case config.ridesThreshold of
                      Just threshold -> ridesSinceCompletion >= threshold
                      Nothing -> False

                -- Create reminder if either threshold is met
                when (daysThresholdMet || ridesThresholdMet) $ do
                  logInfo $
                    "Threshold met for "
                      <> show documentType
                      <> " (entity: "
                      <> entityIdText
                      <> ", driver: "
                      <> driverId.getId
                      <> ", days: "
                      <> show (floor daysSinceCompletion :: Int)
                      <> ", rides: "
                      <> show ridesSinceCompletion
                      <> ")"
                  -- Create reminder with immediate due date (now)
                  -- The reminder system will keep sending reminders every 24 hours until inspection is completed
                  createReminderForDocumentType documentType driverId merchantId merchantOpCityId (Just now) Nothing
              Nothing -> logInfo $ "No completion history found for " <> show documentType <> " (entity: " <> entityIdText <> "), skipping threshold check"
          Nothing -> logInfo $ "No active entity found for driver " <> driverId.getId <> " and document type " <> show documentType <> ", skipping threshold check"
    Nothing -> pure () -- Reminder system disabled or config not found
