{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Allocator.Jobs.Reminder.ProcessReminder
  ( processReminder,
  )
where

import Control.Applicative (liftA2)
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Data.Time as Time
import qualified Data.Tuple.Extra as TE
import qualified Domain.Action.UI.DriverOnboarding.VehicleRegistrationCertificate as DomainRC
import qualified Domain.Types.BusinessLicense as DBL
import qualified Domain.Types.DocumentVerificationConfig as DVC
import qualified Domain.Types.DriverInformation as DI
import qualified Domain.Types.DriverLicense as DDL
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantMessage as DMM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Reminder as DR
import qualified Domain.Types.ReminderConfig as DRC
import qualified Domain.Types.TransporterConfig as DTC
import qualified Domain.Types.VehicleFitnessCertificate as DFC
import qualified Domain.Types.VehicleInsurance as DVI
import qualified Domain.Types.VehiclePUC as DPUC
import qualified Domain.Types.VehiclePermit as DVPermit
import qualified Domain.Types.VehicleRegistrationCertificate as DVRC
import Kernel.Beam.Functions as BF
import Kernel.Beam.Lib.UtilsTH (HasSchemaName)
import Kernel.External.Encryption (decrypt)
import qualified Kernel.External.Notification as Notification
import Kernel.External.Types (SchedulerFlow, ServiceFlow)
import Kernel.Prelude
import Kernel.Sms.Config (SmsConfig)
import qualified Kernel.Storage.Clickhouse.Config as CH
import Kernel.Storage.Esqueleto.Config
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import qualified Kernel.Types.Documents as Documents
import Kernel.Types.Error (PersonError (PersonDoesNotExist))
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Scheduler
import Lib.Scheduler.JobStorageType.DB.Table (SchedulerJobT)
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import SharedLogic.Allocator (AllocatorJobType (..))
import qualified SharedLogic.Allocator as Allocator
import qualified SharedLogic.Analytics as Analytics
import qualified SharedLogic.DriverOnboarding.Status as DriverOnboardingStatus (ResponseStatus (..), checkLMSTrainingStatus)
import qualified SharedLogic.Reminder.Helper as ReminderHelper
import Storage.Beam.SchedulerJob ()
import qualified Storage.Cac.TransporterConfig as CTC
import qualified Storage.CachedQueries.Merchant.MerchantMessage as CMM
import qualified Storage.CachedQueries.Merchant.MerchantPushNotification as CPN
import qualified Storage.Queries.BusinessLicense as QBL
import qualified Storage.Queries.DriverInformation as QDriverInfo
import qualified Storage.Queries.DriverInformationExtra as QDIExtra
import qualified Storage.Queries.DriverLicense as QDL
import qualified Storage.Queries.FleetDriverAssociationExtra as QFDA
import qualified Storage.Queries.Image as QImage
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Reminder as QReminder
import qualified Storage.Queries.ReminderConfig as QReminderConfig
import qualified Storage.Queries.VehicleFitnessCertificate as QFC
import qualified Storage.Queries.VehicleInsurance as QVI
import qualified Storage.Queries.VehiclePUC as QVPUC
import qualified Storage.Queries.VehiclePermit as QVPermit
import qualified Storage.Queries.VehicleRegistrationCertificate as QVRC
import Tools.Error (DriverOnboardingError (ImageNotValid), GenericError (InternalError))
import Tools.Notifications (NotifReq (..), notifyDriverOnEvents, notifyFleetWithGRPCProvider, notifyWithGRPCProvider)
import qualified Tools.SMS as Sms

-- Document types that have expiry dates and are processed by processDocumentExpiryReminder
documentExpiryTypes :: [DVC.DocumentType]
documentExpiryTypes =
  [ DVC.DriverLicense,
    DVC.VehicleRegistrationCertificate,
    DVC.VehicleInsurance,
    DVC.VehiclePermit,
    DVC.VehiclePUC,
    DVC.VehicleFitnessCertificate,
    DVC.BusinessLicense
  ]

-- Vehicle-related document types: invalidation removes vehicle (via RC), driver enablement is separate
vehicleRelatedDocumentTypes :: [DVC.DocumentType]
vehicleRelatedDocumentTypes =
  [ DVC.VehicleRegistrationCertificate,
    DVC.VehicleInsurance,
    DVC.VehiclePermit,
    DVC.VehiclePUC,
    DVC.VehicleFitnessCertificate
  ]

defaultRescheduleIntervalSeconds :: Int
defaultRescheduleIntervalSeconds = 24 * 60 * 60

defaultOnRideRescheduleIntervalSeconds :: Int
defaultOnRideRescheduleIntervalSeconds = 2 * 60 * 60

scheduleReminderJob ::
  ( CoreMetrics m,
    EsqDBReplicaFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    Redis.HedisFlow m r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv,
    SchedulerFlow r,
    EncFlow m r,
    HasFlowEnv m r '["smsCfg" ::: SmsConfig, "maxNotificationShards" ::: Int],
    ServiceFlow m r,
    HasField "blackListedJobs" r [Text],
    HasSchemaName SchedulerJobT
  ) =>
  Time.NominalDiffTime ->
  Id DR.Reminder ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  m ()
scheduleReminderJob scheduleAfter reminderId merchantId merchantOpCityId = do
  let jobData = Allocator.ProcessReminderJobData {reminderId, merchantId, merchantOperatingCityId = merchantOpCityId}
  void $ createJobIn @_ @'ProcessReminder (Just merchantId) (Just merchantOpCityId) scheduleAfter jobData

-- | Disable driver and update fleet/operator analytics. Used when a mandatory reminder has expired.
disableDriverForMandatoryReminder ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv
  ) =>
  DI.DriverInformation ->
  DTC.TransporterConfig ->
  Id DP.Person ->
  Text ->
  m ()
disableDriverForMandatoryReminder driverInfo transporterConfig driverId reason = do
  Analytics.updateEnabledVerifiedStateWithAnalytics (Just driverInfo) transporterConfig driverId False Nothing
  logInfo $ "Disabled driver " <> driverId.getId <> " due to " <> reason

processReminder ::
  ( CoreMetrics m,
    EsqDBReplicaFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    Redis.HedisFlow m r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv,
    SchedulerFlow r,
    EncFlow m r,
    HasFlowEnv m r '["smsCfg" ::: SmsConfig, "maxNotificationShards" ::: Int],
    ServiceFlow m r,
    HasField "blackListedJobs" r [Text],
    HasSchemaName SchedulerJobT
  ) =>
  Job 'ProcessReminder ->
  m ExecutionResult
processReminder Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) do
  let jobData = jobInfo.jobData
      reminderId = jobData.reminderId
      merchantId = jobData.merchantId
      merchantOpCityId = jobData.merchantOperatingCityId

  reminder <- QReminder.findByPrimaryKey reminderId >>= fromMaybeM (InternalError $ "Reminder not found: " <> reminderId.getId)

  if reminder.status /= DR.PENDING
    then do
      logInfo $ "Reminder " <> reminderId.getId <> " is not pending (status: " <> show reminder.status <> "), skipping"
      return Complete
    else do
      driver <- BF.runInReplica $ QPerson.findById reminder.driverId >>= fromMaybeM (PersonDoesNotExist reminder.driverId.getId)

      logInfo $ "Processing reminder " <> reminderId.getId <> " of type " <> show reminder.documentType <> " for driver " <> reminder.driverId.getId

      mbReminderConfig <- QReminderConfig.findByMerchantOpCityIdAndDocumentType merchantOpCityId reminder.documentType

      -- Process based on document type
      case mbReminderConfig of
        Just config -> do
          processReminderByType reminder driver config merchantId merchantOpCityId
          -- Only mark as SENT if it's a document expiry reminder that doesn't reschedule
          -- Non-mandatory expired document expiry reminders reschedule, so they stay PENDING
          -- Inspection/training reminders are rescheduled above, so they stay PENDING
          when (reminder.documentType `elem` documentExpiryTypes) $ do
            now <- getCurrentTime
            let hasExpired = reminder.dueDate <= now
            -- Mark as SENT only if: not expired yet, or expired but mandatory (mandatory ones don't reschedule)
            when (not hasExpired || (hasExpired && config.isMandatory)) $ do
              QReminder.updateByPrimaryKey reminder {DR.status = DR.SENT}
              logInfo $ "Reminder " <> reminderId.getId <> " sent and marked as SENT"
        Nothing -> logError $ "ReminderConfig not found for documentType: " <> show reminder.documentType

      return Complete

-- | Route reminder processing based on document type
processReminderByType ::
  ( CoreMetrics m,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv,
    HasFlowEnv m r '["smsCfg" ::: SmsConfig, "maxNotificationShards" ::: Int],
    ServiceFlow m r,
    SchedulerFlow r,
    EncFlow m r,
    HasField "blackListedJobs" r [Text],
    HasSchemaName SchedulerJobT
  ) =>
  DR.Reminder ->
  DP.Person ->
  DRC.ReminderConfig ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  m ()
processReminderByType reminder driver config merchantId merchantOpCityId =
  if reminder.documentType `elem` documentExpiryTypes
    then processDocumentExpiryReminder reminder driver config merchantId merchantOpCityId
    else case reminder.documentType of
      DVC.InspectionHub -> do
        -- If overdue and mandatory but driver is on ride, reschedule instead of removing vehicle
        now <- getCurrentTime
        let isOverdue = reminder.dueDate <= now
            isMandatory = config.isMandatory
            shouldDisable = isOverdue && isMandatory
        rescheduledForOnRide <-
          if shouldDisable
            then do
              isOnRide <- QDIExtra.findByDriverIdActiveRide (cast reminder.driverId)
              if isJust isOnRide
                then do
                  let scheduleAfter = fromIntegral $ fromMaybe defaultOnRideRescheduleIntervalSeconds config.reminderOnRideRescheduleIntervalSeconds
                  scheduleReminderJob scheduleAfter reminder.id merchantId merchantOpCityId
                  logInfo $ "Driver " <> reminder.driverId.getId <> " on ride, rescheduling vehicle inspection reminder in " <> show scheduleAfter <> " seconds"
                  return True
                else return False
            else return False
        unless rescheduledForOnRide $
          processInspectionReminder
            reminder
            driver
            config
            merchantId
            merchantOpCityId
            "Vehicle Inspection"
            "VEHICLE_INSPECTION"
            $ do
              -- Invalidate RC, deactivate association and remove vehicle (only when not on ride)
              let rcId = Id @DVRC.VehicleRegistrationCertificate reminder.entityId
              DomainRC.invalidateRCAndRemoveVehicleForReminder rcId reminder.driverId merchantOpCityId "Expired mandatory vehicle inspection reminder"
              logInfo $ "Invalidated RC " <> reminder.entityId <> " and removed vehicle for driver " <> reminder.driverId.getId <> " due to expired mandatory vehicle inspection reminder"
      DVC.DriverInspectionHub ->
        processInspectionReminder
          reminder
          driver
          config
          merchantId
          merchantOpCityId
          "Driver Inspection"
          "DRIVER_INSPECTION"
          $ do
            -- Set approved flag to False for DriverInformation
            QDIExtra.updateApproved (Just False) reminder.driverId
            logInfo $ "Set approved = false for driver " <> reminder.driverId.getId <> " due to expired mandatory driver inspection reminder"
      DVC.TrainingForm -> do
        -- Check LMS training status: if all trainings completed, cancel other pending training reminders and mark current SENT
        mbStatus <- DriverOnboardingStatus.checkLMSTrainingStatus reminder.driverId merchantOpCityId
        case mbStatus of
          Just DriverOnboardingStatus.VALID -> do
            -- All trainings completed: cancel all pending training reminders for this driver, then mark current as SENT (only this one completed)
            pendingTrainingReminders <-
              QReminder.findAllPendingByDriverIdAndDocumentType reminder.driverId DR.PENDING DVC.TrainingForm
            ReminderHelper.cancelPendingReminders pendingTrainingReminders "LMS training completed"
            QReminder.updateByPrimaryKey reminder {DR.status = DR.SENT}
            logInfo $
              "All LMS trainings completed for driver " <> reminder.driverId.getId
                <> ", marked current training reminder as SENT and cancelled "
                <> show (max 0 (length pendingTrainingReminders - 1))
                <> " other pending training reminder(s)"
          _ ->
            -- Not all completed: continue with normal flow (after due date driver will be disabled)
            processInspectionReminder
              reminder
              driver
              config
              merchantId
              merchantOpCityId
              "Training Video"
              "TRAINING_VIDEO"
              $ do
                logInfo $ "Disabled driver " <> reminder.driverId.getId <> " due to expired mandatory training reminder"
      _ -> logError $ "Unknown documentType: " <> show reminder.documentType

-- | Process inspection reminders (vehicle, driver inspection, or training)
-- Handles blocking, setting approved flags, sending notifications, and scheduling next reminder
processInspectionReminder ::
  ( EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv,
    HasFlowEnv m r '["smsCfg" ::: SmsConfig, "maxNotificationShards" ::: Int],
    ServiceFlow m r,
    SchedulerFlow r,
    HasField "blackListedJobs" r [Text],
    HasSchemaName SchedulerJobT
  ) =>
  DR.Reminder ->
  DP.Person ->
  DRC.ReminderConfig ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Text ->
  Text ->
  m () ->
  m ()
processInspectionReminder reminder driver config merchantId merchantOpCityId displayName notificationKey setApprovedAction = do
  now <- getCurrentTime
  let isOverdue = reminder.dueDate <= now
      isMandatory = config.isMandatory
      shouldDisable = isOverdue && isMandatory
  -- Check if reminder is overdue and mandatory, then disable driver and set approved = false
  when shouldDisable $ do
    driverInfo <- QDriverInfo.findById reminder.driverId >>= fromMaybeM (InternalError "DriverInformation not found")
    when driverInfo.enabled $ do
      transporterConfig <- CTC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (InternalError $ "Transporter config not found for merchantOpCityId: " <> merchantOpCityId.getId)
      disableDriverForMandatoryReminder driverInfo transporterConfig reminder.driverId ("expired mandatory " <> displayName <> " reminder")
    setApprovedAction
  -- Before sending notification, check if reminder still exists and is still PENDING
  -- (it may have been cancelled if inspection was approved)
  mbCurrentReminder <- QReminder.findByPrimaryKey reminder.id
  case mbCurrentReminder of
    Just currentReminder | currentReminder.status == DR.PENDING -> do
      -- Reminder is still PENDING, send notification
      sendInspectionOrTrainingNotification currentReminder driver merchantOpCityId notificationKey displayName config
      -- Only reschedule if driver was NOT disabled (i.e., not overdue and mandatory)
      -- If driver was disabled, don't reschedule to avoid unnecessary job execution
      unless shouldDisable $ do
        let scheduleAfter = fromIntegral $ fromMaybe defaultRescheduleIntervalSeconds config.reminderRescheduleIntervalSeconds
        scheduleReminderJob scheduleAfter reminder.id merchantId merchantOpCityId
        logInfo $ "Scheduled next " <> displayName <> " reminder job for driver " <> driver.id.getId <> " in " <> show scheduleAfter <> " seconds"
      when shouldDisable $
        logInfo $ "Driver disabled due to overdue mandatory " <> displayName <> " reminder, skipping reschedule"
    _ -> do
      logInfo $ "Reminder " <> reminder.id.getId <> " is no longer PENDING (status: " <> maybe "not found" (show . (.status)) mbCurrentReminder <> "), skipping notification and reschedule"

processDocumentExpiryReminder ::
  ( EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    CacheFlow m r,
    MonadFlow m,
    Redis.HedisFlow m r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv,
    HasFlowEnv m r '["smsCfg" ::: SmsConfig, "maxNotificationShards" ::: Int],
    ServiceFlow m r,
    SchedulerFlow r,
    HasField "blackListedJobs" r [Text],
    HasSchemaName SchedulerJobT
  ) =>
  DR.Reminder ->
  DP.Person ->
  DRC.ReminderConfig ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  m ()
processDocumentExpiryReminder reminder driver reminderConfig merchantId merchantOpCityId = do
  now <- getCurrentTime
  let intervals = reminderConfig.reminderIntervals
      mbCurrentIntervalMinutes = listToMaybe (drop reminder.currentIntervalIndex intervals)
      documentTypeName = show reminder.documentType

  case mbCurrentIntervalMinutes of
    Nothing -> do
      logError $ "Invalid currentIntervalIndex " <> show reminder.currentIntervalIndex <> " for reminder " <> reminder.id.getId <> " (intervals length: " <> show (length intervals) <> "). Skipping processing."
      return ()
    Just currentIntervalMinutes -> do
      let daysBeforeExpiry = currentIntervalMinutes `div` (24 * 60)

      if reminder.dueDate <= now
        then do
          -- Reminder has expired - check if mandatory before blocking
          logInfo $ "Reminder expired for driver " <> driver.id.getId <> " (documentType: " <> documentTypeName <> ")"

          -- Check if reminder is mandatory (from ReminderConfig)
          driverInfo <- QDriverInfo.findById reminder.driverId >>= fromMaybeM (InternalError "DriverInformation not found")
          let isMandatory = reminderConfig.isMandatory
          if isMandatory
            then do
              transporterConfig <- CTC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (InternalError $ "Transporter config not found for merchantOpCityId: " <> merchantOpCityId.getId)
              -- Disable driver when: not vehicle-related, OR vehicle-related but separateDriverVehicleEnablement is not true
              let isVehicleRelated = reminder.documentType `elem` vehicleRelatedDocumentTypes
                  shouldDisableDriver = driverInfo.enabled && (not isVehicleRelated || transporterConfig.separateDriverVehicleEnablement /= Just True)
              when shouldDisableDriver $
                disableDriverForMandatoryReminder driverInfo transporterConfig reminder.driverId ("expired mandatory reminder: " <> documentTypeName)
              -- For all vehicle-related docs: invalidate RC and remove vehicle; then invalidate the specific document
              if isVehicleRelated
                then do
                  let rcId = Id @DVRC.VehicleRegistrationCertificate reminder.entityId
                  DomainRC.invalidateRCAndRemoveVehicleForReminder rcId reminder.driverId merchantOpCityId "Expired mandatory vehicle document reminder"
                  invalidateExpiredDocument reminder.documentType reminder.entityId merchantId reminder.driverId
                  logInfo $ "Invalidated vehicle (RC " <> rcId.getId <> ") and document " <> reminder.entityId <> " (type: " <> documentTypeName <> ") for driver " <> reminder.driverId.getId <> " due to expiry"
                else do
                  invalidateExpiredDocument reminder.documentType reminder.entityId merchantId reminder.driverId
                  logInfo $ "Invalidated document " <> reminder.entityId <> " (type: " <> documentTypeName <> ") due to expiry"
            else do
              let rescheduleSeconds = fromMaybe defaultRescheduleIntervalSeconds reminderConfig.reminderRescheduleIntervalSeconds
                  nextReminderDate = Time.addUTCTime (fromIntegral rescheduleSeconds) now
                  scheduleAfter = max 0 (Time.diffUTCTime nextReminderDate now)
              QReminder.updateByPrimaryKey reminder {DR.reminderDate = nextReminderDate}
              scheduleReminderJob scheduleAfter reminder.id merchantId merchantOpCityId
              logInfo $ "Non-mandatory reminder expired for driver " <> driver.id.getId <> ", rescheduling in " <> show scheduleAfter <> " seconds"

          -- Send expiry notification
          sendDocumentExpiryNotification reminder driver merchantOpCityId True Nothing
        else do
          -- Document is expiring - send reminder notification
          logInfo $ "Sending document expiry reminder for driver " <> driver.id.getId <> " (documentType: " <> documentTypeName <> ", " <> show daysBeforeExpiry <> " days before expiry)"
          sendDocumentExpiryNotification reminder driver merchantOpCityId False (Just daysBeforeExpiry)

-- | Invalidate an expired document by setting its verification status to INVALID
invalidateExpiredDocument ::
  ( EsqDBFlow m r,
    MonadFlow m,
    CacheFlow m r
  ) =>
  DVC.DocumentType ->
  Text ->
  Id DM.Merchant ->
  Id DP.Person ->
  m ()
invalidateExpiredDocument documentType entityId merchantId driverId = do
  let expiryReason = "Document expired"
  case documentType of
    DVC.DriverLicense -> do
      let docId = Id @DDL.DriverLicense entityId
      mbDL <- QDL.findById docId
      Kernel.Prelude.whenJust mbDL $ \dl -> do
        QDL.updateVerificationStatusAndRejectReason Documents.INVALID expiryReason dl.documentImageId1
        void $ uncurry (liftA2 (,)) $ TE.both (maybe (return ()) (QImage.updateVerificationStatusAndFailureReason Documents.INVALID (ImageNotValid expiryReason))) (Just dl.documentImageId1, dl.documentImageId2)
    DVC.VehicleRegistrationCertificate -> do
      let docId = Id @DVRC.VehicleRegistrationCertificate entityId
      mbRC <- QVRC.findById docId
      Kernel.Prelude.whenJust mbRC $ \rc -> do
        QVRC.updateVerificationStatusAndRejectReason Documents.INVALID expiryReason rc.documentImageId
        QImage.updateVerificationStatusAndFailureReason Documents.INVALID (ImageNotValid expiryReason) rc.documentImageId
    DVC.VehicleInsurance -> do
      -- VehicleInsurance reminders can have entityId as either insurance ID or RC ID
      -- Try insurance ID first, then fall back to RC ID lookup
      let insuranceId = Id @DVI.VehicleInsurance entityId
      mbInsurance <- QVI.findByPrimaryKey insuranceId
      case mbInsurance of
        Just insurance -> do
          QVI.updateVerificationStatusAndRejectReason Documents.INVALID expiryReason insurance.documentImageId
          QImage.updateVerificationStatusAndFailureReason Documents.INVALID (ImageNotValid expiryReason) insurance.documentImageId
        Nothing -> do
          -- If not found by insurance ID, try RC ID (entityId might be RC ID)
          let rcId = Id @DVRC.VehicleRegistrationCertificate entityId
          insurances <- QVI.findByRcIdAndDriverId rcId driverId
          forM_ insurances $ \insurance -> do
            QVI.updateVerificationStatusAndRejectReason Documents.INVALID expiryReason insurance.documentImageId
            QImage.updateVerificationStatusAndFailureReason Documents.INVALID (ImageNotValid expiryReason) insurance.documentImageId
    DVC.VehiclePermit -> do
      -- VehiclePermit reminders can have entityId as either permit ID or RC ID
      -- Try permit ID first, then fall back to RC ID lookup
      let permitId = Id @DVPermit.VehiclePermit entityId
      mbPermit <- QVPermit.findByPrimaryKey permitId
      case mbPermit of
        Just permit -> do
          QVPermit.updateVerificationStatusByImageId Documents.INVALID permit.documentImageId
          QImage.updateVerificationStatusAndFailureReason Documents.INVALID (ImageNotValid expiryReason) permit.documentImageId
        Nothing -> do
          -- If not found by permit ID, try RC ID (entityId might be RC ID)
          let rcId = Id @DVRC.VehicleRegistrationCertificate entityId
          permits <- QVPermit.findByRcIdAndDriverId rcId driverId
          forM_ permits $ \permit -> do
            QVPermit.updateVerificationStatusByImageId Documents.INVALID permit.documentImageId
            QImage.updateVerificationStatusAndFailureReason Documents.INVALID (ImageNotValid expiryReason) permit.documentImageId
    DVC.VehiclePUC -> do
      -- VehiclePUC reminders can have entityId as either PUC ID or RC ID
      -- Try PUC ID first, then fall back to RC ID lookup
      let pucId = Id @DPUC.VehiclePUC entityId
      mbPUC <- QVPUC.findByPrimaryKey pucId
      case mbPUC of
        Just puc -> do
          QVPUC.updateVerificationStatusByImageId Documents.INVALID puc.documentImageId
          QImage.updateVerificationStatusAndFailureReason Documents.INVALID (ImageNotValid expiryReason) puc.documentImageId
        Nothing -> do
          -- If not found by PUC ID, try RC ID (entityId might be RC ID)
          let rcId = Id @DVRC.VehicleRegistrationCertificate entityId
          pucs <- QVPUC.findByRcIdAndDriverId rcId driverId
          forM_ pucs $ \puc -> do
            QVPUC.updateVerificationStatusByImageId Documents.INVALID puc.documentImageId
            QImage.updateVerificationStatusAndFailureReason Documents.INVALID (ImageNotValid expiryReason) puc.documentImageId
    DVC.VehicleFitnessCertificate -> do
      -- VehicleFitnessCertificate reminders can have entityId as either fitness ID or RC ID
      -- Try fitness ID first, then fall back to RC ID lookup
      let fitnessId = Id @DFC.VehicleFitnessCertificate entityId
      mbFitness <- QFC.findByPrimaryKey fitnessId
      case mbFitness of
        Just fitness -> do
          QFC.updateVerificationStatus Documents.INVALID fitness.documentImageId
          QImage.updateVerificationStatusAndFailureReason Documents.INVALID (ImageNotValid expiryReason) fitness.documentImageId
        Nothing -> do
          -- If not found by fitness ID, try RC ID (entityId might be RC ID)
          let rcId = Id @DVRC.VehicleRegistrationCertificate entityId
          fitnessCerts <- QFC.findByRcIdAndDriverId rcId driverId
          forM_ fitnessCerts $ \fitness -> do
            QFC.updateVerificationStatus Documents.INVALID fitness.documentImageId
            QImage.updateVerificationStatusAndFailureReason Documents.INVALID (ImageNotValid expiryReason) fitness.documentImageId
    DVC.BusinessLicense -> do
      let docId = Id @DBL.BusinessLicense entityId
      mbBL <- QBL.findByPrimaryKey docId
      Kernel.Prelude.whenJust mbBL $ \bl -> do
        QBL.updateVerificationStatusByImageId Documents.INVALID bl.documentImageId
        QImage.updateVerificationStatusAndFailureReason Documents.INVALID (ImageNotValid expiryReason) bl.documentImageId
    DVC.TrainingForm -> do
      -- For training, use the driverId passed as parameter (from reminder.driverId)
      -- Query Image table directly by personId and imageType
      trainingImages <- QImage.findImagesByPersonAndType Nothing Nothing merchantId driverId DVC.TrainingForm
      -- Mark all training images as INVALID
      forM_ trainingImages $ \image ->
        QImage.updateVerificationStatusAndFailureReason Documents.INVALID (ImageNotValid expiryReason) image.id
    _ -> logError $ "Unknown document type for invalidation: " <> show documentType

-- Helper function to send FCM and SMS notifications to driver
sendDriverNotifications ::
  ( EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    CacheFlow m r,
    HasFlowEnv m r '["smsCfg" ::: SmsConfig, "maxNotificationShards" ::: Int],
    ServiceFlow m r
  ) =>
  DR.Reminder ->
  DP.Person ->
  Id DMOC.MerchantOperatingCity ->
  Text ->
  Text ->
  m ()
sendDriverNotifications reminder driver merchantOpCityId notificationKey smsKey = do
  -- Send FCM notification to driver
  mbMerchantPN <- CPN.findMatchingMerchantPN merchantOpCityId notificationKey Nothing Nothing driver.language Nothing
  case mbMerchantPN of
    Just merchantPN -> do
      let entityData = NotifReq {entityId = reminder.entityId, title = merchantPN.title, message = merchantPN.body}
      notifyDriverOnEvents merchantOpCityId driver.id driver.deviceToken entityData merchantPN.fcmNotificationType
    Nothing -> logInfo $ "MerchantPushNotification not found for " <> notificationKey <> ", skipping FCM notification"

  -- Send SMS to driver
  let mbMessageKey = readMaybe (Text.unpack smsKey) :: Maybe DMM.MessageKey
  case mbMessageKey of
    Just messageKey -> do
      mbMerchantMessage <- CMM.findByMerchantOpCityIdAndMessageKeyVehicleCategory merchantOpCityId messageKey Nothing Nothing
      case mbMerchantMessage of
        Just merchantMsg -> do
          smsCfg <- asks (.smsCfg)
          let sender = fromMaybe smsCfg.sender merchantMsg.senderHeader
          mbPhoneNumber <- mapM decrypt driver.mobileNumber
          case mbPhoneNumber of
            Just phoneNumber -> do
              Sms.sendSMS driver.merchantId merchantOpCityId (Sms.SendSMSReq merchantMsg.message phoneNumber sender merchantMsg.templateId merchantMsg.messageType) >>= Sms.checkSmsResult
            Nothing -> logInfo $ "Driver mobile number not available, skipping SMS"
        Nothing -> logInfo $ "MerchantMessage not found for " <> smsKey <> ", skipping SMS"
    Nothing -> logInfo $ "Invalid MessageKey: " <> smsKey <> ", skipping SMS"

-- Helper function to send GRPC notifications to fleet owners and operators
sendFleetAndOperatorNotifications ::
  ( EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    CacheFlow m r,
    HasFlowEnv m r '["smsCfg" ::: SmsConfig, "maxNotificationShards" ::: Int],
    ServiceFlow m r
  ) =>
  DP.Person ->
  Id DMOC.MerchantOperatingCity ->
  Text ->
  Text ->
  Aeson.Value ->
  m ()
sendFleetAndOperatorNotifications driver merchantOpCityId title message entityData = do
  -- Send notification to fleet owner (if exists) via GRPC
  mbFleetAssociation <- QFDA.findByDriverId driver.id True
  Kernel.Prelude.whenJust mbFleetAssociation $ \fleetAssoc -> do
    logInfo $ "Fleet owner found for driver " <> driver.id.getId <> ": " <> fleetAssoc.fleetOwnerId
    notifyFleetWithGRPCProvider merchantOpCityId Notification.DRIVER_NOTIFY title message driver.id entityData

  -- Send notification to operator(s) via GRPC (direct association or through fleet)
  operatorIdTexts <- Analytics.findOperatorIdForDriver driver.id
  forM_ operatorIdTexts $ \operatorIdText -> do
    let operatorId = Id operatorIdText
    logInfo $ "Operator found for driver " <> driver.id.getId <> ": " <> operatorIdText
    notifyWithGRPCProvider merchantOpCityId Notification.DRIVER_NOTIFY title message operatorId entityData

-- Helper function to send document expiry notifications
sendDocumentExpiryNotification ::
  ( EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    CacheFlow m r,
    HasFlowEnv m r '["smsCfg" ::: SmsConfig, "maxNotificationShards" ::: Int],
    ServiceFlow m r
  ) =>
  DR.Reminder ->
  DP.Person ->
  Id DMOC.MerchantOperatingCity ->
  Bool ->
  Maybe Int ->
  m ()
sendDocumentExpiryNotification reminder driver merchantOpCityId isExpiredParam mbDaysBefore = do
  let documentTypeName = show reminder.documentType
      fleetOperatorTitle = if isExpiredParam then "Driver Document Expired" else "Driver Document Expiring Soon"
      fleetOperatorMessage =
        if isExpiredParam
          then "Driver " <> driver.firstName <> "'s " <> documentTypeName <> " has expired. Please ensure they upload an updated document."
          else "Driver " <> driver.firstName <> "'s " <> documentTypeName <> " will expire in " <> show (fromMaybe 0 mbDaysBefore) <> " days."
      entityData = Aeson.object ["driverId" Aeson..= driver.id.getId, "documentType" Aeson..= documentTypeName, "isExpired" Aeson..= isExpiredParam]

  -- Send FCM and SMS to driver
  sendDriverNotifications reminder driver merchantOpCityId "DOCUMENT_EXPIRY_REMINDER" "DOCUMENT_EXPIRY_REMINDER_SMS"

  -- Send GRPC notifications to fleet and operators
  sendFleetAndOperatorNotifications driver merchantOpCityId fleetOperatorTitle fleetOperatorMessage entityData

-- Helper function to send inspection/training notifications
sendInspectionOrTrainingNotification ::
  ( EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    CacheFlow m r,
    HasFlowEnv m r '["smsCfg" ::: SmsConfig, "maxNotificationShards" ::: Int],
    ServiceFlow m r
  ) =>
  DR.Reminder ->
  DP.Person ->
  Id DMOC.MerchantOperatingCity ->
  Text ->
  Text ->
  DRC.ReminderConfig ->
  m ()
sendInspectionOrTrainingNotification reminder driver merchantOpCityId notificationKey displayName reminderConfig = do
  now <- getCurrentTime
  let intervals = reminderConfig.reminderIntervals
      -- currentIntervalIndex: Index in the reminderIntervals array (0 = first interval, 1 = second, etc.)
      -- This represents which reminder interval this entry corresponds to (e.g., T-30, T-15, T-1)
      mbCurrentIntervalMinutes = listToMaybe (drop reminder.currentIntervalIndex intervals)
      isOverdue = reminder.dueDate <= now

  case mbCurrentIntervalMinutes of
    Nothing -> do
      logError $ "Invalid currentIntervalIndex " <> show reminder.currentIntervalIndex <> " for reminder " <> reminder.id.getId <> " (intervals length: " <> show (length intervals) <> "). Skipping notification."
      return ()
    Just currentIntervalMinutes -> do
      let daysBeforeDue = currentIntervalMinutes `div` (24 * 60)
          fleetOperatorTitle = if isOverdue then "Driver " <> displayName <> " Overdue" else "Driver " <> displayName <> " Required"
          fleetOperatorMessage =
            if isOverdue
              then "Driver " <> driver.firstName <> "'s " <> displayName <> " is overdue. Please ensure they complete this requirement."
              else "Driver " <> driver.firstName <> "'s " <> displayName <> " is due in " <> show daysBeforeDue <> " days. Please ensure they complete this requirement."
          entityData = Aeson.object ["driverId" Aeson..= driver.id.getId, "documentType" Aeson..= notificationKey, "isOverdue" Aeson..= isOverdue, "daysBeforeDue" Aeson..= daysBeforeDue]

      -- Send FCM and SMS to driver
      sendDriverNotifications reminder driver merchantOpCityId notificationKey (notificationKey <> "_SMS")

      -- Send GRPC notifications to fleet and operators
      sendFleetAndOperatorNotifications driver merchantOpCityId fleetOperatorTitle fleetOperatorMessage entityData
