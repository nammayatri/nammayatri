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
import qualified Domain.Action.Internal.DriverMode as DDriverMode
import qualified Domain.Action.UI.DriverOnboarding.VehicleRegistrationCertificate as DomainRC
import qualified Domain.Types.BusinessLicense as DBL
import qualified Domain.Types.Common as DDriverInfo
import qualified Domain.Types.DocumentVerificationConfig as DVC
import qualified Domain.Types.DriverLicense as DDL
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantMessage as DMM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Reminder as DR
import qualified Domain.Types.ReminderConfig as DRC
import qualified Domain.Types.TransporterConfig as DTC
import qualified Domain.Types.VehicleRegistrationCertificate as DVRC
import Kernel.Beam.Functions as BF
import Kernel.Beam.Lib.UtilsTH (HasSchemaName)
import Kernel.External.Encryption (decrypt)
import qualified Kernel.External.Notification as Notification
import Kernel.External.Notification.FCM.Types as FCM
import Kernel.External.Types (SchedulerFlow, ServiceFlow)
import Kernel.Prelude
import Kernel.Sms.Config (SmsConfig)
import qualified Kernel.Storage.Clickhouse.Config as CH
import Kernel.Storage.Esqueleto.Config
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import qualified Kernel.Types.Documents as Documents
import Kernel.Types.Error (PersonError (PersonDoesNotExist), TransporterError (TransporterConfigNotFound))
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Scheduler
import Lib.Scheduler.JobStorageType.DB.Table (SchedulerJobT)
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import SharedLogic.Allocator (AllocatorJobType (..))
import qualified SharedLogic.Allocator as Allocator
import qualified SharedLogic.Analytics as Analytics
import qualified SharedLogic.DriverOnboarding.Status as DriverOnboardingStatus (ResponseStatus (..), checkLMSTrainingStatus)
import qualified SharedLogic.MessageBuilder as MessageBuilder
import qualified SharedLogic.Reminder.Helper as ReminderHelper
import Storage.Beam.SchedulerJob ()
import Storage.ConfigPilot.Config.TransporterConfig (TransporterDimensions (..))
import Storage.ConfigPilot.Interface.Types (getConfig)
import qualified Storage.CachedQueries.Merchant.MerchantMessage as CMM
import qualified Storage.CachedQueries.Merchant.MerchantPushNotification as CPN
import qualified Storage.Queries.BusinessLicense as QBL
import qualified Storage.Queries.DriverInformation as QDriverInfo
import qualified Storage.Queries.DriverInformationExtra as QDIExtra
import qualified Storage.Queries.DriverLicense as QDL
import qualified Storage.Queries.DriverRCAssociationExtra as QDRCAExtra
import qualified Storage.Queries.FleetDriverAssociationExtra as QFDA
import qualified Storage.Queries.FleetOperatorAssociation as QFOA
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

-- | Disable driver, set OFFLINE mode and update fleet/operator analytics. Used when a mandatory reminder has expired.
disableDriverForMandatoryReminder ::
  ( EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    CacheFlow m r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv
  ) =>
  DTC.TransporterConfig ->
  Id DP.Person ->
  UTCTime ->
  Text ->
  m ()
disableDriverForMandatoryReminder transporterConfig driverId now documentTypeName = do
  driverInfo <- QDriverInfo.findById driverId >>= fromMaybeM (InternalError "DriverInformation not found")
  when driverInfo.enabled $ do
    Analytics.updateEnabledVerifiedStateWithAnalytics (Just driverInfo) transporterConfig driverId False Nothing
    logInfo $ "Disabled driver " <> driverId.getId <> " due to expired mandatory " <> documentTypeName <> " reminder"

  let isActive = False
  let mode = DDriverInfo.OFFLINE
  when (driverInfo.active /= isActive || driverInfo.mode /= Just mode) $ do
    let newFlowStatus = DDriverMode.getDriverFlowStatus (Just mode) isActive
    -- Track offline timestamp when driver goes offline
    if driverInfo.mode /= Just DDriverInfo.OFFLINE
      then do
        logInfo $ "Driver going OFFLINE at: " <> show now <> " for driverId: " <> show driverId <> " due to expired mandatory " <> documentTypeName <> " reminder"
        DDriverMode.updateDriverModeAndFlowStatus driverId transporterConfig isActive (Just mode) newFlowStatus driverInfo Nothing (Just now)
      else DDriverMode.updateDriverModeAndFlowStatus driverId transporterConfig isActive (Just mode) newFlowStatus driverInfo Nothing Nothing

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
        Just config -> processReminderByType reminder driver config merchantId merchantOpCityId
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
            FCM.VEHICLE_INSPECTION
            DMM.VEHICLE_INSPECTION_SMS
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
          FCM.DRIVER_INSPECTION
          DMM.DRIVER_INSPECTION_SMS
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
              FCM.TRAINING_VIDEO
              DMM.TRAINING_VIDEO_SMS
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
  FCM.FCMNotificationType ->
  DMM.MessageKey ->
  m () ->
  m ()
processInspectionReminder reminder driver config merchantId merchantOpCityId displayName notificationType messageKey setApprovedAction = do
  now <- getCurrentTime
  transporterConfig <- getConfig (TransporterDimensions {merchantOperatingCityId = merchantOpCityId.getId}) >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  let isOverdue = reminder.dueDate <= now
      isMandatory = config.isMandatory
      isVehicleRelated = reminder.documentType == DVC.InspectionHub
      shouldDisableAction = isOverdue && isMandatory
      shouldDisableDriver = shouldDisableAction && (not isVehicleRelated || transporterConfig.separateDriverVehicleEnablement /= Just True)
  -- Check if reminder is overdue and mandatory, then disable driver and set approved = false
  when shouldDisableDriver $ do
    disableDriverForMandatoryReminder transporterConfig reminder.driverId now displayName
  when shouldDisableAction $ do
    -- Set approved flag to False (action depends on inspection type)
    setApprovedAction
  -- Before sending notification, check if reminder still exists and is still PENDING
  -- (it may have been cancelled if inspection was approved)
  mbCurrentReminder <- QReminder.findByPrimaryKey reminder.id
  case mbCurrentReminder of
    Just currentReminder | currentReminder.status == DR.PENDING -> do
      -- Reminder is still PENDING, send notification
      sendInspectionOrTrainingNotification currentReminder driver merchantOpCityId notificationType messageKey displayName config
      -- Only reschedule if driver was NOT disabled (i.e., not overdue and mandatory)
      -- If driver was disabled, don't reschedule to avoid unnecessary job execution
      unless shouldDisableAction $ do
        let scheduleAfter = fromIntegral $ fromMaybe defaultRescheduleIntervalSeconds config.reminderRescheduleIntervalSeconds
        scheduleReminderJob scheduleAfter reminder.id merchantId merchantOpCityId
        logInfo $ "Scheduled next " <> displayName <> " reminder job for driver " <> driver.id.getId <> " in " <> show scheduleAfter <> " seconds"
      when shouldDisableAction $
        logInfo $ "Disable action was triggered due to overdue mandatory " <> displayName <> " reminder, skipping reschedule"
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
    Just currentIntervalMinutes -> do
      let daysBeforeExpiry = currentIntervalMinutes `div` (24 * 60)

      if reminder.dueDate <= now
        then do
          -- Reminder has expired - check if mandatory before blocking
          logInfo $ "Reminder expired for driver " <> driver.id.getId <> " (documentType: " <> documentTypeName <> ")"

          -- Check if reminder is mandatory (from ReminderConfig)
          let isMandatory = reminderConfig.isMandatory
          if isMandatory
            then do
              transporterConfig <- getConfig (TransporterDimensions {merchantOperatingCityId = merchantOpCityId.getId}) >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
              -- Disable driver when: not vehicle-related, OR vehicle-related but separateDriverVehicleEnablement is not true
              let isVehicleRelated = reminder.documentType `elem` vehicleRelatedDocumentTypes
                  shouldDisableDriver = not isVehicleRelated || transporterConfig.separateDriverVehicleEnablement /= Just True
              when shouldDisableDriver $
                disableDriverForMandatoryReminder transporterConfig reminder.driverId now documentTypeName
              -- For all vehicle-related docs: invalidate RC and remove vehicle; then invalidate the specific document
              if isVehicleRelated
                then do
                  -- Vehicle reminders use entityId = rcId only; RC invalidation and vehicle removal are in invalidateRCAndRemoveVehicleForReminder
                  let rcId = Id @DVRC.VehicleRegistrationCertificate reminder.entityId
                      expiryReason = "Expired mandatory vehicle document reminder"
                  DomainRC.invalidateRCAndRemoveVehicleForReminder rcId reminder.driverId merchantOpCityId expiryReason
                  invalidateExpiredDocument reminder.documentType reminder.entityId merchantId reminder.driverId expiryReason
                  logInfo $ "Invalidated RC " <> reminder.entityId <> " (type: " <> documentTypeName <> ") for driver " <> reminder.driverId.getId <> " due to expiry"
                else do
                  invalidateExpiredDocument reminder.documentType reminder.entityId merchantId reminder.driverId "Document expired"
                  logInfo $ "Invalidated document " <> reminder.entityId <> " (type: " <> documentTypeName <> ") due to expiry"
            else do
              let rescheduleSeconds = fromMaybe defaultRescheduleIntervalSeconds reminderConfig.reminderRescheduleIntervalSeconds
                  scheduleAfter = fromIntegral rescheduleSeconds
                  nextReminderDate = Time.addUTCTime scheduleAfter now
              QReminder.updateByPrimaryKey reminder {DR.reminderDate = nextReminderDate}
              scheduleReminderJob scheduleAfter reminder.id merchantId merchantOpCityId
              logInfo $ "Non-mandatory reminder expired for driver " <> driver.id.getId <> ", rescheduling in " <> show scheduleAfter <> " seconds"

          -- Send expiry notification (for vehicle-related use current RC-associated drivers; if none, use RC fleet owner and fleet operators)
          if reminder.documentType `elem` vehicleRelatedDocumentTypes
            then sendVehicleDocumentExpiryNotification reminder driver merchantOpCityId True Nothing
            else sendDocumentExpiryNotification reminder driver merchantOpCityId True Nothing
        else do
          -- Document is expiring - send reminder notification (for vehicle-related use current RC-associated drivers; if none, use RC fleet owner and fleet operators)
          logInfo $ "Sending document expiry reminder (documentType: " <> documentTypeName <> ", " <> show daysBeforeExpiry <> " days before expiry)"
          if reminder.documentType `elem` vehicleRelatedDocumentTypes
            then sendVehicleDocumentExpiryNotification reminder driver merchantOpCityId False (Just daysBeforeExpiry)
            else sendDocumentExpiryNotification reminder driver merchantOpCityId False (Just daysBeforeExpiry)

      -- Mark as SENT when: not yet expired (pre-expiry reminder sent), or expired and mandatory (terminal state, no reschedule).
      -- Non-mandatory expired reminders are rescheduled above and stay PENDING.
      let hasExpired = reminder.dueDate <= now
      when (not hasExpired || reminderConfig.isMandatory) $ do
        QReminder.updateByPrimaryKey reminder {DR.status = DR.SENT}
        logInfo $ "Reminder " <> reminder.id.getId <> " marked as SENT"

-- | Invalidate an expired document by setting its verification status to INVALID.
-- For vehicle types, entityId is rcId: invalidate only the doc-specific row for that document type and its image (RC itself is done by invalidateRCAndRemoveVehicleForReminder).
invalidateExpiredDocument ::
  ( EsqDBFlow m r,
    MonadFlow m,
    CacheFlow m r
  ) =>
  DVC.DocumentType ->
  Text ->
  Id DM.Merchant ->
  Id DP.Person ->
  Text ->
  m ()
invalidateExpiredDocument documentType entityId _merchantId _driverId expiryReason = do
  case documentType of
    DVC.DriverLicense -> do
      let docId = Id @DDL.DriverLicense entityId
      mbDL <- QDL.findById docId
      Kernel.Prelude.whenJust mbDL $ \dl -> do
        QDL.updateVerificationStatusAndRejectReason Documents.INVALID expiryReason dl.documentImageId1
        void $ uncurry (liftA2 (,)) $ TE.both (maybe (return ()) (QImage.updateVerificationStatusAndFailureReason Documents.INVALID (ImageNotValid expiryReason))) (Just dl.documentImageId1, dl.documentImageId2)
    DVC.VehicleRegistrationCertificate -> pure ()
    DVC.VehicleInsurance -> do
      let rcId = Id @DVRC.VehicleRegistrationCertificate entityId
          rcIdText = Just (getId rcId)
      QVI.updateVerificationStatusAndRejectReasonByRcId Documents.INVALID (Just expiryReason) rcId
      QImage.updateVerificationStatusAndFailureReasonByRcIdAndImageType (Just Documents.INVALID) (Just $ ImageNotValid expiryReason) rcIdText DVC.VehicleInsurance
    DVC.VehiclePermit -> do
      let rcId = Id @DVRC.VehicleRegistrationCertificate entityId
          rcIdText = Just (getId rcId)
      QVPermit.updateVerificationStatusByRcId Documents.INVALID rcId
      QImage.updateVerificationStatusAndFailureReasonByRcIdAndImageType (Just Documents.INVALID) (Just $ ImageNotValid expiryReason) rcIdText DVC.VehiclePermit
    DVC.VehiclePUC -> do
      let rcId = Id @DVRC.VehicleRegistrationCertificate entityId
          rcIdText = Just (getId rcId)
      QVPUC.updateVerificationStatusByRcId Documents.INVALID rcId
      QImage.updateVerificationStatusAndFailureReasonByRcIdAndImageType (Just Documents.INVALID) (Just $ ImageNotValid expiryReason) rcIdText DVC.VehiclePUC
    DVC.VehicleFitnessCertificate -> do
      let rcId = Id @DVRC.VehicleRegistrationCertificate entityId
          rcIdText = Just (getId rcId)
      QFC.updateVerificationStatusByRcId Documents.INVALID rcId
      QImage.updateVerificationStatusAndFailureReasonByRcIdAndImageType (Just Documents.INVALID) (Just $ ImageNotValid expiryReason) rcIdText DVC.VehicleFitnessCertificate
    DVC.BusinessLicense -> do
      let docId = Id @DBL.BusinessLicense entityId
      mbBL <- QBL.findByPrimaryKey docId
      Kernel.Prelude.whenJust mbBL $ \bl -> do
        QBL.updateVerificationStatusByImageId Documents.INVALID bl.documentImageId
        QImage.updateVerificationStatusAndFailureReason Documents.INVALID (ImageNotValid expiryReason) bl.documentImageId
    DVC.TrainingForm -> pure ()
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
  FCM.FCMNotificationType ->
  DMM.MessageKey ->
  m ()
sendDriverNotifications reminder driver merchantOpCityId notificationType messageKey = do
  -- Send FCM notification to driver
  let notificationKey = show notificationType
      documentTypeName = show reminder.documentType
  mbMerchantPN <- CPN.findMatchingMerchantPN merchantOpCityId notificationKey Nothing Nothing driver.language Nothing
  case mbMerchantPN of
    Just merchantPN -> do
      let (title, body) = case notificationType of
            FCM.DOCUMENT_EXPIRY_REMINDER ->
              ( Text.replace "{#documentType#}" documentTypeName merchantPN.title,
                Text.replace "{#documentType#}" documentTypeName merchantPN.body
              )
            _ -> (merchantPN.title, merchantPN.body)
      let entityData = NotifReq {entityId = reminder.entityId, title, message = body}
      notifyDriverOnEvents merchantOpCityId driver.id driver.deviceToken entityData merchantPN.fcmNotificationType
    Nothing -> logInfo $ "MerchantPushNotification not found for " <> notificationKey <> ", skipping FCM notification"

  -- Send SMS to driver
  -- For DOCUMENT_EXPIRY_REMINDER_SMS, use MessageBuilder to replace template variables
  mbSmsInfo <-
    if messageKey == DMM.DOCUMENT_EXPIRY_REMINDER_SMS
      then do
        (mbSender, msg, templateId, messageType) <-
          MessageBuilder.buildDocumentExpiryReminderMessage
            merchantOpCityId
            (MessageBuilder.BuildDocumentExpiryReminderMessageReq {documentType = documentTypeName})
        smsCfg <- asks (.smsCfg)
        let sender = fromMaybe smsCfg.sender mbSender
        pure $ Just (msg, sender, templateId, messageType)
      else do
        CMM.findByMerchantOpCityIdAndMessageKeyVehicleCategory merchantOpCityId messageKey Nothing Nothing >>= \case
          Just merchantMsg -> do
            smsCfg <- asks (.smsCfg)
            let sender = fromMaybe smsCfg.sender merchantMsg.senderHeader
            pure $ Just (merchantMsg.message, sender, merchantMsg.templateId, merchantMsg.messageType)
          Nothing -> do
            logInfo $ "MerchantMessage not found for " <> show messageKey <> ", skipping SMS"
            pure Nothing
  whenJust mbSmsInfo $ \(smsMessage, smsSender, smsTemplateId, messageType) -> do
    mbPhoneNumber <- mapM decrypt driver.mobileNumber
    case mbPhoneNumber of
      Just phoneNumber -> do
        Sms.sendSMS driver.merchantId merchantOpCityId (Sms.SendSMSReq smsMessage phoneNumber smsSender smsTemplateId messageType) >>= Sms.checkSmsResult
      Nothing -> logInfo "Driver mobile number not available, skipping SMS"

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
  sendDriverNotifications reminder driver merchantOpCityId FCM.DOCUMENT_EXPIRY_REMINDER DMM.DOCUMENT_EXPIRY_REMINDER_SMS

  -- Send GRPC notifications to fleet and operators
  sendFleetAndOperatorNotifications driver merchantOpCityId fleetOperatorTitle fleetOperatorMessage entityData

-- | For vehicle-related reminders: notify current RC-associated drivers, or if none then RC fleet owner + operators, or fallback to reminder driver.
sendVehicleDocumentExpiryNotification ::
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
sendVehicleDocumentExpiryNotification reminder fallbackDriver merchantOpCityId isExpiredParam mbDaysBefore = do
  let rcId = Id @DVRC.VehicleRegistrationCertificate reminder.entityId
  rcAssocs <- QDRCAExtra.findAllActiveAssociationByRCId rcId
  drivers <- mapM (QPerson.findById . (.driverId)) rcAssocs
  let currentDrivers = catMaybes drivers
  if null currentDrivers
    then do
      mbRC <- QVRC.findById rcId
      case mbRC >>= (.fleetOwnerId) of
        Nothing -> sendDocumentExpiryNotification reminder fallbackDriver merchantOpCityId isExpiredParam mbDaysBefore
        Just fleetOwnerIdText -> do
          fleetOwner <- QPerson.findById (Id @DP.Person fleetOwnerIdText) >>= fromMaybeM (PersonDoesNotExist fleetOwnerIdText)
          sendDocumentExpiryNotification reminder fleetOwner merchantOpCityId isExpiredParam mbDaysBefore
          operators <- QFOA.findAllByFleetOwnerId (Id @DP.Person fleetOwnerIdText) True
          let documentTypeName = show reminder.documentType
              title = if isExpiredParam then "Vehicle Document Expired" else "Vehicle Document Expiring Soon"
              message =
                if isExpiredParam
                  then "A vehicle's " <> documentTypeName <> " in your fleet has expired. Please ensure an updated document is uploaded."
                  else "A vehicle's " <> documentTypeName <> " in your fleet will expire in " <> show (fromMaybe 0 mbDaysBefore) <> " days."
              entityData = Aeson.object ["driverId" Aeson..= fleetOwner.id.getId, "documentType" Aeson..= documentTypeName, "isExpired" Aeson..= isExpiredParam]
          notifyWithGRPCProvider merchantOpCityId Notification.DRIVER_NOTIFY title message fleetOwner.id entityData
          forM_ operators $ \op -> notifyWithGRPCProvider merchantOpCityId Notification.DRIVER_NOTIFY title message (Id op.operatorId) entityData
    else forM_ currentDrivers $ \d -> sendDocumentExpiryNotification reminder d merchantOpCityId isExpiredParam mbDaysBefore

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
  FCM.FCMNotificationType ->
  DMM.MessageKey ->
  Text ->
  DRC.ReminderConfig ->
  m ()
sendInspectionOrTrainingNotification reminder driver merchantOpCityId notificationType messageKey displayName reminderConfig = do
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
          entityData = Aeson.object ["driverId" Aeson..= driver.id.getId, "documentType" Aeson..= show @Text notificationType, "isOverdue" Aeson..= isOverdue, "daysBeforeDue" Aeson..= daysBeforeDue]

      -- Send FCM and SMS to driver
      sendDriverNotifications reminder driver merchantOpCityId notificationType messageKey

      -- Send GRPC notifications to fleet and operators
      sendFleetAndOperatorNotifications driver merchantOpCityId fleetOperatorTitle fleetOperatorMessage entityData
