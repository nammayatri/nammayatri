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

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Data.Time as Time
import qualified Data.Tuple.Extra as TE
import Control.Applicative (liftA2)
import qualified Domain.Types.DocumentVerificationConfig as DVC
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantMessage as DMM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Reminder as DR
import qualified Domain.Types.ReminderConfig as DRC
import Kernel.Beam.Functions as BF
import Kernel.Beam.Lib.UtilsTH (HasSchemaName)
import Kernel.External.Encryption (decrypt)
import qualified Kernel.External.Notification as Notification
import Kernel.External.Types (SchedulerFlow, ServiceFlow)
import Kernel.Prelude
import Kernel.Sms.Config (SmsConfig)
import Kernel.Storage.Esqueleto.Config
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Scheduler
import Lib.Scheduler.JobStorageType.DB.Table (SchedulerJobT)
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import SharedLogic.Allocator (AllocatorJobType (..))
import qualified SharedLogic.Allocator as Allocator
import qualified SharedLogic.Analytics as Analytics
import Storage.Beam.SchedulerJob ()
import qualified Storage.CachedQueries.Merchant.MerchantMessage as CMM
import qualified Storage.CachedQueries.Merchant.MerchantPushNotification as CPN
import qualified Domain.Types.BusinessLicense as DBL
import qualified Domain.Types.DriverLicense as DDL
import qualified Domain.Types.VehicleFitnessCertificate as DFC
import qualified Domain.Types.VehicleInsurance as DVI
import qualified Domain.Types.VehiclePUC as DPUC
import qualified Domain.Types.VehiclePermit as DVPermit
import qualified Domain.Types.VehicleRegistrationCertificate as DVRC
import qualified Kernel.Types.Documents as Documents
import qualified Storage.Queries.Image as QImage
import qualified Storage.Queries.BusinessLicense as QBL
import qualified Storage.Queries.DriverInformation as QDriverInfo
import qualified Storage.Queries.DriverInformationExtra as QDIExtra
import qualified Storage.Queries.DriverLicense as QDL
import qualified Storage.Queries.FleetDriverAssociationExtra as QFDA
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Reminder as QReminder
import qualified Storage.Queries.ReminderConfig as QReminderConfig
import qualified Storage.Queries.VehicleFitnessCertificate as QFC
import qualified Storage.Queries.VehicleInsurance as QVI
import qualified Storage.Queries.VehiclePUC as QVPUC
import qualified Storage.Queries.VehiclePermit as QVPermit
import qualified Storage.Queries.VehicleRegistrationCertificate as QVRC
import Tools.Error (DriverOnboardingError (ImageNotValid), GenericError (InternalError))
import Kernel.Types.Error (PersonError (PersonDoesNotExist))
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

processReminder ::
  ( EsqDBReplicaFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
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

      -- Only mark as SENT if it's a document expiry reminder (they don't reschedule)
      -- Inspection/training reminders are rescheduled above, so they stay PENDING
      when (reminder.documentType `elem` documentExpiryTypes) $ do
        QReminder.updateByPrimaryKey reminder {DR.status = DR.SENT}
        logInfo $ "Reminder " <> reminderId.getId <> " sent and marked as SENT"

      return Complete

-- | Route reminder processing based on document type
processReminderByType ::
  ( EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    CacheFlow m r,
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
      DVC.VehicleInspectionForm ->
        processInspectionReminder
          reminder
          driver
          config
          merchantId
          merchantOpCityId
          "Vehicle Inspection"
          "VEHICLE_INSPECTION"
          $ do
            -- Set approved flag to False for RC
            rcId <- Id reminder.entityId & pure
            QVRC.updateApproved (Just False) rcId
            logInfo $ "Set approved = false for RC " <> reminder.entityId <> " due to expired mandatory vehicle inspection reminder"
      DVC.DriverInspectionForm ->
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
      DVC.TrainingForm ->
        processInspectionReminder
          reminder
          driver
          config
          merchantId
          merchantOpCityId
          "Training Video"
          "TRAINING_VIDEO"
          $ do
            -- Set approved flag to False for DriverInformation
            QDIExtra.updateApproved (Just False) reminder.driverId
            logInfo $ "Set approved = false for driver " <> reminder.driverId.getId <> " due to expired mandatory training reminder"
      _ -> logError $ "Unknown documentType: " <> show reminder.documentType

-- | Process inspection reminders (vehicle, driver inspection, or training)
-- Handles blocking, setting approved flags, sending notifications, and scheduling next reminder
processInspectionReminder ::
  ( EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    CacheFlow m r,
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
  -- Check if reminder is overdue and mandatory, then disable driver and set approved = false
  when (isOverdue && isMandatory) $ do
    driverInfo <- QDriverInfo.findById reminder.driverId >>= fromMaybeM (InternalError "DriverInformation not found")
    when driverInfo.enabled $ do
      QDIExtra.updateEnabledVerifiedState reminder.driverId False Nothing
      logInfo $ "Disabled driver " <> driver.id.getId <> " due to expired mandatory " <> displayName <> " reminder"
    -- Set approved flag to False (action depends on inspection type)
    setApprovedAction
  sendInspectionOrTrainingNotification reminder driver merchantOpCityId notificationKey displayName config
  -- Schedule next ProcessReminder job for 24 hours later to keep reminding until completion
  let scheduleAfter = 24 * 60 * 60 -- 24 hours in seconds
      inspectionJobData = Allocator.ProcessReminderJobData {reminderId = reminder.id, merchantId = merchantId, merchantOperatingCityId = merchantOpCityId}
  void $ createJobIn @_ @'ProcessReminder (Just merchantId) (Just merchantOpCityId) scheduleAfter inspectionJobData
  logInfo $ "Scheduled next " <> displayName <> " reminder job for driver " <> driver.id.getId <> " in 24 hours"

processDocumentExpiryReminder ::
  ( EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    CacheFlow m r,
    HasFlowEnv m r '["smsCfg" ::: SmsConfig, "maxNotificationShards" ::: Int],
    ServiceFlow m r
  ) =>
  DR.Reminder ->
  DP.Person ->
  DRC.ReminderConfig ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  m ()
processDocumentExpiryReminder reminder driver reminderConfig _merchantId merchantOpCityId = do
  now <- getCurrentTime
  let intervals = reminderConfig.reminderIntervals
      currentIntervalMinutes = intervals !! reminder.currentIntervalIndex
      daysBeforeExpiry = currentIntervalMinutes `div` (24 * 60)
      documentTypeName = show reminder.documentType

  if reminder.dueDate <= now
    then do
      -- Reminder has expired - check if mandatory before blocking
      logInfo $ "Reminder expired for driver " <> driver.id.getId <> " (documentType: " <> documentTypeName <> ")"

      -- Check if reminder is mandatory (from ReminderConfig)
      driverInfo <- QDriverInfo.findById reminder.driverId >>= fromMaybeM (InternalError "DriverInformation not found")
      let isMandatory = reminderConfig.isMandatory
      if isMandatory
        then do
          -- Disable driver (until requirement is completed) only if mandatory
          when driverInfo.enabled $ do
            QDIExtra.updateEnabledVerifiedState reminder.driverId False Nothing
            logInfo $ "Disabled driver " <> driver.id.getId <> " due to expired mandatory reminder: " <> documentTypeName
          -- Invalidate the expired document
          invalidateExpiredDocument reminder.documentType reminder.entityId
          logInfo $ "Invalidated document " <> reminder.entityId <> " (type: " <> documentTypeName <> ") due to expiry"
        else do
          -- For non-mandatory reminders, reschedule for daily (24 hours from now)
          let nextReminderDate = Time.addUTCTime (24 * 60 * 60) now
          QReminder.updateByPrimaryKey reminder {DR.reminderDate = nextReminderDate}
          logInfo $ "Non-mandatory reminder expired for driver " <> driver.id.getId <> ", rescheduling reminder for daily notifications"

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
  m ()
invalidateExpiredDocument documentType entityId = do
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
      let docId = Id @DVI.VehicleInsurance entityId
      mbInsurance <- QVI.findByPrimaryKey docId
      Kernel.Prelude.whenJust mbInsurance $ \insurance -> do
        QVI.updateVerificationStatusAndRejectReason Documents.INVALID expiryReason insurance.documentImageId
        QImage.updateVerificationStatusAndFailureReason Documents.INVALID (ImageNotValid expiryReason) insurance.documentImageId
    DVC.VehiclePermit -> do
      let docId = Id @DVPermit.VehiclePermit entityId
      mbPermit <- QVPermit.findByPrimaryKey docId
      Kernel.Prelude.whenJust mbPermit $ \permit -> do
        QVPermit.updateVerificationStatusByImageId Documents.INVALID permit.documentImageId
        QImage.updateVerificationStatusAndFailureReason Documents.INVALID (ImageNotValid expiryReason) permit.documentImageId
    DVC.VehiclePUC -> do
      let docId = Id @DPUC.VehiclePUC entityId
      mbPUC <- QVPUC.findByPrimaryKey docId
      Kernel.Prelude.whenJust mbPUC $ \puc -> do
        QVPUC.updateVerificationStatusByImageId Documents.INVALID puc.documentImageId
        QImage.updateVerificationStatusAndFailureReason Documents.INVALID (ImageNotValid expiryReason) puc.documentImageId
    DVC.VehicleFitnessCertificate -> do
      let docId = Id @DFC.VehicleFitnessCertificate entityId
      mbFitness <- QFC.findByPrimaryKey docId
      Kernel.Prelude.whenJust mbFitness $ \fitness -> do
        QFC.updateVerificationStatus Documents.INVALID fitness.documentImageId
        QImage.updateVerificationStatusAndFailureReason Documents.INVALID (ImageNotValid expiryReason) fitness.documentImageId
    DVC.BusinessLicense -> do
      let docId = Id @DBL.BusinessLicense entityId
      mbBL <- QBL.findByPrimaryKey docId
      Kernel.Prelude.whenJust mbBL $ \bl -> do
        QBL.updateVerificationStatusByImageId Documents.INVALID bl.documentImageId
        QImage.updateVerificationStatusAndFailureReason Documents.INVALID (ImageNotValid expiryReason) bl.documentImageId
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
              Sms.sendSMS (Id "") merchantOpCityId (Sms.SendSMSReq merchantMsg.message phoneNumber sender merchantMsg.templateId) >>= Sms.checkSmsResult
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
      currentIntervalMinutes = intervals !! reminder.currentIntervalIndex
      daysBeforeDue = currentIntervalMinutes `div` (24 * 60)
      isOverdue = reminder.dueDate <= now
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
