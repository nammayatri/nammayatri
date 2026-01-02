module Domain.Action.UI.Sos where

import API.Types.UI.Sos
import AWS.S3 as S3
import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.Foldable as Foldable
import qualified Data.HashMap.Strict as HM
import qualified Data.List as List
import Data.Text as T
import Data.Time.Format
import qualified Domain.Action.UI.Call as DUCall
import qualified Domain.Action.UI.FollowRide as DFR
import qualified Domain.Action.UI.PersonDefaultEmergencyNumber as DPDEN
import qualified Domain.Action.UI.Profile as DP
import qualified Domain.Types.CallStatus as DCall
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MerchantServiceConfig as DMSC
import qualified Domain.Types.Person as Person
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.RiderConfig as DRC
import qualified Domain.Types.Sos as DSos
import Environment
import qualified EulerHS.Language as L
import EulerHS.Prelude (withFile)
import EulerHS.Types (base64Encode)
import GHC.IO.Handle (hFileSize)
import GHC.IO.IOMode (IOMode (..))
import qualified IssueManagement.Common as IC
import qualified IssueManagement.Domain.Types.MediaFile as DMF
import qualified IssueManagement.Storage.Queries.MediaFile as MFQuery
import Kernel.Beam.Functions
import Kernel.External.Encryption
import qualified Kernel.External.IncidentReport as IncidentReport
import Kernel.External.IncidentReport.Interface.Types as IncidentReportTypes
import qualified Kernel.External.Maps.Types as Maps
import qualified Kernel.External.Notification as Notification
import Kernel.External.Ticket.Interface.Types as Ticket
import Kernel.Prelude
import Kernel.ServantMultipart
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Types.APISuccess as APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import SharedLogic.JobScheduler
import qualified SharedLogic.MessageBuilder as MessageBuilder
import SharedLogic.Person as SLP
import SharedLogic.PersonDefaultEmergencyNumber as SPDEN
import SharedLogic.Scheduler.Jobs.CallPoliceApi
import SharedLogic.Scheduler.Jobs.SafetyCSAlert as SIVR
import SharedLogic.SosLocationTracking as SOSLocation
import Storage.Beam.IssueManagement ()
import Storage.Beam.SchedulerJob ()
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.RiderConfig as QRC
import qualified Storage.CachedQueries.Sos as CQSos
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.CallStatus as QCallStatus
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.PersonDefaultEmergencyNumber as QPDEN
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.SafetySettings as QSafety
import qualified Storage.Queries.Sos as QSos
import qualified Tools.Call as Call
import Tools.Error
import qualified Tools.Notifications as Notify
import Tools.Ticket as Ticket

data SOSVideoUploadReq = SOSVideoUploadReq
  { payload :: FilePath,
    fileType :: S3.FileType,
    fileExtension :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance FromMultipart Tmp SOSVideoUploadReq where
  fromMultipart form = do
    fileData <- lookupFile "payload" form
    let mimeType = fdFileCType fileData
    let file = fdPayload fileData
    let fileExtension = getFileExtension mimeType
    fileType <- validateContentType mimeType
    return $ SOSVideoUploadReq file fileType fileExtension
    where
      validateContentType = \case
        "video/mp4" -> Right S3.Video
        "audio/wave" -> Right S3.Audio
        "audio/mpeg" -> Right S3.Audio
        "audio/mp4" -> Right S3.Audio
        _ -> Left "Unsupported file format"

      getFileExtension :: Text -> Text
      getFileExtension = T.takeWhileEnd (/= '/')

instance ToMultipart Tmp SOSVideoUploadReq where
  toMultipart sosVideoUploadReq =
    MultipartData
      []
      [FileData (show sosVideoUploadReq.fileType) "" (show sosVideoUploadReq.fileType) (sosVideoUploadReq.payload)]

newtype AddSosVideoRes = AddSosVideoRes
  { fileUrl :: Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

getSosGetDetails :: (Maybe (Id Person.Person), Id Merchant.Merchant) -> Id DRide.Ride -> Flow SosDetailsRes
getSosGetDetails (mbPersonId, _) rideId_ = do
  personId_ <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  mbSosDetails <- CQSos.findByRideId rideId_
  case mbSosDetails of
    Nothing -> do
      mockSos :: Maybe DSos.SosMockDrill <- Redis.safeGet $ CQSos.mockSosKey personId_
      case mockSos of
        Nothing -> return SosDetailsRes {sos = Nothing}
        Just mSos -> do
          now <- getCurrentTime
          return SosDetailsRes {sos = Just $ buildMockSos mSos now}
    Just sosDetails -> do
      unless (personId_ == sosDetails.personId) $ throwError $ InvalidRequest "PersonId not same"
      return SosDetailsRes {sos = Just sosDetails}
  where
    buildMockSos :: DSos.SosMockDrill -> UTCTime -> DSos.Sos
    buildMockSos mockSos now =
      DSos.Sos
        { flow = DSos.SafetyFlow,
          id = "mock-sos",
          personId = mockSos.personId,
          rideId = Just rideId_,
          status = mockSos.status,
          ticketId = Nothing,
          mediaFiles = [],
          merchantId = Nothing,
          merchantOperatingCityId = Nothing,
          trackingExpiresAt = Nothing,
          createdAt = now,
          updatedAt = now
        }

postSosCreate :: (Maybe (Id Person.Person), Id Merchant.Merchant) -> SosReq -> Flow SosRes
postSosCreate (mbPersonId, _merchantId) req = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  person <- QP.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  Redis.del $ CQSos.mockSosKey personId
  safetySettings <- QSafety.findSafetySettingsWithFallback personId (Just person)
  (sosId, trackLink, mbRideEndTime) <- case req.rideId of
    Just rideId -> do
      ride <- QRide.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
      booking <- QBooking.findById ride.bookingId >>= fromMaybeM (BookingDoesNotExist ride.bookingId.getId)
      riderConfig <- QRC.findByMerchantOperatingCityIdInRideFlow person.merchantOperatingCityId booking.configInExperimentVersions >>= fromMaybeM (RiderConfigDoesNotExist person.merchantOperatingCityId.getId)
      let trackLink' = Notify.buildTrackingUrl ride.id [("vp", "shareRide")] riderConfig.trackingShortUrlPattern
      let localRideEndTime = addUTCTime (secondsToNominalDiffTime riderConfig.timeDiffFromUtc) <$> ride.rideEndTime
      sosId' <- createTicketForNewSos person ride riderConfig trackLink' req
      return (sosId', trackLink', localRideEndTime)
    Nothing -> do
      riderConfig <- QRC.findByMerchantOperatingCityId person.merchantOperatingCityId Nothing >>= fromMaybeM (RiderConfigDoesNotExist person.merchantOperatingCityId.getId)
      sosDetails <- SIVR.buildSosDetails person req Nothing
      void $ QSos.create sosDetails

      now <- getCurrentTime
      let eightHoursInSeconds :: Int = 8 * 60 * 60
      let maxTrackingExpiresAt = addUTCTime (fromIntegral eightHoursInSeconds) now
      let cappedTrackingExpiresAt = case req.trackingExpiresAt of
            Just requestedExpiry -> Just $ min requestedExpiry maxTrackingExpiresAt
            Nothing -> Nothing
      whenJust cappedTrackingExpiresAt $ \expiry ->
        void $ QSos.updateTrackingExpiresAt (Just expiry) sosDetails.id

      whenJust req.customerLocation $ \location -> do
        SOSLocation.updateSosRiderLocation sosDetails.id location Nothing cappedTrackingExpiresAt
      let finalTrackLink = buildSosTrackingUrl sosDetails.id riderConfig.trackingShortUrlPattern
      return (sosDetails.id, finalTrackLink, Nothing)
  buildSmsReq <-
    MessageBuilder.buildSOSAlertMessage person.merchantOperatingCityId $
      MessageBuilder.BuildSOSAlertMessageReq
        { userName = SLP.getName person,
          rideLink = trackLink,
          rideEndTime = T.pack . formatTime defaultTimeLocale "%e-%-m-%Y %-I:%M%P" <$> mbRideEndTime,
          isRideEnded = fromMaybe False req.isRideEnded
        }
  case req.rideId of
    Just _ -> do
      when (triggerShareRideAndNotifyContacts safetySettings) $ do
        emergencyContacts <- DP.getDefaultEmergencyNumbers (personId, person.merchantId)
        when (req.isRideEnded /= Just True) $ do
          void $ QPDEN.updateShareRideForAll personId True
          enableFollowRideInSos emergencyContacts.defaultEmergencyNumbers
        let sosType = if req.isRideEnded == Just True then Notification.POST_RIDE_SOS_ALERT else Notification.SOS_TRIGGERED
        when shouldNotifyContacts $ SPDEN.notifyEmergencyContacts person (notificationBody person) notificationTitle sosType (Just buildSmsReq) True emergencyContacts.defaultEmergencyNumbers Nothing
    Nothing -> do
      emergencyContacts <- DP.getDefaultEmergencyNumbers (personId, person.merchantId)
      SPDEN.notifyEmergencyContacts person (notificationBody person) notificationTitle Notification.SOS_TRIGGERED (Just buildSmsReq) True emergencyContacts.defaultEmergencyNumbers (Just sosId)
  return $
    SosRes
      { sosId = sosId
      }
  where
    triggerShareRideAndNotifyContacts safetySettings = (fromMaybe safetySettings.notifySosWithEmergencyContacts req.notifyAllContacts) && req.flow == DSos.SafetyFlow
    suffixNotificationBody = case req.rideId of
      Just _ -> if req.isRideEnded /= Just True then " has initiated an SOS. Tap to follow and respond to the emergency situation" else " has activated SOS after their Namma Yatri ride. Please check on their safety"
      Nothing -> " has initiated an SOS. Tap to track their live location and respond to the emergency situation"
    notificationBody person_ = SLP.getName person_ <> suffixNotificationBody
    notificationTitle = "SOS Alert"
    shouldNotifyContacts = bool True (req.sendPNOnPostRideSOS == Just True) (req.isRideEnded == Just True)

enableFollowRideInSos :: [DPDEN.PersonDefaultEmergencyNumberAPIEntity] -> Flow ()
enableFollowRideInSos emergencyContacts = do
  mapM_
    ( \contact -> do
        case contact.contactPersonId of
          Nothing -> pure ()
          Just id -> do
            contactPersonEntity <- QP.findById id >>= fromMaybeM (PersonDoesNotExist id.getId)
            DFR.updateFollowDetails contactPersonEntity contact
    )
    emergencyContacts

createTicketForNewSos :: Person.Person -> DRide.Ride -> DRC.RiderConfig -> Text -> SosReq -> Flow (Id DSos.Sos)
createTicketForNewSos person ride riderConfig trackLink req = do
  sosRes <- CQSos.findByRideId ride.id
  case sosRes of
    Just sosDetails -> do
      void $ QSos.updateStatus DSos.Pending sosDetails.id
      void $ callUpdateTicket person sosDetails $ Just "SOS Re-Activated"
      when (req.flow == DSos.SafetyFlow) $ CQSos.cacheSosIdByRideId ride.id $ sosDetails {DSos.status = DSos.Pending}
      return sosDetails.id
    Nothing -> do
      phoneNumber <- mapM decrypt person.mobileNumber
      let rideInfo = SIVR.buildRideInfo ride person phoneNumber
          kaptureQueue = fromMaybe riderConfig.kaptureConfig.queue riderConfig.kaptureConfig.sosQueue
      ticketId <- do
        if riderConfig.enableSupportForSafety
          then do
            ticketResponse <- withTryCatch "createTicket:sosTrigger" (createTicket person.merchantId person.merchantOperatingCityId (SIVR.mkTicket person phoneNumber ["https://" <> trackLink] rideInfo req.flow riderConfig.kaptureConfig.disposition kaptureQueue))
            case ticketResponse of
              Right ticketResponse' -> return (Just ticketResponse'.ticketId)
              Left _ -> return Nothing
          else return Nothing
      sosDetails <- SIVR.buildSosDetails person req ticketId
      when (req.flow == DSos.SafetyFlow) $ CQSos.cacheSosIdByRideId ride.id sosDetails
      void $ QSos.create sosDetails
      return sosDetails.id

postSosStatus :: (Maybe (Id Person.Person), Id Merchant.Merchant) -> Id DSos.Sos -> SosUpdateReq -> Flow APISuccess.APISuccess
postSosStatus (mbPersonId, _) sosId req = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  person <- QP.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  sosDetails <- runInReplica $ QSos.findById sosId >>= fromMaybeM (SosIdDoesNotExist sosId.getId)
  unless (personId == sosDetails.personId) $ throwError $ InvalidRequest "PersonId not same"
  void $ QSos.updateStatus req.status sosId
  void $ callUpdateTicket person sosDetails req.comment
  pure APISuccess.Success

postSosMarkRideAsSafe :: (Maybe (Id Person.Person), Id Merchant.Merchant) -> Id DSos.Sos -> MarkAsSafeReq -> Flow APISuccess.APISuccess
postSosMarkRideAsSafe (mbPersonId, merchantId) sosId MarkAsSafeReq {..} = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  emergencyContacts <- DP.getDefaultEmergencyNumbers (personId, merchantId)
  person <- QP.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  safetySettings <- QSafety.findSafetySettingsWithFallback personId (Just person)
  case isMock of
    Just True -> do
      mockSos :: Maybe DSos.SosMockDrill <- Redis.safeGet $ CQSos.mockSosKey personId
      case mockSos of
        Nothing -> pure ()
        Just _ -> do
          Redis.setExp (CQSos.mockSosKey personId) (DSos.SosMockDrill {personId, status = DSos.MockResolved}) 13400
      SPDEN.notifyEmergencyContacts person (notificationBody person) notificationTitle Notification.SOS_RESOLVED Nothing False emergencyContacts.defaultEmergencyNumbers Nothing
      return APISuccess.Success
    _ -> do
      sosDetails <- runInReplica $ QSos.findById sosId >>= fromMaybeM (SosIdDoesNotExist sosId.getId)
      when (sosDetails.status == DSos.Resolved) $ throwError $ InvalidRequest "Sos already resolved."
      void $ callUpdateTicket person sosDetails $ Just "Mark Ride as Safe"
      void $ QSos.updateStatus DSos.Resolved sosId
      whenJust sosDetails.rideId $ \rideId ->
        CQSos.cacheSosIdByRideId rideId $ sosDetails {DSos.status = DSos.Resolved}
      SOSLocation.clearSosRiderLocation sosId
      case sosDetails.rideId of
        Just _ -> do
          when (safetySettings.notifySosWithEmergencyContacts && isRideEnded /= Just True) $ do
            SPDEN.notifyEmergencyContacts person (notificationBody person) notificationTitle Notification.SOS_RESOLVED Nothing False emergencyContacts.defaultEmergencyNumbers Nothing
        Nothing -> do
          SPDEN.notifyEmergencyContacts person (notificationBody person) notificationTitle Notification.SOS_RESOLVED Nothing False emergencyContacts.defaultEmergencyNumbers (Just sosId)
      pure APISuccess.Success
  where
    notificationBody person_ =
      SLP.getName person_
        <> if fromMaybe False isMock
          then " has marked ride as safe in test safety drill. This is a practice exercise, not a real emergency situation."
          else " has marked the ride as safe. Tap to view the ride details"
    notificationTitle = if fromMaybe False isMock then "Test Safety Drill Alert" else "Ride Safe"

postSosCreateMockSos :: (Maybe (Id Person.Person), Id Merchant.Merchant) -> MockSosReq -> Flow APISuccess.APISuccess
postSosCreateMockSos (mbPersonId, _) MockSosReq {..} = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  person <- QP.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  emergencyContacts <- DP.getDefaultEmergencyNumbers (personId, person.merchantId)
  safetySettings <- QSafety.findSafetySettingsWithFallback personId (Just person)
  case startDrill of
    Just True -> do
      SPDEN.notifyEmergencyContacts person (notificationBody person True) notificationTitle Notification.SOS_MOCK_DRILL_NOTIFY Nothing False emergencyContacts.defaultEmergencyNumbers Nothing
      when (fromMaybe False onRide) $ do
        void $ QPDEN.updateShareRideForAll personId True
        enableFollowRideInSos emergencyContacts.defaultEmergencyNumbers
    _ -> do
      unless (fromMaybe False safetySettings.hasCompletedMockSafetyDrill) $ QSafety.updateMockSafetyDrillStatus (Just True) personId
      when (fromMaybe False onRide) $ do
        let mockEntity = DSos.SosMockDrill {personId, status = DSos.MockPending}
        Redis.setExp (CQSos.mockSosKey personId) mockEntity 13400
      SPDEN.notifyEmergencyContacts person (notificationBody person False) notificationTitle Notification.SOS_MOCK_DRILL Nothing False emergencyContacts.defaultEmergencyNumbers Nothing
  pure APISuccess.Success
  where
    notificationBody person_ isStartDrill =
      SLP.getName person_
        <> if isStartDrill
          then " is going to start a test safety drill with you. Tap to follow the test ride. This is a practice exercise, and not a real ride."
          else " has initiated a test safety drill with you. This is a practice exercise, not a real emergency situation..."
    notificationTitle = "Test Safety Drill Alert"

uploadMedia :: Id DSos.Sos -> Id Person.Person -> SOSVideoUploadReq -> Flow AddSosVideoRes
uploadMedia sosId personId SOSVideoUploadReq {..} = do
  sosDetails <- runInReplica $ QSos.findById sosId >>= fromMaybeM (SosIdDoesNotExist sosId.getId)
  person <- runInReplica $ QP.findById personId >>= fromMaybeM (PersonNotFound (getId personId))
  merchantConfig <- CQM.findById (person.merchantId) >>= fromMaybeM (MerchantNotFound person.merchantId.getId)
  riderConfig <- QRC.findByMerchantOperatingCityId person.merchantOperatingCityId Nothing >>= fromMaybeM (RiderConfigDoesNotExist person.merchantOperatingCityId.getId)
  fileSize <- L.runIO $ withFile payload ReadMode hFileSize
  when (fileSize > fromIntegral riderConfig.videoFileSizeUpperLimit) $
    throwError $ FileSizeExceededError (show fileSize)
  mediaFile <- L.runIO $ base64Encode <$> BS.readFile payload
  filePath <- createFilePath "/sos/" ("sos-" <> getId sosId) fileType fileExtension
  mediaFileId <- generateGUID
  now <- getCurrentTime
  let fileUrl =
        merchantConfig.mediaFileUrlPattern
          & T.replace "<DOMAIN>" "sos"
          & T.replace "<FILE_PATH>" filePath
      fileEntity =
        DMF.MediaFile
          { id = mediaFileId,
            _type = fileType,
            url = fileUrl,
            s3FilePath = Just filePath,
            createdAt = now
          }
  result <- withTryCatch "S3:put:uploadSosMedia" $ S3.put (T.unpack filePath) mediaFile
  case result of
    Left err -> throwError $ InternalError ("S3 Upload Failed: " <> show err)
    Right _ -> do
      MFQuery.create fileEntity
      let updatedMediaFiles = sosDetails.mediaFiles <> [mediaFileId]
      void $ QSos.updateMediaFiles updatedMediaFiles sosId
      phoneNumber <- mapM decrypt person.mobileNumber
      let kaptureQueue = fromMaybe riderConfig.kaptureConfig.queue riderConfig.kaptureConfig.sosQueue
          dashboardFileUrl =
            maybe
              []
              ( \patternS ->
                  [ patternS
                      & T.replace "<FILE_PATH>" filePath
                  ]
              )
              riderConfig.dashboardMediaFileUrlPattern
      when riderConfig.enableSupportForSafety $ do
        whenJust sosDetails.rideId $ \rideId -> do
          ride <- QRide.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
          let rideInfo = SIVR.buildRideInfo ride person phoneNumber
              trackLink = Notify.buildTrackingUrl ride.id [("vp", "shareRide")] riderConfig.trackingShortUrlPattern
          void $ withTryCatch "createTicket:sendSosTracking" $ withShortRetry (createTicket person.merchantId person.merchantOperatingCityId (SIVR.mkTicket person phoneNumber (["https://" <> trackLink] <> dashboardFileUrl) rideInfo DSos.AudioRecording riderConfig.kaptureConfig.disposition kaptureQueue))
      return $ AddSosVideoRes {fileUrl = fileUrl}

callUpdateTicket :: Person.Person -> DSos.Sos -> Maybe Text -> Flow APISuccess.APISuccess
callUpdateTicket person sosDetails mbComment = do
  case sosDetails.ticketId of
    Just ticketId -> do
      fork "update ticket request" $
        void $ Ticket.updateTicket (person.merchantId) person.merchantOperatingCityId (Ticket.UpdateTicketReq (fromMaybe "" mbComment) ticketId Ticket.IN)
      pure APISuccess.Success
    Nothing -> pure APISuccess.Success

getSosIvrOutcome :: Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Flow APISuccess.APISuccess
getSosIvrOutcome mbCallFrom mbCallSid mbCallStatus mbDigitPressed = do
  logDebug $ "IVR response from Exotel: " <> show mbCallSid <> ", " <> show mbDigitPressed <> ", " <> show mbCallStatus <> ", " <> show mbCallFrom
  when (isNothing mbCallSid) $
    throwError CallSidNullError
  let callSid = fromMaybe "" mbCallSid
      validStatus = fromMaybe Call.INVALID_STATUS $ (A.decode . A.encode) =<< mbCallStatus
  callRecord <- QCallStatus.findByCallId callSid
  case callRecord of
    Just res -> processCallRecord callSid mbDigitPressed validStatus res
    Nothing -> throwError $ CallRecordNotFoundError callSid
  pure APISuccess.Success
  where
    processCallRecord :: Text -> Maybe Text -> Call.CallStatus -> DCall.CallStatus -> Flow ()
    processCallRecord callSid mbdigitPressed validStatus res = do
      let digitPressed = fromMaybe "1" $ T.replace "\"" "" <$> mbdigitPressed
      QCallStatus.updateCustomerIvrResponse callSid (Just digitPressed) validStatus
      logDebug $ "digitPressed : " <> digitPressed
      case res.rideId of
        Just rideId -> do
          if digitPressed /= "1"
            then handleEmergencyCall rideId
            else do
              logDebug $ "In exotel response customer pressed : " <> show digitPressed <> " and marking ride as safe."
              void $ QRide.updateSafetyJourneyStatus rideId DRide.Safe
        Nothing -> throwError $ RideIdEmptyInCallRecord callSid

    handleEmergencyCall :: Id DRide.Ride -> Flow ()
    handleEmergencyCall rideId = do
      ride <- runInReplica $ QRide.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
      booking <- runInReplica $ QBooking.findById ride.bookingId >>= fromMaybeM (BookingDoesNotExist ride.bookingId.getId)
      void $ postSosCallPolice (Just booking.riderId, booking.merchantId) (CallPoliceAPI {rideId = rideId})

postSosCallPolice :: (Maybe (Id Person.Person), Id Merchant.Merchant) -> CallPoliceAPI -> Flow APISuccess.APISuccess
postSosCallPolice (mbPersonId, merchantId) CallPoliceAPI {..} = do
  logDebug "User didn't pressed 1 on exotel call"
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  person <- runInReplica $ QP.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  ride <- runInReplica $ QRide.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  booking <- runInReplica $ QBooking.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
  let merchantOpCityId = booking.merchantOperatingCityId
  riderConfig <- QRC.findByMerchantOperatingCityIdInRideFlow merchantOpCityId booking.configInExperimentVersions >>= fromMaybeM (RiderConfigDoesNotExist merchantOpCityId.getId)
  if riderConfig.incidentReportSupport
    then do
      coordinates <- fetchLatLong ride merchantId
      token <- getTokenofJMService merchantId merchantOpCityId
      incidentReportHandler person merchantId merchantOpCityId ride.id token coordinates riderConfig
      void $ QRide.updateSafetyJourneyStatus rideId DRide.PoliceMonitoring
    else do
      logDebug $ "Incident Report Support is not enabled for this merchant so creating ticket for rideID : " <> show ride.id
      SIVR.createSafetyTicket person ride
  return APISuccess.Success

incidentReportHandler :: Person.Person -> Id Merchant.Merchant -> Id DMOC.MerchantOperatingCity -> Id DRide.Ride -> Text -> Maps.LatLong -> DRC.RiderConfig -> Flow ()
incidentReportHandler person merchantId merchantOpCityId rideId token coordinates riderConfig = do
  logDebug "Initiating Incident Report Police Call"
  merchantIRSvcCfg <- getServiceConfig merchantId merchantOpCityId (DMSC.IncidentReportService ERSS)
  contactNo <- fmap (fromMaybe "0000000000") $ traverse decrypt person.mobileNumber
  res <-
    IncidentReport.reportIncident
      merchantIRSvcCfg
      IncidentReportTypes.IncidentReportReq
        { latitude = coordinates.lat,
          longitude = coordinates.lon,
          mpin = "",
          contactNo = contactNo,
          token = token
        }
  logDebug $ "Incident Report API Response: " <> show res
  scheduleIncidentFollowUp res
  where
    scheduleIncidentFollowUp :: IncidentReportRes -> Flow ()
    scheduleIncidentFollowUp res = do
      let scheduleAfter = max 2 riderConfig.policeTriggerDelay
          callPoliceAPIJobData = CallPoliceApiJobData {rideId, personId = person.id, jmCode = res.incidentData.jmCode}
      logDebug $ "Scheduling safety alert police call after : " <> show scheduleAfter
      Redis.withCrossAppRedis $ Redis.setExp (mkRideCallPoliceAPIKey rideId) (1 :: Int) riderConfig.hardLimitForSafetyJobs
      createJobIn @_ @'CallPoliceApi (Just merchantId) (Just merchantOpCityId) scheduleAfter callPoliceAPIJobData
      pure ()

sendUnattendedSosTicketAlert :: Text -> Flow ()
sendUnattendedSosTicketAlert ticketId = do
  unattendedSosRedisKey :: Maybe Bool <- Redis.safeGet mkUnattendedSosAlertKey
  case unattendedSosRedisKey of
    Just _value -> do
      logTagInfo "Unattended SOS Ticket Alert" ("Alert discarded for Ticket ID: " <> ticketId)
      return ()
    Nothing -> do
      Redis.setExp mkUnattendedSosAlertKey True 60
      sos <- QSos.findByTicketId (Just ticketId) >>= fromMaybeM (InvalidRequest $ "SOS with ticketId-" <> ticketId <> " does not exist.")
      merchantOpCityId <-
        maybe
          ( ( QP.findById sos.personId
                >>= fromMaybeM (PersonNotFound sos.personId.getId)
            )
              <&> (.merchantOperatingCityId)
          )
          return
          sos.merchantOperatingCityId
      merchantOperatingCity <- CQMOC.findById merchantOpCityId >>= fromMaybeM (MerchantOperatingCityNotFound merchantOpCityId.getId)
      riderConfig <- QRC.findByMerchantOperatingCityId merchantOpCityId Nothing >>= fromMaybeM (RiderConfigDoesNotExist merchantOpCityId.getId)
      let maybeAppId = (HM.lookup DRC.UnattendedTicketAppletID . DRC.exotelMap) =<< riderConfig.exotelAppIdMapping
      mapM_ (sendAlert merchantOperatingCity sos maybeAppId) (fromMaybe [] riderConfig.cxAgentDetails)
  where
    sendAlert :: DMOC.MerchantOperatingCity -> DSos.Sos -> Maybe Text -> IC.CxAgentDetails -> Flow ()
    sendAlert merchantOpCity sos maybeAppId cxAgentDetails =
      whenJust sos.rideId $ \rideId -> do
        fork ("Sending unattended sos ticket alert to agentDetails - " <> show cxAgentDetails) $ do
          callStatusId <- generateGUID
          let callReq =
                Call.InitiateCallReq
                  { fromPhoneNum = cxAgentDetails.agentMobileNumber,
                    toPhoneNum = Nothing,
                    attachments = Call.Attachments $ DUCall.CallAttachments {callStatusId = callStatusId, rideId = rideId},
                    appletId = maybeAppId
                  }
          exotelResponse <- Call.initiateCall merchantOpCity.merchantId merchantOpCity.id callReq
          callStatus <- buildCallStatus callStatusId exotelResponse sos merchantOpCity.id
          QCallStatus.create callStatus

    buildCallStatus :: Id DCall.CallStatus -> Call.InitiateCallResp -> DSos.Sos -> Id DMOC.MerchantOperatingCity -> Flow DCall.CallStatus
    buildCallStatus callStatusId exotelResponse sos merchantOpCityId = do
      now <- getCurrentTime
      return $
        DCall.CallStatus
          { id = callStatusId,
            callId = exotelResponse.callId,
            rideId = sos.rideId,
            dtmfNumberUsed = Nothing,
            status = exotelResponse.callStatus,
            callAttempt = Nothing,
            conversationDuration = 0,
            recordingUrl = Nothing,
            merchantId = sos.merchantId <&> (.getId),
            merchantOperatingCityId = Just merchantOpCityId,
            callService = Just Call.Exotel,
            callError = Nothing,
            createdAt = now,
            updatedAt = now,
            customerIvrResponse = Nothing
          }

    mkUnattendedSosAlertKey :: Text
    mkUnattendedSosAlertKey = "Unattended:SOS:Alert:TicketId-" <> ticketId

postSosUpdateLocation :: (Maybe (Id Person.Person), Id Merchant.Merchant) -> Id DSos.Sos -> SosLocationUpdateReq -> Flow APISuccess.APISuccess
postSosUpdateLocation (mbPersonId, _) sosId req = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  sosDetails <- runInReplica $ QSos.findById sosId >>= fromMaybeM (SosIdDoesNotExist sosId.getId)
  unless (personId == sosDetails.personId) $ throwError $ InvalidRequest "PersonId not same"
  unless (sosDetails.status == DSos.Pending) $ throwError $ InvalidRequest "Can only update location for pending SOS"

  let location = Maps.LatLong {lat = req.lat, lon = req.lon}
  SOSLocation.updateSosRiderLocation sosId location req.accuracy sosDetails.trackingExpiresAt
  pure APISuccess.Success

getSosTracking :: Id DSos.Sos -> Flow SosTrackingRes
getSosTracking sosId = do
  sosDetails <- runInReplica $ QSos.findById sosId >>= fromMaybeM (SosIdDoesNotExist sosId.getId)
  unless (sosDetails.flow == DSos.SafetyFlow && isNothing sosDetails.rideId) $
    throwError $ InvalidRequest "Invalid SOS for tracking"

  now <- getCurrentTime
  let isTrackingActive =
        sosDetails.status == DSos.Pending && case sosDetails.trackingExpiresAt of
          Just expiry -> expiry > now
          Nothing -> False

  currentLocation <-
    if isTrackingActive
      then SOSLocation.getSosRiderLocation sosId
      else return Nothing

  return $
    SosTrackingRes
      { currentLocation = convertToApiLocation <$> currentLocation,
        isTrackingActive = isTrackingActive
      }
  where
    convertToApiLocation :: SOSLocation.SosLocationData -> SosLocationRes
    convertToApiLocation loc =
      SosLocationRes
        { lat = loc.lat,
          lon = loc.lon,
          accuracy = loc.accuracy
        }

getSosTrackingDetails :: (Maybe (Id Person.Person), Id Merchant.Merchant) -> Id DSos.Sos -> Flow SosTrackingDetailsRes
getSosTrackingDetails (mbPersonId, _) sosId = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  sosDetails <- runInReplica $ QSos.findById sosId >>= fromMaybeM (SosIdDoesNotExist sosId.getId)
  unless (sosDetails.flow == DSos.SafetyFlow && isNothing sosDetails.rideId) $
    throwError $ InvalidRequest "Invalid SOS for tracking"
  unless (sosDetails.status == DSos.Pending) $
    throwError $ InvalidRequest "Location sharing has expired"

  let isSosCreator = personId == sosDetails.personId
  isEmergencyContact <-
    if isSosCreator
      then return True
      else do
        emergencyContacts <- runInReplica $ QPDEN.findAllByPersonId sosDetails.personId
        return $ Foldable.any (\ec -> ec.contactPersonId == Just personId) emergencyContacts
  unless isEmergencyContact $
    throwError $ InvalidRequest "Access denied"

  expiryTimeStamp <- sosDetails.trackingExpiresAt & fromMaybeM (InvalidRequest "Tracking expiry not set")
  now <- getCurrentTime
  unless (expiryTimeStamp > now) $
    throwError $ InvalidRequest "Location sharing has expired"

  person <- QP.findById sosDetails.personId >>= fromMaybeM (PersonDoesNotExist sosDetails.personId.getId)
  phoneNumber <- mapM decrypt person.mobileNumber >>= fromMaybeM (PersonFieldNotPresent "mobileNumber")

  return $
    SosTrackingDetailsRes
      { personName = SLP.getName person,
        mobileNumber = phoneNumber,
        expiryTimeStamp = expiryTimeStamp
      }

shareLocationWithContacts ::
  (Id Person.Person, Id Merchant.Merchant) ->
  Person.Person ->
  Maybe Maps.LatLong ->
  Int ->
  [DPDEN.PersonDefaultEmergencyNumberAPIEntity] ->
  Flow ShareLocationRes
shareLocationWithContacts (personId, _merchantId) person customerLocation durationInMinutes contactsToNotify = do
  when (durationInMinutes < 1 || durationInMinutes > 1440) $
    throwError $ InvalidRequest "Duration must be between 1 and 1440 minutes (24 hours)"

  when (List.null contactsToNotify) $ throwError $ InvalidRequest "No contacts to notify"
  (sosId, _isNewSos) <- getOrCreateLocationSharingSos personId person customerLocation durationInMinutes

  riderConfig <- QRC.findByMerchantOperatingCityId person.merchantOperatingCityId Nothing >>= fromMaybeM (RiderConfigDoesNotExist person.merchantOperatingCityId.getId)
  let trackLink = buildSosTrackingUrl sosId riderConfig.trackingShortUrlPattern
  now <- getCurrentTime
  let durationSeconds = durationInMinutes * 60
  let expiresAt = addUTCTime (fromIntegral durationSeconds) now

  buildSmsReq <-
    MessageBuilder.buildSOSAlertMessage person.merchantOperatingCityId $
      MessageBuilder.BuildSOSAlertMessageReq
        { userName = SLP.getName person,
          rideLink = trackLink,
          rideEndTime = Just $ T.pack . formatTime defaultTimeLocale "%e-%-m-%Y %-I:%M%P" $ expiresAt,
          isRideEnded = False
        }

  SPDEN.notifyEmergencyContacts person (notificationBody person expiresAt) notificationTitle Notification.SHARE_RIDE (Just buildSmsReq) True contactsToNotify (Just sosId)

  return $
    ShareLocationRes
      { sosId = sosId
      }
  where
    notificationBody person_ expiresAt_ =
      SLP.getName person_
        <> " has shared their location with you. Tap to track their live location. Location sharing expires at "
        <> (T.pack . formatTime defaultTimeLocale "%I:%M %p" $ expiresAt_)
    notificationTitle = "Location Shared"

postSosShareLocationContact :: (Maybe (Id Person.Person), Id Merchant.Merchant) -> ShareLocationWithContactReq -> Flow ShareLocationRes
postSosShareLocationContact (mbPersonId, merchantId) ShareLocationWithContactReq {..} = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  person <- QP.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  emergencyContacts <- DP.getDefaultEmergencyNumbers (personId, merchantId)
  let contactToNotify = List.find (\contact -> contact.mobileNumber == mobileNumber) emergencyContacts.defaultEmergencyNumbers

  case contactToNotify of
    Nothing -> throwError $ InvalidRequest "Contact not found in emergency contacts"
    Just contact -> shareLocationWithContacts (personId, merchantId) person customerLocation durationInMinutes [contact]

postSosShareLocationAll :: (Maybe (Id Person.Person), Id Merchant.Merchant) -> ShareLocationWithAllReq -> Flow ShareLocationRes
postSosShareLocationAll (mbPersonId, merchantId) ShareLocationWithAllReq {..} = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  person <- QP.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  emergencyContacts <- DP.getDefaultEmergencyNumbers (personId, merchantId)
  shareLocationWithContacts (personId, merchantId) person customerLocation durationInMinutes emergencyContacts.defaultEmergencyNumbers

getOrCreateLocationSharingSos :: Id Person.Person -> Person.Person -> Maybe Maps.LatLong -> Int -> Flow (Id DSos.Sos, Bool)
getOrCreateLocationSharingSos personId person mbLocation durationInMinutes = do
  existingSosList <- QSos.findByPersonId personId
  let activeNonRideSos = List.find (\sos -> sos.status == DSos.Pending && isNothing sos.rideId && sos.flow == DSos.SafetyFlow) existingSosList

  now <- getCurrentTime
  let durationSeconds = durationInMinutes * 60
  let expiryTimeStamp = addUTCTime (fromIntegral durationSeconds) now

  case activeNonRideSos of
    Just sos -> do
      void $ QSos.updateTrackingExpiresAt (Just expiryTimeStamp) sos.id
      whenJust mbLocation $ \location -> do
        SOSLocation.updateSosRiderLocation sos.id location Nothing (Just expiryTimeStamp)
      return (sos.id, False)
    Nothing -> do
      sosId <- generateGUID
      let sosDetails =
            DSos.Sos
              { id = sosId,
                personId = personId,
                flow = DSos.SafetyFlow,
                status = DSos.Pending,
                rideId = Nothing,
                ticketId = Nothing,
                mediaFiles = [],
                merchantId = Just person.merchantId,
                merchantOperatingCityId = Just person.merchantOperatingCityId,
                trackingExpiresAt = Just expiryTimeStamp,
                createdAt = now,
                updatedAt = now
              }
      void $ QSos.create sosDetails
      whenJust mbLocation $ \location -> do
        SOSLocation.updateSosRiderLocation sosDetails.id location Nothing (Just expiryTimeStamp)

      return (sosDetails.id, True)

-- | Build tracking URL for SOS rider location (non-ride scenario)
-- Uses the same pattern-based approach as ride tracking for consistency
-- Pattern format: "https://nammayatri.in/u?vp={#vp#}&rideId="
-- For SOS: replaces rideId with sosId and uses vp=sosTracking
buildSosTrackingUrl :: Id DSos.Sos -> Text -> Text
buildSosTrackingUrl sosId trackingUrlPattern =
  let patternWithSosId = T.replace "rideId=" "sosId=" trackingUrlPattern
      templateText txt = "{#" <> txt <> "#}"
      builtUrl =
        Foldable.foldl'
          ( \msg (findKey, replaceVal) ->
              T.replace (templateText findKey) replaceVal msg
          )
          patternWithSosId
          [("vp", "sosTracking")]
   in builtUrl <> sosId.getId
