module Domain.Action.UI.Sos where

import API.Types.UI.Sos
import AWS.S3 as S3
import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.Foldable as Foldable
import qualified Data.HashMap.Strict as HM
import qualified Data.List as List
import Data.Text as T hiding (map)
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
-- Domain.Types.Sos removed - using Safety.Domain.Types.Sos everywhere
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
import Kernel.Utils.SlidingWindowLimiter
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import qualified Safety.Domain.Action.UI.Sos as SafetySos
import qualified Safety.Domain.Types.Common as SafetyCommon
import qualified Safety.Domain.Types.Sos as SafetyDSos
import SharedLogic.JobScheduler
import qualified SharedLogic.MessageBuilder as MessageBuilder
import SharedLogic.Person as SLP
import SharedLogic.PersonDefaultEmergencyNumber as SPDEN
import SharedLogic.Scheduler.Jobs.CallPoliceApi
import SharedLogic.Scheduler.Jobs.SafetyCSAlert as SIVR
import SharedLogic.SosLocationTracking as SOSLocation
import Storage.Beam.IssueManagement ()
import Storage.Beam.SchedulerJob ()
import Storage.Beam.Sos ()
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
      mockSos :: Maybe SafetyDSos.SosMockDrill <- Redis.safeGet $ CQSos.mockSosKey personId_
      case mockSos of
        Nothing -> return SosDetailsRes {sos = Nothing}
        Just mSos -> do
          now <- getCurrentTime
          return SosDetailsRes {sos = Just $ buildMockSos mSos now}
    Just sosDetails -> do
      unless (personId_ == cast sosDetails.personId) $ throwError $ InvalidRequest "PersonId not same"
      return SosDetailsRes {sos = Just sosDetails}
  where
    buildMockSos :: SafetyDSos.SosMockDrill -> UTCTime -> SafetyDSos.Sos
    buildMockSos mockSos now =
      SafetyDSos.Sos
        { flow = SafetyDSos.SafetyFlow,
          id = Id "mock-sos",
          personId = cast mockSos.personId,
          rideId = Just (cast rideId_),
          status = mockSos.status,
          ticketId = Nothing,
          mediaFiles = [],
          merchantId = Nothing,
          merchantOperatingCityId = Nothing,
          trackingExpiresAt = Nothing,
          sosState = Nothing,
          entityType = Nothing,
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
      now <- getCurrentTime
      let eightHoursInSeconds :: Int = 8 * 60 * 60
      let trackingExpiresAt = addUTCTime (fromIntegral eightHoursInSeconds) now
      let sosDetailsWithEntityType = sosDetails {SafetyDSos.entityType = Just SafetyDSos.NonRide, SafetyDSos.sosState = Just SafetyDSos.SosActive, SafetyDSos.trackingExpiresAt = Just trackingExpiresAt}
      void $ SafetySos.createSos sosDetailsWithEntityType

      whenJust req.customerLocation $ \location -> do
        SOSLocation.updateSosRiderLocation sosDetailsWithEntityType.id location Nothing (Just trackingExpiresAt)
      let finalTrackLink = buildSosTrackingUrl sosDetailsWithEntityType.id riderConfig.trackingShortUrlPattern
      return (sosDetailsWithEntityType.id, finalTrackLink, Nothing)
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
        when shouldNotifyContacts $ SPDEN.notifyEmergencyContactsWithKey person "SOS_ALERT" sosType [("userName", SLP.getName person)] (Just buildSmsReq) True emergencyContacts.defaultEmergencyNumbers Nothing
    Nothing -> do
      emergencyContacts <- DP.getDefaultEmergencyNumbers (personId, person.merchantId)
      SPDEN.notifyEmergencyContactsWithKey person "SOS_ALERT" Notification.SOS_TRIGGERED [("userName", SLP.getName person)] (Just buildSmsReq) True emergencyContacts.defaultEmergencyNumbers (Just sosId)
  return $
    SosRes
      { sosId = sosId
      }
  where
    triggerShareRideAndNotifyContacts safetySettings = (fromMaybe safetySettings.notifySosWithEmergencyContacts req.notifyAllContacts) && req.flow == SafetyDSos.SafetyFlow
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

createTicketForNewSos :: Person.Person -> DRide.Ride -> DRC.RiderConfig -> Text -> SosReq -> Flow (Id SafetyDSos.Sos)
createTicketForNewSos person ride riderConfig trackLink req = do
  sosRes <- CQSos.findByRideId ride.id
  case sosRes of
    Just sosDetails -> do
      void $ SafetySos.updateSosStatus SafetyDSos.Pending (cast sosDetails.id)
      void $ callUpdateTicket person sosDetails $ Just "SOS Re-Activated"
      when (req.flow == SafetyDSos.SafetyFlow) $ CQSos.cacheSosIdByRideId ride.id $ sosDetails {SafetyDSos.status = SafetyDSos.Pending, SafetyDSos.entityType = Just SafetyDSos.Ride, SafetyDSos.sosState = Just SafetyDSos.SosActive}
      return (cast sosDetails.id)
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
      let sosDetailsWithEntityType = sosDetails {SafetyDSos.entityType = Just SafetyDSos.Ride, SafetyDSos.sosState = Just SafetyDSos.SosActive}
      when (req.flow == SafetyDSos.SafetyFlow) $ CQSos.cacheSosIdByRideId ride.id sosDetailsWithEntityType
      void $ SafetySos.createSos sosDetailsWithEntityType
      return sosDetailsWithEntityType.id

postSosStatus :: (Maybe (Id Person.Person), Id Merchant.Merchant) -> Id SafetyDSos.Sos -> SosUpdateReq -> Flow APISuccess.APISuccess
postSosStatus (mbPersonId, _) sosId req = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  person <- QP.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  sosDetails <- runInReplica $ SafetySos.findSosById sosId >>= fromMaybeM (SosIdDoesNotExist sosId.getId)
  unless (personId == cast sosDetails.personId) $ throwError $ InvalidRequest "PersonId not same"
  void $ SafetySos.updateSosStatus req.status sosId
  void $ callUpdateTicket person sosDetails req.comment
  pure APISuccess.Success

isRideBasedSos :: Maybe SafetyDSos.SosEntityType -> Bool
isRideBasedSos (Just SafetyDSos.Ride) = True
isRideBasedSos Nothing = True
isRideBasedSos _ = False

postSosMarkRideAsSafe :: (Maybe (Id Person.Person), Id Merchant.Merchant) -> Id SafetyDSos.Sos -> MarkAsSafeReq -> Flow APISuccess.APISuccess
postSosMarkRideAsSafe (mbPersonId, merchantId) sosId MarkAsSafeReq {..} = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  emergencyContacts <- DP.getDefaultEmergencyNumbers (personId, merchantId)
  let contactsToNotify =
        case contacts of
          Nothing -> emergencyContacts.defaultEmergencyNumbers
          Just contactsList ->
            if List.null contactsList
              then []
              else List.filter (\ec -> List.elem ec.mobileNumber contactsList) emergencyContacts.defaultEmergencyNumbers

  person <- QP.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  safetySettings <- QSafety.findSafetySettingsWithFallback personId (Just person)
  case isMock of
    Just True -> do
      mockSos :: Maybe SafetyDSos.SosMockDrill <- Redis.safeGet $ CQSos.mockSosKey personId
      case mockSos of
        Nothing -> pure ()
        Just _ -> do
          Redis.setExp (CQSos.mockSosKey personId) (SafetyDSos.SosMockDrill {personId = cast personId, status = SafetyDSos.MockResolved}) 13400
      SPDEN.notifyEmergencyContactsWithKey person "SOS_RESOLVED_SAFE" Notification.SOS_RESOLVED [("userName", SLP.getName person)] Nothing False emergencyContacts.defaultEmergencyNumbers Nothing
      return APISuccess.Success
    _ -> do
      sosDetails <- runInReplica $ SafetySos.findSosById sosId >>= fromMaybeM (SosIdDoesNotExist sosId.getId)
      when (sosDetails.status == SafetyDSos.Resolved) $ throwError $ InvalidRequest "Sos already resolved."

      let wasLiveTracking = sosDetails.sosState == Just SafetyDSos.LiveTracking
      let shouldStopTracking = fromMaybe True isEndLiveTracking
      void $ callUpdateTicket person sosDetails $ Just "Mark Ride as Safe"

      let shouldMarkAsResolved =
            (isRideBasedSos sosDetails.entityType)
              || (isEndLiveTracking == Just True)

      when shouldMarkAsResolved $ do
        void $ SafetySos.updateSosStatus SafetyDSos.Resolved sosId
        when (isRideBasedSos sosDetails.entityType) $ do
          rideId <- sosDetails.rideId & fromMaybeM (RideDoesNotExist "Ride ID not found")
          CQSos.cacheSosIdByRideId (cast rideId) sosDetails {SafetyDSos.status = SafetyDSos.Resolved}

      when (sosDetails.entityType == Just SafetyDSos.NonRide && sosDetails.sosState == Just SafetyDSos.SosActive && isEndLiveTracking == Just False) $ do
        void $ SafetySos.updateSosState (Just SafetyDSos.LiveTracking) sosId

      when shouldStopTracking $ do
        SOSLocation.clearSosRiderLocation sosId
      if isRideBasedSos sosDetails.entityType
        then do
          when (safetySettings.notifySosWithEmergencyContacts && isRideEnded /= Just True) $ do
            SPDEN.notifyEmergencyContactsWithKey person "SOS_RESOLVED_SAFE" Notification.SOS_RESOLVED [("userName", SLP.getName person)] Nothing False contactsToNotify Nothing
        else do
          if shouldStopTracking && wasLiveTracking
            then do
              SPDEN.notifyEmergencyContactsWithKey person "LIVE_TRACKING_STOPPED" Notification.SOS_RESOLVED [("userName", SLP.getName person)] Nothing False contactsToNotify (Just sosId)
            else do
              SPDEN.notifyEmergencyContactsWithKey person "SOS_RESOLVED_SAFE" Notification.SOS_RESOLVED [("userName", SLP.getName person)] Nothing False contactsToNotify (Just sosId)
      pure APISuccess.Success

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
      unless (fromMaybe False safetySettings.hasCompletedMockSafetyDrill) $ SafetySos.updateMockSafetyDrillStatus (Just True) (cast personId)
      when (fromMaybe False onRide) $ do
        let mockEntity = SafetyDSos.SosMockDrill {personId = cast personId, status = SafetyDSos.MockPending}
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

uploadMedia :: Id SafetyDSos.Sos -> Id Person.Person -> SOSVideoUploadReq -> Flow AddSosVideoRes
uploadMedia sosId personId SOSVideoUploadReq {..} = do
  sosDetails <- runInReplica $ SafetySos.findSosById sosId >>= fromMaybeM (SosIdDoesNotExist sosId.getId)
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
      let currentMediaFiles = sosDetails.mediaFiles
          updatedMediaFiles = currentMediaFiles <> [mediaFileId]
      void $ SafetySos.updateSosMediaFiles updatedMediaFiles sosId
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
        when (isRideBasedSos sosDetails.entityType) $ do
          rideId <- sosDetails.rideId & fromMaybeM (RideDoesNotExist "Ride ID not found")
          ride <- QRide.findById (cast rideId) >>= fromMaybeM (RideDoesNotExist (getId rideId))
          let rideInfo = SIVR.buildRideInfo ride person phoneNumber
              trackLink = Notify.buildTrackingUrl ride.id [("vp", "shareRide")] riderConfig.trackingShortUrlPattern
          void $
            withTryCatch "createTicket:sendSosTracking" $
              withShortRetry $
                createTicket
                  person.merchantId
                  person.merchantOperatingCityId
                  (mkTicket person phoneNumber (["https://" <> trackLink] <> dashboardFileUrl) rideInfo SafetyDSos.AudioRecording (riderConfig.kaptureConfig.disposition) kaptureQueue)
      return $ AddSosVideoRes {fileUrl = fileUrl}

callUpdateTicket :: Person.Person -> SafetyDSos.Sos -> Maybe Text -> Flow APISuccess.APISuccess
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
      sos <- SafetySos.findSosByTicketId (Just ticketId) >>= fromMaybeM (InvalidRequest $ "SOS with ticketId-" <> ticketId <> " does not exist.")
      merchantOpCityId <-
        maybe
          ( ( QP.findById (cast sos.personId)
                >>= fromMaybeM (PersonNotFound (getId sos.personId))
            )
              <&> (.merchantOperatingCityId)
          )
          (return . cast)
          sos.merchantOperatingCityId
      merchantOperatingCity <- CQMOC.findById merchantOpCityId >>= fromMaybeM (MerchantOperatingCityNotFound merchantOpCityId.getId)
      riderConfig <- QRC.findByMerchantOperatingCityId merchantOpCityId Nothing >>= fromMaybeM (RiderConfigDoesNotExist merchantOpCityId.getId)
      let maybeAppId = (HM.lookup DRC.UnattendedTicketAppletID . DRC.exotelMap) =<< riderConfig.exotelAppIdMapping
      mapM_ (sendAlert merchantOperatingCity sos maybeAppId) (fromMaybe [] riderConfig.cxAgentDetails)
  where
    sendAlert :: DMOC.MerchantOperatingCity -> SafetyDSos.Sos -> Maybe Text -> IC.CxAgentDetails -> Flow ()
    sendAlert merchantOpCity sos maybeAppId cxAgentDetails =
      when (isRideBasedSos sos.entityType) $ do
        fork ("Sending unattended sos ticket alert to agentDetails - " <> show cxAgentDetails) $ do
          callStatusId <- generateGUID
          rideId <- sos.rideId & fromMaybeM (RideDoesNotExist "Ride ID not found")
          let callReq =
                Call.InitiateCallReq
                  { fromPhoneNum = cxAgentDetails.agentMobileNumber,
                    toPhoneNum = Nothing,
                    attachments = Call.Attachments $ DUCall.CallAttachments {callStatusId = callStatusId, rideId = cast @SafetyCommon.Ride @DRide.Ride rideId},
                    appletId = maybeAppId
                  }
          exotelResponse <- Call.initiateCall merchantOpCity.merchantId merchantOpCity.id callReq
          callStatus <- buildCallStatus callStatusId exotelResponse sos merchantOpCity.id
          QCallStatus.create callStatus

    buildCallStatus :: Id DCall.CallStatus -> Call.InitiateCallResp -> SafetyDSos.Sos -> Id DMOC.MerchantOperatingCity -> Flow DCall.CallStatus
    buildCallStatus callStatusId exotelResponse sos merchantOpCityId = do
      now <- getCurrentTime
      return $
        DCall.CallStatus
          { id = callStatusId,
            callId = exotelResponse.callId,
            rideId = cast <$> sos.rideId,
            dtmfNumberUsed = Nothing,
            status = exotelResponse.callStatus,
            callAttempt = Nothing,
            conversationDuration = 0,
            recordingUrl = Nothing,
            merchantId = (cast <$> sos.merchantId) <&> (.getId),
            merchantOperatingCityId = Just merchantOpCityId,
            callService = Just Call.Exotel,
            callError = Nothing,
            createdAt = now,
            updatedAt = now,
            customerIvrResponse = Nothing
          }

    mkUnattendedSosAlertKey :: Text
    mkUnattendedSosAlertKey = "Unattended:SOS:Alert:TicketId-" <> ticketId

postSosUpdateLocation :: (Maybe (Id Person.Person), Id Merchant.Merchant) -> Id SafetyDSos.Sos -> SosLocationUpdateReq -> Flow APISuccess.APISuccess
postSosUpdateLocation (mbPersonId, _) sosId req = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  sosDetails <- runInReplica $ SafetySos.findSosById sosId >>= fromMaybeM (SosIdDoesNotExist sosId.getId)
  unless (personId == cast sosDetails.personId) $ throwError $ InvalidRequest "PersonId not same"
  unless (sosDetails.status == SafetyDSos.Pending) $ throwError $ InvalidRequest "Can only update location for pending SOS"

  let location = Maps.LatLong {lat = req.lat, lon = req.lon}
  SOSLocation.updateSosRiderLocation sosId location req.accuracy sosDetails.trackingExpiresAt
  pure APISuccess.Success

getSosTracking :: Id SafetyDSos.Sos -> Flow SosTrackingRes
getSosTracking sosId = do
  sosTrackingRateLimitOptions <- asks (.sosTrackingRateLimitOptions)
  checkSlidingWindowLimitWithOptions (sosTrackingHitsCountKey sosId) sosTrackingRateLimitOptions

  sosDetails <- runInReplica $ SafetySos.findSosById sosId >>= fromMaybeM (SosIdDoesNotExist sosId.getId)
  unless (sosDetails.flow == SafetyDSos.SafetyFlow && sosDetails.entityType == Just SafetyDSos.NonRide) $
    throwError $ InvalidRequest "Invalid SOS for tracking"

  now <- getCurrentTime
  let isTrackingActive =
        sosDetails.status == SafetyDSos.Pending && case sosDetails.trackingExpiresAt of
          Just expiry -> expiry > now
          Nothing -> False

  currentLocation <-
    if isTrackingActive
      then SOSLocation.getSosRiderLocation sosId
      else return Nothing

  return $
    SosTrackingRes
      { currentLocation = convertToApiLocation <$> currentLocation,
        sosState = sosDetails.sosState,
        status = sosDetails.status
      }
  where
    convertToApiLocation :: SOSLocation.SosLocationData -> SosLocationRes
    convertToApiLocation loc =
      SosLocationRes
        { lat = loc.lat,
          lon = loc.lon,
          accuracy = loc.accuracy
        }

getSosTrackingDetails :: (Maybe (Id Person.Person), Id Merchant.Merchant) -> Id SafetyDSos.Sos -> Flow SosTrackingDetailsRes
getSosTrackingDetails (mbPersonId, _) sosId = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  sosDetails <- runInReplica $ SafetySos.findSosById sosId >>= fromMaybeM (SosIdDoesNotExist sosId.getId)
  unless (sosDetails.flow == SafetyDSos.SafetyFlow && sosDetails.entityType == Just SafetyDSos.NonRide) $
    throwError $ InvalidRequest "Invalid SOS for tracking"
  unless (sosDetails.status == SafetyDSos.Pending) $
    throwError $ InvalidRequest "Location sharing has expired"

  let isSosCreator = personId == cast sosDetails.personId
  isEmergencyContact <-
    if isSosCreator
      then return True
      else do
        emergencyContacts <- runInReplica $ QPDEN.findAllByPersonId (cast sosDetails.personId)
        return $ Foldable.any (\ec -> ec.contactPersonId == Just personId) emergencyContacts
  unless isEmergencyContact $
    throwError $ InvalidRequest "Access denied"

  person <- QP.findById (cast sosDetails.personId) >>= fromMaybeM (PersonDoesNotExist (getId sosDetails.personId))
  phoneNumber <- mapM decrypt person.mobileNumber >>= fromMaybeM (PersonFieldNotPresent "mobileNumber")

  return $
    SosTrackingDetailsRes
      { personName = SLP.getName person,
        mobileNumber = phoneNumber
      }

sosTrackingHitsCountKey :: Id SafetyDSos.Sos -> Text
sosTrackingHitsCountKey sosId = "SosTrackingHits:" <> sosId.getId <> ":hitsCount"

-- | Build tracking URL for SOS rider location (non-ride scenario)
-- Uses the same pattern-based approach as ride tracking for consistency
-- Pattern format: "https://nammayatri.in/u?vp={#vp#}&rideId=" or "https://nammayatri.in/u?vp=shareRide&rideId="
-- For SOS: replaces rideId with sosId and uses vp=sosTracking
buildSosTrackingUrl :: Id SafetyDSos.Sos -> Text -> Text
buildSosTrackingUrl sosId trackingUrlPattern =
  let patternWithSosId = T.replace "rideId=" "sosId=" trackingUrlPattern
      templateText txt = "{#" <> txt <> "#}"
      urlWithVpReplaced =
        if T.isInfixOf (templateText "vp") patternWithSosId
          then -- Template format: replace {#vp#} with sosTracking

            Foldable.foldl'
              ( \msg (findKey, replaceVal) ->
                  T.replace (templateText findKey) replaceVal msg
              )
              patternWithSosId
              [("vp", "sosTracking")]
          else T.replace "vp=shareRide" "vp=sosTracking" patternWithSosId
   in urlWithVpReplaced <> sosId.getId

postSosStartTracking :: (Maybe (Id Person.Person), Id Merchant.Merchant) -> StartTrackingReq -> Flow StartTrackingRes
postSosStartTracking (mbPersonId, merchantId) StartTrackingReq {..} = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  person <- QP.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)

  when (durationInMinutes < 1 || durationInMinutes > 1440) $
    throwError $ InvalidRequest "Duration must be between 1 and 1440 minutes (24 hours)"

  now <- getCurrentTime
  let durationSeconds = durationInMinutes * 60
  let expiryTimeStamp = addUTCTime (fromIntegral durationSeconds) now

  (finalSosId, trackLink) <- case sosId of
    Just existingSosId -> do
      sosDetails <- runInReplica $ SafetySos.findSosById existingSosId >>= fromMaybeM (SosIdDoesNotExist existingSosId.getId)
      unless (personId == cast sosDetails.personId) $ throwError $ InvalidRequest "PersonId not same"
      unless (sosDetails.entityType == Just SafetyDSos.NonRide) $ throwError $ InvalidRequest "Invalid SOS for tracking"
      when (sosDetails.status == SafetyDSos.Resolved) $ throwError $ InvalidRequest "Cannot start tracking for resolved SOS"
      void $ SafetySos.updateSosTrackingExpiresAt (Just expiryTimeStamp) existingSosId
      whenJust customerLocation $ \location -> do
        SOSLocation.updateSosRiderLocation existingSosId location Nothing (Just expiryTimeStamp)
      riderConfig <- QRC.findByMerchantOperatingCityId person.merchantOperatingCityId Nothing >>= fromMaybeM (RiderConfigDoesNotExist person.merchantOperatingCityId.getId)
      let trackLink' = buildSosTrackingUrl existingSosId riderConfig.trackingShortUrlPattern
      return (existingSosId, trackLink')
    Nothing -> do
      sosId' <- generateGUID
      uniqueRideId <- generateGUID
      let sosDetails =
            SafetyDSos.Sos
              { id = sosId',
                personId = cast personId,
                flow = SafetyDSos.SafetyFlow,
                status = SafetyDSos.Pending,
                rideId = Just (cast (Id uniqueRideId)),
                ticketId = Nothing,
                mediaFiles = [],
                merchantId = Just (cast person.merchantId),
                merchantOperatingCityId = Just (cast person.merchantOperatingCityId),
                trackingExpiresAt = Just expiryTimeStamp,
                sosState = Just SafetyDSos.LiveTracking,
                entityType = Just SafetyDSos.NonRide,
                createdAt = now,
                updatedAt = now
              }
      void $ SafetySos.createSos sosDetails
      whenJust customerLocation $ \location -> do
        SOSLocation.updateSosRiderLocation sosDetails.id location Nothing (Just expiryTimeStamp)
      riderConfig <- QRC.findByMerchantOperatingCityId person.merchantOperatingCityId Nothing >>= fromMaybeM (RiderConfigDoesNotExist person.merchantOperatingCityId.getId)
      let trackLink' = buildSosTrackingUrl sosDetails.id riderConfig.trackingShortUrlPattern
      return (sosDetails.id, trackLink')

  emergencyContacts <- DP.getDefaultEmergencyNumbers (personId, merchantId)
  let contactsToNotify =
        if List.null contacts
          then []
          else List.filter (\ec -> List.elem ec.mobileNumber contacts) emergencyContacts.defaultEmergencyNumbers

  when (not (List.null contacts) && List.null contactsToNotify) $
    throwError $ InvalidRequest "No valid emergency contacts found"

  when (not (List.null contactsToNotify)) $ do
    fork "Notifying emergency contacts about live tracking start" $ do
      let formattedExpiryTime = T.pack . formatTime defaultTimeLocale "%I:%M %p" $ expiryTimeStamp
      SPDEN.notifyEmergencyContactsWithKey person "LIVE_TRACKING_STARTED" Notification.SHARE_RIDE [("userName", SLP.getName person), ("expiryTime", formattedExpiryTime)] Nothing False contactsToNotify (Just finalSosId)

  return $
    StartTrackingRes
      { sosId = finalSosId,
        trackingUrl = trackLink
      }

postSosUpdateState :: (Maybe (Id Person.Person), Id Merchant.Merchant) -> Id SafetyDSos.Sos -> UpdateStateReq -> Flow APISuccess.APISuccess
postSosUpdateState (mbPersonId, _) sosId UpdateStateReq {..} = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  sosDetails <- runInReplica $ SafetySos.findSosById sosId >>= fromMaybeM (SosIdDoesNotExist sosId.getId)
  unless (personId == cast sosDetails.personId) $ throwError $ InvalidRequest "PersonId not same"
  unless (sosDetails.entityType == Just SafetyDSos.NonRide) $ throwError $ InvalidRequest "Can only update state for non-ride SOS"
  unless (sosDetails.status == SafetyDSos.Pending) $ throwError $ InvalidRequest "Can only update state for pending SOS"
  if sosDetails.sosState == Just sosState
    then pure APISuccess.Success
    else do
      person <- QP.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
      emergencyContacts <- DP.getDefaultEmergencyNumbers (personId, person.merchantId)
      riderConfig <- QRC.findByMerchantOperatingCityId person.merchantOperatingCityId Nothing >>= fromMaybeM (RiderConfigDoesNotExist person.merchantOperatingCityId.getId)
      let trackLink = buildSosTrackingUrl sosId riderConfig.trackingShortUrlPattern

      case (sosDetails.sosState, sosState) of
        (Just SafetyDSos.LiveTracking, SafetyDSos.SosActive) -> do
          now <- getCurrentTime
          let eightHoursInSeconds :: Int = 8 * 60 * 60
          let newExpiryTime = addUTCTime (fromIntegral eightHoursInSeconds) now
          void $ SafetySos.updateSosTrackingExpiresAt (Just newExpiryTime) sosId
          void $ SafetySos.updateSosState (Just sosState) sosId

          mbCurrentLocation <- SOSLocation.getSosRiderLocation sosId
          whenJust mbCurrentLocation $ \locationData -> do
            let location = Maps.LatLong {lat = locationData.lat, lon = locationData.lon}
            SOSLocation.updateSosRiderLocation sosId location locationData.accuracy (Just newExpiryTime)

          buildSmsReq <-
            MessageBuilder.buildSOSAlertMessage person.merchantOperatingCityId $
              MessageBuilder.BuildSOSAlertMessageReq
                { userName = SLP.getName person,
                  rideLink = trackLink,
                  rideEndTime = Nothing,
                  isRideEnded = False
                }

          SPDEN.notifyEmergencyContactsWithKey person "SOS_ALERT" Notification.SOS_TRIGGERED [("userName", SLP.getName person)] (Just buildSmsReq) True emergencyContacts.defaultEmergencyNumbers (Just sosId)
        (Just SafetyDSos.SosActive, SafetyDSos.LiveTracking) -> do
          void $ SafetySos.updateSosState (Just sosState) sosId

          SPDEN.notifyEmergencyContactsWithKey person "LIVE_TRACKING_STOPPED" Notification.SOS_RESOLVED [("userName", SLP.getName person)] Nothing False emergencyContacts.defaultEmergencyNumbers (Just sosId)
        _ -> do
          void $ SafetySos.updateSosState (Just sosState) sosId

      whenJust sosDetails.ticketId $ \_ticketId -> do
        void $ callUpdateTicket person sosDetails $ Just "SOS State Updated"

      pure APISuccess.Success

postSosUpdateToRide :: (Maybe (Id Person.Person), Id Merchant.Merchant) -> Id SafetyDSos.Sos -> UpdateToRideReq -> Flow APISuccess.APISuccess
postSosUpdateToRide (mbPersonId, _) sosId UpdateToRideReq {..} = do
  -- Validate person
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  -- Validate SOS exists and belongs to person
  sosDetails <- runInReplica $ SafetySos.findSosById sosId >>= fromMaybeM (SosIdDoesNotExist sosId.getId)
  unless (personId == cast sosDetails.personId) $ throwError $ InvalidRequest "PersonId not same"
  -- Validate rideId exists and belongs to person
  ride <- runInReplica $ QRide.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  booking <- runInReplica $ QBooking.findById ride.bookingId >>= fromMaybeM (BookingDoesNotExist ride.bookingId.getId)
  unless (personId == booking.riderId) $ throwError $ InvalidRequest "Ride does not belong to this person"
  -- Update SOS from NonRide to Ride
  void $ SafetySos.updateSosFromNonRideToRide sosId (cast @DRide.Ride @SafetyCommon.Ride rideId)
  pure APISuccess.Success
