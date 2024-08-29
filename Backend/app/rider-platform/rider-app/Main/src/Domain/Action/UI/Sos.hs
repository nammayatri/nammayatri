{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Action.UI.Sos where

import API.Types.UI.Sos
import AWS.S3 as S3
import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Management.Message as Common
import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
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
import qualified Slack.AWS.Flow as Slack
import Storage.Beam.IssueManagement ()
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.RiderConfig as QRC
import qualified Storage.CachedQueries.Sos as CQSos
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.CallStatus as QCallStatus
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.PersonDefaultEmergencyNumber as QPDEN
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.Sos as QSos
import qualified Tools.Call as Call
import Tools.Error
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
          rideId = rideId_,
          status = mockSos.status,
          ticketId = Nothing,
          merchantId = Nothing,
          merchantOperatingCityId = Nothing,
          createdAt = now,
          updatedAt = now
        }

postSosCreate :: (Maybe (Id Person.Person), Id Merchant.Merchant) -> SosReq -> Flow SosRes
postSosCreate (mbPersonId, _merchantId) req = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  person <- QP.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  Redis.del $ CQSos.mockSosKey personId
  ride <- QRide.findById req.rideId >>= fromMaybeM (RideDoesNotExist req.rideId.getId)
  riderConfig <- QRC.findByMerchantOperatingCityId person.merchantOperatingCityId >>= fromMaybeM (RiderConfigDoesNotExist person.merchantOperatingCityId.getId)
  fork "Notify on slack " $ do
    sosAlertsTopicARN <- asks (.sosAlertsTopicARN)
    desc <- generateSlackMessageDesc person ride req.customerLocation
    let message = createJsonMessage desc
    void $ L.runIO $ Slack.publishMessage sosAlertsTopicARN message
  let trackLink = riderConfig.trackingShortUrlPattern <> ride.shortId.getShortId
  sosId <- createTicketForNewSos person ride riderConfig trackLink req
  buildSmsReq <-
    MessageBuilder.buildSOSAlertMessage person.merchantOperatingCityId $
      MessageBuilder.BuildSOSAlertMessageReq
        { userName = SLP.getName person,
          rideLink = trackLink
        }
  when (req.isRideEnded /= Just True) $ do
    emergencyContacts <- DP.getDefaultEmergencyNumbers (personId, person.merchantId)
    when (shouldSendSms person) $ do
      void $ QPDEN.updateShareRideForAll personId True
      enableFollowRideInSos emergencyContacts.defaultEmergencyNumbers
      SPDEN.notifyEmergencyContacts person (notificationBody person) notificationTitle Notification.SOS_TRIGGERED (Just buildSmsReq) True emergencyContacts.defaultEmergencyNumbers
  return $
    SosRes
      { sosId = sosId
      }
  where
    createJsonMessage :: Text -> T.Text
    createJsonMessage descriptionText =
      let jsonValue :: A.Value
          jsonValue =
            A.object
              [ "version" A..= ("1.0" :: String),
                "source" A..= ("custom" :: String),
                "content"
                  A..= A.object
                    [ "description" A..= descriptionText
                    ]
              ]
       in TL.toStrict $ TLE.decodeUtf8 $ A.encode jsonValue

    generateSlackMessageDesc :: Person.Person -> DRide.Ride -> Maybe Maps.LatLong -> Flow Text
    generateSlackMessageDesc person ride customerLocation = do
      mbCustomerPhone <- mapM decrypt person.mobileNumber
      mbDriverPhoneNumber <- mapM decrypt ride.driverPhoneNumber
      merchantOperatingCity <- do
        case ride.merchantOperatingCityId of
          Nothing -> pure Nothing
          Just merchantOpCityId -> CQMOC.findById merchantOpCityId
      let city = maybe "NA" (\x -> show $ x.city) merchantOperatingCity
          customerPhone = fromMaybe "NA" mbCustomerPhone
          customerName = SLP.getName person
          driverPhoneNumber = fromMaybe "NA" mbDriverPhoneNumber
          dropLoc = maybe "NA" (TL.toStrict . TLE.decodeUtf8 . A.encode . A.toJSON) (ride.toLocation)
          sosRaisedLocation = maybe "NA" (TL.toStrict . TLE.decodeUtf8 . A.encode . A.toJSON) customerLocation
          pickupLocation = TL.toStrict $ TLE.decodeUtf8 $ A.encode $ A.toJSON ride.fromLocation
      return $
        "There is an SOS raised by customer_id " <> person.id.getId <> "\n"
          <> "Customer Name - "
          <> customerName
          <> ", phone no: "
          <> customerPhone
          <> "\n"
          <> "On ride id : "
          <> ride.id.getId
          <> "\n"
          <> "Driver details - "
          <> ride.driverName
          <> ", phone no: "
          <> driverPhoneNumber
          <> "\n"
          <> "Ride Details - city: "
          <> city
          <> ", variant: "
          <> show ride.vehicleVariant
          <> "\n"
          <> "Pickup location: "
          <> pickupLocation
          <> "\n"
          <> "Drop location: "
          <> dropLoc
          <> "\n"
          <> "SOS raised location: "
          <> sosRaisedLocation

    shouldSendSms person_ = person_.shareEmergencyContacts && req.flow /= DSos.Police
    notificationBody person_ = SLP.getName person_ <> " has initiated an SOS. Tap to follow and respond to the emergency situation"
    notificationTitle = "SOS Alert"

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
      CQSos.cacheSosIdByRideId ride.id $ sosDetails {DSos.status = DSos.Pending}
      return sosDetails.id
    Nothing -> do
      phoneNumber <- mapM decrypt person.mobileNumber
      let rideInfo = buildRideInfo ride person phoneNumber
          kaptureQueue = fromMaybe riderConfig.kaptureConfig.queue riderConfig.kaptureConfig.sosQueue
      ticketId <- do
        if riderConfig.enableSupportForSafety
          then do
            ticketResponse <- try @_ @SomeException (createTicket person.merchantId person.merchantOperatingCityId (mkTicket person phoneNumber ["https://" <> trackLink] rideInfo req.flow riderConfig.kaptureConfig.disposition kaptureQueue))
            case ticketResponse of
              Right ticketResponse' -> return (Just ticketResponse'.ticketId)
              Left _ -> return Nothing
          else return Nothing
      sosDetails <- buildSosDetails person req ticketId
      CQSos.cacheSosIdByRideId ride.id sosDetails
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
  case isMock of
    Just True -> do
      mockSos :: Maybe DSos.SosMockDrill <- Redis.safeGet $ CQSos.mockSosKey personId
      case mockSos of
        Nothing -> pure ()
        Just _ -> do
          Redis.setExp (CQSos.mockSosKey personId) (DSos.SosMockDrill {personId, status = DSos.MockResolved}) 13400
      SPDEN.notifyEmergencyContacts person (notificationBody person) notificationTitle Notification.SOS_RESOLVED Nothing False emergencyContacts.defaultEmergencyNumbers
      return APISuccess.Success
    _ -> do
      sosDetails <- runInReplica $ QSos.findById sosId >>= fromMaybeM (SosIdDoesNotExist sosId.getId)
      when (sosDetails.status == DSos.Resolved) $ throwError $ InvalidRequest "Sos already resolved."
      void $ callUpdateTicket person sosDetails $ Just "Mark Ride as Safe"
      void $ QSos.updateStatus DSos.Resolved sosId
      CQSos.cacheSosIdByRideId sosDetails.rideId $ sosDetails {DSos.status = DSos.Resolved}
      when (person.shareEmergencyContacts && isRideEnded /= Just True) $ do
        SPDEN.notifyEmergencyContacts person (notificationBody person) notificationTitle Notification.SOS_RESOLVED Nothing False emergencyContacts.defaultEmergencyNumbers
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
  case startDrill of
    Just True -> do
      SPDEN.notifyEmergencyContacts person (notificationBody person True) notificationTitle Notification.SOS_MOCK_DRILL_NOTIFY Nothing False emergencyContacts.defaultEmergencyNumbers
      when (fromMaybe False onRide) $ do
        void $ QPDEN.updateShareRideForAll personId True
        enableFollowRideInSos emergencyContacts.defaultEmergencyNumbers
    _ -> do
      when (not $ fromMaybe False person.hasCompletedMockSafetyDrill) $ QP.updateSafetyDrillStatus (Just True) personId
      when (fromMaybe False onRide) $ do
        let mockEntity = DSos.SosMockDrill {personId, status = DSos.MockPending}
        Redis.setExp (CQSos.mockSosKey personId) mockEntity 13400
      SPDEN.notifyEmergencyContacts person (notificationBody person False) notificationTitle Notification.SOS_MOCK_DRILL Nothing False emergencyContacts.defaultEmergencyNumbers
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
  riderConfig <- QRC.findByMerchantOperatingCityId person.merchantOperatingCityId >>= fromMaybeM (RiderConfigDoesNotExist person.merchantOperatingCityId.getId)
  fileSize <- L.runIO $ withFile payload ReadMode hFileSize
  when (fileSize > fromIntegral riderConfig.videoFileSizeUpperLimit) $
    throwError $ FileSizeExceededError (show fileSize)
  mediaFile <- L.runIO $ base64Encode <$> BS.readFile payload
  filePath <- createFilePath "/sos/" ("sos-" <> getId sosId) fileType fileExtension
  let fileUrl =
        merchantConfig.publicMediaFileUrlPattern
          & T.replace "<DOMAIN>" "sos"
          & T.replace "<FILE_PATH>" filePath
  result <- try @_ @SomeException $ S3.putPublic (T.unpack filePath) mediaFile
  case result of
    Left err -> throwError $ InternalError ("S3 Upload Failed: " <> show err)
    Right _ -> do
      ride <- QRide.findById sosDetails.rideId >>= fromMaybeM (RideDoesNotExist sosDetails.rideId.getId)
      phoneNumber <- mapM decrypt person.mobileNumber
      let rideInfo = buildRideInfo ride person phoneNumber
          trackLink = riderConfig.trackingShortUrlPattern <> ride.shortId.getShortId
          kaptureQueue = fromMaybe riderConfig.kaptureConfig.queue riderConfig.kaptureConfig.sosQueue
      when riderConfig.enableSupportForSafety $
        void $ try @_ @SomeException $ withShortRetry (createTicket person.merchantId person.merchantOperatingCityId (mkTicket person phoneNumber ["https://" <> trackLink, fileUrl] rideInfo DSos.SafetyFlow riderConfig.kaptureConfig.disposition kaptureQueue))
      createMediaEntry Common.AddLinkAsMedia {url = fileUrl, fileType}

createMediaEntry :: Common.AddLinkAsMedia -> Flow AddSosVideoRes
createMediaEntry Common.AddLinkAsMedia {..} = do
  fileEntity <- mkFile url
  MFQuery.create fileEntity
  return $
    AddSosVideoRes
      { fileUrl = url
      }
  where
    mkFile fileUrl = do
      id <- generateGUID
      now <- getCurrentTime
      return $
        DMF.MediaFile
          { id,
            _type = S3.Video,
            url = fileUrl,
            createdAt = now
          }

buildRideInfo :: DRide.Ride -> Person.Person -> Maybe Text -> Ticket.RideInfo
buildRideInfo ride person phoneNumber =
  Ticket.RideInfo
    { rideShortId = ride.shortId.getShortId,
      rideCity = show person.currentCity,
      customerName = Just $ SLP.getName person,
      customerPhoneNo = phoneNumber,
      driverName = Just ride.driverName,
      driverPhoneNo = Just ride.driverMobileNumber,
      vehicleNo = ride.vehicleNumber,
      vehicleCategory = Just $ show ride.vehicleVariant,
      vehicleServiceTier = show <$> ride.vehicleServiceTierType,
      status = show ride.status,
      rideCreatedAt = ride.createdAt,
      pickupLocation = castLocationAPIEntity ride.fromLocation,
      dropLocation = castLocationAPIEntity <$> ride.toLocation,
      fare = Nothing
    }
  where
    castLocationAPIEntity ent =
      Ticket.Location
        { lat = ent.lat,
          lon = ent.lon,
          street = ent.address.street,
          city = ent.address.city,
          state = ent.address.state,
          country = ent.address.country,
          building = ent.address.building,
          areaCode = ent.address.areaCode,
          area = ent.address.area
        }

callUpdateTicket :: Person.Person -> DSos.Sos -> Maybe Text -> Flow APISuccess.APISuccess
callUpdateTicket person sosDetails mbComment = do
  case sosDetails.ticketId of
    Just ticketId -> do
      fork "update ticket request" $
        void $ Ticket.updateTicket (person.merchantId) person.merchantOperatingCityId (Ticket.UpdateTicketReq (fromMaybe "" mbComment) ticketId Ticket.IN)
      pure APISuccess.Success
    Nothing -> pure APISuccess.Success

mkTicket :: Person.Person -> Maybe Text -> [Text] -> Ticket.RideInfo -> DSos.SosType -> Text -> Text -> Ticket.CreateTicketReq
mkTicket person phoneNumber mediaLinks info flow disposition queue = do
  Ticket.CreateTicketReq
    { category = "Code Red",
      subCategory = Just "SOS Alert (follow-back)",
      issueId = Nothing,
      issueDescription,
      mediaFiles = Just mediaLinks,
      name = Just $ SLP.getName person,
      phoneNo = phoneNumber,
      personId = person.id.getId,
      classification = Ticket.CUSTOMER,
      rideDescription = Just info,
      disposition,
      queue
    }
  where
    issueDescription = case flow of
      DSos.Police -> "112 called"
      _ -> "SOS activated"

buildSosDetails :: (EncFlow m r) => Person.Person -> SosReq -> Maybe Text -> m DSos.Sos
buildSosDetails person req ticketId = do
  pid <- generateGUID
  now <- getCurrentTime
  return
    DSos.Sos
      { id = pid,
        personId = person.id,
        status = DSos.Pending,
        flow = req.flow,
        rideId = req.rideId,
        ticketId = ticketId,
        merchantId = Just person.merchantId,
        merchantOperatingCityId = Just person.merchantOperatingCityId,
        createdAt = now,
        updatedAt = now
      }

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
      let digitPressed = fromMaybe "0" $ T.replace "\"" "" <$> mbdigitPressed
      QCallStatus.updateCustomerIvrResponse callSid (Just digitPressed) validStatus
      logDebug $ "digitPressed : " <> digitPressed
      when (digitPressed /= "1") $ handleEmergencyCall callSid res

    handleEmergencyCall :: Text -> DCall.CallStatus -> Flow ()
    handleEmergencyCall callSid res = case res.rideId of
      Just rideId -> do
        ride <- runInReplica $ QRide.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
        booking <- runInReplica $ QBooking.findById ride.bookingId >>= fromMaybeM (BookingDoesNotExist ride.bookingId.getId)
        void $ postSosCallPolice (Just booking.riderId, booking.merchantId) (CallPoliceAPI {rideId = rideId})
        pure ()
      Nothing -> throwError $ RideIdEmptyInCallRecord callSid

postSosCallPolice :: (Maybe (Id Person.Person), Id Merchant.Merchant) -> CallPoliceAPI -> Flow APISuccess.APISuccess
postSosCallPolice (mbPersonId, merchantId) CallPoliceAPI {..} = do
  logDebug "Calling Police API started for SOS"
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  person <- runInReplica $ QP.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  ride <- runInReplica $ QRide.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  booking <- runInReplica $ QBooking.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
  let merchantOpCityId = booking.merchantOperatingCityId
  riderConfig <- QRC.findByMerchantOperatingCityId merchantOpCityId >>= fromMaybeM (RiderConfigDoesNotExist merchantOpCityId.getId)
  unless (riderConfig.incidentReportSupport) $
    throwError $ IncidentReportServiceUnavailable merchantOpCityId.getId
  coordinates <- fetchLatLong ride merchantId
  token <- getTokenofJMService merchantId merchantOpCityId
  incidentReportHandler person merchantId merchantOpCityId ride.id token coordinates riderConfig
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
      maxShards <- asks (.maxShards)
      let scheduleAfter = max 2 riderConfig.policeTriggerDelay
          callPoliceAPIJobData = CallPoliceApiJobData {rideId, personId = person.id, jmCode = res.incidentData.jmCode}
      logDebug $ "Scheduling safety alert police call after : " <> show scheduleAfter
      Redis.withCrossAppRedis $ Redis.setExp (mkRideCallPoliceAPIKey rideId) (1 :: Int) riderConfig.hardLimitForSafetyJobs
      createJobIn @_ @'CallPoliceApi scheduleAfter maxShards callPoliceAPIJobData
      pure ()

sendUnattendedSosTicketAlert :: Text -> Flow ()
sendUnattendedSosTicketAlert ticketId = do
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
  riderConfig <- QRC.findByMerchantOperatingCityId merchantOpCityId >>= fromMaybeM (RiderConfigDoesNotExist merchantOpCityId.getId)
  let maybeAppId = (HM.lookup DRC.UnattendedTicketAppletID . DRC.exotelMap) =<< riderConfig.exotelAppIdMapping
  mapM_ (sendAlert merchantOperatingCity sos maybeAppId) (fromMaybe [] riderConfig.cxAgentDetails)
  where
    sendAlert :: DMOC.MerchantOperatingCity -> DSos.Sos -> Maybe Text -> IC.CxAgentDetails -> Flow ()
    sendAlert merchantOpCity sos maybeAppId cxAgentDetails =
      fork ("Sending unattended sos ticket alert to agentDetails - " <> show cxAgentDetails) $ do
        callStatusId <- generateGUID
        let callReq =
              Call.InitiateCallReq
                { fromPhoneNum = cxAgentDetails.agentMobileNumber,
                  toPhoneNum = Nothing,
                  attachments = Call.Attachments $ DUCall.CallAttachments {callStatusId = callStatusId, rideId = sos.rideId},
                  appletId = maybeAppId
                }
        exotelResponse <- Call.initiateCall merchantOpCity.merchantId merchantOpCity.id callReq
        callStatus <- buildCallStatus callStatusId exotelResponse sos
        QCallStatus.create callStatus

    buildCallStatus :: Id DCall.CallStatus -> Call.InitiateCallResp -> DSos.Sos -> Flow DCall.CallStatus
    buildCallStatus callStatusId exotelResponse sos = do
      now <- getCurrentTime
      return $
        DCall.CallStatus
          { id = callStatusId,
            callId = exotelResponse.callId,
            rideId = Just sos.rideId,
            dtmfNumberUsed = Nothing,
            status = exotelResponse.callStatus,
            callAttempt = Nothing,
            conversationDuration = 0,
            recordingUrl = Nothing,
            merchantId = sos.merchantId <&> (.getId),
            callService = Just Call.Exotel,
            callError = Nothing,
            createdAt = now,
            updatedAt = now,
            customerIvrResponse = Nothing
          }
