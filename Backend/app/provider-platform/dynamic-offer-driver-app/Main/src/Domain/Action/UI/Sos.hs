{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.UI.Sos
  ( getSosGetDetails,
    postSosCreate,
    postSosMarkRideAsSafe,
    uploadMedia,
    SOSVideoUploadReq (..),
    AddSosVideoRes (..),
  )
where

import qualified API.Types.UI.Sos
import qualified API.Types.UI.Sos as APISos
import qualified AWS.S3 as S3
import qualified Data.ByteString as BS
import qualified Data.List as List
import qualified Data.Text as T
import Data.Time.Format (defaultTimeLocale, formatTime)
import qualified Domain.Types.Booking as DBooking
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person as Person
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.RideDetails as DRideDetails
import qualified Environment
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import EulerHS.Types (base64Encode)
import GHC.IO.Handle (hFileSize)
import GHC.IO.IOMode (IOMode (..))
import qualified IssueManagement.Domain.Types.MediaFile as DMF
import qualified IssueManagement.Storage.Queries.MediaFile as MFQuery
import Kernel.Beam.Functions (runInReplica)
import Kernel.External.Encryption (decrypt)
import qualified Kernel.External.Notification as Notification
import Kernel.External.Ticket.Interface.Types as Ticket
import Kernel.Prelude (ToSchema)
import qualified Kernel.Prelude
import Kernel.ServantMultipart
import qualified Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Logging
import Kernel.Utils.Servant.Client (withShortRetry)
import qualified Safety.Domain.Action.UI.Sos as SafetySos
import qualified Safety.Domain.Types.Common as SafetyCommon
import qualified Safety.Domain.Types.Sos
import qualified Safety.Domain.Types.Sos as SafetyDSos
import qualified Safety.Storage.CachedQueries.Sos as SafetyCQSos
import Servant hiding (throwError)
import qualified SharedLogic.MessageBuilder as MessageBuilder
import qualified SharedLogic.PersonDefaultEmergencyNumber as SPDEN
import Storage.Beam.IssueManagement ()
import Storage.Beam.Sos ()
import qualified Storage.Cac.TransporterConfig as SCTC
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RideDetails as QRideDetails
import qualified Storage.Queries.RiderDetails as QRiderDetails
import qualified Storage.Queries.SafetySettingsExtra as QSafetyExtra
import Tools.Auth
import Tools.Error
import qualified Tools.Notifications as Notify
import qualified Tools.Ticket as TicketTools

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

getSosGetDetails ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Types.Id.Id DRide.Ride ->
    Environment.Flow API.Types.UI.Sos.SosDetailsRes
  )
getSosGetDetails (mbPersonId, _, _) rideId_ = do
  personId_ <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  ride <- QRide.findById rideId_ >>= fromMaybeM (RideDoesNotExist rideId_.getId)
  unless (personId_ == ride.driverId) $ throwError $ InvalidRequest "Ride does not belong to this person"
  cached <- SafetyCQSos.findByRideId (cast rideId_)
  mbSosDetails <- case cached of
    Just x -> return (Just x)
    Nothing -> SafetySos.findSosByRideId (cast rideId_)
  case mbSosDetails of
    Nothing -> return APISos.SosDetailsRes {sos = Nothing}
    Just sosDetails -> do
      unless (personId_ == cast sosDetails.personId) $ throwError $ InvalidRequest "PersonId not same"
      return APISos.SosDetailsRes {sos = Just sosDetails}

-- Tracking URL: use transporterConfig pattern when present (parity with rider), else fallback
buildDriverSosTrackLink :: Maybe Text -> Id DRide.Ride -> Text
buildDriverSosTrackLink mbPattern rideId =
  case mbPattern of
    Nothing -> "https://nammayatri.in/t/?vp=shareRide&rideId=" <> rideId.getId
    Just urlPattern -> Notify.buildTemplate [("vp", "shareRide")] urlPattern <> rideId.getId

driverGetName :: Person.Person -> Text
driverGetName person = person.firstName <> " " <> fromMaybe "" person.lastName

buildRideInfo :: DRide.Ride -> DBooking.Booking -> DRideDetails.RideDetails -> Person.Person -> Maybe Text -> Maybe Text -> Ticket.RideInfo
buildRideInfo ride booking rideDetails person driverPhoneNumber customerPhoneNumber =
  Ticket.RideInfo
    { rideShortId = ride.shortId.getShortId,
      rideCity = show person.merchantOperatingCityId,
      customerName = booking.riderName,
      customerPhoneNo = customerPhoneNumber,
      driverName = Just $ driverGetName person,
      driverPhoneNo = driverPhoneNumber,
      vehicleNo = rideDetails.vehicleNumber,
      vehicleCategory = show <$> ride.vehicleVariant,
      vehicleServiceTier = ride.vehicleServiceTierName,
      status = show ride.status,
      rideCreatedAt = ride.createdAt,
      pickupLocation = castLocation ride.fromLocation,
      dropLocation = castLocation <$> ride.toLocation,
      fare = Nothing
    }
  where
    castLocation loc =
      Ticket.Location
        { lat = loc.lat,
          lon = loc.lon,
          street = loc.address.street,
          city = loc.address.city,
          state = loc.address.state,
          country = loc.address.country,
          building = loc.address.building,
          areaCode = loc.address.areaCode,
          area = loc.address.area
        }

mkTicket :: Person.Person -> Maybe Text -> [Text] -> Ticket.RideInfo -> SafetyDSos.SosType -> Text -> Text -> Ticket.CreateTicketReq
mkTicket person phoneNumber mediaLinks info flow disposition queue =
  Ticket.CreateTicketReq
    { category = "Code Red",
      subCategory = Just "SOS Alert (follow-back)",
      issueId = Nothing,
      issueDescription = sosIssueDescription flow,
      mediaFiles = Just mediaLinks,
      name = Just $ driverGetName person,
      phoneNo = phoneNumber,
      personId = person.id.getId,
      classification = Ticket.DRIVER,
      rideDescription = Just info,
      disposition = disposition,
      queue = queue,
      becknIssueId = Nothing
    }
  where
    sosIssueDescription = \case
      SafetyDSos.Police -> "112 called"
      SafetyDSos.AudioRecording -> "Audio recording shared."
      SafetyDSos.CustomerCare -> "Customer care called."
      _ -> "SOS activated (driver)"

createTicketForNewSos :: Person.Person -> DRide.Ride -> DBooking.Booking -> Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Id Domain.Types.Merchant.Merchant -> Bool -> Text -> Text -> Text -> API.Types.UI.Sos.SosReq -> Environment.Flow (Id SafetyDSos.Sos)
createTicketForNewSos person ride booking _merchantOperatingCityId _merchantId enableSupportForSafety kaptureDisposition kaptureQueue trackLink req = do
  cached <- SafetyCQSos.findByRideId (cast ride.id)
  mbExistingSos <- case cached of
    Just x -> pure (Just x)
    Nothing -> SafetySos.findSosByRideId (cast ride.id)
  case mbExistingSos of
    Just existingSos -> do
      logDebug $ "createTicketForNewSos: reactivating SOS, rideId=" <> ride.id.getId <> ", personId=" <> person.id.getId <> ", existingSosId=" <> existingSos.id.getId
      result <- SafetySos.createRideBasedSos (cast person.id) (cast ride.id) (cast _merchantOperatingCityId) (cast _merchantId) req.flow (Just existingSos) existingSos.ticketId Nothing
      logDebug $ "createTicketForNewSos: createRideBasedSos (reactivate) returned, sosId=" <> result.sosId.getId
      void $ callUpdateTicket person result.sosDetails $ Just "SOS Re-Activated"
      return (cast result.sosId)
    Nothing -> do
      driverPhoneNumber <- mapM decrypt person.mobileNumber
      rideDetails <- QRideDetails.findById ride.id >>= fromMaybeM (InvalidRequest $ "RideDetailsNotFound: " <> ride.id.getId)
      customerPhoneNumber <- case booking.riderId of
        Nothing -> pure Nothing
        Just riderDetailsId -> do
          riderDetails <- QRiderDetails.findById riderDetailsId >>= fromMaybeM (RiderDetailsNotFound riderDetailsId.getId)
          mobileNumber <- decrypt riderDetails.mobileNumber
          pure $ Just (riderDetails.mobileCountryCode <> mobileNumber)
      let rideInfo = buildRideInfo ride booking rideDetails person driverPhoneNumber customerPhoneNumber
      ticketId <- do
        if enableSupportForSafety
          then do
            ticketResponse <-
              withTryCatch "createTicket:sosTrigger" $
                TicketTools.createTicket person.merchantId person.merchantOperatingCityId $
                  mkTicket person driverPhoneNumber ["https://" <> trackLink] rideInfo req.flow kaptureDisposition kaptureQueue
            case ticketResponse of
              Right ticketResponse' -> return (Just ticketResponse'.ticketId)
              Left err -> do
                logError $ "createTicket:sosTrigger failed for ride " <> ride.id.getId <> " / person " <> person.id.getId <> ": " <> T.pack (show err)
                return Nothing
          else return Nothing
      logDebug $ "createTicketForNewSos: creating new SOS, rideId=" <> ride.id.getId <> ", personId=" <> person.id.getId
      result <- SafetySos.createRideBasedSos (cast person.id) (cast ride.id) (cast _merchantOperatingCityId) (cast _merchantId) req.flow Nothing ticketId Nothing
      logDebug $ "createTicketForNewSos: createRideBasedSos (new) returned, sosId=" <> result.sosId.getId
      return (cast result.sosId)

callUpdateTicket :: Person.Person -> SafetyDSos.Sos -> Maybe Text -> Environment.Flow Kernel.Types.APISuccess.APISuccess
callUpdateTicket person sosDetails mbComment = do
  case sosDetails.ticketId of
    Just ticketId -> do
      fork "update ticket request" $
        void $
          TicketTools.updateTicket
            person.merchantId
            person.merchantOperatingCityId
            (Ticket.UpdateTicketReq (fromMaybe "" mbComment) ticketId Ticket.IN)
      pure Kernel.Types.APISuccess.Success
    Nothing -> pure Kernel.Types.APISuccess.Success

postSosCreate ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    API.Types.UI.Sos.SosReq ->
    Environment.Flow API.Types.UI.Sos.SosRes
  )
postSosCreate (mbPersonId, _merchantId, _merchantOperatingCityId) req = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  person <- QP.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  safetySettings <- QSafetyExtra.findSafetySettingsWithFallback personId
  rideId <- req.rideId & fromMaybeM (RideDoesNotExist "Ride Id is required")
  ride <- QRide.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  unless (personId == ride.driverId) $ throwError $ InvalidRequest "Ride does not belong to this person"
  booking <- QBooking.findById ride.bookingId >>= fromMaybeM (BookingDoesNotExist ride.bookingId.getId)
  transporterConfig <- SCTC.findByMerchantOpCityId person.merchantOperatingCityId Nothing >>= fromMaybeM (TransporterConfigNotFound person.merchantOperatingCityId.getId)
  let enableSupportForSafety = fromMaybe False transporterConfig.enableSupportForSafety
      kaptureDisposition = transporterConfig.kaptureDisposition
      kaptureQueue = transporterConfig.kaptureQueue
      trackLink = buildDriverSosTrackLink transporterConfig.trackingShortUrlPattern ride.id
  sosId <- createTicketForNewSos person ride booking _merchantOperatingCityId _merchantId enableSupportForSafety kaptureDisposition kaptureQueue trackLink req
  buildSmsReq <-
    MessageBuilder.buildSOSAlertMessage person.merchantOperatingCityId $
      MessageBuilder.BuildSOSAlertMessageReq
        { userName = driverGetName person,
          rideLink = trackLink,
          rideEndTime = fmap (T.pack . formatTime defaultTimeLocale "%e-%-m-%Y %-I:%M%P") (ride.tripEndTime :: Maybe UTCTime),
          isRideEnded = fromMaybe False req.isRideEnded
        }
  emergencyContacts <- SPDEN.getDriverDefaultEmergencyNumbers personId
  when (triggerShareRideAndNotifyContacts safetySettings) $
    SPDEN.notifyEmergencyContactsWithKey person "SOS_ALERT" Notification.SOS_TRIGGERED [("userName", driverGetName person)] (Just buildSmsReq) True emergencyContacts (Just sosId)
  return $ APISos.SosRes {sosId = sosId}
  where
    triggerShareRideAndNotifyContacts safetySettings = (fromMaybe safetySettings.notifySosWithEmergencyContacts req.notifyAllContacts) && req.flow == SafetyDSos.SafetyFlow

postSosMarkRideAsSafe ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Types.Id.Id Safety.Domain.Types.Sos.Sos ->
    API.Types.UI.Sos.MarkAsSafeReq ->
    Environment.Flow Kernel.Types.APISuccess.APISuccess
  )
postSosMarkRideAsSafe (mbPersonId, _merchantId, _) sosId API.Types.UI.Sos.MarkAsSafeReq {..} = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  person <- QP.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  void $ QSafetyExtra.findSafetySettingsWithFallback personId
  emergencyContacts <- SPDEN.getDriverDefaultEmergencyNumbers personId
  let contactsToNotify =
        case contacts of
          Nothing -> emergencyContacts
          Just contactsList ->
            if List.null contactsList
              then []
              else List.filter (\ec -> ec.mobileNumber `List.elem` contactsList) emergencyContacts
  let safetyPersonId = cast @Person.Person @SafetyCommon.Person personId
  result <- SafetySos.markSosAsSafe sosId safetyPersonId isEndLiveTracking isRideEnded
  void $ callUpdateTicket person result.updatedSos $ Just "Mark Ride as Safe"
  when result.shouldNotifyContacts $
    SPDEN.notifyEmergencyContactsWithKey person result.notificationKey Notification.SOS_RESOLVED [("userName", driverGetName person)] Nothing False contactsToNotify Nothing
  pure Kernel.Types.APISuccess.Success

uploadMedia :: Id SafetyDSos.Sos -> Id Person.Person -> SOSVideoUploadReq -> Environment.Flow AddSosVideoRes
uploadMedia sosId personId SOSVideoUploadReq {..} = do
  sosDetails <- runInReplica $ SafetySos.findSosById sosId >>= fromMaybeM (SosIdDoesNotExist sosId.getId)
  person <- runInReplica $ QP.findById personId >>= fromMaybeM (PersonNotFound (getId personId))
  transporterConfig <- SCTC.findByMerchantOpCityId person.merchantOperatingCityId Nothing >>= fromMaybeM (TransporterConfigNotFound person.merchantOperatingCityId.getId)
  fileSize <- L.runIO $ withFile payload ReadMode hFileSize
  when (fileSize > fromIntegral transporterConfig.mediaFileSizeUpperLimit) $
    throwError $ FileSizeExceededError (show fileSize)
  mediaFile <- L.runIO $ base64Encode <$> BS.readFile payload
  filePath <- S3.createFilePath "/sos/" ("sos-" <> getId sosId) fileType fileExtension
  mediaFileId <- generateGUID
  now <- getCurrentTime
  let fileUrl =
        transporterConfig.mediaFileUrlPattern
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
    Left err -> do
      logError $ "S3:put:uploadSosMedia failed, filePath=" <> filePath <> ", error=" <> T.pack (show err)
      throwError $ InternalError "S3 Upload Failed"
    Right _ -> do
      MFQuery.create fileEntity
      let currentMediaFiles = sosDetails.mediaFiles
          updatedMediaFiles = currentMediaFiles <> [mediaFileId]
      void $ SafetySos.updateSosMediaFiles updatedMediaFiles sosId
      -- Create/update Kapture ticket with media link when support for safety is enabled (parity with rider)
      let enableSupportForSafety = fromMaybe False transporterConfig.enableSupportForSafety
          dashboardFileUrl =
            maybe
              []
              ( \urlPattern ->
                  [ urlPattern
                      & T.replace "<FILE_PATH>" filePath
                  ]
              )
              transporterConfig.dashboardMediaFileUrlPattern
      when enableSupportForSafety $ do
        when (SafetySos.isRideBasedSos sosDetails.entityType) $ do
          rideId <- sosDetails.rideId & fromMaybeM (RideDoesNotExist "Ride ID not found")
          ride <- QRide.findById (cast rideId) >>= fromMaybeM (RideDoesNotExist (getId rideId))
          booking <- QBooking.findById ride.bookingId >>= fromMaybeM (BookingDoesNotExist ride.bookingId.getId)
          driverPhoneNumber <- mapM decrypt person.mobileNumber
          rideDetails <- runInReplica $ QRideDetails.findById ride.id >>= fromMaybeM (InvalidRequest $ "RideDetailsNotFound: " <> ride.id.getId)
          customerPhoneNumber <- case booking.riderId of
            Nothing -> pure Nothing
            Just riderDetailsId -> do
              riderDetails <- runInReplica $ QRiderDetails.findById riderDetailsId >>= fromMaybeM (RiderDetailsNotFound riderDetailsId.getId)
              mobileNumber <- decrypt riderDetails.mobileNumber
              pure $ Just (riderDetails.mobileCountryCode <> mobileNumber)
          let rideInfo = buildRideInfo ride booking rideDetails person driverPhoneNumber customerPhoneNumber
              trackLink = buildDriverSosTrackLink transporterConfig.trackingShortUrlPattern ride.id
              mediaLinks = ["https://" <> trackLink] <> dashboardFileUrl
          case sosDetails.ticketId of
            Just ticketId -> do
              let comment =
                    "Audio recording/shared media uploaded. Links: "
                      <> T.intercalate ", " mediaLinks
              void $
                withTryCatch "updateTicket:sendSosTracking" $
                  withShortRetry $
                    TicketTools.updateTicket person.merchantId person.merchantOperatingCityId (Ticket.UpdateTicketReq comment ticketId Ticket.IN)
            Nothing -> do
              -- Fallback: create a separate ticket only when SOS has no ticketId (e.g. ticket creation failed earlier)
              void $
                withTryCatch "createTicket:sendSosTracking" $
                  withShortRetry $
                    TicketTools.createTicket person.merchantId person.merchantOperatingCityId $
                      mkTicket person driverPhoneNumber mediaLinks rideInfo SafetyDSos.AudioRecording transporterConfig.kaptureDisposition transporterConfig.kaptureQueue
      return $ AddSosVideoRes {fileUrl = fileUrl}
