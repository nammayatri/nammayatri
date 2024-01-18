{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Domain.Action.UI.Sos
  ( SosReq (..),
    SosRes (..),
    SosDetailsRes (..),
    SosUpdateReq (..),
    SOSVideoUploadReq (..),
    AddSosVideoRes (..),
    createSosDetails,
    updateSosDetails,
    markRideAsSafe,
    addSosVideo,
    getSosDetails,
  )
where

import AWS.S3 as S3
import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Message as Common
import qualified Data.ByteString as BS
import Data.Text as T
import Data.Time.Format.ISO8601 (iso8601Show)
import Domain.Action.UI.Profile (getDefaultEmergencyNumbers)
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.Person as Person
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.Sos as DSos
import qualified Domain.Types.Sos as SosStatus
import Environment
import qualified EulerHS.Language as L
import EulerHS.Prelude (withFile)
import EulerHS.Types (base64Encode)
import GHC.IO.Handle (hFileSize)
import GHC.IO.IOMode (IOMode (..))
import qualified IssueManagement.Domain.Types.MediaFile as DMF
import qualified IssueManagement.Storage.Queries.MediaFile as MFQuery
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.External.Ticket.Interface.Types as Ticket
import Kernel.Prelude
import Kernel.ServantMultipart
import Kernel.Sms.Config
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Types.APISuccess as APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.MessageBuilder as MessageBuilder
import Storage.Beam.IssueManagement ()
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.RiderConfig as QRC
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.Sos as QSos
import qualified Text.Read as Read
import Tools.Error
import Tools.SMS as Sms
import Tools.Ticket as Ticket

data SosReq = SosReq
  { flow :: DSos.SosType,
    rideId :: Id DRide.Ride
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data SOSVideoUploadReq = SOSVideoUploadReq
  { video :: FilePath,
    fileType :: Common.FileType,
    reqContentType :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance FromMultipart Tmp SOSVideoUploadReq where
  fromMultipart form = do
    SOSVideoUploadReq
      <$> fmap fdPayload (lookupFile "video" form)
      <*> (lookupInput "fileType" form >>= (Read.readEither . T.unpack))
      <*> fmap fdFileCType (lookupFile "video" form)

instance ToMultipart Tmp SOSVideoUploadReq where
  toMultipart sosVideoUploadReq =
    MultipartData
      [Input "fileType" (show sosVideoUploadReq.fileType)]
      [FileData "video" (T.pack sosVideoUploadReq.video) "" (sosVideoUploadReq.video)]

data SosUpdateReq = SosUpdateReq
  { status :: DSos.SosStatus,
    comment :: Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

newtype SosRes = SosRes
  { sosId :: Id DSos.Sos
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

newtype SosDetailsRes = SosDetailsRes
  { sosId :: Maybe (Id DSos.Sos)
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

newtype AddSosVideoRes = AddSosVideoRes
  { fileUrl :: Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

createSosDetails ::
  ( EsqDBReplicaFlow m r,
    EsqDBFlow m r,
    HasFlowEnv m r '["smsCfg" ::: SmsConfig],
    EncFlow m r,
    CacheFlow m r,
    MonadFlow m
  ) =>
  Id Person.Person ->
  Id Merchant.Merchant ->
  SosReq ->
  m SosRes
createSosDetails personId merchantId req = do
  smsCfg <- asks (.smsCfg)
  person <- QP.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  phoneNumber <- mapM decrypt person.mobileNumber
  ride <- QRide.findById req.rideId >>= fromMaybeM (RideDoesNotExist req.rideId.getId)
  merchantConfig <- CQM.findById (person.merchantId) >>= fromMaybeM (MerchantNotFound person.merchantId.getId)
  riderConfig <- QRC.findByMerchantOperatingCityId person.merchantOperatingCityId >>= fromMaybeM (RiderConfigDoesNotExist person.merchantOperatingCityId.getId)
  let rideInfo = buildRideInfo ride person phoneNumber
      trackLink = merchantConfig.trackingShortUrlPattern <> ride.shortId.getShortId
  ticketId <- do
    if riderConfig.enableSupportForSafety
      then do
        ticketResponse <- try @_ @SomeException (createTicket person.merchantId person.merchantOperatingCityId (mkTicket person phoneNumber ["https://" <> trackLink] rideInfo merchantConfig.kaptureDisposition))
        case ticketResponse of
          Right ticketResponse' -> return (Just ticketResponse'.ticketId)
          Left _ -> return Nothing
      else return Nothing
  when (person.shareEmergencyContacts) $ do
    emergencyContacts <- getDefaultEmergencyNumbers (personId, merchantId)
    let sender = smsCfg.sender
    message <-
      MessageBuilder.buildSOSAlertMessage person.merchantOperatingCityId $
        MessageBuilder.BuildSOSAlertMessageReq
          { userName = getName person,
            rideLink = trackLink
          }
    for_ emergencyContacts.defaultEmergencyNumbers $ \emergencyContact -> do
      let emergencyPhoneNumber = emergencyContact.mobileCountryCode <> emergencyContact.mobileNumber
      Sms.sendSMS person.merchantId person.merchantOperatingCityId (Sms.SendSMSReq message emergencyPhoneNumber sender)
        >>= Sms.checkSmsResult
  sosDetails <- buildSosDetails personId req ticketId
  void $ QSos.create sosDetails
  return $
    SosRes
      { sosId = sosDetails.id
      }

getSosDetails :: (CacheFlow m r, MonadFlow m, EsqDBFlow m r) => Id DRide.Ride -> Id Person.Person -> m SosDetailsRes
getSosDetails rideId_ personId_ = do
  sosDetails <- QSos.findByRideIdAndStatus rideId_ DSos.Resolved
  unless (personId_ == maybe "" (.personId) sosDetails) $ throwError $ InvalidRequest "PersonId not same"
  return SosDetailsRes {sosId = (.id) <$> sosDetails}

updateSosDetails ::
  Id DSos.Sos -> (Id Person.Person, Id Merchant.Merchant) -> SosUpdateReq -> Flow APISuccess.APISuccess
updateSosDetails sosId (personId, _) req = do
  person <- QP.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  sosDetails <- runInReplica $ QSos.findById sosId >>= fromMaybeM (SosIdDoesNotExist sosId.getId)
  unless (personId == sosDetails.personId) $ throwError $ InvalidRequest "PersonId not same"
  void $ QSos.updateStatus sosId (req.status)
  void $ callUpdateTicket person sosDetails req.comment
  pure APISuccess.Success

buildSosDetails :: (EncFlow m r) => Id Person.Person -> SosReq -> Maybe Text -> m DSos.Sos
buildSosDetails personId req ticketId = do
  pid <- generateGUID
  now <- getCurrentTime
  return
    DSos.Sos
      { id = pid,
        personId = personId,
        status = DSos.Pending,
        flow = req.flow,
        rideId = req.rideId,
        ticketId = ticketId,
        createdAt = now,
        updatedAt = now
      }

addSosVideo :: Id DSos.Sos -> Id Person.Person -> SOSVideoUploadReq -> Flow AddSosVideoRes
addSosVideo sosId personId SOSVideoUploadReq {..} = do
  sosDetails <- runInReplica $ QSos.findById sosId >>= fromMaybeM (SosIdDoesNotExist sosId.getId)
  person <- runInReplica $ QP.findById personId >>= fromMaybeM (PersonNotFound (getId personId))
  contentType <- validateContentType
  merchantConfig <- CQM.findById (person.merchantId) >>= fromMaybeM (MerchantNotFound person.merchantId.getId)
  riderConfig <- QRC.findByMerchantOperatingCityId person.merchantOperatingCityId >>= fromMaybeM (RiderConfigDoesNotExist person.merchantOperatingCityId.getId)
  fileSize <- L.runIO $ withFile video ReadMode hFileSize
  when (fileSize > fromIntegral riderConfig.videoFileSizeUpperLimit) $
    throwError $ FileSizeExceededError (show fileSize)
  mediaFile <- L.runIO $ base64Encode <$> BS.readFile video
  filePath <- createFilePath (getId sosId) DMF.Video contentType
  let fileUrl =
        merchantConfig.publicMediaFileUrlPattern
          & T.replace "<DOMAIN>" "sos-video"
          & T.replace "<FILE_PATH>" filePath
  result <- try @_ @SomeException $ S3.putPublic (T.unpack filePath) mediaFile
  case result of
    Left err -> throwError $ InternalError ("S3 Upload Failed: " <> show err)
    Right _ -> do
      ride <- QRide.findById sosDetails.rideId >>= fromMaybeM (RideDoesNotExist sosDetails.rideId.getId)
      phoneNumber <- mapM decrypt person.mobileNumber
      let rideInfo = buildRideInfo ride person phoneNumber
          trackLink = merchantConfig.trackingShortUrlPattern <> ride.shortId.getShortId
      when riderConfig.enableSupportForSafety $
        void $ try @_ @SomeException $ withShortRetry (createTicket person.merchantId person.merchantOperatingCityId (mkTicket person phoneNumber ["https://" <> trackLink, fileUrl] rideInfo merchantConfig.kaptureDisposition))
      createMediaEntry Common.AddLinkAsMedia {url = fileUrl, fileType}
  where
    validateContentType = do
      case fileType of
        Common.Video | reqContentType == "video/mp4" -> pure "mp4"
        _ -> throwError $ FileFormatNotSupported reqContentType

createFilePath ::
  Text ->
  DMF.MediaType ->
  Text ->
  Flow Text
createFilePath sosId fileType videoExtension = do
  pathPrefix <- asks (.s3EnvPublic.pathPrefix)
  now <- getCurrentTime
  let fileName = T.replace (T.singleton ':') (T.singleton '-') (T.pack $ iso8601Show now)
  return
    ( pathPrefix <> "/sos-video/" <> "sos-" <> sosId <> "/"
        <> show fileType
        <> "/"
        <> fileName
        <> videoExtension
    )

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
            _type = DMF.Video,
            url = fileUrl,
            createdAt = now
          }

mkTicket :: Person.Person -> Maybe Text -> [Text] -> Ticket.RideInfo -> Text -> Ticket.CreateTicketReq
mkTicket person phoneNumber mediaLinks info disposition = do
  Ticket.CreateTicketReq
    { category = "Code Red",
      subCategory = Just "SOS Alert (follow-back)",
      disposition,
      issueId = Nothing,
      issueDescription = "SOS called",
      mediaFiles = Just mediaLinks,
      name = Just $ getName person,
      phoneNo = phoneNumber,
      personId = person.id.getId,
      classification = Ticket.CUSTOMER,
      rideDescription = Just info
    }

buildRideInfo :: DRide.Ride -> Person.Person -> Maybe Text -> Ticket.RideInfo
buildRideInfo ride person phoneNumber =
  Ticket.RideInfo
    { rideShortId = ride.shortId.getShortId,
      customerName = Just $ getName person,
      customerPhoneNo = phoneNumber,
      driverName = Just ride.driverName,
      driverPhoneNo = Just ride.driverMobileNumber,
      vehicleNo = ride.vehicleNumber,
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

markRideAsSafe ::
  (Id Person.Person, Id Merchant.Merchant) ->
  Id DSos.Sos ->
  Flow APISuccess.APISuccess
markRideAsSafe (personId, merchantId) sosId = do
  person <- QP.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  sosDetails <- runInReplica $ QSos.findById sosId >>= fromMaybeM (SosIdDoesNotExist sosId.getId)
  smsCfg <- asks (.smsCfg)
  void $ callUpdateTicket person sosDetails "Mark Ride as Safe"
  void $ QSos.updateStatus sosId SosStatus.Resolved
  emergencyContacts <- getDefaultEmergencyNumbers (personId, merchantId)
  let sender = smsCfg.sender
  when (person.shareEmergencyContacts) $ do
    message <-
      MessageBuilder.buildMarkRideAsSafeMessage person.merchantOperatingCityId $
        MessageBuilder.BuildMarkRideAsSafeMessageReq
          { userName = getName person
          }
    for_ emergencyContacts.defaultEmergencyNumbers $ \emergencyContact -> do
      let phoneNumber = emergencyContact.mobileCountryCode <> emergencyContact.mobileNumber
      Sms.sendSMS merchantId person.merchantOperatingCityId (Sms.SendSMSReq message phoneNumber sender)
        >>= Sms.checkSmsResult
  pure APISuccess.Success

callUpdateTicket :: Person.Person -> DSos.Sos -> Text -> Flow APISuccess.APISuccess
callUpdateTicket person sosDetails comment = do
  case sosDetails.ticketId of
    Just ticketId -> do
      fork "update ticket request" $
        void $ Ticket.updateTicket (person.merchantId) person.merchantOperatingCityId (Ticket.UpdateTicketReq comment ticketId Ticket.IN)
      pure APISuccess.Success
    Nothing -> pure APISuccess.Success

getName :: Person.Person -> Text
getName person = (fromMaybe "" person.firstName) <> " " <> (fromMaybe "" person.lastName)
