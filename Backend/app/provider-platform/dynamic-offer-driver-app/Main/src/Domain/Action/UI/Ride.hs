{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.Ride
  ( DriverRideListRes (..),
    OTPRideReq (..),
    UploadOdometerReq (..),
    UploadOdometerResp (..),
    DeliveryImageUploadReq (..),
    StopAction (..),
    listDriverRides,
    arrivedAtPickup,
    arrivedAtDestination,
    otpRideCreate,
    arrivedAtStop,
    stopAction,
    uploadOdometerReading,
    uploadDeliveryImage,
    setDriverGpsTurnedOff,
  )
where

import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM hiding (filter)
import Data.List (sortOn)
import Data.Ord
import qualified Data.Text as T hiding (count, map)
import qualified Data.Text as Text
import Data.Time (Day)
import qualified Domain.Action.Internal.ViolationDetection as VID
import qualified Domain.Action.UI.Ride.Common as RideCommon
import qualified Domain.Action.UI.RideDetails as RD
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.Location as DLoc
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.StopInformation as DSI
import qualified Domain.Types.TransporterConfig as DTC
import Environment
import qualified EulerHS.Language as L
import EulerHS.Prelude (withFile)
import EulerHS.Types (base64Encode)
import GHC.IO.Handle (hFileSize)
import GHC.IO.IOMode (IOMode (..))
import qualified IssueManagement.Domain.Types.MediaFile as MediaFile
import qualified IssueManagement.Storage.Queries.MediaFile as QMediaFile
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.External.Maps (HasCoordinates (getCoordinates))
import Kernel.External.Maps.Types
import qualified Kernel.External.Types as L
import Kernel.Prelude
import Kernel.ServantMultipart
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types (KafkaProducerTools)
import Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Domain as Domain
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import Kernel.Utils.Common
import Kernel.Utils.Error.BaseError.HTTPError.BecknAPIError
import qualified SharedLogic.Booking as SBooking
import qualified SharedLogic.CallBAP as BP
import qualified SharedLogic.CallBAPInternal as CallBAPInternal
import SharedLogic.Ride
import Storage.Beam.IssueManagement ()
import Storage.Cac.TransporterConfig as SCTC
import qualified Storage.CachedQueries.BapMetadata as CQSM
import qualified Storage.CachedQueries.Exophone as CQExophone
import Storage.CachedQueries.Merchant as QM
import qualified Storage.CachedQueries.Merchant.Overlay as CMP
import qualified Storage.CachedQueries.ValueAddNP as CQVAN
import qualified Storage.CachedQueries.VehicleServiceTier as CQVST
import qualified Storage.Flow as Storage
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.FleetDriverAssociation as FDV
import qualified Storage.Queries.FleetDriverAssociation as QFDA
import qualified Storage.Queries.Location as QLoc
import qualified Storage.Queries.LocationMapping as QLM
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Rating as QR
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RideDetails as QRD
import Storage.Queries.RiderDetails as QRID
import qualified Storage.Queries.StopInformation as QSI
import Storage.Queries.Vehicle as QVeh
import Storage.Types (FileType (..))
import qualified Text.Read as TR (read)
import Tools.Error
import qualified Tools.Notifications as TN
import TransactionLogs.Types
import Utils.Common.Cac.KeyNameConstants

data DeliveryImageUploadReq = DeliveryImageUploadReq
  { file :: FilePath,
    reqContentType :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance FromMultipart Tmp DeliveryImageUploadReq where
  fromMultipart form = do
    DeliveryImageUploadReq
      <$> fmap fdPayload (lookupFile "file" form)
      <*> fmap fdFileCType (lookupFile "file" form)

instance ToMultipart Tmp DeliveryImageUploadReq where
  toMultipart deliveryImageUploadReq =
    MultipartData
      []
      [FileData "file" (T.pack deliveryImageUploadReq.file) "" (deliveryImageUploadReq.file)]

newtype DeliveryImageUploadRes = DeliveryImageUploadRes
  { fileId :: Id MediaFile.MediaFile
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data StopAction = DEPART | ARRIVE
  deriving stock (Eq, Show, Generic, Ord)

data UploadOdometerReq = UploadOdometerReq
  { file :: FilePath,
    reqContentType :: Text,
    fileType :: FileType
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance FromMultipart Tmp UploadOdometerReq where
  fromMultipart form = do
    UploadOdometerReq
      <$> fmap fdPayload (lookupFile "file" form)
      <*> fmap fdFileCType (lookupFile "file" form)
      <*> fmap (TR.read . T.unpack) (lookupInput "fileType" form)

instance ToMultipart Tmp UploadOdometerReq where
  toMultipart uploadOdometerReq =
    MultipartData
      [Input "fileType" (show uploadOdometerReq.fileType)]
      [FileData "file" (T.pack uploadOdometerReq.file) "" (uploadOdometerReq.file)]

newtype UploadOdometerResp = UploadOdometerResp
  { fileId :: Id MediaFile.MediaFile
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CoinsEarned = CoinsEarned
  { coins :: Int,
    eventType :: Text
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data OTPRideReq = OTPRideReq
  { specialZoneOtpCode :: Text,
    point :: LatLong,
    odometer :: Maybe DRide.OdometerReading
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

newtype DriverRideListRes = DriverRideListRes
  { list :: [RideCommon.DriverRideRes]
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

listDriverRides ::
  (EsqDBReplicaFlow m r, EncFlow m r, EsqDBFlow m r, CacheFlow m r) =>
  Id DP.Person ->
  Maybe (Id DMOC.MerchantOperatingCity) ->
  Maybe Integer ->
  Maybe Integer ->
  Maybe Bool ->
  Maybe DRide.RideStatus ->
  Maybe Day ->
  Maybe Text ->
  Maybe Int ->
  m DriverRideListRes
listDriverRides driverId mocId mbLimit mbOffset mbOnlyActive mbRideStatus mbDay mbFleetOwnerId mbNumOfDays = do
  driverInfo <- runInReplica $ QDI.findById driverId >>= fromMaybeM (DriverNotFound driverId.getId)
  transporterConfig <- case mocId of
    Just id -> SCTC.findByMerchantOpCityId id Nothing
    Nothing -> pure Nothing
  rides <- case (driverInfo.onRide, mbOnlyActive) of
    (True, Just True) -> QRide.getActiveBookingAndRideByDriverId driverId
    (False, Just True) -> return []
    _ -> QRide.findAllByDriverId driverId mbLimit mbOffset mbOnlyActive mbRideStatus mbDay mbNumOfDays

  driverRideLis <- forM rides $ \(ride, booking) -> do
    rideDetail <- runInReplica $ QRD.findById ride.id >>= fromMaybeM (VehicleNotFound driverId.getId)
    rideRating <- runInReplica $ QR.findRatingForRide ride.id
    driverNumber <- RD.getDriverNumber rideDetail
    mbExophone <- CQExophone.findByPrimaryPhone booking.primaryExophone
    bapMetadata <- CQSM.findBySubscriberIdAndDomain (Id booking.bapId) Domain.MOBILITY
    riderCallingNumber <- case transporterConfig >>= DTC.driverCallingOption of
      Just option | ride.status `notElem` [DRide.COMPLETED, DRide.CANCELLED] -> getRiderMobileNumber booking option
      _ -> pure Nothing
    isValueAddNP <- CQVAN.isValueAddNP booking.bapId
    stopsInfo <- if (fromMaybe False ride.hasStops) then QSI.findAllByRideId ride.id else return []
    let goHomeReqId = ride.driverGoHomeRequestId
    RideCommon.mkDriverRideRes rideDetail driverNumber rideRating mbExophone (ride, booking) bapMetadata goHomeReqId (Just driverInfo) isValueAddNP stopsInfo riderCallingNumber
  filteredRides <- case mbFleetOwnerId of
    Just fleetOwnerId -> do
      isFleetDriver <- FDV.findByDriverIdAndFleetOwnerId driverId fleetOwnerId True
      unless (isJust isFleetDriver) $ throwError (InvalidRequest "Driver is not the  part of this fleet")
      return $ filter (\ride -> ride.fleetOwnerId == (Just fleetOwnerId)) driverRideLis
    Nothing -> return driverRideLis
  pure . DriverRideListRes $ sortOn (Down . (.bookingType)) filteredRides

arrivedAtPickup :: (EncFlow m r, CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, HasShortDurationRetryCfg r c, HasFlowEnv m r '["nwAddress" ::: BaseUrl], HasHttpClientOptions r c, HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl], HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools], HasFlowEnv m r '["ondcTokenHashMap" ::: HM.HashMap KeyConfig TokenConfig]) => Id DRide.Ride -> LatLong -> m APISuccess
arrivedAtPickup rideId req = do
  ride <- runInReplica (QRide.findById rideId) >>= fromMaybeM (RideDoesNotExist rideId.getId)
  unless (isValidRideStatus (ride.status)) $ throwError $ RideInvalidStatus ("The ride has already started." <> Text.pack (show ride.status))
  booking <- runInReplica $ QBooking.findById ride.bookingId >>= fromMaybeM (BookingDoesNotExist ride.bookingId.getId)
  let pickupLoc = getCoordinates booking.fromLocation
  let distance = distanceBetweenInMeters req pickupLoc
  transporterConfig <- SCTC.findByMerchantOpCityId booking.merchantOperatingCityId (Just (TransactionId (Id booking.transactionId))) >>= fromMaybeM (TransporterConfigNotFound booking.merchantOperatingCityId.getId)
  unless (distance < transporterConfig.arrivedPickupThreshold) $ throwError $ DriverNotAtPickupLocation ride.driverId.getId
  unless (isJust ride.driverArrivalTime) $ do
    now <- getCurrentTime
    QRide.updateArrival rideId now
    BP.sendDriverArrivalUpdateToBAP booking ride (Just now)
    -- Extra Fare Mitigation warning --
    driverInfo <- runInReplica $ QDI.findById ride.driverId >>= fromMaybeM (DriverNotFound ride.driverId.getId)
    when (fromMaybe False driverInfo.extraFareMitigationFlag) $ fork "Extra Fare Mitigation Warning" $ notifyDriverOnExtraFareWarning ride.driverId ride.merchantOperatingCityId

  pure Success
  where
    isValidRideStatus status = status == DRide.NEW

    notifyDriverOnExtraFareWarning driverId moCityId = do
      driver <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
      mbVehicle <- QVeh.findById driverId
      let mbVehicleCategory = mbVehicle >>= (.category)
      overlay <- CMP.findByMerchantOpCityIdPNKeyLangaugeUdfVehicleCategory moCityId "EXTRA_FARE_MITIGATION_WARNING" L.ENGLISH Nothing mbVehicleCategory Nothing >>= fromMaybeM (OverlayKeyNotFound "EXTRA_FARE_MITIGATION_WARNING")
      TN.sendOverlay moCityId driver $ TN.mkOverlayReq overlay
      QDI.updateExtraFareMitigation (Just False) driverId

otpRideCreate :: DP.Person -> Text -> DRB.Booking -> Maybe Text -> Flow RideCommon.DriverRideRes
otpRideCreate driver otpCode booking clientId = do
  transporter <-
    QM.findById booking.providerId
      >>= fromMaybeM (MerchantNotFound booking.providerId.getId)
  vehicle <- QVeh.findById driver.id >>= fromMaybeM (VehicleNotFound driver.id.getId)
  transporterConfig <- SCTC.findByMerchantOpCityId booking.merchantOperatingCityId (Just (TransactionId (Id booking.transactionId))) >>= fromMaybeM (TransporterConfigNotFound booking.merchantOperatingCityId.getId)
  isVehicleVariantNotAllowed <- isNotAllowedVehicleVariant vehicle.variant booking.vehicleServiceTier
  when isVehicleVariantNotAllowed $ throwError $ InvalidRequest "Wrong Vehicle Variant"
  when (booking.status `elem` [DRB.COMPLETED, DRB.CANCELLED]) $ throwError (BookingInvalidStatus $ show booking.status)
  driverInfo <- QDI.findById (cast driver.id) >>= fromMaybeM DriverInfoNotFound
  unless (driverInfo.subscribed) $ throwError DriverUnsubscribed
  unless (driverInfo.enabled) $ throwError DriverAccountDisabled
  when driverInfo.blocked $ throwError (DriverAccountBlocked (BlockErrorPayload driverInfo.blockExpiryTime driverInfo.blockReasonFlag))
  throwErrorOnRide transporterConfig.includeDriverCurrentlyOnRide driverInfo False
  mFleetOwnerId <- QFDA.findByDriverId driver.id True
  (ride, rideDetails, _) <- initializeRide transporter driver booking (Just otpCode) Nothing clientId Nothing (mFleetOwnerId <&> (.fleetOwnerId) <&> Id)
  uBooking <- runInReplica $ QBooking.findById booking.id >>= fromMaybeM (BookingNotFound booking.id.getId) -- in replica db we can have outdated value
  handle (errHandler uBooking transporter) $ BP.sendRideAssignedUpdateToBAP uBooking ride driver vehicle False

  driverNumber <- RD.getDriverNumber rideDetails
  stopsInfo <- if (fromMaybe False ride.hasStops) then QSI.findAllByRideId ride.id else return []
  mbExophone <- CQExophone.findByPrimaryPhone booking.primaryExophone
  bapMetadata <- CQSM.findBySubscriberIdAndDomain (Id booking.bapId) Domain.MOBILITY
  riderCallingNumber <- case DTC.driverCallingOption transporterConfig of
    Just option | ride.status `notElem` [DRide.COMPLETED, DRide.CANCELLED] -> getRiderMobileNumber booking option
    _ -> pure Nothing
  isValueAddNP <- CQVAN.isValueAddNP booking.bapId
  RideCommon.mkDriverRideRes rideDetails driverNumber Nothing mbExophone (ride, booking) bapMetadata ride.driverGoHomeRequestId Nothing isValueAddNP stopsInfo riderCallingNumber
  where
    errHandler uBooking transporter exc
      | Just BecknAPICallError {} <- fromException @BecknAPICallError exc = SBooking.cancelBooking uBooking (Just driver) transporter >> throwM exc
      | Just ExternalAPICallError {} <- fromException @ExternalAPICallError exc = SBooking.cancelBooking uBooking (Just driver) transporter >> throwM exc
      | otherwise = throwM exc

    isNotAllowedVehicleVariant driverVehicleVariant bookingServiceTier = do
      vehicleServiceTierItem <- CQVST.findByServiceTierTypeAndCityIdInRideFlow bookingServiceTier booking.merchantOperatingCityId booking.configInExperimentVersions >>= fromMaybeM (VehicleServiceTierNotFound (show bookingServiceTier))
      return $ driverVehicleVariant `notElem` vehicleServiceTierItem.allowedVehicleVariant

arrivedAtStop :: Id DRide.Ride -> LatLong -> Flow APISuccess
arrivedAtStop rideId pt = do
  ride <- runInReplica (QRide.findById rideId) >>= fromMaybeM (RideDoesNotExist rideId.getId)
  driver <- QPerson.findById ride.driverId >>= fromMaybeM (PersonNotFound ride.driverId.getId)
  vehicle <- QVeh.findById ride.driverId >>= fromMaybeM (DriverWithoutVehicle ride.driverId.getId)
  booking <- runInReplica $ QBooking.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
  unless (isValidRideStatus ride.status) $ throwError $ RideInvalidStatus ("This ride " <> ride.id.getId <> " is not in progress" <> Text.pack (show ride.status))
  unless (isJust booking.stopLocationId) $ throwError (InvalidRequest $ "Can't find stop to be reached for ride " <> ride.id.getId)
  case booking.stopLocationId of
    Nothing -> throwError $ InvalidRequest ("No stop present to be reached for ride " <> ride.id.getId)
    Just nextStopId -> do
      stopLoc <- runInReplica $ QLoc.findById nextStopId >>= fromMaybeM (InvalidRequest $ "Stop location doesn't exist for ride " <> ride.id.getId)
      let curPt = LatLong stopLoc.lat stopLoc.lon
          distance = distanceBetweenInMeters pt curPt
      transporterConfig <- SCTC.findByMerchantOpCityId booking.merchantOperatingCityId (Just (TransactionId (Id booking.transactionId))) >>= fromMaybeM (TransporterConfigNotFound booking.merchantOperatingCityId.getId)
      unless (distance < fromMaybe 500 transporterConfig.arrivedStopThreshold) $ throwError $ InvalidRequest ("Driver is not at stop location for ride " <> ride.id.getId)
      QBooking.updateStopArrival booking.id
      BP.sendStopArrivalUpdateToBAP booking ride driver vehicle
      pure Success
  where
    isValidRideStatus status = status == DRide.INPROGRESS

stopAction :: Id DRide.Ride -> LatLong -> Id DLoc.Location -> StopAction -> Flow APISuccess
stopAction rideId pt stopLocId action = do
  ride <- runInReplica (QRide.findById rideId) >>= fromMaybeM (RideDoesNotExist rideId.getId)
  void $ QPerson.findById ride.driverId >>= fromMaybeM (PersonNotFound ride.driverId.getId)
  void $ QVeh.findById ride.driverId >>= fromMaybeM (DriverWithoutVehicle ride.driverId.getId)
  void $ runInReplica $ QBooking.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
  stopsLM <- QLM.getLatestStopsByEntityId rideId.getId
  unless (isValidRideStatus ride.status) $ throwError $ RideInvalidStatus ("This ride " <> ride.id.getId <> " is not in progress" <> Text.pack (show ride.status))
  when (null stopsLM) $ throwError $ InvalidRequest ("No stop present to be reached for ride " <> ride.id.getId)
  stopLM <- find (\lm -> lm.locationId == stopLocId) stopsLM & fromMaybeM (InvalidRequest $ "Stop location with Id " <> stopLocId.getId <> "doesn't exist for ride " <> ride.id.getId)
  stopsInfo <- QSI.findAllByRideId rideId
  now <- getCurrentTime
  appBackendBapInternal <- asks (.appBackendBapInternal)
  case action of
    DEPART -> do
      stopInfo <- find (\stop -> stop.stopLocId == stopLocId) stopsInfo & fromMaybeM (InvalidRequest ("Invalid Stop depart request with stopLocId" <> stopLocId.getId <> "for ride " <> ride.id.getId))
      QSI.updateByStopLocIdAndRideId (Just now) (Just pt) stopLocId rideId
      let request = CallBAPInternal.StopEventsReq CallBAPInternal.Depart rideId stopLM.order stopInfo.waitingTimeStart (Just now)
      void $ CallBAPInternal.stopEvents appBackendBapInternal.apiKey appBackendBapInternal.url request
      let currentPassStop = LatLong stopInfo.stopStartLatLng.lat stopInfo.stopStartLatLng.lon
      existingStops <- Redis.runInMultiCloudRedis False $ Redis.get (VID.mkReachedStopKey rideId)
      case existingStops of
        Just reachedStopList -> do
          unless (currentPassStop `elem` reachedStopList) $ do
            let updatedList = reachedStopList ++ [currentPassStop]
            Redis.setExp (VID.mkReachedStopKey rideId) updatedList 86400
        Nothing -> do
          Redis.setExp (VID.mkReachedStopKey rideId) [currentPassStop] 86400
      pure Success
    ARRIVE -> do
      unless (isValidStopArrivedAction stopLM stopsInfo) $ throwError $ InvalidRequest ("Invalid Stop arrived request with stopLocId " <> stopLocId.getId <> "for ride " <> ride.id.getId)
      id <- generateGUID
      let stopInfo =
            DSI.StopInformation
              { stopLocId = stopLocId,
                stopOrder = stopLM.order,
                waitingTimeStart = now,
                waitingTimeEnd = Nothing,
                stopStartLatLng = pt,
                rideId = rideId,
                stopEndLatLng = Nothing,
                createdAt = now,
                updatedAt = now,
                id,
                merchantOperatingCityId = Just ride.merchantOperatingCityId,
                merchantId = ride.merchantId
              }
      QSI.create stopInfo
      let request = CallBAPInternal.StopEventsReq CallBAPInternal.Arrive rideId stopLM.order now Nothing
      void $ CallBAPInternal.stopEvents appBackendBapInternal.apiKey appBackendBapInternal.url request
      pure Success
  where
    isValidStopArrivedAction stopLM =
      all (\stopInfo -> stopInfo.stopOrder < stopLM.order && isJust stopInfo.waitingTimeEnd && isJust stopInfo.stopEndLatLng)
    -- isValidStopDepartedAction stopsInfo = do
    --   let stopInfo = find (\stop -> stop.stopLocId == stopLocId) stopsInfo
    --   isJust stopInfo
    isValidRideStatus status = status == DRide.INPROGRESS

uploadOdometerReading ::
  Id DMOC.MerchantOperatingCity ->
  Id DRide.Ride ->
  UploadOdometerReq ->
  Flow UploadOdometerResp
uploadOdometerReading merchantOpCityId rideId UploadOdometerReq {..} = do
  contentType <- validateContentType
  ride <- QRide.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  booking <- QBooking.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
  config <- SCTC.findByMerchantOpCityId merchantOpCityId (Just (TransactionId (Id booking.transactionId))) >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  fileSize <- L.runIO $ withFile file ReadMode hFileSize
  when (fileSize > fromIntegral config.mediaFileSizeUpperLimit) $
    throwError $ FileSizeExceededError (show fileSize)
  mediaFile <- L.runIO $ base64Encode <$> BS.readFile file
  filePath <- Storage.createFilePath "/odometer-reading/" ("rideId-" <> rideId.getId) fileType ("." <> contentType)
  let fileUrl =
        config.mediaFileUrlPattern
          & T.replace "<DOMAIN>" "issue"
          & T.replace "<FILE_PATH>" filePath
  _ <- fork "Storage Put Odometer Reading File" $ Storage.put (T.unpack filePath) mediaFile
  createMediaEntry fileUrl filePath
  where
    validateContentType = do
      case fileType of
        Audio | reqContentType == "audio/wave" -> pure "wav"
        Audio | reqContentType == "audio/mpeg" -> pure "mp3"
        Audio | reqContentType == "audio/mp4" -> pure "mp4"
        Image | reqContentType == "image/png" -> pure "png"
        Image | reqContentType == "image/jpeg" -> pure "jpg"
        _ -> throwError $ FileFormatNotSupported reqContentType

    createMediaEntry url filePath = do
      fileEntity <- mkFile url
      QMediaFile.create fileEntity
      return $ UploadOdometerResp {fileId = cast $ fileEntity.id}
      where
        mkFile fileUrl = do
          id <- generateGUID
          now <- getCurrentTime
          return $
            MediaFile.MediaFile
              { id,
                _type = fileType,
                url = fileUrl,
                s3FilePath = Just filePath,
                createdAt = now
              }

uploadDeliveryImage ::
  Id DMOC.MerchantOperatingCity ->
  Id DRide.Ride ->
  DeliveryImageUploadReq ->
  Flow APISuccess
uploadDeliveryImage merchantOpCityId rideId DeliveryImageUploadReq {..} = do
  contentType <- validateContentType
  ride <- QRide.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  unless (ride.status == DRide.NEW) $ throwError $ InvalidRequest "Cannot upload image due to unexpected ride status"
  booking <- QBooking.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
  config <- SCTC.findByMerchantOpCityId merchantOpCityId (Just (TransactionId (Id booking.transactionId))) >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  fileSize <- L.runIO $ withFile file ReadMode hFileSize
  when (fileSize > fromIntegral config.mediaFileSizeUpperLimit) $
    throwError $ FileSizeExceededError (show fileSize)
  mediaFile <- L.runIO $ base64Encode <$> BS.readFile file
  filePath <- Storage.createFilePath "/delivery/" ("rideId-" <> rideId.getId) Image ("." <> contentType)
  let fileUrl =
        config.mediaFileUrlPattern
          & T.replace "<DOMAIN>" "issue"
          & T.replace "<FILE_PATH>" filePath
  _ <- fork "Storage Put Delivery Image File" $ Storage.put (T.unpack filePath) mediaFile
  now <- getCurrentTime
  fileId <- createMediaEntry fileUrl filePath now
  let deliveryFileIds = fromMaybe [fileId] (ride.deliveryFileIds <&> (\dFileIds -> dFileIds ++ [fileId]))
  QRide.updateDeliveryFileIds rideId deliveryFileIds now
  BP.notfyDeliveryImageUploadedToBAP booking ride
  return Success
  where
    validateContentType = do
      case reqContentType of
        "image/png" -> pure "png"
        "image/jpeg" -> pure "jpg"
        _ -> throwError $ FileFormatNotSupported reqContentType

    createMediaEntry url filePath now = do
      fileEntity <- mkFile url
      QMediaFile.create fileEntity
      return fileEntity.id
      where
        mkFile fileUrl = do
          id <- generateGUID
          return $
            MediaFile.MediaFile
              { id,
                _type = Image,
                url = fileUrl,
                s3FilePath = Just filePath,
                createdAt = now
              }

arrivedAtDestination :: Id DRide.Ride -> LatLong -> Flow APISuccess
arrivedAtDestination rideId pt = do
  ride <- runInReplica (QRide.findById rideId) >>= fromMaybeM (RideDoesNotExist rideId.getId)
  booking <- runInReplica $ QBooking.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
  unless (isValidRideStatus ride.status) $ throwError $ RideInvalidStatus ("This ride " <> ride.id.getId <> " is not in progress" <> Text.pack (show ride.status))
  destLoc <- booking.toLocation & fromMaybeM (InvalidRequest "To location doesn't exist for ride")
  let curPt = LatLong destLoc.lat destLoc.lon
      distance = distanceBetweenInMeters pt curPt
  transporterConfig <- SCTC.findByMerchantOpCityId booking.merchantOperatingCityId (Just (TransactionId (Id booking.transactionId))) >>= fromMaybeM (TransporterConfigNotFound booking.merchantOperatingCityId.getId)
  let dropLocThreshold = metersToHighPrecMeters transporterConfig.dropLocThreshold
  unless (distance < dropLocThreshold) $ throwError $ InvalidRequest ("Driver is not at destination location for ride " <> ride.id.getId)
  unless (isJust ride.destinationReachedAt) $ do
    now <- getCurrentTime
    QRide.updateDestinationArrival ride.id now
    BP.sendDestinationArrivalUpdateToBAP booking ride (Just now)
  pure Success
  where
    isValidRideStatus status = status == DRide.INPROGRESS

getRiderMobileNumber ::
  (EsqDBReplicaFlow m r, EncFlow m r, EsqDBFlow m r, CacheFlow m r) =>
  DRB.Booking ->
  DTC.CallingOption ->
  m (Maybe Text)
getRiderMobileNumber booking option = do
  mbRiderNumber <-
    if option == DTC.DirectCall || option == DTC.DualCall
      then case booking.riderId of
        Nothing -> pure Nothing
        Just riderId -> do
          mbRider <- QRID.findById riderId
          case mbRider of
            Nothing -> pure Nothing
            Just rider -> Just <$> decrypt rider.mobileNumber
      else pure Nothing
  pure mbRiderNumber

setDriverGpsTurnedOff :: Id DRide.Ride -> Flow APISuccess
setDriverGpsTurnedOff rideId = do
  ride <- runInReplica (QRide.findById rideId) >>= fromMaybeM (RideDoesNotExist rideId.getId)
  unless (ride.status == DRide.INPROGRESS) $ throwError $ RideInvalidStatus ("This ride " <> ride.id.getId <> " is not in progress")
  when (not $ fromMaybe False ride.driverGpsTurnedOff) $ void $ QRide.updateDriverGpsTurnedOff (Just True) rideId
  logInfo $ "Driver GPS turned off flag set for rideId: " <> rideId.getId
  pure Success
