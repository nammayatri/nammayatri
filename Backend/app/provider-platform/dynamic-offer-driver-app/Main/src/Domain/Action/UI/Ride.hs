{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Domain.Action.UI.Ride
  ( DriverRideRes (..),
    DriverRideListRes (..),
    OTPRideReq (..),
    UploadOdometerReq (..),
    UploadOdometerResp (..),
    listDriverRides,
    arrivedAtPickup,
    otpRideCreate,
    arrivedAtStop,
    uploadOdometerReading,
  )
where

import qualified AWS.S3 as S3
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T hiding (count, map)
import Data.Time (Day)
import Domain.Action.Dashboard.Ride
import qualified Domain.Types.BapMetadata as DSM
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.Common as DTC
import qualified Domain.Types.Driver.GoHomeFeature.DriverGoHomeRequest as DDGR
import qualified Domain.Types.DriverInformation as DI
import qualified Domain.Types.Exophone as DExophone
import qualified Domain.Types.Location as DLoc
import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Rating as DRating
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.RideDetails as RD
import qualified Domain.Types.Vehicle as DVeh
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
import Kernel.Prelude
import Kernel.ServantMultipart
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Types.APISuccess
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import Kernel.Utils.Common
import Kernel.Utils.Error.BaseError.HTTPError.BecknAPIError
import qualified SharedLogic.Booking as SBooking
import qualified SharedLogic.CallBAP as BP
import SharedLogic.FareCalculator (fareSum)
import SharedLogic.Ride
import Storage.Beam.IssueManagement ()
import qualified Storage.CachedQueries.BapMetadata as CQSM
import qualified Storage.CachedQueries.Exophone as CQExophone
import Storage.CachedQueries.Merchant as QM
import Storage.CachedQueries.Merchant.TransporterConfig as QMTC
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.Location as QLoc
import qualified Storage.Queries.LocationMapping as QLM
import qualified Storage.Queries.Rating as QR
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RideDetails as QRD
import Storage.Queries.Vehicle as QVeh
import qualified Text.Read as TR (read)
import Tools.Error

data UploadOdometerReq = UploadOdometerReq
  { file :: FilePath,
    reqContentType :: Text,
    fileType :: S3.FileType
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

data DriverRideRes = DriverRideRes
  { id :: Id DRide.Ride,
    shortRideId :: ShortId DRide.Ride,
    status :: DRide.RideStatus,
    fromLocation :: DLoc.LocationAPIEntity,
    toLocation :: Maybe DLoc.LocationAPIEntity,
    driverName :: Text,
    driverNumber :: Maybe Text,
    vehicleVariant :: DVeh.Variant,
    pickupDropOutsideOfThreshold :: Maybe Bool,
    vehicleModel :: Text,
    vehicleColor :: Text,
    vehicleNumber :: Text,
    computedFare :: Maybe Money,
    estimatedBaseFare :: Money,
    estimatedDistance :: Maybe Meters,
    driverSelectedFare :: Money,
    actualRideDistance :: HighPrecMeters,
    rideRating :: Maybe Int,
    riderName :: Maybe Text,
    tripStartTime :: Maybe UTCTime,
    tripEndTime :: Maybe UTCTime,
    specialLocationTag :: Maybe Text,
    chargeableDistance :: Maybe Meters,
    exoPhone :: Text,
    bapName :: Maybe Text,
    bapLogo :: Maybe BaseUrl,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    customerExtraFee :: Maybe Money,
    disabilityTag :: Maybe Text,
    requestedVehicleVariant :: DVeh.Variant,
    driverGoHomeRequestId :: Maybe (Id DDGR.DriverGoHomeRequest),
    payerVpa :: Maybe Text,
    autoPayStatus :: Maybe DI.DriverAutoPayStatus,
    customerCancellationDues :: HighPrecMoney,
    isFreeRide :: Maybe Bool,
    stopLocationId :: Maybe (Id DLoc.Location),
    tripCategory :: DTC.TripCategory,
    nextStopLocation :: Maybe DLoc.Location,
    lastStopLocation :: Maybe DLoc.Location,
    tripScheduledAt :: UTCTime
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data OTPRideReq = OTPRideReq
  { specialZoneOtpCode :: Text,
    point :: LatLong,
    odometer :: Maybe DRide.OdometerReading
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

newtype DriverRideListRes = DriverRideListRes
  { list :: [DriverRideRes]
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

listDriverRides ::
  (EsqDBReplicaFlow m r, EncFlow m r, EsqDBFlow m r, CacheFlow m r) =>
  Id DP.Person ->
  Maybe Integer ->
  Maybe Integer ->
  Maybe Bool ->
  Maybe DRide.RideStatus ->
  Maybe Day ->
  m DriverRideListRes
listDriverRides driverId mbLimit mbOffset mbOnlyActive mbRideStatus mbDay = do
  rides <-
    if mbOnlyActive == Just True
      then runInReplica $ QRide.getActiveBookingAndRideByDriverId driverId
      else runInReplica $ QRide.findAllByDriverId driverId mbLimit mbOffset mbOnlyActive mbRideStatus mbDay
  driverInfo <- runInReplica $ QDI.findById driverId >>= fromMaybeM (DriverNotFound driverId.getId)
  driverRideLis <- forM rides $ \(ride, booking) -> do
    rideDetail <- runInReplica $ QRD.findById ride.id >>= fromMaybeM (VehicleNotFound driverId.getId)
    rideRating <- runInReplica $ QR.findRatingForRide ride.id
    driverNumber <- RD.getDriverNumber rideDetail
    mbExophone <- CQExophone.findByPrimaryPhone booking.primaryExophone
    bapMetadata <- CQSM.findById (Id booking.bapId)
    let goHomeReqId = ride.driverGoHomeRequestId
    mkDriverRideRes rideDetail driverNumber rideRating mbExophone (ride, booking) bapMetadata goHomeReqId (Just driverInfo)
  pure . DriverRideListRes $ driverRideLis

mkDriverRideRes ::
  ( EncFlow m r,
    CacheFlow m r,
    EsqDBFlow m r
  ) =>
  RD.RideDetails ->
  Maybe Text ->
  Maybe DRating.Rating ->
  Maybe DExophone.Exophone ->
  (DRide.Ride, DRB.Booking) ->
  Maybe DSM.BapMetadata ->
  Maybe (Id DDGR.DriverGoHomeRequest) ->
  Maybe DI.DriverInformation ->
  m DriverRideRes
mkDriverRideRes rideDetails driverNumber rideRating mbExophone (ride, booking) bapMetadata goHomeReqId driverInfo = do
  let fareParams = booking.fareParams
      estimatedBaseFare =
        fareSum $
          fareParams{driverSelectedFare = Nothing -- it should not be part of estimatedBaseFare
                    }
  let initial = "" :: Text
  (nextStopLocation, lastStopLocation) <- case booking.tripCategory of
    DTC.Rental _ -> calculateLocations booking.id booking.stopLocationId
    _ -> return (Nothing, Nothing)
  return $
    DriverRideRes
      { id = ride.id,
        shortRideId = ride.shortId,
        status = ride.status,
        fromLocation = DLoc.makeLocationAPIEntity booking.fromLocation,
        toLocation = DLoc.makeLocationAPIEntity <$> booking.toLocation,
        driverName = rideDetails.driverName,
        driverNumber,
        vehicleNumber = rideDetails.vehicleNumber,
        vehicleColor = fromMaybe initial rideDetails.vehicleColor,
        vehicleVariant = fromMaybe DVeh.SEDAN rideDetails.vehicleVariant,
        vehicleModel = fromMaybe initial rideDetails.vehicleModel,
        computedFare = ride.fare,
        estimatedBaseFare = estimatedBaseFare,
        estimatedDistance = booking.estimatedDistance,
        driverSelectedFare = fromMaybe 0 fareParams.driverSelectedFare,
        actualRideDistance = ride.traveledDistance,
        createdAt = ride.createdAt,
        updatedAt = ride.updatedAt,
        riderName = booking.riderName,
        pickupDropOutsideOfThreshold = ride.pickupDropOutsideOfThreshold,
        tripStartTime = ride.tripStartTime,
        tripEndTime = ride.tripEndTime,
        specialLocationTag = booking.specialLocationTag,
        rideRating = rideRating <&> (.ratingValue),
        chargeableDistance = ride.chargeableDistance,
        exoPhone = maybe booking.primaryExophone (\exophone -> if not exophone.isPrimaryDown then exophone.primaryPhone else exophone.backupPhone) mbExophone,
        customerExtraFee = fareParams.customerExtraFee,
        bapName = bapMetadata <&> (.name),
        bapLogo = bapMetadata <&> (.logoUrl),
        disabilityTag = booking.disabilityTag,
        requestedVehicleVariant = booking.vehicleVariant,
        driverGoHomeRequestId = goHomeReqId,
        payerVpa = driverInfo >>= (.payerVpa),
        autoPayStatus = driverInfo >>= (.autoPayStatus),
        isFreeRide = ride.isFreeRide,
        customerCancellationDues = fareParams.customerCancellationDues,
        stopLocationId = (.id) <$> booking.toLocation,
        tripCategory = booking.tripCategory,
        nextStopLocation = nextStopLocation,
        lastStopLocation = lastStopLocation,
        tripScheduledAt = booking.startTime
      }

calculateLocations ::
  ( CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Id DRB.Booking ->
  Maybe (Id DLoc.Location) ->
  m (Maybe DLoc.Location, Maybe DLoc.Location)
calculateLocations bookingId stopLocationId = do
  maxOrder <- QLM.maxOrderByEntity bookingId.getId
  case stopLocationId of
    Nothing -> do
      lastLoc <- mkLocationFromLocationMapping bookingId.getId maxOrder
      return (Nothing, lastLoc)
    Just nextStopId -> do
      nextLoc <- QLoc.findById nextStopId
      lastLoc <- mkLocationFromLocationMapping bookingId.getId (maxOrder - 1)
      return (nextLoc, lastLoc)

arrivedAtPickup :: (EncFlow m r, CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, HasShortDurationRetryCfg r c, HasFlowEnv m r '["nwAddress" ::: BaseUrl], HasField "isBecknSpecVersion2" r Bool, HasHttpClientOptions r c, HasFlowEnv m r '["driverReachedDistance" ::: HighPrecMeters], HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl]) => Id DRide.Ride -> LatLong -> m APISuccess
arrivedAtPickup rideId req = do
  ride <- runInReplica (QRide.findById rideId) >>= fromMaybeM (RideDoesNotExist rideId.getId)
  unless (isValidRideStatus (ride.status)) $ throwError $ RideInvalidStatus "The ride has already started."
  booking <- runInReplica $ QBooking.findById ride.bookingId >>= fromMaybeM (BookingDoesNotExist ride.bookingId.getId)
  let pickupLoc = getCoordinates booking.fromLocation
  let distance = distanceBetweenInMeters req pickupLoc
  driverReachedDistance <- asks (.driverReachedDistance)
  unless (distance < driverReachedDistance) $ throwError $ DriverNotAtPickupLocation ride.driverId.getId
  unless (isJust ride.driverArrivalTime) $ do
    QRide.updateArrival rideId
    BP.sendDriverArrivalUpdateToBAP booking ride ride.driverArrivalTime
  pure Success
  where
    isValidRideStatus status = status == DRide.NEW

otpRideCreate :: DP.Person -> Text -> DRB.Booking -> Flow DriverRideRes
otpRideCreate driver otpCode booking = do
  transporter <-
    QM.findById booking.providerId
      >>= fromMaybeM (MerchantNotFound booking.providerId.getId)
  vehicle <- QVeh.findById driver.id >>= fromMaybeM (VehicleNotFound driver.id.getId)
  when (isNotAllowedVehicleVariant vehicle.variant booking.vehicleVariant) $ throwError $ InvalidRequest "Wrong Vehicle Variant"
  when (booking.status `elem` [DRB.COMPLETED, DRB.CANCELLED]) $ throwError (BookingInvalidStatus $ show booking.status)
  driverInfo <- QDI.findById (cast driver.id) >>= fromMaybeM DriverInfoNotFound
  unless (driverInfo.subscribed) $ throwError DriverUnsubscribed
  unless (driverInfo.enabled) $ throwError DriverAccountDisabled
  when driverInfo.onRide $ throwError DriverOnRide

  (ride, rideDetails, _) <- initializeRide transporter.id driver booking (Just otpCode) booking.id.getId
  uBooking <- runInReplica $ QBooking.findById booking.id >>= fromMaybeM (BookingNotFound booking.id.getId) -- in replica db we can have outdated value
  handle (errHandler uBooking transporter) $ BP.sendRideAssignedUpdateToBAP uBooking ride driver vehicle

  driverNumber <- RD.getDriverNumber rideDetails
  mbExophone <- CQExophone.findByPrimaryPhone booking.primaryExophone
  bapMetadata <- CQSM.findById (Id booking.bapId)
  mkDriverRideRes rideDetails driverNumber Nothing mbExophone (ride, booking) bapMetadata ride.driverGoHomeRequestId Nothing
  where
    errHandler uBooking transporter exc
      | Just BecknAPICallError {} <- fromException @BecknAPICallError exc = SBooking.cancelBooking uBooking (Just driver) transporter >> throwM exc
      | Just ExternalAPICallError {} <- fromException @ExternalAPICallError exc = SBooking.cancelBooking uBooking (Just driver) transporter >> throwM exc
      | otherwise = throwM exc

    isNotAllowedVehicleVariant driverVehicle bookingVehicle =
      (bookingVehicle == DVeh.TAXI_PLUS || bookingVehicle == DVeh.SEDAN || bookingVehicle == DVeh.SUV || bookingVehicle == DVeh.HATCHBACK)
        && driverVehicle == DVeh.TAXI

arrivedAtStop :: Id DRide.Ride -> LatLong -> Flow APISuccess
arrivedAtStop rideId pt = do
  ride <- runInReplica (QRide.findById rideId) >>= fromMaybeM (RideDoesNotExist rideId.getId)
  booking <- runInReplica $ QBooking.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
  unless (isValidRideStatus ride.status) $ throwError $ RideInvalidStatus ("This ride " <> ride.id.getId <> " is not in progress")
  unless (isJust booking.stopLocationId) $ throwError (InvalidRequest $ "Can't find stop to be reached for ride " <> ride.id.getId)
  case booking.stopLocationId of
    Nothing -> throwError $ InvalidRequest ("No stop present to be reached for ride " <> ride.id.getId)
    Just nextStopId -> do
      stopLoc <- runInReplica $ QLoc.findById nextStopId >>= fromMaybeM (InvalidRequest $ "Stop location doesn't exist for ride " <> ride.id.getId)
      let curPt = LatLong stopLoc.lat stopLoc.lon
          distance = distanceBetweenInMeters pt curPt
      driverReachedDistance <- asks (.driverReachedDistance)
      unless (distance < driverReachedDistance) $ throwError $ InvalidRequest ("Driver is not at stop location for ride " <> ride.id.getId)
      QBooking.updateStopArrival booking.id
      BP.sendStopArrivalUpdateToBAP booking ride
      pure Success
  where
    isValidRideStatus status = status == DRide.INPROGRESS

uploadOdometerReading ::
  Id DMOC.MerchantOperatingCity ->
  Id DRide.Ride ->
  UploadOdometerReq ->
  Flow UploadOdometerResp
uploadOdometerReading merchantOpCityId rideId UploadOdometerReq {..} = do
  contentType <- validateContentType
  config <- QMTC.findByMerchantOpCityId merchantOpCityId >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  fileSize <- L.runIO $ withFile file ReadMode hFileSize
  when (fileSize > fromIntegral config.mediaFileSizeUpperLimit) $
    throwError $ FileSizeExceededError (show fileSize)
  mediaFile <- L.runIO $ base64Encode <$> BS.readFile file
  filePath <- S3.createFilePath "odometer-reading/" ("rideId-" <> rideId.getId) fileType contentType
  let fileUrl =
        config.mediaFileUrlPattern
          & T.replace "<DOMAIN>" "issue"
          & T.replace "<FILE_PATH>" filePath
  _ <- fork "S3 Put Odometer Reading File" $ S3.put (T.unpack filePath) mediaFile
  createMediaEntry fileUrl
  where
    validateContentType = do
      case fileType of
        S3.Audio | reqContentType == "audio/wave" -> pure "wav"
        S3.Audio | reqContentType == "audio/mpeg" -> pure "mp3"
        S3.Audio | reqContentType == "audio/mp4" -> pure "mp4"
        S3.Image | reqContentType == "image/png" -> pure "png"
        S3.Image | reqContentType == "image/jpeg" -> pure "jpg"
        _ -> throwError $ FileFormatNotSupported reqContentType

    createMediaEntry url = do
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
                createdAt = now
              }
