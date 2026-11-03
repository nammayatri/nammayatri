module Domain.Action.UI.RideEvidence
  ( postRideEvidence,
    getRideEvidence,
    loadMediaFiles,
    checkEvidenceUploadRate,
    isRideEvidenceEnabled,
    evidenceLinkExpirySeconds,
    storeThumbnail,
    EvidenceLinks (..),
    signEvidenceLinks,
  )
where

import qualified API.Types.UI.RideEvidence
import qualified AWS.S3 as S3
import Codec.Picture (convertRGB8, decodeImage, encodeJpegAtQuality, imageHeight, imageWidth)
import Codec.Picture.Extra (scaleBilinear)
import Codec.Picture.Types (convertImage)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.Ride
import qualified Domain.Types.RideEvidence as DRE
import qualified Domain.Types.TransporterConfig as DTConf
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified FileManagement.Common.UI.File as FileCommon
import qualified FileManagement.Domain.Action.UI.File as DFile
import qualified IssueManagement.Domain.Types.MediaFile as MediaFile
import qualified IssueManagement.Storage.Queries.MediaFile as QMediaFile
import qualified Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Types.Id
import Kernel.Types.SlidingWindowLimiter (APIRateLimitOptions (..))
import Kernel.Utils.Common
import Kernel.Utils.SlidingWindowLimiter (checkSlidingWindowLimitWithOptions)
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import Storage.Beam.IssueManagement ()
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RideEvidence as QRideEvidence
import System.Directory (removeFile)
import System.IO.Temp (getCanonicalTemporaryDirectory, openBinaryTempFile)
import Tools.Error

maxDescriptionLength :: Int
maxDescriptionLength = 1000

defaultMaxFilesPerRide :: Int
defaultMaxFilesPerRide = 100

defaultCompletedRideWindowHours :: Int
defaultCompletedRideWindowHours = 72

defaultLinkExpirySeconds :: Int
defaultLinkExpirySeconds = 300

minLinkExpirySeconds :: Int
minLinkExpirySeconds = 60

maxLinkExpirySeconds :: Int
maxLinkExpirySeconds = 3600

thumbnailMaxSide :: Int
thumbnailMaxSide = 400

-- A picture larger than this is not decoded for a thumbnail, so one upload cannot take up a lot of memory.
thumbnailMaxPixels :: Int
thumbnailMaxPixels = 12000000

thumbnailHandoffTtlSeconds :: Int
thumbnailHandoffTtlSeconds = 86400

postRideEvidence ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Types.Id.Id Domain.Types.Ride.Ride ->
    API.Types.UI.RideEvidence.AddRideEvidenceReq ->
    Environment.Flow API.Types.UI.RideEvidence.RideEvidenceItem
  )
postRideEvidence (mbDriverId, merchantId, merchantOpCityId) rideId req = do
  driverId <- fromMaybeM (PersonNotFound "driver id not present in token") mbDriverId
  ride <- QRide.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  unless (ride.driverId == driverId) $ throwError (RideDoesNotExist rideId.getId)
  transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = ride.merchantOperatingCityId.getId}) Nothing >>= fromMaybeM (TransporterConfigNotFound ride.merchantOperatingCityId.getId)
  now <- getCurrentTime
  let description = T.strip <$> req.description >>= \d -> if T.null d then Nothing else Just d
  whenJust description $ \d ->
    when (T.length d > maxDescriptionLength) $
      throwError (InvalidRequest $ "Description cannot exceed " <> show maxDescriptionLength <> " characters")
  -- The file lock keeps one file from being attached twice at the same time. The ride lock keeps the count check and the insert together.
  Redis.withWaitAndLockRedis (attachFileLockKey req.fileId) 60 50000 $
    Redis.withWaitAndLockRedis (attachRideLockKey rideId) 60 50000 $ do
      mediaFile <- QMediaFile.findById req.fileId >>= fromMaybeM (InvalidRequest "Evidence file not found")
      validateEvidenceFile driverId mediaFile
      thumbnailFileId :: Maybe Text <- Redis.safeGet (thumbnailHandoffKey req.fileId)
      mbThumbnailFile <- fmap join . forM thumbnailFileId $ QMediaFile.findById . Kernel.Types.Id.Id
      alreadyAttached <- QRideEvidence.findByMediaFileId req.fileId
      when (isJust alreadyAttached) $
        throwError (InvalidRequest "This file is already attached to a ride")
      refusal <- findRefusal now transporterConfig ride
      whenJust refusal $ \reason -> do
        discardUploadedFile mediaFile
        whenJust mbThumbnailFile discardUploadedFile
        throwError reason
      evidenceId <- generateGUID
      let evidence =
            DRE.RideEvidence
              { id = evidenceId,
                rideId,
                driverId,
                mediaFileId = req.fileId,
                thumbnailMediaFileId = (.id) <$> mbThumbnailFile,
                description,
                rideStatus = ride.status,
                merchantId,
                merchantOperatingCityId = merchantOpCityId,
                hiddenAt = Nothing,
                hiddenBy = Nothing,
                hiddenReason = Nothing,
                createdAt = now,
                updatedAt = now
              }
      QRideEvidence.create evidence
      let storedFiles = Map.fromList $ map (\file -> (file.id.getId, file)) (mediaFile : maybeToList mbThumbnailFile)
      links <- signEvidenceLinks (evidenceLinkExpirySeconds (Just transporterConfig)) now storedFiles evidence
      pure $ mkRideEvidenceItem evidence (Just mediaFile) links

attachFileLockKey :: Kernel.Types.Id.Id MediaFile.MediaFile -> Text
attachFileLockKey fileId = "RideEvidence:Attach:FileId-" <> fileId.getId

attachRideLockKey :: Kernel.Types.Id.Id Domain.Types.Ride.Ride -> Text
attachRideLockKey rideId = "RideEvidence:Attach:RideId-" <> rideId.getId

getRideEvidence ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Types.Id.Id Domain.Types.Ride.Ride ->
    Environment.Flow [API.Types.UI.RideEvidence.RideEvidenceItem]
  )
getRideEvidence (mbDriverId, _, _) rideId = do
  driverId <- fromMaybeM (PersonNotFound "driver id not present in token") mbDriverId
  ride <- QRide.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  unless (ride.driverId == driverId) $ throwError (RideDoesNotExist rideId.getId)
  mbTransporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = ride.merchantOperatingCityId.getId}) Nothing
  evidences <- QRideEvidence.findAllByRideId rideId
  mediaFiles <- loadMediaFiles (evidences >>= \evidence -> evidence.mediaFileId : maybeToList evidence.thumbnailMediaFileId)
  now <- getCurrentTime
  forM (sortOn (.createdAt) evidences) $ \evidence -> do
    links <- signEvidenceLinks (evidenceLinkExpirySeconds mbTransporterConfig) now mediaFiles evidence
    pure $ mkRideEvidenceItem evidence (Map.lookup evidence.mediaFileId.getId mediaFiles) links

-- The stored files of a whole list in one query. A file the query does not return is read on its own, so a file that is still only in the key-value store is not lost.
loadMediaFiles :: [Kernel.Types.Id.Id MediaFile.MediaFile] -> Environment.Flow (Map.Map Text MediaFile.MediaFile)
loadMediaFiles fileIds = do
  found <- if null fileIds then pure [] else QMediaFile.findAllIn fileIds
  let foundById = Map.fromList $ map (\mediaFile -> (mediaFile.id.getId, mediaFile)) found
  missed <- fmap catMaybes . forM (filter (\fileId -> Map.notMember fileId.getId foundById) fileIds) $ QMediaFile.findById
  pure $ foldr (\mediaFile -> Map.insert mediaFile.id.getId mediaFile) foundById missed

-- Proofs can be added at every stage of a ride except UPCOMING, a scheduled ride that has not reached its time: the driver may be doing other rides meanwhile. Once a ride is completed or cancelled they are accepted for a configurable window. The count is checked last.
findRefusal :: UTCTime -> DTConf.TransporterConfig -> Domain.Types.Ride.Ride -> Environment.Flow (Maybe RideEvidenceError)
findRefusal now transporterConfig ride
  | not (isRideEvidenceEnabled transporterConfig) = pure (Just RideEvidenceDisabled)
  | ride.status == Domain.Types.Ride.UPCOMING = pure (Just RideEvidenceNotAllowed)
  | isPastWindow = pure (Just RideEvidenceWindowClosed)
  | otherwise = do
    attachedCount <- length <$> QRideEvidence.findAllByRideId ride.id
    pure $ if attachedCount >= maxFilesPerRide transporterConfig then Just RideEvidenceLimitReached else Nothing
  where
    isPastWindow =
      ride.status `elem` [Domain.Types.Ride.COMPLETED, Domain.Types.Ride.CANCELLED]
        && diffUTCTime now (fromMaybe ride.updatedAt ride.tripEndTime) > fromIntegral (completedRideWindowHours transporterConfig * 3600)

-- A refused proof is retried with a fresh upload, so the file that was just uploaded for it is removed instead of left behind.
discardUploadedFile :: MediaFile.MediaFile -> Environment.Flow ()
discardUploadedFile mediaFile =
  void . withTryCatch "discardRefusedRideEvidence" $ do
    whenJust mediaFile.s3FilePath $ \filePath -> S3.delete (T.unpack filePath)
    QMediaFile.deleteById mediaFile.id

-- A driver cannot upload more evidence files in an hour than a single ride may hold.
checkEvidenceUploadRate :: Kernel.Types.Id.Id Domain.Types.Person.Person -> DTConf.TransporterConfig -> Environment.Flow ()
checkEvidenceUploadRate driverId transporterConfig =
  checkSlidingWindowLimitWithOptions
    ("RideEvidence:Upload:DriverId-" <> driverId.getId)
    (APIRateLimitOptions {limit = maxFilesPerRide transporterConfig, limitResetTimeInSec = 3600})

-- Proofs are off until a city sets enabled to true. Turning them off again stops new proofs only. The proofs already stored stay readable, and ops hide them one by one through the dashboard.
isRideEvidenceEnabled :: DTConf.TransporterConfig -> Bool
isRideEvidenceEnabled transporterConfig = fromMaybe False (transporterConfig.rideEvidenceConfig >>= (.enabled))

maxFilesPerRide :: DTConf.TransporterConfig -> Int
maxFilesPerRide transporterConfig = fromMaybe defaultMaxFilesPerRide (transporterConfig.rideEvidenceConfig >>= (.maxFilesPerRide))

completedRideWindowHours :: DTConf.TransporterConfig -> Int
completedRideWindowHours transporterConfig = fromMaybe defaultCompletedRideWindowHours (transporterConfig.rideEvidenceConfig >>= (.completedRideWindowHours))

-- How long the download links of a proof stay valid.
evidenceLinkExpirySeconds :: Maybe DTConf.TransporterConfig -> Int
evidenceLinkExpirySeconds mbTransporterConfig = max minLinkExpirySeconds . min maxLinkExpirySeconds $ fromMaybe defaultLinkExpirySeconds (mbTransporterConfig >>= (.rideEvidenceConfig) >>= (.linkExpirySeconds))

-- Only a finished image that this driver uploaded through the ride evidence domain can be attached.
validateEvidenceFile :: Kernel.Types.Id.Id Domain.Types.Person.Person -> MediaFile.MediaFile -> Environment.Flow ()
validateEvidenceFile driverId mediaFile = do
  filePath <- fromMaybeM (InvalidRequest "Evidence file has no stored path") mediaFile.s3FilePath
  unless (FileCommon.isOwnedBy FileCommon.RideEvidence driverId.getId filePath) $
    throwError (InvalidRequest "File was not uploaded as ride evidence by this driver")
  unless (mediaFile._type == S3.Image) $
    throwError (InvalidRequest "Only images can be attached as evidence")
  unless (mediaFile.status == Just MediaFile.COMPLETED) $
    throwError (InvalidRequest "Evidence file upload is not complete")

-- The download links of one proof. A proof without a thumbnail gets the link of its photo in both places.
data EvidenceLinks = EvidenceLinks
  { linkToThumbnail :: Maybe Text,
    linkToImage :: Maybe Text,
    linksExpireAt :: Maybe UTCTime
  }

signEvidenceLinks :: Int -> UTCTime -> Map.Map Text MediaFile.MediaFile -> DRE.RideEvidence -> Environment.Flow EvidenceLinks
signEvidenceLinks linkExpirySeconds now storedFiles evidence = do
  linkToImage <- signMediaFileLink linkExpirySeconds (Map.lookup evidence.mediaFileId.getId storedFiles)
  linkToThumbnail <- signMediaFileLink linkExpirySeconds (evidence.thumbnailMediaFileId >>= \thumbnailFileId -> Map.lookup thumbnailFileId.getId storedFiles)
  let thumbnailOrImageLink = maybe linkToImage Just linkToThumbnail
  pure
    EvidenceLinks
      { linkToThumbnail = thumbnailOrImageLink,
        linkToImage,
        linksExpireAt = addUTCTime (fromIntegral linkExpirySeconds) now <$ thumbnailOrImageLink
      }

-- A download link that only works for a short time, so an app can load the image straight from the store. A file that is not yet stored, or cannot be signed, has no link.
signMediaFileLink :: Int -> Maybe MediaFile.MediaFile -> Environment.Flow (Maybe Text)
signMediaFileLink linkExpirySeconds mbMediaFile =
  fmap join . forM (mbMediaFile >>= \mediaFile -> if mediaFile.status == Just MediaFile.COMPLETED then mediaFile.s3FilePath else Nothing) $ \filePath -> do
    signed <- withTryCatch "signRideEvidenceLink" $ S3.generateDownloadUrl (T.unpack filePath) (Seconds linkExpirySeconds)
    case signed of
      Right url -> pure (Just url)
      Left err -> do
        logError $ "Could not sign a ride evidence link: " <> show err
        pure Nothing

mkRideEvidenceItem :: DRE.RideEvidence -> Maybe MediaFile.MediaFile -> EvidenceLinks -> API.Types.UI.RideEvidence.RideEvidenceItem
mkRideEvidenceItem evidence mediaFile links =
  API.Types.UI.RideEvidence.RideEvidenceItem
    { id = evidence.id,
      fileId = evidence.mediaFileId,
      filePath = mediaFile >>= (.s3FilePath),
      description = evidence.description,
      rideStatus = evidence.rideStatus,
      fileStatus = mediaFile >>= (.status),
      createdAt = evidence.createdAt,
      thumbnailUrl = links.linkToThumbnail,
      imageUrl = links.linkToImage,
      urlsExpireAt = links.linksExpireAt
    }

-- The upload and the attach are two calls, so the thumbnail made at upload waits under the id of its photo until the photo is attached.
thumbnailHandoffKey :: Kernel.Types.Id.Id MediaFile.MediaFile -> Text
thumbnailHandoffKey fileId = "RideEvidence:Thumbnail:FileId-" <> fileId.getId

-- Stores a small copy of a photo that was just uploaded. A photo that cannot be made smaller is left without one, and the customer then gets the photo itself.
storeThumbnail :: Int -> Text -> Text -> FileCommon.FileUploadReq -> FileCommon.FileUploadRes -> Environment.Flow ()
storeThumbnail mediaFileSizeUpperLimit mediaFileUrlPattern uploaderId req uploaded = do
  outcome <- withTryCatch "storeRideEvidenceThumbnail" $ do
    bytes <- liftIO $ BS.readFile req.file
    whenJust (makeThumbnailJpeg bytes) $ \thumbnail -> do
      tmpDir <- liftIO getCanonicalTemporaryDirectory
      (thumbnailPath, thumbnailHandle) <- liftIO $ openBinaryTempFile tmpDir "ride-evidence-thumbnail.jpg"
      written <- withTryCatch "writeRideEvidenceThumbnailFile" . liftIO $ BS.hPut thumbnailHandle thumbnail
      void . withTryCatch "closeRideEvidenceThumbnailFile" . liftIO $ hClose thumbnailHandle
      let thumbnailReq =
            FileCommon.FileUploadReq
              { FileCommon.file = thumbnailPath,
                FileCommon.reqContentType = "image/jpeg",
                FileCommon.fileType = req.fileType,
                FileCommon.domain = req.domain
              }
      uploadResult <- case written of
        Left err -> pure (Left err)
        Right _ -> withTryCatch "uploadRideEvidenceThumbnail" $ DFile.fileUploadToS3 mediaFileSizeUpperLimit mediaFileUrlPattern uploaderId thumbnailReq
      void . withTryCatch "removeRideEvidenceThumbnailFile" . liftIO $ removeFile thumbnailPath
      case uploadResult of
        Left err -> logError $ "Could not upload a ride evidence thumbnail: " <> show err
        Right thumbnailUpload -> Redis.setExp (thumbnailHandoffKey uploaded.fileId) thumbnailUpload.fileId.getId thumbnailHandoffTtlSeconds
  case outcome of
    Left err -> logError $ "Could not make a ride evidence thumbnail: " <> show err
    Right _ -> pure ()

-- A JPEG copy of the picture whose longer side is at most thumbnailMaxSide pixels. Only JPEG and PNG are read.
makeThumbnailJpeg :: BS.ByteString -> Maybe BS.ByteString
makeThumbnailJpeg bytes = do
  (width, height) <- imageDimensions bytes
  guard (width > 0 && height > 0 && width <= thumbnailMaxPixels && height <= thumbnailMaxPixels && width * height <= thumbnailMaxPixels)
  image <- either (const Nothing) (Just . convertRGB8) (decodeImage bytes)
  let scale = fromIntegral thumbnailMaxSide / fromIntegral (max (imageWidth image) (imageHeight image)) :: Double
      scaledSide side = max 1 (round (fromIntegral side * scale))
      resized
        | scale >= 1 = image
        | otherwise = scaleBilinear (scaledSide (imageWidth image)) (scaledSide (imageHeight image)) image
  pure . BL.toStrict $ encodeJpegAtQuality 75 (convertImage resized)

-- Width and height read from the header of a JPEG or PNG, without decoding the picture.
imageDimensions :: BS.ByteString -> Maybe (Int, Int)
imageDimensions bytes
  | BS.take 8 bytes == BS.pack [0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A] && BS.length bytes >= 24 = Just (number 16 4, number 20 4)
  | BS.take 2 bytes == BS.pack [0xFF, 0xD8] = jpegDimensions 2
  | otherwise = Nothing
  where
    number start len = BS.foldl' (\acc byte -> acc * 256 + fromIntegral byte) 0 (BS.take len (BS.drop start bytes))
    isStartOfFrame marker = marker >= 0xC0 && marker <= 0xCF && marker `notElem` [0xC4, 0xC8, 0xCC]
    jpegDimensions offset
      | offset + 9 > BS.length bytes = Nothing
      | BS.index bytes offset /= 0xFF = Nothing
      | marker == 0xFF = jpegDimensions (offset + 1)
      | isStartOfFrame marker = Just (number (offset + 7) 2, number (offset + 5) 2)
      | otherwise = jpegDimensions (offset + 2 + number (offset + 2) 2)
      where
        marker = BS.index bytes (offset + 1)
