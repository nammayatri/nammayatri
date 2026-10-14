module Domain.Action.UI.RideEvidence
  ( postRideEvidence,
    getRideEvidence,
    loadMediaFiles,
    checkEvidenceUploadRate,
    isRideEvidenceEnabled,
  )
where

import qualified API.Types.UI.RideEvidence
import qualified AWS.S3 as S3
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
import Tools.Error

maxDescriptionLength :: Int
maxDescriptionLength = 1000

defaultMaxFilesPerRide :: Int
defaultMaxFilesPerRide = 100

defaultCompletedRideWindowHours :: Int
defaultCompletedRideWindowHours = 72

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
  Redis.withLockRedisAndReturnValue (attachFileLockKey req.fileId) 60 $
    Redis.withLockRedisAndReturnValue (attachRideLockKey rideId) 60 $ do
      mediaFile <- QMediaFile.findById req.fileId >>= fromMaybeM (InvalidRequest "Evidence file not found")
      validateEvidenceFile driverId mediaFile
      alreadyAttached <- QRideEvidence.findByMediaFileId req.fileId
      when (isJust alreadyAttached) $
        throwError (InvalidRequest "This file is already attached to a ride")
      refusal <- findRefusal now transporterConfig ride
      whenJust refusal $ \reason -> do
        discardUploadedFile mediaFile
        throwError reason
      evidenceId <- generateGUID
      let evidence =
            DRE.RideEvidence
              { id = evidenceId,
                rideId,
                driverId,
                mediaFileId = req.fileId,
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
      pure $ mkRideEvidenceItem evidence (Just mediaFile)

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
  evidences <- QRideEvidence.findAllByRideId rideId
  mediaFiles <- loadMediaFiles (map (.mediaFileId) evidences)
  pure $ map (\evidence -> mkRideEvidenceItem evidence (Map.lookup evidence.mediaFileId.getId mediaFiles)) (sortOn (.createdAt) evidences)

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

-- Turning this off stops new proofs only. The proofs already stored stay readable, and ops hide them one by one through the dashboard.
isRideEvidenceEnabled :: DTConf.TransporterConfig -> Bool
isRideEvidenceEnabled transporterConfig = fromMaybe True (transporterConfig.rideEvidenceConfig >>= (.enabled))

maxFilesPerRide :: DTConf.TransporterConfig -> Int
maxFilesPerRide transporterConfig = fromMaybe defaultMaxFilesPerRide (transporterConfig.rideEvidenceConfig >>= (.maxFilesPerRide))

completedRideWindowHours :: DTConf.TransporterConfig -> Int
completedRideWindowHours transporterConfig = fromMaybe defaultCompletedRideWindowHours (transporterConfig.rideEvidenceConfig >>= (.completedRideWindowHours))

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

mkRideEvidenceItem :: DRE.RideEvidence -> Maybe MediaFile.MediaFile -> API.Types.UI.RideEvidence.RideEvidenceItem
mkRideEvidenceItem evidence mediaFile =
  API.Types.UI.RideEvidence.RideEvidenceItem
    { id = evidence.id,
      fileId = evidence.mediaFileId,
      filePath = mediaFile >>= (.s3FilePath),
      description = evidence.description,
      rideStatus = evidence.rideStatus,
      fileStatus = mediaFile >>= (.status),
      createdAt = evidence.createdAt
    }
