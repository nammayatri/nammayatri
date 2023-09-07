{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE ApplicativeDo #-}

module Domain.Action.UI.DriverOnboarding.AadhaarVerification where

import qualified AWS.S3 as S3
import Codec.Picture
import Codec.Picture.Extra
import Codec.Picture.Types
import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Message as Common
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Base64.Lazy as B64L
import qualified Data.ByteString.Lazy as LBS
import Data.Text (pack, unpack)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Format.ISO8601 (iso8601Show)
import qualified Domain.Action.UI.DriverOnboarding.Status as Status
import Domain.Types.DriverInformation (DriverInformation)
import qualified Domain.Types.DriverOnboarding.AadhaarOtp as Domain
import Domain.Types.DriverOnboarding.AadhaarVerification
import qualified Domain.Types.DriverOnboarding.AadhaarVerification as VDomain
import Domain.Types.DriverOnboarding.Error
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as Person
import Environment
import Kernel.Beam.Functions
import Kernel.External.Encryption (DbHash, getDbHash)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess (APISuccess (..))
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Driver.DriverImage as CQDI
import Storage.CachedQueries.Merchant.TransporterConfig as CTC
import qualified Storage.Queries.DriverInformation as DriverInfo
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.DriverOnboarding.AadhaarOtp as Query
import qualified Storage.Queries.DriverOnboarding.AadhaarVerification as Q
import qualified Storage.Queries.DriverOnboarding.AadhaarVerification as QAV
import qualified Storage.Queries.Person as Person
import qualified Tools.AadhaarVerification as AadhaarVerification
import Tools.Error

data VerifyAadhaarOtpReq = VerifyAadhaarOtpReq
  { otp :: Int,
    shareCode :: Text
  }
  deriving (Show, Generic, ToSchema, ToJSON, FromJSON)

data UnVerifiedDataReq = UnVerifiedDataReq
  { driverName :: Text,
    driverGender :: Text,
    driverDob :: Text
  }
  deriving (Show, Generic, ToSchema, ToJSON, FromJSON)

data ImageType = JPG | PNG | UNKNOWN deriving (Generic, Show, Eq)

generateAadhaarOtp ::
  Bool ->
  Maybe DM.Merchant ->
  Id Person.Person ->
  AadhaarVerification.AadhaarOtpReq ->
  Flow AadhaarVerification.AadhaarVerificationResp
generateAadhaarOtp isDashboard mbMerchant personId req = do
  person <- Person.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  driverInfo <- DriverInfo.findById (cast personId) >>= fromMaybeM (PersonNotFound personId.getId)
  when driverInfo.blocked $ throwError DriverAccountBlocked
  when (driverInfo.aadhaarVerified) $ throwError AadhaarAlreadyVerified
  aadhaarHash <- getDbHash req.aadhaarNumber
  checkForDuplicacy aadhaarHash
  whenJust mbMerchant $ \merchant -> do
    -- merchant access checking
    unless (merchant.id == person.merchantId) $ throwError (PersonNotFound personId.getId)
  let tryKey = makeGenerateOtpTryKey person.id
  numberOfTries :: Maybe Int <- Redis.safeGet tryKey
  let tried = fromMaybe 0 numberOfTries
  transporterConfig <- CTC.findByMerchantId person.merchantId >>= fromMaybeM (TransporterConfigNotFound person.merchantId.getId)
  unless (isDashboard || tried < transporterConfig.onboardingTryLimit) $ throwError (GenerateAadhaarOtpExceedLimit personId.getId)
  res <- AadhaarVerification.generateAadhaarOtp person.merchantId $ req
  aadhaarOtpEntity <- mkAadhaarOtp personId res
  _ <- Query.createForGenerate aadhaarOtpEntity
  cacheAadhaarVerifyTries personId tried res.transactionId aadhaarHash isDashboard
  pure res

cacheAadhaarVerifyTries :: Id Person.Person -> Int -> Maybe Text -> DbHash -> Bool -> Flow ()
cacheAadhaarVerifyTries _ _ Nothing _ _ = return ()
cacheAadhaarVerifyTries personId tried transactionId aadhaarNumberHash isDashboard = do
  let key = makeTransactionIdAndAadhaarHashKey personId
  let tryKey = makeGenerateOtpTryKey personId
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Redis.setExp key (transactionId, aadhaarNumberHash) expTime
  unless isDashboard $ Redis.setExp tryKey (tried + 1) expTime

verifyAadhaarOtp ::
  Maybe DM.Merchant ->
  Id Person.Person ->
  VerifyAadhaarOtpReq ->
  Flow AadhaarVerification.AadhaarOtpVerifyRes
verifyAadhaarOtp mbMerchant personId req = do
  person <- Person.findById personId >>= fromMaybeM (PersonNotFound (getId personId))
  driverInfo <- DriverInfo.findById (cast personId) >>= fromMaybeM (PersonNotFound (getId personId))
  when (driverInfo.blocked) $ throwError DriverAccountBlocked
  when (driverInfo.aadhaarVerified) $ throwError AadhaarAlreadyVerified
  whenJust mbMerchant $ \merchant -> do
    -- merchant access checking
    unless (merchant.id == person.merchantId) $ throwError (PersonNotFound (getId personId))
  let key = makeTransactionIdAndAadhaarHashKey personId
  mtIdAndAadhaarHash <- Redis.safeGet key
  case mtIdAndAadhaarHash of
    Just (tId, aadhaarNumberHash) -> do
      let aadhaarVerifyReq =
            AadhaarVerification.AadhaarOtpVerifyReq
              { otp = req.otp,
                shareCode = req.shareCode,
                transactionId = tId
              }
      res <- AadhaarVerification.verifyAadhaarOtp person.merchantId aadhaarVerifyReq
      aadhaarVerifyEntity <- mkAadhaarVerify personId tId res
      Query.createForVerify aadhaarVerifyEntity
      if res.code == pack "1002"
        then do
          Redis.del key
          let imageType = getImageExtension res.image
          (orgImageFilePath, resultOrg) <- uploadOriginalAadhaarImage person res.image imageType
          case resultOrg of
            Left err -> throwError $ InternalError ("Aadhaar Verification failed due to S3 upload failure, Please try again : " <> show err)
            Right _ -> do
              aadhaarEntity <- mkAadhaar personId res.name res.gender res.date_of_birth (Just aadhaarNumberHash) Nothing True (Just orgImageFilePath)
              Q.create aadhaarEntity
              DriverInfo.updateAadhaarVerifiedState (cast personId) True
              Status.statusHandler (person.id, person.merchantId) Nothing
              uploadCompressedAadhaarImage person res.image imageType >> pure ()
        else throwError $ InternalError "Aadhaar Verification failed, Please try again"
      pure res
    Nothing -> throwError TransactionIdNotFound

fetchAndCacheAadhaarImage :: (MonadFlow m, MonadTime m, MonadReader r m, HasField "s3Env" r (S3.S3Env m), CacheFlow m r, EsqDBFlow m r) => Person.Person -> DriverInformation -> m (Maybe Text)
fetchAndCacheAadhaarImage driver driverInfo =
  if driverInfo.aadhaarVerified
    then case driverInfo.compAadhaarImagePath of
      Just path -> Just <$> CQDI.getDriverImageByDriverId driverInfo.driverId path
      Nothing -> do
        aadhaarVerification <- runInReplica (QAV.findByDriverId driverInfo.driverId) >>= fromMaybeM (InternalError $ "Count not find aadhaar verification data for the provided user : " <> getId driverInfo.driverId)
        case aadhaarVerification.driverImagePath of
          Nothing -> backfillAadhaarImage driver aadhaarVerification
          Just imgPath -> do
            uploadedImage <- CQDI.getDriverImageByDriverId driverInfo.driverId imgPath
            let imageType = getImageExtension uploadedImage
            (compImage, resultComp) <- uploadCompressedAadhaarImage driver uploadedImage imageType
            case resultComp of
              Left _ -> return (Just uploadedImage)
              Right _ -> CQDI.cacheDriverImageByDriverId driverInfo.driverId compImage >> return (Just compImage)
    else pure Nothing

backfillAadhaarImage :: (MonadFlow m, MonadTime m, MonadReader r m, HasField "s3Env" r (S3.S3Env m), CacheFlow m r, EsqDBFlow m r) => Person.Person -> AadhaarVerification -> m (Maybe Text)
backfillAadhaarImage person aadhaarVerification =
  case aadhaarVerification.driverImage of
    Nothing -> return Nothing
    Just image -> do
      let imageType = getImageExtension image
      (orgImageFilePath, resultOrg) <- uploadOriginalAadhaarImage person image imageType
      case resultOrg of
        Left _ -> return $ Just image
        Right _ -> do
          QAV.updateDriverImagePath person.id orgImageFilePath
          (compImage, resultComp) <- uploadCompressedAadhaarImage person image imageType
          case resultComp of
            Left _ -> return $ Just image
            Right _ -> return $ Just compImage

uploadOriginalAadhaarImage :: (HasField "s3Env" r (S3.S3Env m), MonadFlow m, MonadTime m, CacheFlow m r, EsqDBFlow m r) => Person.Person -> Text -> ImageType -> m (Text, Either SomeException ())
uploadOriginalAadhaarImage person image imageType = do
  orgImageFilePath <- createFilePath (getId person.id) Common.Image "/driver-aadhaar-photo/" (parseImageExtension imageType)
  resultOrg <- try @_ @SomeException $ S3.put (unpack orgImageFilePath) image
  pure (orgImageFilePath, resultOrg)

uploadCompressedAadhaarImage :: (HasField "s3Env" r (S3.S3Env m), MonadFlow m, MonadTime m, CacheFlow m r, EsqDBFlow m r) => Person.Person -> Text -> ImageType -> m (Text, Either SomeException ())
uploadCompressedAadhaarImage person image imageType = do
  transporterConfig <- CTC.findByMerchantId (person.merchantId) >>= fromMaybeM (TransporterConfigNotFound (getId (person.merchantId)))
  let mbconfig = transporterConfig.aadhaarImageResizeConfig
  compImageFilePath <- createFilePath (getId person.id) Common.Image "/driver-aadhaar-photo-resized/" (parseImageExtension imageType)
  compImage <- maybe (return image) (\cfg -> fromMaybe image <$> resizeImage cfg.height cfg.width image imageType) mbconfig
  resultComp <- try @_ @SomeException $ S3.put (unpack compImageFilePath) compImage
  case resultComp of
    Left err -> logDebug ("Failed to Upload Compressed Aadhaar Image to S3 : " <> show err)
    Right _ -> QDI.updateCompAadhaarImagePath (cast person.id) compImageFilePath
  pure (compImage, resultComp)

resizeImage :: MonadFlow m => Int -> Int -> Text -> ImageType -> m (Maybe Text)
resizeImage newHeight newWidth base64Image imageType = do
  let byteStringImg = B64.decodeLenient (TE.encodeUtf8 base64Image)
  case imageType of
    PNG -> do
      let dynamicImage = decodePng byteStringImg
      resizeImageHelper dynamicImage encodePng newHeight newWidth
    JPG -> do
      let dynamicImage = decodeJpeg byteStringImg
      resizeImageHelper dynamicImage (encodeJpeg . convertImage) newHeight newWidth
    _ -> return Nothing

resizeImageHelper :: (MonadFlow m) => Either String DynamicImage -> (Image PixelRGB8 -> LBS.ByteString) -> Int -> Int -> m (Maybe Text)
resizeImageHelper dynamicImage encodingFunc newHeight newWidth = do
  case dynamicImage of
    Left err -> logDebug ("Failed to create a dynamic image : " <> show err) >> return Nothing
    Right dImage -> do
      let imageCONV = convertRGB8 dImage
          resizedImage = scaleBilinear newWidth newHeight imageCONV
          encodedImage = encodingFunc resizedImage
          base64Encoded = B64L.encode encodedImage
      return $ Just (TE.decodeUtf8 (LBS.toStrict base64Encoded))

getImageExtension :: Text -> ImageType
getImageExtension base64Image = case T.take 1 base64Image of
  "/" -> JPG
  "i" -> PNG
  _ -> UNKNOWN

parseImageExtension :: ImageType -> Text
parseImageExtension ext = case ext of
  JPG -> ".jpg"
  PNG -> ".png"
  _ -> ""

createFilePath ::
  (MonadTime m, MonadReader r m, HasField "s3Env" r (S3.S3Env m)) =>
  Text ->
  Common.FileType ->
  Text ->
  Text ->
  m Text
createFilePath driverId fileType identifier imageExtension = do
  pathPrefix <- asks (.s3Env.pathPrefix)
  now <- getCurrentTime
  let fileName = T.replace (T.singleton ':') (T.singleton '-') (T.pack $ iso8601Show now)
  return
    ( pathPrefix <> identifier <> "driver-" <> driverId <> "/"
        <> show fileType
        <> "/"
        <> fileName
        <> imageExtension
    )

unVerifiedAadhaarData ::
  Id Person.Person ->
  UnVerifiedDataReq ->
  Flow APISuccess
unVerifiedAadhaarData personId req = do
  mAadhaarCard <- Q.findByDriverId personId
  when (isJust mAadhaarCard) $ throwError AadhaarDataAlreadyPresent
  aadhaarEntity <- mkAadhaar personId req.driverName req.driverGender req.driverDob Nothing Nothing False Nothing
  Q.create aadhaarEntity
  return Success

makeTransactionIdAndAadhaarHashKey :: Id Person.Person -> Text
makeTransactionIdAndAadhaarHashKey id = "AadhaarVerificationTransactionIdAndAadhaarHash:PersonId-" <> id.getId

makeGenerateOtpTryKey :: Id Person.Person -> Text
makeGenerateOtpTryKey id = "GenerateOtpTryKeyId:PersonId-" <> id.getId

mkAadhaarOtp ::
  (MonadGuid m, MonadTime m) =>
  Id Person.Person ->
  AadhaarVerification.AadhaarVerificationResp ->
  m Domain.AadhaarOtpReq
mkAadhaarOtp personId res = do
  id <- generateGUID
  now <- getCurrentTime
  return $
    Domain.AadhaarOtpReq
      { id,
        driverId = personId,
        requestId = res.requestId,
        statusCode = res.statusCode,
        transactionId = res.transactionId,
        requestMessage = res.message,
        createdAt = now
      }

mkAadhaarVerify ::
  (MonadGuid m, MonadTime m) =>
  Id Person.Person ->
  Text ->
  AadhaarVerification.AadhaarOtpVerifyRes ->
  m Domain.AadhaarOtpVerify
mkAadhaarVerify personId tId res = do
  id <- generateGUID
  now <- getCurrentTime
  return $
    Domain.AadhaarOtpVerify
      { id,
        driverId = personId,
        requestId = res.request_id,
        statusCode = res.code,
        transactionId = tId,
        requestMessage = res.message,
        createdAt = now
      }

mkAadhaar ::
  (MonadGuid m, MonadTime m) =>
  Id Person.Person ->
  Text ->
  Text ->
  Text ->
  Maybe DbHash ->
  Maybe Text ->
  Bool ->
  Maybe Text ->
  m VDomain.AadhaarVerification
mkAadhaar personId name gender dob aadhaarHash img aadhaarVerified imgPath = do
  now <- getCurrentTime
  return $
    VDomain.AadhaarVerification
      { driverId = personId,
        driverName = name,
        driverGender = gender,
        driverDob = dob,
        driverImage = img,
        aadhaarNumberHash = aadhaarHash,
        isVerified = aadhaarVerified,
        createdAt = now,
        updatedAt = now,
        driverImagePath = imgPath
      }

checkForDuplicacy :: DbHash -> Flow ()
checkForDuplicacy aadhaarHash = do
  aadhaarInfo <- Q.findByAadhaarNumberHash aadhaarHash
  when (isJust aadhaarInfo) $ throwError AadhaarAlreadyLinked
