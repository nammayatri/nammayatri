{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.DriverOnboarding.AadhaarVerification where

import qualified AWS.S3 as S3
import Codec.Picture
import Codec.Picture.Extra
import Codec.Picture.Types
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Base64.Lazy as B64L
import qualified Data.ByteString.Lazy as LBS
import Data.Text (pack, unpack)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Domain.Action.UI.DriverOnboarding.Status as Status
import qualified Domain.Types.AadhaarCard as VDomain
import qualified Domain.Types.AadhaarOtpReq as DAR
import qualified Domain.Types.AadhaarOtpVerify as DAV
import Domain.Types.DriverInformation (DriverInformation)
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as Person
import Environment
import Kernel.Beam.Functions
import Kernel.External.Encryption (DbHash, getDbHash)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess (APISuccess (..))
import qualified Kernel.Types.Documents as KTD
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Cac.TransporterConfig as SCTC
import qualified Storage.CachedQueries.Driver.DriverImage as CQDI
import qualified Storage.Queries.AadhaarCard as QAadhaarCard
import qualified Storage.Queries.AadhaarOtpReq as QueryAR
import qualified Storage.Queries.AadhaarOtpVerify as QueryAV
import qualified Storage.Queries.DriverInformation as DriverInfo
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.Person as Person
import qualified Tools.AadhaarVerification as AadhaarVerification
import Tools.Error
import Utils.Common.Cac.KeyNameConstants

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
  Id DMOC.MerchantOperatingCity ->
  AadhaarVerification.AadhaarOtpReq ->
  Flow AadhaarVerification.AadhaarVerificationResp
generateAadhaarOtp isDashboard mbMerchant personId merchantOpCityId req = do
  person <- Person.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  driverInfo <- DriverInfo.findById (cast personId) >>= fromMaybeM (PersonNotFound personId.getId)
  when driverInfo.blocked $ throwError $ DriverAccountBlocked (BlockErrorPayload driverInfo.blockExpiryTime driverInfo.blockReasonFlag)
  when (driverInfo.aadhaarVerified) $ throwError AadhaarAlreadyVerified
  aadhaarHash <- getDbHash req.aadhaarNumber
  checkForDuplicacy aadhaarHash
  whenJust mbMerchant $ \merchant -> do
    -- merchant access checking
    unless (merchant.id == person.merchantId) $ throwError (PersonNotFound personId.getId)
  let tryKey = makeGenerateOtpTryKey person.id
  numberOfTries :: Maybe Int <- Redis.safeGet tryKey
  let tried = fromMaybe 0 numberOfTries
  transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCityId (Just (DriverId (cast driverInfo.driverId))) >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  unless (isDashboard || tried < transporterConfig.onboardingTryLimit) $ throwError (GenerateAadhaarOtpExceedLimit personId.getId)
  res <- AadhaarVerification.generateAadhaarOtp person.merchantId merchantOpCityId req
  aadhaarOtpEntity <- mkAadhaarOtp personId res
  _ <- QueryAR.create aadhaarOtpEntity
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
  Id DMOC.MerchantOperatingCity ->
  VerifyAadhaarOtpReq ->
  Flow AadhaarVerification.AadhaarOtpVerifyRes
verifyAadhaarOtp mbMerchant personId merchantOpCityId req = do
  person <- Person.findById personId >>= fromMaybeM (PersonNotFound (getId personId))
  driverInfo <- DriverInfo.findById (cast personId) >>= fromMaybeM (PersonNotFound (getId personId))
  when (driverInfo.blocked) $ throwError $ DriverAccountBlocked (BlockErrorPayload driverInfo.blockExpiryTime driverInfo.blockReasonFlag)
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
      res <- AadhaarVerification.verifyAadhaarOtp person.merchantId merchantOpCityId aadhaarVerifyReq
      aadhaarVerifyEntity <- mkAadhaarVerify personId tId res
      QueryAV.create aadhaarVerifyEntity
      if res.code == pack "1002"
        then do
          Redis.del key
          let imageType = getImageExtension res.image
          (orgImageFilePath, resultOrg) <- uploadOriginalAadhaarImage person res.image imageType
          case resultOrg of
            Left err -> throwError $ InternalError ("Aadhaar Verification failed due to S3 upload failure, Please try again : " <> show err)
            Right _ -> do
              aadhaarEntity <- mkAadhaar person.merchantId person.merchantOperatingCityId personId res.name res.gender res.date_of_birth (Just aadhaarNumberHash) Nothing True (Just orgImageFilePath)
              QAadhaarCard.create aadhaarEntity
              DriverInfo.updateAadhaarVerifiedState True (cast personId)
              let onlyMandatoryDocs = Just True
              void $ Status.statusHandler (person.id, person.merchantId, merchantOpCityId) (Just True) Nothing Nothing Nothing (Just False) onlyMandatoryDocs
              uploadCompressedAadhaarImage person merchantOpCityId res.image imageType >> pure ()
        else throwError $ InternalError "Aadhaar Verification failed, Please try again"
      pure res
    Nothing -> throwError TransactionIdNotFound

fetchAndCacheAadhaarImage :: (MonadFlow m, MonadTime m, MonadReader r m, HasField "s3Env" r (S3.S3Env m), CacheFlow m r, EsqDBFlow m r) => Person.Person -> DriverInformation -> m (Maybe Text)
fetchAndCacheAadhaarImage driver driverInfo =
  if driverInfo.aadhaarVerified
    then case driverInfo.compAadhaarImagePath of
      Just path -> Just <$> CQDI.getDriverImageByDriverId driverInfo.driverId path
      Nothing -> do
        aadhaarVerification <- runInReplica (QAadhaarCard.findByPrimaryKey driverInfo.driverId) >>= fromMaybeM (InternalError $ "Count not find aadhaar verification data for the provided user : " <> getId driverInfo.driverId)
        case aadhaarVerification.driverImagePath of
          Nothing -> backfillAadhaarImage driver driver.merchantOperatingCityId aadhaarVerification
          Just imgPath -> do
            uploadedImage <- CQDI.getDriverImageByDriverId driverInfo.driverId imgPath
            let imageType = getImageExtension uploadedImage
            (compImage, resultComp) <- uploadCompressedAadhaarImage driver driver.merchantOperatingCityId uploadedImage imageType
            case resultComp of
              Left _ -> return (Just uploadedImage)
              Right _ -> CQDI.cacheDriverImageByDriverId driverInfo.driverId compImage >> return (Just compImage)
    else pure Nothing

backfillAadhaarImage :: (MonadFlow m, MonadTime m, MonadReader r m, HasField "s3Env" r (S3.S3Env m), CacheFlow m r, EsqDBFlow m r) => Person.Person -> Id DMOC.MerchantOperatingCity -> VDomain.AadhaarCard -> m (Maybe Text)
backfillAadhaarImage person merchantOpCityId aadhaarVerification =
  case aadhaarVerification.driverImage of
    Nothing -> return Nothing
    Just image -> do
      let imageType = getImageExtension image
      (orgImageFilePath, resultOrg) <- uploadOriginalAadhaarImage person image imageType
      case resultOrg of
        Left _ -> return $ Just image
        Right _ -> do
          QAadhaarCard.updateDriverImagePath (Just orgImageFilePath) person.id
          (compImage, resultComp) <- uploadCompressedAadhaarImage person merchantOpCityId image imageType
          case resultComp of
            Left _ -> return $ Just image
            Right _ -> return $ Just compImage

uploadOriginalAadhaarImage :: (HasField "s3Env" r (S3.S3Env m), MonadFlow m, MonadTime m, CacheFlow m r, EsqDBFlow m r) => Person.Person -> Text -> ImageType -> m (Text, Either SomeException ())
uploadOriginalAadhaarImage person image imageType = do
  orgImageFilePath <- S3.createFilePath "/driver-aadhaar-photo/" ("driver-" <> getId person.id) S3.Image (parseImageExtension imageType)
  resultOrg <- try @_ @SomeException $ S3.put (unpack orgImageFilePath) image
  pure (orgImageFilePath, resultOrg)

uploadCompressedAadhaarImage :: (HasField "s3Env" r (S3.S3Env m), MonadFlow m, MonadTime m, CacheFlow m r, EsqDBFlow m r) => Person.Person -> Id DMOC.MerchantOperatingCity -> Text -> ImageType -> m (Text, Either SomeException ())
uploadCompressedAadhaarImage person merchantOpCityId image imageType = do
  transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCityId (Just (DriverId (cast person.id))) >>= fromMaybeM (TransporterConfigNotFound (merchantOpCityId.getId))
  let mbconfig = transporterConfig.aadhaarImageResizeConfig
  compImageFilePath <- S3.createFilePath "/driver-aadhaar-photo-resized/" ("driver-" <> getId person.id) S3.Image (parseImageExtension imageType)
  compImage <- maybe (return image) (\cfg -> fromMaybe image <$> resizeImage cfg.height cfg.width image imageType) mbconfig
  resultComp <- try @_ @SomeException $ S3.put (unpack compImageFilePath) compImage
  case resultComp of
    Left err -> logDebug ("Failed to Upload Compressed Aadhaar Image to S3 : " <> show err)
    Right _ -> QDI.updateCompAadhaarImagePath (Just compImageFilePath) (cast person.id)
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

unVerifiedAadhaarData ::
  Id Person.Person ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  UnVerifiedDataReq ->
  Flow APISuccess
unVerifiedAadhaarData personId merchantId merchantOpCityId req = do
  mAadhaarCard <- QAadhaarCard.findByPrimaryKey personId
  when (isJust mAadhaarCard) $ throwError AadhaarDataAlreadyPresent
  aadhaarEntity <- mkAadhaar merchantId merchantOpCityId personId req.driverName req.driverGender req.driverDob Nothing Nothing False Nothing
  QAadhaarCard.create aadhaarEntity
  return Success

makeTransactionIdAndAadhaarHashKey :: Id Person.Person -> Text
makeTransactionIdAndAadhaarHashKey id = "AadhaarVerificationTransactionIdAndAadhaarHash:PersonId-" <> id.getId

makeGenerateOtpTryKey :: Id Person.Person -> Text
makeGenerateOtpTryKey id = "GenerateOtpTryKeyId:PersonId-" <> id.getId

mkAadhaarOtp ::
  (MonadGuid m, MonadTime m) =>
  Id Person.Person ->
  AadhaarVerification.AadhaarVerificationResp ->
  m DAR.AadhaarOtpReq
mkAadhaarOtp personId res = do
  id <- generateGUID
  now <- getCurrentTime
  return $
    DAR.AadhaarOtpReq
      { id,
        driverId = personId,
        requestId = res.requestId,
        statusCode = res.statusCode,
        transactionId = res.transactionId,
        requestMessage = res.message,
        createdAt = now,
        updatedAt = now
      }

mkAadhaarVerify ::
  (MonadGuid m, MonadTime m) =>
  Id Person.Person ->
  Text ->
  AadhaarVerification.AadhaarOtpVerifyRes ->
  m DAV.AadhaarOtpVerify
mkAadhaarVerify personId tId res = do
  id <- generateGUID
  now <- getCurrentTime
  return $
    DAV.AadhaarOtpVerify
      { id,
        driverId = personId,
        requestId = res.request_id,
        statusCode = res.code,
        transactionId = tId,
        requestMessage = res.message,
        createdAt = now,
        updatedAt = now
      }

mkAadhaar ::
  (MonadGuid m, MonadTime m) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Id Person.Person ->
  Text ->
  Text ->
  Text ->
  Maybe DbHash ->
  Maybe Text ->
  Bool ->
  Maybe Text ->
  m VDomain.AadhaarCard
mkAadhaar merchantId merchantOpCityId personId name gender dob aadhaarHash img aadhaarVerified imgPath = do
  now <- getCurrentTime
  return $
    VDomain.AadhaarCard
      { driverId = personId,
        aadhaarFrontImageId = Nothing,
        aadhaarBackImageId = Nothing,
        maskedAadhaarNumber = Nothing,
        aadhaarNumberHash = aadhaarHash,
        nameOnCard = Just name,
        driverGender = Just gender,
        dateOfBirth = Just dob,
        address = Nothing,
        consent = True,
        consentTimestamp = now,
        driverImage = img,
        driverImagePath = imgPath,
        verificationStatus = bool KTD.INVALID KTD.VALID aadhaarVerified,
        merchantOperatingCityId = merchantOpCityId,
        merchantId = merchantId,
        createdAt = now,
        updatedAt = now
      }

checkForDuplicacy :: DbHash -> Flow ()
checkForDuplicacy aadhaarHash = do
  aadhaarInfo <- QAadhaarCard.findByAadhaarNumberHash (Just aadhaarHash)
  when (isJust aadhaarInfo) $ throwError AadhaarAlreadyLinked
