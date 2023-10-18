{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE ApplicativeDo #-}

module Domain.Action.UI.AadhaarVerification where

import qualified AWS.S3 as S3
import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Message as Common
import Data.Text (pack, unpack)
import qualified Data.Text as T
import Data.Time.Format.ISO8601 (iso8601Show)
import qualified Domain.Types.AadhaarVerification.AadhaarOtp as Domain
import Domain.Types.AadhaarVerification.AadhaarVerification
import qualified Domain.Types.AadhaarVerification.AadhaarVerification as VDomain
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as Person
import Environment
import Kernel.External.Encryption (DbHash, getDbHash)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.CachedQueries.Merchant as CM
import qualified Storage.Queries.AadhaarVerification.AadhaarOtp as Query
import qualified Storage.Queries.AadhaarVerification.AadhaarVerification as QAV
import qualified Storage.Queries.Person as Person
import qualified Tools.AadhaarVerification as AadhaarVerification
import Tools.Error

data VerifyAadhaarOtpReq = VerifyAadhaarOtpReq
  { otp :: Int,
    shareCode :: Text
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
  when person.blocked $ throwError (InternalError "Person Account is Blocked")
  when (person.aadhaarVerified) $ throwError AadhaarAlreadyVerified
  aadhaarHash <- getDbHash req.aadhaarNumber
  checkForDuplicacy aadhaarHash
  whenJust mbMerchant $ \merchant -> do
    unless (merchant.id == person.merchantId) $ throwError (PersonNotFound personId.getId)
  let tryKey = makeGenerateOtpTryKey person.id
  numberOfTries :: Maybe Int <- Redis.safeGet tryKey
  let tried = fromMaybe 0 numberOfTries
  merchant <- CM.findById person.merchantId >>= fromMaybeM (MerchantNotFound person.merchantId.getId)
  unless (isDashboard || tried < merchant.aadhaarVerificationTryLimit) $ throwError (GenerateAadhaarOtpExceedLimit personId.getId)
  let merchantOperatingCityId = person.merchantOperatingCityId
  res <- AadhaarVerification.generateAadhaarOtp person.merchantId merchantOperatingCityId req
  aadhaarOtpEntity <- mkAadhaarOtp personId res
  _ <- Query.createForGenerate aadhaarOtpEntity
  cacheAadhaarVerifyTries personId tried res.transactionId aadhaarHash isDashboard merchant
  pure res

cacheAadhaarVerifyTries :: Id Person.Person -> Int -> Maybe Text -> DbHash -> Bool -> DM.Merchant -> Flow ()
cacheAadhaarVerifyTries _ _ Nothing _ _ _ = return ()
cacheAadhaarVerifyTries personId tried transactionId aadhaarNumberHash isDashboard merchant = do
  let key = makeTransactionIdAndAadhaarHashKey personId
  let tryKey = makeGenerateOtpTryKey personId
  let expTime = fromIntegral merchant.aadhaarKeyExpiryTime
  Redis.setExp key (transactionId, aadhaarNumberHash) expTime
  unless isDashboard $ Redis.setExp tryKey (tried + 1) expTime

verifyAadhaarOtp ::
  Maybe DM.Merchant ->
  Id Person.Person ->
  VerifyAadhaarOtpReq ->
  Flow AadhaarVerification.AadhaarOtpVerifyRes
verifyAadhaarOtp mbMerchant personId req = do
  person <- Person.findById personId >>= fromMaybeM (PersonNotFound (getId personId))
  when (person.blocked) $ throwError (InternalError "Person Account Blocked")
  when (person.aadhaarVerified) $ throwError AadhaarAlreadyVerified
  whenJust mbMerchant $ \merchant -> do
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
      let merchantOperatingCityId = person.merchantOperatingCityId
      res <- AadhaarVerification.verifyAadhaarOtp person.merchantId merchantOperatingCityId aadhaarVerifyReq
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
              aadhaarEntity <- mkAadhaar personId res.name res.gender res.date_of_birth (Just aadhaarNumberHash) (Just orgImageFilePath) True
              QAV.create aadhaarEntity
              Person.updateAadhaarVerifiedState (cast personId) True
        else throwError $ InternalError "Aadhaar Verification failed, Please try again"
      pure res
    Nothing -> throwError TransactionIdNotFound

uploadOriginalAadhaarImage :: (HasField "s3Env" r (S3.S3Env m), MonadFlow m, MonadTime m, CacheFlow m r, EsqDBFlow m r) => Person.Person -> Text -> ImageType -> m (Text, Either SomeException ())
uploadOriginalAadhaarImage person image imageType = do
  orgImageFilePath <- createFilePath (getId person.id) Common.Image "/person-aadhaar-photo/" (parseImageExtension imageType)
  resultOrg <- try @_ @SomeException $ S3.put (unpack orgImageFilePath) image
  pure (orgImageFilePath, resultOrg)

createFilePath ::
  (MonadTime m, MonadReader r m, HasField "s3Env" r (S3.S3Env m)) =>
  Text ->
  Common.FileType ->
  Text ->
  Text ->
  m Text
createFilePath personId fileType identifier imageExtension = do
  pathPrefix <- asks (.s3Env.pathPrefix)
  now <- getCurrentTime
  let fileName = T.replace (T.singleton ':') (T.singleton '-') (T.pack $ iso8601Show now)
  return
    ( pathPrefix <> identifier <> "person-" <> personId <> "/"
        <> show fileType
        <> "/"
        <> fileName
        <> imageExtension
    )

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

makeTransactionIdAndAadhaarHashKey :: Id Person.Person -> Text
makeTransactionIdAndAadhaarHashKey id = "AVTIdAH:PId-" <> id.getId

makeGenerateOtpTryKey :: Id Person.Person -> Text
makeGenerateOtpTryKey id = "GOTKId:PId-" <> id.getId

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
        personId = personId,
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
        personId = personId,
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
  m VDomain.AadhaarVerification
mkAadhaar personId name gender dob aadhaarHash imgPath aadhaarVerified = do
  now <- getCurrentTime
  return $
    VDomain.AadhaarVerification
      { personId = personId,
        personName = name,
        personGender = gender,
        personDob = dob,
        personImagePath = imgPath,
        aadhaarNumberHash = aadhaarHash,
        isVerified = aadhaarVerified,
        createdAt = now,
        updatedAt = now
      }

checkForDuplicacy :: DbHash -> Flow ()
checkForDuplicacy aadhaarHash = do
  aadhaarInfo <- QAV.findByAadhaarNumberHash aadhaarHash
  when (isJust aadhaarInfo) $ throwError AadhaarAlreadyLinked
