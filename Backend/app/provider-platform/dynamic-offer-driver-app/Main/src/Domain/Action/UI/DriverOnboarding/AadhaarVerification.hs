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

import Data.Text (pack)
import qualified Domain.Action.UI.DriverOnboarding.Status as Status
import qualified Domain.Types.DriverOnboarding.AadhaarOtp as Domain
import qualified Domain.Types.DriverOnboarding.AadhaarVerification as VDomain
import Domain.Types.DriverOnboarding.Error
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as Person
import Environment
import Kernel.External.Encryption (DbHash, getDbHash)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess (APISuccess (..))
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.DriverInformation as CQDriverInfo
import qualified Storage.CachedQueries.DriverInformation as DriverInfo
import Storage.CachedQueries.Merchant.MerchantConfig as CTC
import qualified Storage.Queries.DriverOnboarding.AadhaarOtp as Query
import qualified Storage.Queries.DriverOnboarding.AadhaarVerification as Q
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
          aadhaarEntity <- mkAadhaar personId res.name res.gender res.date_of_birth (Just aadhaarNumberHash) (Just res.image) True
          _ <- Q.create aadhaarEntity
          void $ CQDriverInfo.updateAadhaarVerifiedState (cast personId) True
          Status.statusHandler (person.id, person.merchantId) Nothing
        else throwError $ InternalError "Aadhaar Verification failed, Please try again"
      pure res
    Nothing -> throwError TransactionIdNotFound

unVerifiedAadhaarData ::
  Id Person.Person ->
  UnVerifiedDataReq ->
  Flow APISuccess
unVerifiedAadhaarData personId req = do
  mAadhaarCard <- Q.findByDriverId personId
  when (isJust mAadhaarCard) $ throwError AadhaarDataAlreadyPresent
  aadhaarEntity <- mkAadhaar personId req.driverName req.driverGender req.driverDob Nothing Nothing False
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
  m VDomain.AadhaarVerification
mkAadhaar personId name gender dob aadhaarHash img aadhaarVerified = do
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
        updatedAt = now
      }

checkForDuplicacy :: DbHash -> Flow ()
checkForDuplicacy aadhaarHash = do
  aadhaarInfo <- Q.findByAadhaarNumberHash aadhaarHash
  when (isJust aadhaarInfo) $ throwError AadhaarAlreadyLinked
