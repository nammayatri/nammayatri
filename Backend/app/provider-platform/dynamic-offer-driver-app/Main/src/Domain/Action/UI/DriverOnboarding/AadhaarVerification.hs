{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE UndecidableInstances #-}

module Domain.Action.UI.DriverOnboarding.AadhaarVerification
  ( generateAadhaarOtp,
    verifyAadhaarOtp,
    VerifyOtpReq,
  )
where

import Data.Text (pack)
import qualified Domain.Types.DriverOnboarding.AadhaarOtpReq as RDomain
import qualified Domain.Types.DriverOnboarding.AadhaarOtpVerify as Domain
import qualified Domain.Types.DriverOnboarding.AadhaarVerification as VDomain
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as Person
import Environment
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.DriverInformation as DriverInfo
import qualified Storage.Queries.DriverOnboarding.AadhaarOtpReq as Query
import qualified Storage.Queries.DriverOnboarding.AadhaarOtpVerify as VQuery
import qualified Storage.Queries.DriverOnboarding.AadhaarVerification as Q
import qualified Storage.Queries.Person as Person
import qualified Tools.AadhaarVerification as AadhaarVerification
import Tools.Error

data VerifyOtpReq = VerifyOtpReq
  { otp :: Int,
    includeXml :: Bool,
    shareCode :: Text
  }
  deriving (Show, Generic, ToSchema, ToJSON, FromJSON)

generateAadhaarOtp ::
  Bool ->
  Maybe DM.Merchant ->
  (Id Person.Person, Id DM.Merchant) ->
  AadhaarVerification.AadhaarOtpReq ->
  Flow AadhaarVerification.AadhaarVerificationResp
generateAadhaarOtp _ mbMerchant (personId, _) req = do
  person <- Person.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  driverInfo <- DriverInfo.findById (cast personId) >>= fromMaybeM (PersonNotFound personId.getId)
  when driverInfo.blocked $ throwError DriverAccountBlocked
  whenJust mbMerchant $ \merchant -> do
    -- merchant access checking
    unless (merchant.id == person.merchantId) $ throwError (PersonNotFound personId.getId)
  res <- AadhaarVerification.generateAadhaarOtp person.merchantId $ req
  aadhaarOtpEntity <- mkAadhaarOtp personId res
  Esq.runTransaction $ Query.create aadhaarOtpEntity
  let key = makeTransactionNumberKey personId
  Redis.setExp key res.transactionId 600
  pure res

verifyAadhaarOtp ::
  Bool ->
  Maybe DM.Merchant ->
  (Id Person.Person, Id DM.Merchant) ->
  VerifyOtpReq ->
  Flow AadhaarVerification.AadhaarOtpVerifyRes
verifyAadhaarOtp _ mbMerchant (personId, _) req = do
  person <- Person.findById personId >>= fromMaybeM (PersonNotFound (getId personId))
  driverInfo <- DriverInfo.findById (cast personId) >>= fromMaybeM (PersonNotFound (getId personId))
  when (driverInfo.blocked) $ throwError DriverAccountBlocked
  whenJust mbMerchant $ \merchant -> do
    -- merchant access checking
    unless (merchant.id == person.merchantId) $ throwError (PersonNotFound (getId personId))
  let key = makeTransactionNumberKey personId
  transactionId <- Redis.safeGet key
  case transactionId of
    Just tId -> do
      Redis.del key
      let temp =
            AadhaarVerification.AadhaarOtpVerifyReq
              { otp = req.otp,
                includeXml = req.includeXml,
                shareCode = req.shareCode,
                transactionId = tId
              }
      res <- AadhaarVerification.verifyAadhaarOtp person.merchantId temp
      aadhaarVerifyEntity <- mkAadhaarVerify personId tId res
      Esq.runTransaction $ VQuery.create aadhaarVerifyEntity
      if res.code == pack "1002"
        then do
          aadhaarEntity <- mkAadhaar personId res
          Esq.runTransaction $ Q.create aadhaarEntity
        else throwError $ InternalError "Aadhaar Verification failed, Please try again"
      pure res
    Nothing -> throwError $ InternalError "transaction Id not found ,Try again"

makeTransactionNumberKey :: Id Person.Person -> Text
makeTransactionNumberKey id = "AadhaarVerificationTransactionId:PersonId-" <> id.getId

mkAadhaarOtp ::
  (MonadGuid m, MonadTime m) =>
  Id Person.Person ->
  AadhaarVerification.AadhaarVerificationResp ->
  m RDomain.AadhaarOtpReq
mkAadhaarOtp personId res = do
  id <- generateGUID
  now <- getCurrentTime
  return $
    RDomain.AadhaarOtpReq
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
  AadhaarVerification.AadhaarOtpVerifyRes ->
  m VDomain.AadhaarVerification
mkAadhaar personId res = do
  id <- generateGUID
  now <- getCurrentTime
  return $
    VDomain.AadhaarVerification
      { id,
        driverId = personId,
        driverName = res.name,
        driverGender = res.gender,
        driverDob = res.date_of_birth,
        driverImage = res.image,
        createdAt = now
      }
