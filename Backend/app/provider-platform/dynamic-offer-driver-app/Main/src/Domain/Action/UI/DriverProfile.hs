{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.DriverProfile
  ( postDriverProfileUpdateAuthDataTriggerOTP,
    postDriverProfileUpdateAuthDataVerifyOTP,
    AuthData (..),
  )
where

import qualified API.Types.UI.DriverProfile as DriverProfileTypes
-- import qualified Domain.Types.Person as SP
-- import Environment
-- import Kernel.External.Encryption
-- import Kernel.Prelude
-- import Kernel.Sms.Config
-- import qualified Kernel.Storage.Hedis as Redis
-- import Kernel.Streaming.Kafka.Producer.Types (KafkaProducerTools)
-- import Kernel.Types.APISuccess
-- import qualified Kernel.Types.APISuccess as APISuccess
-- import Kernel.Storage.Esqueleto.Config
-- import Kernel.Tools.Metrics.CoreMetrics
-- import Kernel.Beam.Functions (runInReplica)
-- import Kernel.Types.Id
-- import Kernel.Utils.Common
-- import qualified SharedLogic.OTP as SOTP
-- import qualified Storage.Queries.Person as QPerson
-- import qualified Storage.Queries.PersonExtra as QPersonExtra
-- import Tools.Error

import Control.Applicative ((<|>))
import Data.Aeson as DA
import qualified Data.Text as T
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as SP
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Sms.Config (SmsConfig, useFakeSms)
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types (KafkaProducerTools)
import Kernel.Tools.Metrics.CoreMetrics
import qualified Kernel.Types.APISuccess as APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.OTP as SOTP
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.PersonExtra as QPersonExtra
import Tools.Error

data AuthData = AuthData
  { mobileNumber :: Maybe Text,
    mobileNumberCountryCode :: Maybe Text,
    email :: Maybe Text,
    otp :: DbHash
  }
  deriving (Generic, Show, FromJSON, ToJSON)

postDriverProfileUpdateAuthDataTriggerOTP ::
  ( HasFlowEnv m r '["smsCfg" ::: SmsConfig, "kafkaProducerTools" ::: KafkaProducerTools],
    CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    EsqDBReplicaFlow m r
  ) =>
  (Maybe (Id SP.Person), Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  DriverProfileTypes.TriggerUpdateAuthOTPReq ->
  m APISuccess.APISuccess
postDriverProfileUpdateAuthDataTriggerOTP (mbPersonId, _merchantId, merchantOpCityId) req = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "Person ID not found")
  person <- runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  smsCfg <- asks (.smsCfg)

  let identifierType = req.identifier
  let useFakeOtpM = (show <$> useFakeSms smsCfg) <|> person.useFakeOtp

  otpCode <- maybe generateOTPCode return useFakeOtpM

  when (isNothing useFakeOtpM) $ do
    SOTP.sendOTPByIdentifierType
      identifierType
      otpCode
      personId
      person.merchantId
      merchantOpCityId
      req.mobileCountryCode
      (Just req.value)
      Nothing

  case identifierType of
    SP.MOBILENUMBER -> do
      countryCode <- req.mobileCountryCode & fromMaybeM (InvalidRequest "MobileCountryCode is required for MOBILENUMBER identifier")
      let mobileNumber = req.value

      mobileNumberDbHash <- getDbHash mobileNumber
      mobileNumberExists <- QPersonExtra.findByMobileNumberAndMerchantAndRole countryCode mobileNumberDbHash person.merchantId SP.DRIVER
      whenJust mobileNumberExists $ \existing ->
        when (existing.id /= personId) $ throwError (InvalidRequest "Phone number already registered")

      otpHash <- getDbHash otpCode
      let authData =
            AuthData
              { mobileNumber = Just mobileNumber,
                mobileNumberCountryCode = Just countryCode,
                email = Nothing,
                otp = otpHash
              }
      let redisKey = makeUpdateAuthRedisKey identifierType (getId personId)
          expirySeconds = smsCfg.sessionConfig.authExpiry * 60
      Redis.setExp redisKey authData expirySeconds
    SP.EMAIL -> do
      let receiverEmail = req.value
      when (T.null receiverEmail) $ throwError $ InvalidRequest "Email is required for EMAIL identifier"
      existingPerson <- QPersonExtra.findByEmailAndMerchantIdAndRole (Just receiverEmail) person.merchantId SP.DRIVER
      whenJust existingPerson $ \existing ->
        when (existing.id /= personId) $ throwError $ InvalidRequest "Email already registered"
      otpHash <- getDbHash otpCode
      let authData =
            AuthData
              { mobileNumber = Nothing,
                mobileNumberCountryCode = Nothing,
                email = Just receiverEmail,
                otp = otpHash
              }
      let redisKey = makeUpdateAuthRedisKey identifierType (getId personId)
          expirySeconds = smsCfg.sessionConfig.authExpiry * 60
      Redis.setExp redisKey authData expirySeconds
    SP.AADHAAR -> throwError $ InvalidRequest "Aadhaar identifier is not supported"

  pure APISuccess.Success

postDriverProfileUpdateAuthDataVerifyOTP ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    EsqDBReplicaFlow m r,
    HasFlowEnv m r '["version" ::: DeploymentVersion]
  ) =>
  (Maybe (Id SP.Person), Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  DriverProfileTypes.VerifyUpdateAuthOTPReq ->
  m APISuccess.APISuccess
postDriverProfileUpdateAuthDataVerifyOTP (mbPersonId, _merchantId, _) req = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "Person ID not found")
  identifierType <- req.identifier & fromMaybeM (InvalidRequest "Identifier type is required")

  let redisKey = makeUpdateAuthRedisKey identifierType (getId personId)
  mbStoredAuthData <- Redis.get redisKey
  storedAuthData :: AuthData <- case mbStoredAuthData of
    Just v -> pure v
    Nothing -> throwError $ InvalidRequest "OTP_EXPIRED: OTP expired"
  reqOtpHash <- getDbHash req.otp
  when ((storedAuthData.otp) /= reqOtpHash) $ throwError $ InvalidRequest "INVALID_OTP: Invalid OTP"

  void $ case identifierType of
    SP.MOBILENUMBER -> do
      storedMobileNumber <- case storedAuthData.mobileNumber of
        Just num -> pure num
        _ -> throwError $ InvalidRequest "AUTH_MOBILE_NOT_FOUND: Mobile number not found in auth data"

      storedCountryCode <- case storedAuthData.mobileNumberCountryCode of
        Just code -> pure code
        _ -> throwError $ InvalidRequest "AUTH_COUNTRY_CODE_NOT_FOUND: Country code not found in auth data"
      encMobileNumber <- encrypt storedMobileNumber
      mobileNumberHash <- getDbHash storedMobileNumber
      QPersonExtra.updateMobileNumberByPersonId personId encMobileNumber mobileNumberHash storedCountryCode
    SP.EMAIL -> do
      storedEmail <- case storedAuthData of
        AuthData {email = Just em} -> pure em
        _ -> throwError $ InvalidRequest "AUTH_EMAIL_NOT_FOUND: Email not found in auth data"
      encryptedValue <- encrypt storedEmail
      QPersonExtra.updateEmailByPersonId personId encryptedValue
    SP.AADHAAR -> throwError $ InvalidRequest "Aadhaar identifier is not supported"

  void $ Redis.del redisKey

  pure APISuccess.Success

makeUpdateAuthRedisKey :: SP.IdentifierType -> Text -> Text
makeUpdateAuthRedisKey identifierType identifier =
  "updateAuth:" <> show identifierType <> ":" <> identifier
