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
    postDriverOperatingCityUpdate,
    AuthData (..),
  )
where

import qualified API.Types.UI.DriverProfile as DriverProfileTypes
import Control.Applicative ((<|>))
import Data.Aeson as DA
import qualified Data.Text as T
import qualified Domain.Action.Beckn.Search as DSearch
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as SP
import qualified Email.Types
import Environment
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.External.Maps.Types (LatLong (..))
import Kernel.Prelude
import Kernel.Sms.Config (SmsConfig, useFakeSms)
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types (KafkaProducerTools)
import Kernel.Tools.Metrics.CoreMetrics
import qualified Kernel.Types.APISuccess as APISuccess
import Kernel.Types.Geofencing (GeoRestriction (..))
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.DriverCityMigration as SDriverCityMigration
import qualified SharedLogic.OTP as SOTP
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.DriverInformation as QDI
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

postDriverOperatingCityUpdate ::
  (Maybe (Id SP.Person), Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  DriverProfileTypes.UpdateOperatingCityReq ->
  Flow DriverProfileTypes.UpdateOperatingCityRes
postDriverOperatingCityUpdate (mbPersonId, merchantId, currentOpCityId) req = do
  driverId <- mbPersonId & fromMaybeM (PersonNotFound "Person ID not found")
  driverInfo <- QDI.findById (cast driverId) >>= fromMaybeM DriverInfoNotFound
  if driverInfo.onRide
    then keepCurrentCity
    else do
      merchant <- CQM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
      when (merchant.geofencingConfig.origin == Unrestricted) $ throwError (InvalidRequest "Operating city cannot be derived from location for this merchant")
      try @_ @SomeException (DSearch.getNearestOperatingAndSourceCity merchant (LatLong req.lat req.lon)) >>= \case
        Left err -> do
          logInfo $ "Keeping operating city for driver " <> driverId.getId <> ", could not resolve city from location: " <> show err
          keepCurrentCity
        Right DSearch.NearestOperatingAndSourceCity {nearestOperatingCity, sourceCity} ->
          if sourceCity.city /= nearestOperatingCity.city
            then keepCurrentCity
            else do
              targetOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just nearestOperatingCity.city)
              driverPerson <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
              let isCityChanged = targetOpCityId /= driverPerson.merchantOperatingCityId
              when isCityChanged $ do
                logInfo $ "Driver " <> driverId.getId <> " changing operating city from " <> driverPerson.merchantOperatingCityId.getId <> " to " <> targetOpCityId.getId
                SDriverCityMigration.migrateDriverOperatingCity driverPerson merchantId targetOpCityId
              pure $ mkUpdateOperatingCityRes nearestOperatingCity.city nearestOperatingCity.state isCityChanged
  where
    keepCurrentCity = do
      currentOpCity <- CQMOC.findById currentOpCityId >>= fromMaybeM (MerchantOperatingCityNotFound currentOpCityId.getId)
      pure $ mkUpdateOperatingCityRes currentOpCity.city currentOpCity.state False

    mkUpdateOperatingCityRes resCity resState changed =
      DriverProfileTypes.UpdateOperatingCityRes
        { DriverProfileTypes.city = resCity,
          DriverProfileTypes.state = resState,
          DriverProfileTypes.isCityChanged = changed
        }

postDriverProfileUpdateAuthDataTriggerOTP ::
  ( HasFlowEnv m r '["smsCfg" ::: SmsConfig, "kafkaProducerTools" ::: KafkaProducerTools],
    HasField "emailServiceConfig" r Email.Types.EmailServiceConfig,
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

  case identifierType of
    SP.MOBILENUMBER -> do
      countryCode <- req.mobileCountryCode & fromMaybeM (InvalidRequest "MobileCountryCode is required for MOBILENUMBER identifier")
      let mobileNumber = req.value

      mobileNumberDbHash <- getDbHash mobileNumber
      mobileNumberExists <- QPersonExtra.findByMobileNumberAndMerchantAndRole countryCode mobileNumberDbHash person.merchantId person.role
      whenJust mobileNumberExists $ \existing ->
        when (existing.id /= personId) $ throwError (InvalidRequest "Phone number already registered")

      storeAndSendOTP otpCode identifierType personId person smsCfg useFakeOtpM merchantOpCityId (Just mobileNumber) (Just countryCode) Nothing
    SP.EMAIL -> do
      let receiverEmail = req.value
      when (T.null receiverEmail) $ throwError $ InvalidRequest "Email is required for EMAIL identifier"
      existingPerson <- QPersonExtra.findByEmailAndMerchantIdAndRole (Just receiverEmail) person.merchantId person.role
      whenJust existingPerson $ \existing ->
        when (existing.id /= personId) $ throwError $ InvalidRequest "Email already registered"
      storeAndSendOTP otpCode identifierType personId person smsCfg useFakeOtpM merchantOpCityId Nothing Nothing (Just receiverEmail)
    SP.AADHAAR -> throwError $ InvalidRequest "Aadhaar identifier is not supported"
    SP.GIMS_EMAIL_PASSWORD -> throwError $ InvalidRequest "GIMS_EMAIL_PASSWORD identifier is not supported for profile update"
    SP.GIMS_EMPLOYEE_ID_PASSWORD -> throwError $ InvalidRequest "GIMS_EMPLOYEE_ID_PASSWORD identifier is not supported for profile update"

  pure APISuccess.Success
  where
    storeAndSendOTP ::
      ( HasFlowEnv m r '["smsCfg" ::: SmsConfig, "kafkaProducerTools" ::: KafkaProducerTools],
        HasField "emailServiceConfig" r Email.Types.EmailServiceConfig,
        CacheFlow m r,
        EsqDBFlow m r,
        EncFlow m r
      ) =>
      Text ->
      SP.IdentifierType ->
      Id SP.Person ->
      SP.Person ->
      SmsConfig ->
      Maybe Text ->
      Id DMOC.MerchantOperatingCity ->
      Maybe Text ->
      Maybe Text ->
      Maybe Text ->
      m ()
    storeAndSendOTP otpCode' identifierType' personId' person' smsCfg' useFakeOtpM' merchantOpCityId' mobileNum countryCode emailVal = do
      otpHash <- getDbHash otpCode'
      let authData =
            AuthData
              { mobileNumber = mobileNum,
                mobileNumberCountryCode = countryCode,
                email = emailVal,
                otp = otpHash
              }
      let redisKey = makeUpdateAuthRedisKey identifierType' (getId personId')
          expirySeconds = smsCfg'.sessionConfig.authExpiry * 60
      Redis.setExp redisKey authData expirySeconds
      when (isNothing useFakeOtpM') $
        SOTP.sendOTPByIdentifierType
          identifierType'
          otpCode'
          personId'
          person'.merchantId
          merchantOpCityId'
          countryCode
          mobileNum
          emailVal
          Nothing

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
postDriverProfileUpdateAuthDataVerifyOTP (mbPersonId, merchantId, _) req = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "Person ID not found")
  person <- runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  identifierType <- req.identifier & fromMaybeM (InvalidRequest "Identifier type is required")

  let redisKey = makeUpdateAuthRedisKey identifierType (getId personId)
  mbStoredAuthData <- Redis.get redisKey
  storedAuthData :: AuthData <- case mbStoredAuthData of
    Just v -> pure v
    Nothing -> throwError $ InvalidRequest "OTP has expired. Please request a new one."
  reqOtpHash <- getDbHash req.otp
  when ((storedAuthData.otp) /= reqOtpHash) $ throwError $ InvalidRequest "The OTP entered is incorrect. Please try again."

  void $ case identifierType of
    SP.MOBILENUMBER -> do
      storedMobileNumber <- case storedAuthData.mobileNumber of
        Just num -> pure num
        _ -> throwError $ InvalidRequest "Mobile number not found. Please try again."

      storedCountryCode <- case storedAuthData.mobileNumberCountryCode of
        Just code -> pure code
        _ -> throwError $ InvalidRequest "Country code not found. Please try again."
      mobileNumberHash <- getDbHash storedMobileNumber
      mobileNumberExists <- QPersonExtra.findByMobileNumberAndMerchantAndRole storedCountryCode mobileNumberHash merchantId person.role
      whenJust mobileNumberExists $ \existing ->
        when (existing.id /= personId) $ throwError (InvalidRequest "Phone number already registered")
      encMobileNumber <- encrypt storedMobileNumber
      QPersonExtra.updateMobileNumberByPersonId personId encMobileNumber mobileNumberHash storedCountryCode
    SP.EMAIL -> do
      storedEmail <- case storedAuthData of
        AuthData {email = Just em} -> pure em
        _ -> throwError $ InvalidRequest "Email not found. Please try again."
      existingPerson <- QPersonExtra.findByEmailAndMerchantIdAndRole (Just storedEmail) merchantId person.role
      whenJust existingPerson $ \existing ->
        when (existing.id /= personId) $ throwError $ InvalidRequest "Email already registered"
      QPersonExtra.updateEmailByPersonId personId storedEmail
    SP.AADHAAR -> throwError $ InvalidRequest "Aadhaar identifier is not supported"
    SP.GIMS_EMAIL_PASSWORD -> throwError $ InvalidRequest "GIMS_EMAIL_PASSWORD identifier is not supported for profile update"
    SP.GIMS_EMPLOYEE_ID_PASSWORD -> throwError $ InvalidRequest "GIMS_EMPLOYEE_ID_PASSWORD identifier is not supported for profile update"

  void $ Redis.del redisKey

  pure APISuccess.Success

makeUpdateAuthRedisKey :: SP.IdentifierType -> Text -> Text
makeUpdateAuthRedisKey identifierType identifier =
  "updateAuth:" <> show identifierType <> ":" <> identifier
