{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE OverloadedStrings #-}

module Domain.Action.UI.Registration
  ( AuthReq (..),
    AuthRes (..),
    ResendAuthRes,
    AuthVerifyReq (..),
    AuthVerifyRes (..),
    SOTP.OTPChannel (..),
    PasswordAuthReq (..),
    GetTokenReq (..),
    TempCodeRes (..),
    CustomerSignatureRes (..),
    SendBusinessEmailVerificationReq (..),
    SendBusinessEmailVerificationRes (..),
    VerifyBusinessEmailReq (..),
    VerifyBusinessEmailRes (..),
    auth,
    signatureAuth,
    createPerson,
    verify,
    resend,
    logout,
    generateCustomerReferralCode,
    createPersonWithPhoneNumber,
    buildPerson,
    getRegistrationTokenE,
    passwordBasedAuth,
    getToken,
    generateTempAppCode,
    makeSignature,
    sendBusinessEmailVerification,
    verifyBusinessEmail,
    resendBusinessEmailVerification,
  )
where

import qualified Data.Aeson as A
import Data.Aeson.Types ((.:), (.:?))
import Data.Either.Extra (eitherToMaybe)
import Data.List (nub)
import Data.Maybe (listToMaybe)
import Data.OpenApi hiding (email, info, tags)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Domain.Action.UI.Person as SP
import qualified Domain.Types.Depot as DDepot
import Domain.Types.Merchant (Merchant)
import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.PartnerOrganization as DPO
import Domain.Types.Person (PersonE (updatedAt))
import qualified Domain.Types.Person as SP
import qualified Domain.Types.PersonStats as DPS
import Domain.Types.RegistrationToken (RegistrationToken)
import qualified Domain.Types.RegistrationToken as SR
import qualified Domain.Types.Yudhishthira as Y
import qualified Email.AWS.Flow as Email
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Functions as B
import Kernel.External.Encryption (decrypt, encrypt, getDbHash)
import qualified Kernel.External.Maps as Maps
import qualified Kernel.External.Types as Language
import Kernel.External.Whatsapp.Interface.Types as Whatsapp
import Kernel.Sms.Config
import Kernel.Storage.Clickhouse.Config
import qualified Kernel.Storage.Esqueleto as DB
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Storage.Hedis.Queries as Hedis
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer, KafkaProducerTools)
import Kernel.Tools.Metrics.CoreMetrics
import Kernel.Types.APISuccess as AP
import qualified Kernel.Types.Beckn.Context as Context
import qualified Kernel.Types.Common as BC
import Kernel.Types.Id
import Kernel.Types.Predicate
import Kernel.Types.SlidingWindowLimiter (APIRateLimitOptions)
import Kernel.Types.Version
import Kernel.Utils.Common
import qualified Kernel.Utils.Predicates as P
import Kernel.Utils.SlidingWindowLimiter
import Kernel.Utils.Validation
import Kernel.Utils.Version
import qualified Lib.Yudhishthira.Event as Yudhishthira
import qualified Lib.Yudhishthira.Types as Yudhishthira
import qualified SharedLogic.MerchantConfig as SMC
import qualified SharedLogic.OTP as SOTP
import qualified SharedLogic.Person as SLP
import qualified Storage.CachedQueries.Merchant as QMerchant
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.MerchantServiceUsageConfig as QMSUC
import qualified Storage.CachedQueries.Merchant.RiderConfig as QRC
import qualified Storage.CachedQueries.MerchantConfig as CQM
import qualified Storage.CachedQueries.Person as CQP
import qualified Storage.Queries.DepotManager as QDepotManager
import qualified Storage.Queries.Person as Person
import qualified Storage.Queries.PersonDefaultEmergencyNumber as QPDEN
import qualified Storage.Queries.PersonDisability as PDisability
import qualified Storage.Queries.PersonExtra as PersonExtra
import qualified Storage.Queries.PersonStats as QPS
import qualified Storage.Queries.RegistrationToken as RegistrationToken
import Storage.Queries.SafetySettings as QSafety
import Tools.Auth (authTokenCacheKey, decryptAES128)
import Tools.Error
import qualified Tools.Notifications as Notify
import Tools.SignatureResponseBody (SignatureResponseConfig (..), SignedResponse, wrapWithSignature)
import Tools.Whatsapp

data AuthReq = AuthReq
  { mobileNumber :: Maybe Text,
    mobileCountryCode :: Maybe Text,
    identifierType :: Maybe SP.IdentifierType,
    merchantId :: ShortId Merchant,
    deviceToken :: Maybe Text,
    notificationToken :: Maybe Text,
    whatsappNotificationEnroll :: Maybe Whatsapp.OptApiMethods,
    firstName :: Maybe Text,
    middleName :: Maybe Text,
    lastName :: Maybe Text,
    email :: Maybe Text,
    businessEmail :: Maybe Text,
    language :: Maybe Maps.Language,
    gender :: Maybe SP.Gender,
    otpChannel :: Maybe SOTP.OTPChannel,
    registrationLat :: Maybe Double,
    registrationLon :: Maybe Double,
    enableOtpLessRide :: Maybe Bool,
    allowBlockedUserLogin :: Maybe Bool,
    isOperatorReq :: Maybe Bool
  }
  deriving (Generic, ToJSON, Show, ToSchema)

instance A.FromJSON AuthReq where
  parseJSON v = case v of
    A.Object obj ->
      AuthReq <$> obj .:? "mobileNumber"
        <*> obj .:? "mobileCountryCode"
        <*> obj .:? "identifierType"
        <*> obj .: "merchantId"
        <*> obj .:? "deviceToken"
        <*> obj .:? "userId" -- TODO :: This needs to be changed to notificationToken
        <*> obj .:? "whatsappNotificationEnroll"
        <*> obj .:? "firstName"
        <*> obj .:? "middleName"
        <*> obj .:? "lastName"
        <*> obj .:? "email"
        <*> obj .:? "businessEmail"
        <*> obj .:? "language"
        <*> obj .:? "gender"
        <*> obj .:? "otpChannel"
        <*> obj .:? "registrationLat"
        <*> obj .:? "registrationLon"
        <*> obj .:? "enableOtpLessRide"
        <*> obj .:? "allowBlockedUserLogin"
        <*> obj .:? "isOperatorReq"
    A.String s ->
      case A.eitherDecodeStrict (TE.encodeUtf8 s) of
        Left err -> fail err
        Right req -> return req
    _ -> fail "Invalid JSON format for AuthReq"

validateAuthReq :: Validate AuthReq
validateAuthReq AuthReq {..} =
  sequenceA_
    [ whenJust mobileNumber $ \justMobileNumber -> validateField "mobileNumber" justMobileNumber P.mobileNumber,
      whenJust mobileCountryCode $ \countryCode -> validateField "mobileCountryCode" countryCode P.mobileCountryCode
    ]

validateSignatureAuthReq :: Validate AuthReq
validateSignatureAuthReq AuthReq {..} =
  sequenceA_
    [ whenJust mobileCountryCode $ \countryCode -> validateField "mobileCountryCode" countryCode P.mobileCountryCode
    ]

data GetTokenReq = GetTokenReq
  { appSecretCode :: Text,
    userMobileNo :: Text
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

data TempCodeRes = TempCodeRes
  {tempCode :: Text}
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

data CustomerSignatureRes = CustomerSignatureRes
  { customerId :: Id SP.Person,
    customerPhoneNumber :: Maybe Text,
    customerName :: Maybe Text
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

data PasswordAuthReq = PasswordAuthReq
  { userEmail :: Maybe Text,
    userMobileNumber :: Maybe Text,
    userCountryCode :: Maybe Text,
    userMerchantId :: Id Merchant,
    userPassword :: Text,
    isOperatorReq :: Maybe Bool
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

data AuthRes = AuthRes
  { authId :: Id RegistrationToken,
    attempts :: Int,
    authType :: SR.LoginType,
    token :: Maybe Text,
    person :: Maybe PersonAPIEntity,
    isPersonBlocked :: Bool,
    depotCode :: Maybe (Id DDepot.Depot),
    isDepotAdmin :: Maybe Bool
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

type ResendAuthRes = AuthRes

---------- Verify Login --------
data AuthVerifyReq = AuthVerifyReq
  { otp :: Text,
    deviceToken :: Text,
    whatsappNotificationEnroll :: Maybe Whatsapp.OptApiMethods,
    userPasswordString :: Maybe Text
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

validateAuthVerifyReq :: Validate AuthVerifyReq
validateAuthVerifyReq AuthVerifyReq {..} =
  sequenceA_
    [ validateField "otp" otp $ ExactLength 4 `And` star P.digit
    ]

data AuthVerifyRes = AuthVerifyRes
  { token :: Text,
    person :: PersonAPIEntity
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

authHitsCountKey :: SP.Person -> Text
authHitsCountKey person = "BAP:Registration:auth" <> getId person.id <> ":hitsCount"

auth ::
  ( HasFlowEnv m r ["apiRateLimitOptions" ::: APIRateLimitOptions, "smsCfg" ::: SmsConfig, "version" ::: DeploymentVersion, "kafkaProducerTools" ::: KafkaProducerTools],
    CacheFlow m r,
    DB.EsqDBReplicaFlow m r,
    ClickhouseFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    HasKafkaProducer r
  ) =>
  AuthReq ->
  Maybe Version ->
  Maybe Version ->
  Maybe Version ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  m AuthRes
auth req' mbBundleVersion mbClientVersion mbClientConfigVersion mbRnVersion mbDevice mbXForwardedFor mbSenderHash = do
  let req = if req'.merchantId.getShortId == "YATRI" then req' {merchantId = ShortId "NAMMA_YATRI"} else req'
  let mbClientIP =
        mbXForwardedFor
          >>= \headerValue ->
            (listToMaybe $ T.splitOn "," headerValue) >>= \firstIP ->
              let ipWithoutPort = T.takeWhile (/= ':') $ T.strip firstIP
               in if T.null ipWithoutPort then Nothing else Just ipWithoutPort
  whenJust mbClientIP $ \clientIP -> do
    logInfo $ "Auth request from IP: " <> clientIP <> " for identifier: " <> show req'.mobileNumber
    ipBlocked <- SMC.isIPBlocked clientIP
    when ipBlocked $ throwError IpHitsLimitExceeded
  runRequestValidation validateAuthReq req
  let identifierType = fromMaybe SP.MOBILENUMBER req'.identifierType
  now <- getCurrentTime
  merchantTemp <-
    QMerchant.findByShortId req.merchantId
      >>= fromMaybeM (MerchantNotFound $ getShortId req.merchantId)
  merchant <-
    if merchantTemp.shortId == merchantTemp.fallbackShortId
      then return merchantTemp
      else
        QMerchant.findByShortId merchantTemp.fallbackShortId
          >>= fromMaybeM (MerchantNotFound $ getShortId merchantTemp.fallbackShortId)

  (person, otpChannel) <-
    case identifierType of
      SP.MOBILENUMBER -> do
        countryCode <- req.mobileCountryCode & fromMaybeM (InvalidRequest "MobileCountryCode is required for mobileNumber auth")
        mobileNumber <- req.mobileNumber & fromMaybeM (InvalidRequest "MobileCountryCode is required for mobileNumber auth")
        let notificationToken = req.notificationToken
            otpChannel = fromMaybe SOTP.defaultOTPChannel req.otpChannel
        mobileNumberHash <- getDbHash mobileNumber
        person <-
          Person.findByRoleAndMobileNumberAndMerchantId SP.USER countryCode mobileNumberHash merchant.id
            >>= maybe (createPerson req SP.MOBILENUMBER notificationToken mbBundleVersion mbClientVersion mbClientConfigVersion mbRnVersion (getDeviceFromText mbDevice) merchant Nothing) return
        return (person, otpChannel)
      SP.EMAIL -> do
        email <- req.email & fromMaybeM (InvalidRequest "Email is required for email auth")
        person <-
          Person.findByEmailAndMerchantId merchant.id email
            >>= maybe (createPerson req SP.EMAIL Nothing mbBundleVersion mbClientVersion mbClientConfigVersion mbRnVersion (getDeviceFromText mbDevice) merchant Nothing) return
        return (person, SOTP.EMAIL)
      SP.AADHAAR -> throwError $ InvalidRequest "Aadhaar auth is not supported"

  when (fromMaybe False person.authBlocked && maybe False (now <) person.blockedUntil) $ throwError IpHitsLimitExceeded
  void $ cachePersonOTPChannel person.id otpChannel
  let merchantOperatingCityId = person.merchantOperatingCityId
  riderConfig <- QRC.findByMerchantOperatingCityId merchantOperatingCityId Nothing >>= fromMaybeM (RiderConfigDoesNotExist $ "merchantOperatingCityId:- " <> merchantOperatingCityId.getId)
  merchantConfigs <- CQM.findAllByMerchantOperatingCityId merchantOperatingCityId Nothing
  fork "Fraud Auth Check Processing" $ do
    when (fromMaybe False person.authBlocked || maybe False (now >) person.blockedUntil) $ Person.updatingAuthEnabledAndBlockedState person.id Nothing (Just False) Nothing
    whenJust mbClientIP $ \clientIP -> do
      SMC.updateCustomerAuthCountersByIP clientIP merchantConfigs
      (isFraudDetected, mbMerchantConfigId) <- SMC.checkAuthFraudByIP merchantConfigs clientIP
      when isFraudDetected $ do
        whenJust mbMerchantConfigId $ \mcId ->
          SMC.blockCustomerByIP clientIP (Just mcId) riderConfig.blockedUntilInMins

  checkSlidingWindowLimit (authHitsCountKey person)
  smsCfg <- asks (.smsCfg)
  let entityId = getId $ person.id
      useFakeOtpM = (show <$> useFakeSms smsCfg) <|> person.useFakeOtp
      scfg = sessionConfig smsCfg
  let mkId = getId $ merchant.id
  regToken <- makeSession (castChannelToMedium otpChannel) scfg entityId mkId useFakeOtpM False
  if (fromMaybe False req.allowBlockedUserLogin) || not person.blocked
    then do
      deploymentVersion <- asks (.version)
      void $ Person.updatePersonVersions person mbBundleVersion mbClientVersion mbClientConfigVersion (getDeviceFromText mbDevice) deploymentVersion.getDeploymentVersion mbRnVersion
      RegistrationToken.create regToken
      when (isNothing useFakeOtpM) $ do
        let otpCode = SR.authValueHash regToken
        SOTP.sendOTP
          otpChannel
          otpCode
          person.id
          person.merchantId
          merchantOperatingCityId
          req.mobileCountryCode
          req.mobileNumber
          req.email
          riderConfig.emailOtpConfig
          mbSenderHash
    else logInfo $ "Person " <> getId person.id <> " is not enabled. Skipping send OTP"
  (mbDepotCode, mbIsDepotAdmin) <-
    if req.isOperatorReq == Just True
      then do
        mbDepotManager <- QDepotManager.findByPersonId person.id
        return (mbDepotManager <&> (.depotCode), mbDepotManager <&> (.isAdmin))
      else return (Nothing, Nothing)
  return $ AuthRes regToken.id regToken.attempts regToken.authType Nothing Nothing person.blocked mbDepotCode mbIsDepotAdmin
  where
    castChannelToMedium :: SOTP.OTPChannel -> SR.Medium
    castChannelToMedium SOTP.SMS = SR.SMS
    castChannelToMedium SOTP.WHATSAPP = SR.WHATSAPP
    castChannelToMedium SOTP.EMAIL = SR.EMAIL

signatureAuth ::
  ( HasFlowEnv m r '["smsCfg" ::: SmsConfig, "version" ::: DeploymentVersion],
    CacheFlow m r,
    DB.EsqDBReplicaFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    HasKafkaProducer r
  ) =>
  AuthReq ->
  Maybe Version ->
  Maybe Version ->
  Maybe Version ->
  Maybe Text ->
  Maybe Text ->
  m AuthRes
signatureAuth req' mbBundleVersion mbClientVersion mbClientConfigVersion mbRnVersion mbDevice = do
  let req = if req'.merchantId.getShortId == "YATRI" then req' {merchantId = ShortId "NAMMA_YATRI"} else req'
  runRequestValidation validateSignatureAuthReq req
  smsCfg <- asks (.smsCfg)
  countryCode <- req.mobileCountryCode & fromMaybeM (InvalidRequest "MobileCountryCode is required for signature auth")
  mobileNumber <- req.mobileNumber & fromMaybeM (InvalidRequest "MobileCountryCode is required for signature auth")
  let deviceToken = req.deviceToken
  merchant <-
    QMerchant.findByShortId req.merchantId
      >>= fromMaybeM (MerchantNotFound $ getShortId req.merchantId)
  mobileNumberDecrypted <- decryptAES128 merchant.cipherText mobileNumber
  let reqWithMobileNumebr = req {mobileNumber = Just mobileNumberDecrypted}
  notificationToken <- mapM (decryptAES128 merchant.cipherText) req.notificationToken
  mobileNumberHash <- getDbHash mobileNumberDecrypted
  person <-
    Person.findByRoleAndMobileNumberAndMerchantId SP.USER countryCode mobileNumberHash merchant.id
      >>= maybe (createPerson reqWithMobileNumebr SP.MOBILENUMBER notificationToken mbBundleVersion mbClientVersion mbClientConfigVersion mbRnVersion (getDeviceFromText mbDevice) merchant Nothing) return
  let entityId = getId $ person.id
      useFakeOtpM = (show <$> useFakeSms smsCfg) <|> person.useFakeOtp
      scfg = sessionConfig smsCfg
  let mkId = getId $ merchant.id
  regToken <- makeSession SR.SIGNATURE scfg entityId mkId useFakeOtpM False
  if not person.blocked
    then do
      deploymentVersion <- asks (.version)
      void $ Person.updatePersonVersions person mbBundleVersion mbClientVersion mbClientConfigVersion (getDeviceFromText mbDevice) deploymentVersion.getDeploymentVersion mbRnVersion
      _ <- RegistrationToken.create regToken
      mbEncEmail <- encrypt `mapM` reqWithMobileNumebr.email
      mbEncBusinessEmail <- encrypt `mapM` reqWithMobileNumebr.businessEmail
      _ <- RegistrationToken.setDirectAuth regToken.id SR.SIGNATURE
      _ <- Person.updatePersonalInfo person.id (reqWithMobileNumebr.firstName <|> person.firstName <|> Just "User") reqWithMobileNumebr.middleName reqWithMobileNumebr.lastName mbEncEmail mbEncBusinessEmail deviceToken notificationToken (reqWithMobileNumebr.language <|> person.language <|> Just Language.ENGLISH) (reqWithMobileNumebr.gender <|> Just person.gender) mbRnVersion (mbClientVersion <|> Nothing) (mbBundleVersion <|> Nothing) mbClientConfigVersion (getDeviceFromText mbDevice) deploymentVersion.getDeploymentVersion person.enableOtpLessRide Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing person Nothing Nothing Nothing
      personAPIEntity <- verifyFlow person regToken reqWithMobileNumebr.whatsappNotificationEnroll deviceToken
      return $ AuthRes regToken.id regToken.attempts SR.DIRECT (Just regToken.token) (Just personAPIEntity) person.blocked Nothing Nothing
    else return $ AuthRes regToken.id regToken.attempts regToken.authType Nothing Nothing person.blocked Nothing Nothing

mkUserIdFromTempAppSecretKey :: Text -> Text
mkUserIdFromTempAppSecretKey appSecretKey = "rider-platform:getUserIdKey:" <> appSecretKey

setUserIdToTempAppSecretKey :: (CacheFlow m r, Redis.HedisFlow m r) => Text -> Id SP.Person -> m ()
setUserIdToTempAppSecretKey appSecretKey personId = Redis.setExp (mkUserIdFromTempAppSecretKey appSecretKey) (getId personId) 120 -- 2 minutes

getUserIdFromTempAppSecretKey :: (CacheFlow m r, Redis.HedisFlow m r) => Text -> m (Maybe (Id SP.Person))
getUserIdFromTempAppSecretKey appSecretKey = (fmap (\(a :: Text) -> Id a)) <$> Redis.safeGet (mkUserIdFromTempAppSecretKey appSecretKey)

generateTempAppCode ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    Redis.HedisFlow m r
  ) =>
  Id SP.Person ->
  m TempCodeRes
generateTempAppCode personId = do
  tempCode <- show . (`mod` 10000) <$> Redis.incr mkTempAppSecretKey
  person <- Person.findById personId >>= fromMaybeM (PersonNotFound $ personId.getId)
  case A.toJSON . (.hash) <$> person.mobileNumber of
    Just (A.String mobileNumberHash) -> do
      let appSecretKey = mobileNumberHash <> ":" <> tempCode
      setUserIdToTempAppSecretKey appSecretKey person.id
      return $ TempCodeRes tempCode
    _ -> throwError $ InvalidRequest "Mobile number not found"
  where
    mkTempAppSecretKey :: Text
    mkTempAppSecretKey = "rider-platform:temp-app-secret-key:"

makeSignature ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r
  ) =>
  Id SP.Person ->
  Id DMerchant.Merchant ->
  m (SignedResponse CustomerSignatureRes)
makeSignature personId merchantId = do
  person <- Person.findById personId >>= fromMaybeM (PersonNotFound $ personId.getId)
  merchant <- QMerchant.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)

  phoneNumber <- mapM decrypt person.mobileNumber
  let fullName = case (person.firstName, person.lastName) of
        (Just fn, Just ln) -> Just $ fn <> " " <> ln
        (Just fn, Nothing) -> Just fn
        _ -> Nothing
  let customerData =
        CustomerSignatureRes
          { customerId = person.id,
            customerPhoneNumber = phoneNumber,
            customerName = fullName
          }

  signingPrivateKey <- merchant.signingPrivateKey & fromMaybeM (InvalidRequest "Signing private key not found")
  let signatureConfig =
        SignatureResponseConfig
          { merchantShortId = merchant.shortId,
            signingPrivateKey = signingPrivateKey
          }

  wrapWithSignature signatureConfig customerData

getToken ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    EsqDBFlow m r,
    HasFlowEnv m r '["apiRateLimitOptions" ::: APIRateLimitOptions]
  ) =>
  GetTokenReq ->
  m AuthRes
getToken req = do
  mobileNumberHashVal <- A.toJSON <$> getDbHash req.userMobileNo
  case mobileNumberHashVal of
    A.String mobileNumberHash -> do
      getTokenAttempts <- Redis.incr $ getUserLimitKey mobileNumberHash
      when (getTokenAttempts > 3) $ throwError $ InvalidRequest "Too many attempts"
      Redis.expire (getUserLimitKey mobileNumberHash) 3600 -- 1 hour
      let appSecretKey = mobileNumberHash <> ":" <> req.appSecretCode
      mbPersonId <- getUserIdFromTempAppSecretKey appSecretKey
      case mbPersonId of
        Just personId -> do
          person <- Person.findById personId >>= fromMaybeM (PersonNotFound $ personId.getId)
          registrationToken <- (listToMaybe <$> RegistrationToken.findAllByPersonId personId) >>= fromMaybeM (InternalError $ "Registration token not found for person id: " <> getId personId)
          return $ AuthRes registrationToken.id 1 SR.PASSWORD (Just registrationToken.token) Nothing person.blocked Nothing Nothing
        Nothing -> do
          throwError $ GetUserIdError appSecretKey
    _ -> throwError $ InvalidRequest "Mobile number not found"
  where
    getUserLimitKey :: Text -> Text
    getUserLimitKey userMobileNo = "getUserLimitKey:" <> userMobileNo

passwordBasedAuth ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    EsqDBFlow m r,
    HasFlowEnv m r '["apiRateLimitOptions" ::: APIRateLimitOptions]
  ) =>
  PasswordAuthReq ->
  m AuthRes
passwordBasedAuth req = do
  (mbPerson, method) <-
    case (req.userMobileNumber, req.userCountryCode, req.userEmail) of
      (Just mobileNumber, Just countryCode, Nothing) -> do
        checkSlidingWindowLimit (verifyHitsCountKey . Id $ mobileNumber <> "_PASSWORD_AUTH")
        mobileNumberHash <- getDbHash mobileNumber
        (,SR.MOBILE_NUMBER) <$> Person.findByRoleAndMobileNumberAndMerchantId SP.USER countryCode mobileNumberHash req.userMerchantId
      (Nothing, _, Just email) -> do
        checkSlidingWindowLimit (verifyHitsCountKey . Id $ email <> "_PASSWORD_AUTH")
        (,SR.EMAIL) <$> Person.findByEmailAndMerchantId req.userMerchantId email
      _ -> throwError $ InvalidRequest "Invalid request"
  person <- fromMaybeM (PersonNotFound $ getId req.userMerchantId) mbPerson
  passwordHash <- getDbHash req.userPassword
  unless (person.passwordHash == Just passwordHash) $ throwError $ InvalidRequest "Invalid password"

  (mbDepotCode, mbIsDepotAdmin) <-
    if req.isOperatorReq == Just True
      then do
        mbDepotManager <- QDepotManager.findByPersonId person.id
        return (mbDepotManager <&> (.depotCode), mbDepotManager <&> (.isAdmin))
      else return (Nothing, Nothing)
  let scfg =
        SmsSessionConfig
          { attempts = 3,
            authExpiry = 30,
            tokenExpiry = 30
          }
  registrationToken <- makeSession method scfg person.id.getId req.userMerchantId.getId Nothing True
  _ <- RegistrationToken.create registrationToken
  _ <- RegistrationToken.deleteByPersonIdExceptNew person.id registrationToken.id
  return $ AuthRes registrationToken.id 1 SR.PASSWORD (Just registrationToken.token) Nothing person.blocked mbDepotCode mbIsDepotAdmin

buildPerson ::
  ( HasFlowEnv m r '["version" ::: DeploymentVersion],
    EncFlow m r,
    DB.EsqDBReplicaFlow m r,
    EsqDBFlow m r,
    Redis.HedisFlow m r,
    CacheFlow m r
  ) =>
  AuthReq ->
  SP.IdentifierType ->
  Maybe Text ->
  Maybe Version ->
  Maybe Version ->
  Maybe Version ->
  Maybe Text ->
  Maybe Device ->
  DMerchant.Merchant ->
  Context.City ->
  Id DMOC.MerchantOperatingCity ->
  Maybe (Id DPO.PartnerOrganization) ->
  m SP.Person
buildPerson req identifierType notificationToken clientBundleVersion clientSdkVersion clientConfigVersion mbRnVersion clientDevice merchant currentCity merchantOperatingCityId mbPartnerOrgId = do
  pid <- BC.generateGUID
  now <- getCurrentTime
  let useFakeOtp =
        (req.mobileNumber >>= (\n -> if n `elem` merchant.fakeOtpMobileNumbers then Just "7891" else Nothing))
          <|> (req.email >>= (\n -> if n `elem` merchant.fakeOtpEmails then Just "7891" else Nothing))
  personWithSameDeviceToken <- listToMaybe <$> runInReplica (Person.findBlockedByDeviceToken req.deviceToken)
  let isBlockedBySameDeviceToken = maybe False (.blocked) personWithSameDeviceToken
  useFraudDetection <- do
    if isBlockedBySameDeviceToken
      then do
        merchantConfig <- QMSUC.findByMerchantOperatingCityId merchantOperatingCityId >>= fromMaybeM (MerchantServiceUsageConfigNotFound $ "merchantOperatingCityId:- " <> merchantOperatingCityId.getId)
        return merchantConfig.useFraudDetection
      else return False
  encMobNum <- mapM encrypt req.mobileNumber
  encEmail <- mapM encrypt req.email
  encBusinessEmail <- mapM encrypt req.businessEmail
  deploymentVersion <- asks (.version)
  return $
    SP.Person
      { id = pid,
        firstName = req.firstName,
        middleName = req.middleName,
        lastName = req.lastName,
        role = SP.USER,
        gender = fromMaybe SP.UNKNOWN req.gender,
        identifierType,
        email = encEmail,
        passwordHash = Nothing,
        mobileNumber = encMobNum,
        mobileCountryCode = req.mobileCountryCode,
        identifier = Nothing,
        rating = Nothing,
        totalRatings = 0,
        totalRatingScore = 0,
        isValidRating = False,
        language = req.language,
        isNew = True,
        enabled = not (useFraudDetection && isBlockedBySameDeviceToken),
        blocked = useFraudDetection && isBlockedBySameDeviceToken,
        deviceToken = req.deviceToken,
        notificationToken = notificationToken,
        description = Nothing,
        merchantId = merchant.id,
        currentCity = currentCity,
        merchantOperatingCityId = merchantOperatingCityId,
        referralCode = Nothing,
        referredAt = Nothing,
        hasTakenValidRide = False,
        hasDisability = Nothing,
        createdAt = now,
        updatedAt = now,
        blockedAt = if useFraudDetection then personWithSameDeviceToken >>= (.blockedAt) else Nothing,
        blockedByRuleId = if useFraudDetection then personWithSameDeviceToken >>= (.blockedByRuleId) else Nothing,
        aadhaarVerified = False,
        clientSdkVersion = clientSdkVersion,
        clientBundleVersion = clientBundleVersion,
        clientConfigVersion = clientConfigVersion,
        clientReactNativeVersion = mbRnVersion,
        clientDevice = clientDevice,
        backendAppVersion = Just deploymentVersion.getDeploymentVersion,
        whatsappNotificationEnrollStatus = Nothing,
        shareEmergencyContacts = False,
        shareTripWithEmergencyContactOption = Nothing,
        hasCompletedMockSafetyDrill = Nothing,
        nightSafetyChecks = True,
        hasCompletedSafetySetup = False,
        registrationLat = req.registrationLat,
        registrationLon = req.registrationLon,
        latestLat = Nothing,
        latestLon = Nothing,
        useFakeOtp,
        followsRide = False,
        falseSafetyAlarmCount = 0,
        safetyCenterDisabledOnDate = Nothing,
        referredByCustomer = Nothing,
        customerReferralCode = Nothing,
        blockedCount = Just 0,
        deviceId = Nothing,
        androidId = Nothing,
        registeredViaPartnerOrgId = mbPartnerOrgId,
        customerPaymentId = Nothing,
        customerTestPaymentId = Nothing,
        juspayCustomerPaymentID = Nothing,
        defaultPaymentMethodId = Nothing,
        defaultTestPaymentMethodId = Nothing,
        enableOtpLessRide = req.enableOtpLessRide,
        totalRidesCount = Just 0,
        customerNammaTags = Nothing,
        informPoliceSos = False,
        payoutVpa = Nothing,
        frequentLocGeohashes = Just [],
        liveActivityToken = Nothing,
        dateOfBirth = Nothing,
        profilePicture = Nothing,
        verificationChannel = Nothing,
        blockedUntil = Nothing,
        authBlocked = Nothing,
        lastUsedVehicleServiceTiers = [],
        lastUsedVehicleCategories = [],
        imeiNumber = Nothing, -- TODO: take it from the request
        comments = Nothing,
        businessProfileVerified = Nothing,
        businessEmail = encBusinessEmail,
        paymentMode = Nothing
      }

-- FIXME Why do we need to store always the same authExpiry and tokenExpiry from config? info field is always Nothing
makeSession ::
  MonadFlow m =>
  SR.Medium ->
  SmsSessionConfig ->
  Text ->
  Text ->
  Maybe Text ->
  Bool ->
  m SR.RegistrationToken
makeSession authMedium SmsSessionConfig {..} entityId merchantId fakeOtp verified = do
  otp <- maybe generateOTPCode return fakeOtp
  rtid <- L.generateGUID
  token <- L.generateGUID
  now <- getCurrentTime
  return $
    SR.RegistrationToken
      { id = Id rtid,
        token = token,
        attempts = attempts,
        authMedium,
        authType = SR.OTP,
        authValueHash = otp,
        verified = verified,
        authExpiry = authExpiry,
        tokenExpiry = tokenExpiry,
        entityId = entityId,
        merchantId = merchantId,
        entityType = SR.USER,
        createdAt = now,
        updatedAt = now,
        info = Nothing,
        createdViaPartnerOrgId = Nothing
      }

verifyHitsCountKey :: Id SP.Person -> Text
verifyHitsCountKey id = "BAP:Registration:verify:" <> getId id <> ":hitsCount"

businessEmailSendHitsCountKey :: Id SP.Person -> Text
businessEmailSendHitsCountKey personId = "BAP:Registration:businessEmail:send:" <> getId personId <> ":hitsCount"

businessEmailVerifyHitsCountKey :: Id SP.Person -> Text
businessEmailVerifyHitsCountKey personId = "BAP:Registration:businessEmail:verify:" <> getId personId <> ":hitsCount"

verifyFlow :: (EsqDBFlow m r, EncFlow m r, CacheFlow m r, MonadFlow m, HasKafkaProducer r) => SP.Person -> SR.RegistrationToken -> Maybe Whatsapp.OptApiMethods -> Maybe Text -> m PersonAPIEntity
verifyFlow person regToken whatsappNotificationEnroll deviceToken = do
  let isNewPerson = person.isNew
  RegistrationToken.deleteByPersonIdExceptNew person.id regToken.id
  when isNewPerson $ Person.setIsNewFalse False person.id
  when isNewPerson $
    Notify.notifyOnRegistration regToken person deviceToken
  updPerson <- Person.findById (Id regToken.entityId) >>= fromMaybeM (PersonDoesNotExist regToken.entityId)
  decPerson <- decrypt updPerson
  customerDisability <- B.runInReplica $ PDisability.findByPersonId person.id
  let tag = customerDisability <&> (.tag)
  safetySettings <- QSafety.findSafetySettingsWithFallback person.id (Just updPerson)
  isSafetyCenterDisabled <- SLP.checkSafetyCenterDisabled updPerson safetySettings
  let personAPIEntity = SP.makePersonAPIEntity decPerson tag isSafetyCenterDisabled safetySettings
  unless (decPerson.whatsappNotificationEnrollStatus == whatsappNotificationEnroll && isJust whatsappNotificationEnroll) $ do
    fork "whatsapp_opt_api_call" $ do
      case decPerson.mobileNumber of
        Nothing -> throwError $ AuthBlocked "Mobile Number is null"
        Just mobileNo -> callWhatsappOptApi mobileNo person.id person.merchantId whatsappNotificationEnroll
  return personAPIEntity

verify ::
  ( CacheFlow m r,
    HasFlowEnv m r '["apiRateLimitOptions" ::: APIRateLimitOptions],
    EsqDBFlow m r,
    DB.EsqDBReplicaFlow m r,
    Redis.HedisFlow m r,
    EncFlow m r,
    HasKafkaProducer r
  ) =>
  Id SR.RegistrationToken ->
  AuthVerifyReq ->
  m AuthVerifyRes
verify tokenId req = do
  runRequestValidation validateAuthVerifyReq req
  regToken@SR.RegistrationToken {..} <- getRegistrationTokenE tokenId
  checkSlidingWindowLimit (verifyHitsCountKey $ Id entityId)
  when verified $ throwError $ AuthBlocked "Already verified."
  checkForExpiry authExpiry updatedAt
  unless (authValueHash == req.otp) $ throwError InvalidAuthData
  person <- checkPersonExists entityId
  let merchantOperatingCityId = person.merchantOperatingCityId
  let deviceToken = Just req.deviceToken
  personWithSameDeviceToken <- listToMaybe <$> runInReplica (Person.findBlockedByDeviceToken deviceToken)
  let isBlockedBySameDeviceToken = maybe False (.blocked) personWithSameDeviceToken
  cleanCachedTokens person.id
  when isBlockedBySameDeviceToken $ do
    merchantConfig <- QMSUC.findByMerchantOperatingCityId merchantOperatingCityId >>= fromMaybeM (MerchantServiceUsageConfigNotFound $ "merchantOperatingCityId:- " <> merchantOperatingCityId.getId)
    when merchantConfig.useFraudDetection $ SMC.blockCustomer person.id ((.blockedByRuleId) =<< personWithSameDeviceToken)
  void $ RegistrationToken.setVerified True tokenId
  void $ Person.updateDeviceToken deviceToken person.id
  personAPIEntity <- verifyFlow person regToken req.whatsappNotificationEnroll deviceToken
  whenJust req.userPasswordString $ \userPasswordString -> do
    passwordHash <- getDbHash userPasswordString
    void $ Person.updatePersonPassword (Just passwordHash) person.id
  when (isNothing person.customerReferralCode) $ do
    newCustomerReferralCode <- generateCustomerReferralCode
    checkIfReferralCodeExists <- Person.findPersonByCustomerReferralCode (Just newCustomerReferralCode)
    when (isNothing checkIfReferralCodeExists) $
      void $ Person.updateCustomerReferralCode person.id newCustomerReferralCode
  return $ AuthVerifyRes token personAPIEntity
  where
    checkForExpiry authExpiry updatedAt =
      whenM (isExpired (realToFrac (authExpiry * 60)) updatedAt) $
        throwError TokenExpired

callWhatsappOptApi ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    HasKafkaProducer r
  ) =>
  Text ->
  Id SP.Person ->
  Id DMerchant.Merchant ->
  Maybe Whatsapp.OptApiMethods ->
  m ()
callWhatsappOptApi mobileNo personId merchantId hasOptedIn = do
  let status = fromMaybe Whatsapp.OPT_IN hasOptedIn
  merchantOperatingCityId <- CQP.findCityInfoById personId >>= fmap (.merchantOperatingCityId) . fromMaybeM (PersonCityInformationDoesNotExist $ "personId:- " <> personId.getId)
  void $ whatsAppOptAPI merchantId merchantOperatingCityId $ Whatsapp.OptApiReq {phoneNumber = mobileNo, method = status}
  void $ Person.updateWhatsappNotificationEnrollStatus (Just status) personId

getRegistrationTokenE :: (CacheFlow m r, EsqDBFlow m r) => Id SR.RegistrationToken -> m SR.RegistrationToken
getRegistrationTokenE tokenId =
  RegistrationToken.findById tokenId >>= fromMaybeM (TokenNotFound $ getId tokenId)

createPerson ::
  ( HasFlowEnv m r '["version" ::: DeploymentVersion],
    EncFlow m r,
    EsqDBFlow m r,
    DB.EsqDBReplicaFlow m r,
    Redis.HedisFlow m r,
    CacheFlow m r
  ) =>
  AuthReq ->
  SP.IdentifierType ->
  Maybe Text ->
  Maybe Version ->
  Maybe Version ->
  Maybe Version ->
  Maybe Text ->
  Maybe Device ->
  DMerchant.Merchant ->
  Maybe (Id DPO.PartnerOrganization) ->
  m SP.Person
createPerson req identifierType notificationToken mbBundleVersion mbClientVersion mbClientConfigVersion mbRnVersion mbDevice merchant mbPartnerOrgId = do
  let currentCity = merchant.defaultCity
  merchantOperatingCityId <-
    CQMOC.findByMerchantIdAndCity merchant.id currentCity
      >>= fmap (.id)
        . fromMaybeM
          ( MerchantOperatingCityNotFound $
              "merchantId: " <> merchant.id.getId <> " ,city: " <> show currentCity
          )
  person <- buildPerson req identifierType notificationToken mbBundleVersion mbClientVersion mbClientConfigVersion mbRnVersion mbDevice merchant currentCity merchantOperatingCityId mbPartnerOrgId
  createPersonStats <- makePersonStats person
  Person.create person
  QPS.create createPersonStats
  addNammaTags person (Y.LoginTagData {id = person.id, gender = req.gender, clientSdkVersion = mbClientVersion, clientBundleVersion = mbBundleVersion, clientReactNativeVersion = mbRnVersion, clientConfigVersion = mbClientConfigVersion, clientDevice = mbDevice})
  fork "update emergency contact id" $
    whenJust req.mobileNumber $ \mobileNumber -> updatePersonIdForEmergencyContacts person.id mobileNumber merchant.id
  pure person
  where
    addNammaTags person tagData = do
      newPersonTags <- withTryCatch "computeNammaTagsWithExpiry:Login" (Yudhishthira.computeNammaTagsWithExpiry Yudhishthira.Login tagData)
      let tags = nub (fromMaybe [] person.customerNammaTags <> fromMaybe [] (eitherToMaybe newPersonTags))
      Person.updateCustomerTags (Just tags) tagData.id

    makePersonStats :: MonadTime m => SP.Person -> m DPS.PersonStats
    makePersonStats person = do
      now <- getCurrentTime
      return
        DPS.PersonStats
          { personId = person.id,
            userCancelledRides = 0,
            driverCancelledRides = 0,
            completedRides = 0,
            weekendRides = 0,
            weekdayRides = 0,
            offPeakRides = 0,
            eveningPeakRides = 0,
            morningPeakRides = 0,
            weekendPeakRides = 0,
            referralCount = 0,
            createdAt = now,
            updatedAt = now,
            ticketsBookedInEvent = Just 0,
            referralAmountPaid = 0,
            referralEarnings = 0,
            referredByEarnings = 0,
            validActivations = 0,
            referredByEarningsPayoutStatus = Nothing,
            backlogPayoutStatus = Nothing,
            backlogPayoutAmount = 0,
            isBackfilled = Just False
          }

checkPersonExists :: (CacheFlow m r, EsqDBFlow m r) => Text -> m SP.Person
checkPersonExists entityId =
  Person.findById (Id entityId) >>= fromMaybeM (PersonDoesNotExist entityId)

resend ::
  ( HasFlowEnv m r ["apiRateLimitOptions" ::: APIRateLimitOptions, "smsCfg" ::: SmsConfig, "kafkaProducerTools" ::: KafkaProducerTools],
    EsqDBFlow m r,
    EncFlow m r,
    CacheFlow m r,
    HasKafkaProducer r
  ) =>
  Id SR.RegistrationToken ->
  Maybe Text ->
  m ResendAuthRes
resend tokenId mbSenderHash = do
  SR.RegistrationToken {..} <- getRegistrationTokenE tokenId
  person <- checkPersonExists entityId
  unless (attempts > 0) $ throwError $ AuthBlocked "Attempts limit exceed."
  let merchantOperatingCityId = person.merchantOperatingCityId
  let otpCode = authValueHash
  otpChannel <- getPersonOTPChannel person.id
  riderConfig <- QRC.findByMerchantOperatingCityId merchantOperatingCityId Nothing >>= fromMaybeM (RiderConfigDoesNotExist $ "merchantOperatingCityId:- " <> merchantOperatingCityId.getId)

  mobileNumber <- mapM decrypt person.mobileNumber
  receiverEmail <- mapM decrypt person.email

  SOTP.sendOTP
    otpChannel
    otpCode
    person.id
    person.merchantId
    merchantOperatingCityId
    person.mobileCountryCode
    mobileNumber
    receiverEmail
    riderConfig.emailOtpConfig
    mbSenderHash

  void $ RegistrationToken.updateAttempts (attempts - 1) id
  return $ AuthRes tokenId (attempts - 1) authType Nothing Nothing person.blocked Nothing Nothing

cleanCachedTokens :: (CacheFlow m r, EsqDBFlow m r, Redis.HedisFlow m r) => Id SP.Person -> m ()
cleanCachedTokens personId = do
  regTokens <- RegistrationToken.findAllByPersonId personId
  for_ regTokens $ \regToken -> do
    let key = authTokenCacheKey regToken.token
    void $ Redis.del key

logout ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    Redis.HedisFlow m r
  ) =>
  Id SP.Person ->
  m APISuccess
logout personId = do
  cleanCachedTokens personId
  void $ Person.updateDeviceToken Nothing personId
  void $ RegistrationToken.deleteByPersonId personId
  pure AP.Success

authHitOTPChannel :: Id SP.Person -> Text
authHitOTPChannel personId = "BAP:Registration:auth" <> getId personId <> ":OtpChannel"

cachePersonOTPChannel :: (CacheFlow m r, MonadFlow m) => Id SP.Person -> SOTP.OTPChannel -> m ()
cachePersonOTPChannel personId otpChannel = do
  Hedis.setExp (authHitOTPChannel personId) otpChannel 1800 -- 30 min

getPersonOTPChannel :: (CacheFlow m r, MonadFlow m) => Id SP.Person -> m SOTP.OTPChannel
getPersonOTPChannel personId = do
  Hedis.get (authHitOTPChannel personId) >>= \case
    Just a -> pure a
    Nothing -> do
      pure SOTP.SMS

updatePersonIdForEmergencyContacts :: (CacheFlow m r, EsqDBFlow m r, EncFlow m r, EsqDBFlow m r) => Id SP.Person -> Text -> Id DMerchant.Merchant -> m ()
updatePersonIdForEmergencyContacts personId mobileNumber merchantId = do
  dbHash <- getDbHash mobileNumber
  QPDEN.updateEmergencyContactPersonId dbHash personId merchantId

generateCustomerReferralCode :: MonadFlow m => m Text
generateCustomerReferralCode = do
  aplhaNumericCode <- generateAplhaNumbericCode 5
  let referralCode = "C" <> aplhaNumericCode
  pure referralCode

createPersonWithPhoneNumber ::
  ( HasFlowEnv m r '["version" ::: DeploymentVersion],
    EncFlow m r,
    EsqDBFlow m r,
    DB.EsqDBReplicaFlow m r,
    Redis.HedisFlow m r,
    CacheFlow m r
  ) =>
  Id DMerchant.Merchant ->
  Text ->
  Maybe Text ->
  m (Id SP.Person)
createPersonWithPhoneNumber merchantId phoneNumber countryCode' = do
  merchant <- QMerchant.findById merchantId >>= fromMaybeM (MerchantNotFound $ getId merchantId)
  mobileDbHash <- getDbHash phoneNumber
  let countryCode = fromMaybe "+91" countryCode'
  findPerson <- Person.findByMobileNumberAndMerchantId countryCode mobileDbHash merchantId
  case findPerson of
    Just foundPerson -> return $ foundPerson.id
    Nothing -> do
      let authReq =
            AuthReq
              { mobileNumber = Just phoneNumber,
                mobileCountryCode = Just countryCode,
                identifierType = Just SP.MOBILENUMBER,
                merchantId = merchant.shortId,
                deviceToken = Nothing,
                notificationToken = Nothing,
                whatsappNotificationEnroll = Nothing,
                firstName = Nothing,
                middleName = Nothing,
                lastName = Nothing,
                email = Nothing,
                businessEmail = Nothing,
                language = Nothing,
                gender = Nothing,
                otpChannel = Nothing,
                registrationLat = Nothing,
                registrationLon = Nothing,
                enableOtpLessRide = Nothing,
                allowBlockedUserLogin = Nothing,
                isOperatorReq = Nothing
              }
      createdPerson <-
        createPerson authReq SP.MOBILENUMBER Nothing Nothing Nothing Nothing Nothing Nothing merchant Nothing
      pure $ createdPerson.id

---------- Business Email Verification --------
data SendBusinessEmailVerificationReq = SendBusinessEmailVerificationReq
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

data SendBusinessEmailVerificationRes = SendBusinessEmailVerificationRes
  { message :: Text
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

data VerifyBusinessEmailReq = VerifyBusinessEmailReq
  { token :: Maybe Text, -- Magic link token from email
    otp :: Maybe Text -- OTP entered by user in app
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

validateVerifyBusinessEmailReq :: Validate VerifyBusinessEmailReq
validateVerifyBusinessEmailReq VerifyBusinessEmailReq {..} =
  sequenceA_
    [ -- If OTP is provided, validate it's 4 digits
      whenJust otp $ \otpValue -> validateField "otp" otpValue $ ExactLength 4 `And` star P.digit
    ]

data VerifyBusinessEmailRes = VerifyBusinessEmailRes
  { message :: Text,
    verified :: Bool
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

sendBusinessEmailVerification ::
  ( HasFlowEnv m r '["smsCfg" ::: SmsConfig, "apiRateLimitOptions" ::: APIRateLimitOptions],
    CacheFlow m r,
    EsqDBFlow m r,
    DB.EsqDBReplicaFlow m r,
    EncFlow m r
  ) =>
  Id SP.Person ->
  Id DMerchant.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  m APISuccess
sendBusinessEmailVerification personId merchantId merchantOperatingCityId = do
  -- Rate limit check
  checkSlidingWindowLimit (businessEmailSendHitsCountKey personId)
  person <- Person.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  businessEmail <- person.businessEmail & fromMaybeM (PersonFieldNotPresent "businessEmail")
  decryptedBusinessEmail <- decrypt businessEmail
  riderConfig <- QRC.findByMerchantOperatingCityId merchantOperatingCityId Nothing >>= fromMaybeM (RiderConfigDoesNotExist $ "merchantOperatingCityId:- " <> merchantOperatingCityId.getId)
  emailBusinessVerificationConfig <- riderConfig.emailBusinessVerificationConfig & fromMaybeM (RiderConfigNotFound $ "emailBusinessVerificationConfig not found for merchantOperatingCityId:- " <> merchantOperatingCityId.getId)

  -- Generate BOTH OTP and magic link token
  otp <- generateOTPCode
  verificationToken <- L.generateGUID
  rtid <- L.generateGUID
  now <- getCurrentTime

  -- Create a RegistrationToken for business email verification
  -- Store OTP in authValueHash and magic link token in token field
  let regToken =
        SR.RegistrationToken
          { id = Id rtid,
            token = verificationToken, -- Magic link token
            attempts = 3,
            authMedium = SR.EMAIL,
            authType = SR.OTP,
            authValueHash = otp, -- Store OTP here for verification
            verified = False,
            authExpiry = 30, -- 30 minutes expiry for email verification
            tokenExpiry = 30, -- 30 minutes token expiry
            entityId = getId personId,
            merchantId = getId merchantId,
            entityType = SR.USER,
            createdAt = now,
            updatedAt = now,
            info = Just "businessEmailVerification", -- Mark this as business email verification
            createdViaPartnerOrgId = Nothing
          }

  -- Store the token
  void $ RegistrationToken.create regToken

  -- Send email with BOTH OTP and magic link
  withLogTag ("personId_" <> getId personId) $ do
    L.runIO $ Email.sendBusinessVerificationEmail emailBusinessVerificationConfig [decryptedBusinessEmail] otp verificationToken

  return AP.Success

verifyBusinessEmail ::
  ( HasFlowEnv m r '["apiRateLimitOptions" ::: APIRateLimitOptions],
    CacheFlow m r,
    EsqDBFlow m r,
    DB.EsqDBReplicaFlow m r,
    EncFlow m r
  ) =>
  Maybe (Id SP.Person) -> -- Optional personId (required for OTP verification)
  VerifyBusinessEmailReq ->
  m VerifyBusinessEmailRes
verifyBusinessEmail mbPersonId req = do
  runRequestValidation validateVerifyBusinessEmailReq req

  -- Rate limit check (use personId if available, otherwise use token for rate limiting)
  let rateLimitKey = case mbPersonId of
        Just personId -> businessEmailVerifyHitsCountKey personId
        Nothing -> maybe "BAP:Registration:businessEmail:verify:unknown:hitsCount" (\t -> "BAP:Registration:businessEmail:verify:token:" <> t <> ":hitsCount") req.token
  checkSlidingWindowLimit rateLimitKey

  -- Determine verification method and get registration token
  regToken <- case (req.token, req.otp, mbPersonId) of
    -- Magic Link verification
    (Just tokenValue, Nothing, _) -> do
      rt <- RegistrationToken.findByToken tokenValue >>= fromMaybeM (InvalidRequest "Invalid verification token")
      -- Check if it's for business email verification
      unless (rt.info == Just "businessEmailVerification") $ throwError (InvalidRequest "Invalid token type")
      -- Check if already verified
      when rt.verified $ throwError (AuthBlocked "Email already verified")
      return rt

    -- OTP verification
    (Nothing, Just otpValue, Just personId) -> do
      -- Find latest business email verification token for this person
      regTokens <- RegistrationToken.findAllByPersonId personId
      let sortedTokens = sortOn (Down . (.createdAt)) regTokens -- Sort by createdAt descending to get latest first
      rt <-
        find (\rt -> rt.info == Just "businessEmailVerification" && not rt.verified && rt.authValueHash == otpValue) sortedTokens
          & fromMaybeM (InvalidRequest "No pending business email verification found")
      return rt

    -- OTP without personId
    (Nothing, Just _, Nothing) ->
      throwError (InvalidRequest "Person ID is required for OTP verification")
    -- Invalid combination
    _ ->
      throwError (InvalidRequest "Either token or otp must be provided")

  -- Check expiry
  whenM (isExpired (realToFrac (regToken.authExpiry * 60)) regToken.updatedAt) $
    throwError BusinessEmailTokenExpired

  -- Get person
  person <- Person.findById (Id regToken.entityId) >>= fromMaybeM (PersonNotFound regToken.entityId)

  -- Mark as verified
  void $ RegistrationToken.setVerified True regToken.id
  void $ PersonExtra.updateBusinessProfileVerified person.id True

  return $ VerifyBusinessEmailRes {message = "Business email verified successfully", verified = True}

resendBusinessEmailVerification ::
  ( HasFlowEnv m r '["smsCfg" ::: SmsConfig, "apiRateLimitOptions" ::: APIRateLimitOptions],
    CacheFlow m r,
    EsqDBFlow m r,
    DB.EsqDBReplicaFlow m r,
    EncFlow m r
  ) =>
  Id SP.Person ->
  Id DMerchant.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  m APISuccess
resendBusinessEmailVerification personId _merchantId merchantOperatingCityId = do
  person <- Person.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  businessEmail <- person.businessEmail & fromMaybeM (PersonFieldNotPresent "businessEmail")
  decryptedBusinessEmail <- decrypt businessEmail

  -- Check if person's business email is already verified
  when (person.businessProfileVerified == Just True) $
    throwError $ AuthBlocked "Business email already verified"

  riderConfig <- QRC.findByMerchantOperatingCityId merchantOperatingCityId Nothing >>= fromMaybeM (RiderConfigDoesNotExist $ "merchantOperatingCityId:- " <> merchantOperatingCityId.getId)
  emailBusinessVerificationConfig <- riderConfig.emailBusinessVerificationConfig & fromMaybeM (RiderConfigNotFound $ "emailBusinessVerificationConfig not found for merchantOperatingCityId:- " <> merchantOperatingCityId.getId)

  -- Find latest business email verification token for this person
  regTokens <- RegistrationToken.findAllByPersonId personId
  let sortedTokens = sortOn (Down . (.createdAt)) regTokens -- Sort by createdAt descending to get latest first
  regToken <-
    find (\rt -> rt.info == Just "businessEmailVerification" && not rt.verified) sortedTokens
      & fromMaybeM (InvalidRequest "No pending business email verification found. Please send a new verification request.")

  -- Check if attempts are remaining
  unless (regToken.attempts > 0) $ throwError $ AuthBlocked "Resend attempts limit exceeded. Please request a new verification."

  -- Check sliding window limit (similar to phone OTP resend)
  checkSlidingWindowLimit (verifyHitsCountKey personId)

  -- Check expiry - if expired, throw error asking for new verification
  whenM (isExpired (realToFrac (regToken.authExpiry * 60)) regToken.updatedAt) $
    throwError $ InvalidRequest "Verification token expired. Please request a new verification."

  -- Generate new OTP and magic link token
  newOtp <- generateOTPCode
  newVerificationToken <- L.generateGUID
  now <- getCurrentTime

  -- Update the registration token with new OTP, token, decremented attempts, and updated timestamp
  void $ RegistrationToken.updateOtpAndToken regToken.id newOtp newVerificationToken (regToken.attempts - 1) now

  -- Send email with new OTP and magic link
  withLogTag ("personId_" <> getId personId) $ do
    L.runIO $ Email.sendBusinessVerificationEmail emailBusinessVerificationConfig [decryptedBusinessEmail] newOtp newVerificationToken

  return AP.Success
