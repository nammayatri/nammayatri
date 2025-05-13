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
    OTPChannel (..),
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
  )
where

import qualified Data.Aeson as A
import Data.Aeson.Types ((.:), (.:?))
import Data.Maybe (listToMaybe)
import Data.OpenApi hiding (email, info)
import qualified Data.Text.Encoding as TE
import Domain.Action.UI.Person as SP
import Domain.Types.Merchant (Merchant)
import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.PartnerOrganization as DPO
import Domain.Types.Person (PersonE (updatedAt))
import qualified Domain.Types.Person as SP
import qualified Domain.Types.PersonStats as DPS
import Domain.Types.RegistrationToken (RegistrationToken)
import qualified Domain.Types.RegistrationToken as SR
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
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer)
import Kernel.Tools.Metrics.CoreMetrics
import Kernel.Types.APISuccess as AP
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common hiding (id)
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
import qualified SharedLogic.MerchantConfig as SMC
import qualified SharedLogic.MessageBuilder as MessageBuilder
import qualified SharedLogic.Person as SLP
import qualified Storage.CachedQueries.Merchant as QMerchant
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.MerchantServiceUsageConfig as QMSUC
import qualified Storage.CachedQueries.Merchant.RiderConfig as QRC
import qualified Storage.CachedQueries.MerchantConfig as CQM
import qualified Storage.CachedQueries.Person as CQP
import qualified Storage.Queries.Person as Person
import qualified Storage.Queries.PersonDefaultEmergencyNumber as QPDEN
import qualified Storage.Queries.PersonDisability as PDisability
import qualified Storage.Queries.PersonStats as QPS
import qualified Storage.Queries.RegistrationToken as RegistrationToken
import Storage.Queries.SafetySettings as QSafety
import Tools.Auth (authTokenCacheKey, decryptAES128)
import Tools.Error
import qualified Tools.Notifications as Notify
import qualified Tools.SMS as Sms
import Tools.Whatsapp
import qualified Tools.Whatsapp as Whatsapp

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
    language :: Maybe Maps.Language,
    gender :: Maybe SP.Gender,
    otpChannel :: Maybe OTPChannel,
    registrationLat :: Maybe Double,
    registrationLon :: Maybe Double,
    enableOtpLessRide :: Maybe Bool,
    allowBlockedUserLogin :: Maybe Bool
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
        <*> obj .:? "language"
        <*> obj .:? "gender"
        <*> obj .:? "otpChannel"
        <*> obj .:? "registrationLat"
        <*> obj .:? "registrationLon"
        <*> obj .:? "enableOtpLessRide"
        <*> obj .:? "allowBlockedUserLogin"
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

data AuthRes = AuthRes
  { authId :: Id RegistrationToken,
    attempts :: Int,
    authType :: SR.LoginType,
    token :: Maybe Text,
    person :: Maybe PersonAPIEntity,
    isPersonBlocked :: Bool
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

-- Need to have discussion around this
data OTPChannel = SMS | WHATSAPP | EMAIL
  deriving (Generic, Show, Enum, Eq, FromJSON, ToJSON, ToSchema)

defaultOTPChannel :: OTPChannel
defaultOTPChannel = SMS

type ResendAuthRes = AuthRes

---------- Verify Login --------
data AuthVerifyReq = AuthVerifyReq
  { otp :: Text,
    deviceToken :: Text,
    whatsappNotificationEnroll :: Maybe Whatsapp.OptApiMethods
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
  ( HasFlowEnv m r ["apiRateLimitOptions" ::: APIRateLimitOptions, "smsCfg" ::: SmsConfig, "version" ::: DeploymentVersion],
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
  m AuthRes
auth req' mbBundleVersion mbClientVersion mbClientConfigVersion mbRnVersion mbDevice = do
  let req = if req'.merchantId.getShortId == "YATRI" then req' {merchantId = ShortId "NAMMA_YATRI"} else req'
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
            otpChannel = fromMaybe defaultOTPChannel req.otpChannel
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
        return (person, EMAIL)
      SP.AADHAAR -> throwError $ InvalidRequest "Aadhaar auth is not supported"

  when (fromMaybe False person.authBlocked && maybe False (now <) person.blockedUntil) $ throwError TooManyHitsLimitError
  void $ cachePersonOTPChannel person.id otpChannel
  let merchantOperatingCityId = person.merchantOperatingCityId
  riderConfig <- QRC.findByMerchantOperatingCityId merchantOperatingCityId Nothing >>= fromMaybeM (RiderConfigDoesNotExist $ "merchantOperatingCityId:- " <> merchantOperatingCityId.getId)
  merchantConfigs <- CQM.findAllByMerchantOperatingCityId merchantOperatingCityId Nothing
  fork "Fraud Auth Check Processing" $ do
    when (fromMaybe False person.authBlocked || maybe False (now >) person.blockedUntil) $ Person.updatingAuthEnabledAndBlockedState person.id Nothing (Just False) Nothing
    SMC.updateCustomerAuthCounters person.id merchantConfigs
    (isFraudDetected, mbMerchantConfigId) <- SMC.checkAuthFraud merchantConfigs person.id
    when isFraudDetected $ do
      whenJust mbMerchantConfigId $ \mcId ->
        SMC.customerAuthBlock person.id (Just mcId) riderConfig.blockedUntilInMins

  checkSlidingWindowLimit (authHitsCountKey person)
  smsCfg <- asks (.smsCfg)
  let entityId = getId $ person.id
      useFakeOtpM = (show <$> useFakeSms smsCfg) <|> person.useFakeOtp
      scfg = sessionConfig smsCfg
  let mkId = getId $ merchant.id
  regToken <- makeSession (castChannelToMedium otpChannel) scfg entityId mkId useFakeOtpM
  if (fromMaybe False req.allowBlockedUserLogin) || not person.blocked
    then do
      deploymentVersion <- asks (.version)
      void $ Person.updatePersonVersions person mbBundleVersion mbClientVersion mbClientConfigVersion (getDeviceFromText mbDevice) deploymentVersion.getDeploymentVersion mbRnVersion
      RegistrationToken.create regToken
      when (isNothing useFakeOtpM) $ do
        let otpCode = SR.authValueHash regToken
        let otpHash = smsCfg.credConfig.otpHash
        case otpChannel of
          SMS -> do
            countryCode <- req.mobileCountryCode & fromMaybeM (InvalidRequest "MobileCountryCode is required for SMS OTP channel")
            mobileNumber <- req.mobileNumber & fromMaybeM (InvalidRequest "MobileCountryCode is required for SMS OTP channel")
            let phoneNumber = countryCode <> mobileNumber
            withLogTag ("personId_" <> getId person.id) $ do
              buildSmsReq <-
                MessageBuilder.buildSendOTPMessage merchantOperatingCityId $
                  MessageBuilder.BuildSendOTPMessageReq
                    { otp = otpCode,
                      hash = otpHash
                    }
              Sms.sendSMS person.merchantId merchantOperatingCityId (buildSmsReq phoneNumber)
                >>= Sms.checkSmsResult
          WHATSAPP -> do
            countryCode <- req.mobileCountryCode & fromMaybeM (InvalidRequest "MobileCountryCode is required for WHATSAPP OTP channel")
            mobileNumber <- req.mobileNumber & fromMaybeM (InvalidRequest "MobileCountryCode is required for WHATSAPP OTP channel")
            let phoneNumber = countryCode <> mobileNumber
            withLogTag ("personId_" <> getId person.id) $ do
              void $ callWhatsappOptApi phoneNumber person.id merchant.id (Just Whatsapp.OPT_IN)
              result <- Whatsapp.whatsAppOtpApi person.merchantId merchantOperatingCityId (Whatsapp.SendOtpApiReq phoneNumber otpCode)
              when (result._response.status /= "success") $ throwError (InternalError "Unable to send Whatsapp OTP message")
          EMAIL -> withLogTag ("personId_" <> getId person.id) $ do
            receiverEmail <- req.email & fromMaybeM (InvalidRequest "Email is required for EMAIL OTP channel")
            emailOTPConfig <- riderConfig.emailOtpConfig & fromMaybeM (RiderConfigNotFound $ "merchantOperatingCityId:- " <> merchantOperatingCityId.getId)
            L.runIO $ Email.sendEmail emailOTPConfig [receiverEmail] otpCode
    else logInfo $ "Person " <> getId person.id <> " is not enabled. Skipping send OTP"
  return $ AuthRes regToken.id regToken.attempts regToken.authType Nothing Nothing person.blocked
  where
    castChannelToMedium :: OTPChannel -> SR.Medium
    castChannelToMedium SMS = SR.SMS
    castChannelToMedium WHATSAPP = SR.WHATSAPP
    castChannelToMedium EMAIL = SR.EMAIL

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
  regToken <- makeSession SR.SIGNATURE scfg entityId mkId useFakeOtpM
  if not person.blocked
    then do
      deploymentVersion <- asks (.version)
      void $ Person.updatePersonVersions person mbBundleVersion mbClientVersion mbClientConfigVersion (getDeviceFromText mbDevice) deploymentVersion.getDeploymentVersion mbRnVersion
      _ <- RegistrationToken.create regToken
      mbEncEmail <- encrypt `mapM` reqWithMobileNumebr.email
      _ <- RegistrationToken.setDirectAuth regToken.id SR.SIGNATURE
      _ <- Person.updatePersonalInfo person.id (reqWithMobileNumebr.firstName <|> person.firstName <|> Just "User") reqWithMobileNumebr.middleName reqWithMobileNumebr.lastName mbEncEmail deviceToken notificationToken (reqWithMobileNumebr.language <|> person.language <|> Just Language.ENGLISH) (reqWithMobileNumebr.gender <|> Just person.gender) mbRnVersion (mbClientVersion <|> Nothing) (mbBundleVersion <|> Nothing) mbClientConfigVersion (getDeviceFromText mbDevice) deploymentVersion.getDeploymentVersion person.enableOtpLessRide Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing person Nothing
      personAPIEntity <- verifyFlow person regToken reqWithMobileNumebr.whatsappNotificationEnroll deviceToken
      return $ AuthRes regToken.id regToken.attempts SR.DIRECT (Just regToken.token) (Just personAPIEntity) person.blocked
    else return $ AuthRes regToken.id regToken.attempts regToken.authType Nothing Nothing person.blocked

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
        juspayCustomerPaymentID = Nothing,
        defaultPaymentMethodId = Nothing,
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
        imeiNumber = Nothing, -- TODO: take it from the request
        blockSource = Nothing, -- useFraudDetection && isBlockedBySameDeviceToken => fraud system
        blockedReason = Nothing
      }

-- FIXME Why do we need to store always the same authExpiry and tokenExpiry from config? info field is always Nothing
makeSession ::
  MonadFlow m =>
  SR.Medium ->
  SmsSessionConfig ->
  Text ->
  Text ->
  Maybe Text ->
  m SR.RegistrationToken
makeSession authMedium SmsSessionConfig {..} entityId merchantId fakeOtp = do
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
        verified = False,
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
    when merchantConfig.useFraudDetection $ SMC.blockCustomer person.id ((.blockedByRuleId) =<< personWithSameDeviceToken) (Just SP.DEVICE_TOKEN) Nothing
  void $ RegistrationToken.setVerified True tokenId
  void $ Person.updateDeviceToken deviceToken person.id
  personAPIEntity <- verifyFlow person regToken req.whatsappNotificationEnroll deviceToken
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
  fork "update emergency contact id" $
    whenJust req.mobileNumber $ \mobileNumber -> updatePersonIdForEmergencyContacts person.id mobileNumber merchant.id
  pure person
  where
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
  ( HasFlowEnv m r ["apiRateLimitOptions" ::: APIRateLimitOptions, "smsCfg" ::: SmsConfig],
    EsqDBFlow m r,
    EncFlow m r,
    CacheFlow m r,
    HasKafkaProducer r
  ) =>
  Id SR.RegistrationToken ->
  m ResendAuthRes
resend tokenId = do
  SR.RegistrationToken {..} <- getRegistrationTokenE tokenId
  person <- checkPersonExists entityId
  unless (attempts > 0) $ throwError $ AuthBlocked "Attempts limit exceed."
  smsCfg <- asks (.smsCfg)
  let merchantOperatingCityId = person.merchantOperatingCityId
  let otpCode = authValueHash
  let otpHash = smsCfg.credConfig.otpHash
  otpChannel <- getPersonOTPChannel person.id
  case otpChannel of
    SMS -> do
      mobileNumber <- mapM decrypt person.mobileNumber >>= fromMaybeM (PersonFieldNotPresent "mobileNumber")
      countryCode <- person.mobileCountryCode & fromMaybeM (PersonFieldNotPresent "mobileCountryCode")
      let phoneNumber = countryCode <> mobileNumber
      withLogTag ("personId_" <> getId person.id) $ do
        buildSmsReq <-
          MessageBuilder.buildSendOTPMessage merchantOperatingCityId $
            MessageBuilder.BuildSendOTPMessageReq
              { otp = otpCode,
                hash = otpHash
              }
        Sms.sendSMS person.merchantId merchantOperatingCityId (buildSmsReq phoneNumber)
          >>= Sms.checkSmsResult
    WHATSAPP -> do
      mobileNumber <- mapM decrypt person.mobileNumber >>= fromMaybeM (PersonFieldNotPresent "mobileNumber")
      countryCode <- person.mobileCountryCode & fromMaybeM (PersonFieldNotPresent "mobileCountryCode")
      let phoneNumber = countryCode <> mobileNumber
      withLogTag ("personId_" <> getId person.id) $ do
        result <- Whatsapp.whatsAppOtpApi person.merchantId merchantOperatingCityId (Whatsapp.SendOtpApiReq phoneNumber otpCode)
        when (result._response.status /= "success") $ throwError (InternalError "Unable to send Whatsapp OTP message")
    EMAIL -> withLogTag ("personId_" <> getId person.id) $ do
      receiverEmail <- mapM decrypt person.email >>= fromMaybeM (PersonFieldNotPresent "email")
      riderConfig <- QRC.findByMerchantOperatingCityId merchantOperatingCityId Nothing >>= fromMaybeM (RiderConfigDoesNotExist $ "merchantOperatingCityId:- " <> merchantOperatingCityId.getId)
      emailOTPConfig <- riderConfig.emailOtpConfig & fromMaybeM (RiderConfigNotFound $ "merchantOperatingCityId:- " <> merchantOperatingCityId.getId)
      L.runIO $ Email.sendEmail emailOTPConfig [receiverEmail] otpCode

  void $ RegistrationToken.updateAttempts (attempts - 1) id
  return $ AuthRes tokenId (attempts - 1) authType Nothing Nothing person.blocked

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

cachePersonOTPChannel :: (CacheFlow m r, MonadFlow m) => Id SP.Person -> OTPChannel -> m ()
cachePersonOTPChannel personId otpChannel = do
  Hedis.setExp (authHitOTPChannel personId) otpChannel 1800 -- 30 min

getPersonOTPChannel :: (CacheFlow m r, MonadFlow m) => Id SP.Person -> m OTPChannel
getPersonOTPChannel personId = do
  Hedis.get (authHitOTPChannel personId) >>= \case
    Just a -> pure a
    Nothing -> do
      pure SMS -- default otpChannel is SMS (for resend)

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
                language = Nothing,
                gender = Nothing,
                otpChannel = Nothing,
                registrationLat = Nothing,
                registrationLon = Nothing,
                enableOtpLessRide = Nothing,
                allowBlockedUserLogin = Nothing
              }
      createdPerson <-
        createPerson authReq SP.MOBILENUMBER Nothing Nothing Nothing Nothing Nothing Nothing merchant Nothing
      pure $ createdPerson.id
